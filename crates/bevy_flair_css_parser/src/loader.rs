use crate::error::ErrorReportGenerator;
use crate::parser::{
    AnimationKeyFrame, CssAnimation, CssRuleset, CssRulesetProperty, CssStyleSheetItem,
    CssTransitionProperty, parse_css,
};
use bevy::asset::io::Reader;
use bevy::asset::{AssetLoader, AsyncReadExt, LoadContext};
use bevy::log::{error, warn};
use bevy::reflect::TypeRegistryArc;
use bevy_flair_core::{ComponentPropertyRef, PropertiesHashMap, PropertiesRegistry, PropertyValue};
use bevy_flair_style::css_selector::CssSelector;
use bevy_flair_style::{
    AnimationKeyframesBuilder, StyleSheet, StyleSheetBuilder, StyleSheetBuilderError,
};
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Error that could happen while loading a CSS stylesheet using [`CssStyleLoaderError`].
#[derive(Debug, Error)]
pub enum CssStyleLoaderError {
    /// Error that could happen while reading the file.
    #[error(transparent)]
    Io(#[from] std::io::Error),
    /// Error that could happen while building the final StyleSheet.
    #[error(transparent)]
    StyleSheetBuilder(#[from] StyleSheetBuilderError),
    /// Css reported errors when error mode is set to [`CssStyleLoaderErrorMode::ReturnError`].
    #[error("{0}")]
    Report(String),
}

/// Different ways to handle css errors
#[derive(Debug, Copy, Clone, Default, Serialize, Deserialize)]
pub enum CssStyleLoaderErrorMode {
    /// Prints all errors in warn!()
    #[default]
    PrintWarn,
    /// Prints all errors in error!()
    PrintError,
    /// Ignore all errors
    Ignore,
    /// Return report as [`CssStyleLoaderError::Report`]
    ReturnError,
}

/// Error that could happen while loading a CSS stylesheet using [`CssStyleLoaderError`].
#[derive(Debug, Copy, Clone, Default, Serialize, Deserialize)]
pub struct CssStyleLoaderSetting {
    /// How to report errors
    #[serde(default)]
    pub error_mode: CssStyleLoaderErrorMode,
}

/// An [`AssetLoader`] for CSS stylesheets.
/// It capable of loading `.css` files.
pub struct CssStyleLoader {
    type_registry_arc: TypeRegistryArc,
    properties_registry: PropertiesRegistry,
}

impl CssStyleLoader {
    /// Extensions that this loader can load. basically `.css`
    pub const EXTENSIONS: &'static [&'static str] = &["css"];

    /// Creates a new [`CssStyleLoader`] with the given [`TypeRegistryArc`] and [`PropertiesRegistry`].
    pub fn new(
        type_registry_arc: TypeRegistryArc,
        properties_registry: PropertiesRegistry,
    ) -> Self {
        Self {
            type_registry_arc,
            properties_registry,
        }
    }
}

impl AssetLoader for CssStyleLoader {
    type Asset = StyleSheet;
    type Settings = CssStyleLoaderSetting;
    type Error = CssStyleLoaderError;

    async fn load(
        &self,
        reader: &mut dyn Reader,
        settings: &Self::Settings,
        load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut contents = String::new();
        reader.read_to_string(&mut contents).await?;

        let path_display = load_context.path().display().to_string();
        let type_registry = self.type_registry_arc.read();

        let mut builder = StyleSheetBuilder::new();
        let mut report_generator = ErrorReportGenerator::new(&path_display, &contents);

        fn report_property_errors_recursively(
            properties: Vec<CssRulesetProperty>,
            report_generator: &mut ErrorReportGenerator,
        ) {
            for property in properties {
                match property {
                    CssRulesetProperty::NestedRuleset(nested) => {
                        if let Err(selectors_error) = nested.selectors {
                            report_generator.add_error(selectors_error);
                        }
                        report_property_errors_recursively(nested.properties, report_generator);
                    }
                    CssRulesetProperty::Error(error) => {
                        report_generator.add_error(error);
                    }
                    _ => {}
                }
            }
        }

        fn process_ruleset_recursively(
            ruleset: CssRuleset,
            parent_selectors: Option<&Vec<CssSelector>>,
            builder: &mut StyleSheetBuilder,
            report_generator: &mut ErrorReportGenerator,
        ) {
            match ruleset.selectors {
                Ok(selectors) => {
                    debug_assert!(!selectors.is_empty());
                    let mut ruleset_builder = builder.new_ruleset();
                    let mut nested_rulesets = Vec::new();

                    let selectors = match parent_selectors {
                        None => selectors,
                        Some(parent_selectors) => selectors
                            .into_iter()
                            .map(|s| s.replace_parent_selector(parent_selectors))
                            .collect(),
                    };

                    for selector in selectors.iter().cloned() {
                        ruleset_builder.add_css_selector(selector);
                    }

                    for property in ruleset.properties {
                        match property {
                            CssRulesetProperty::SingleProperty(id, value) => {
                                ruleset_builder
                                    .add_properties([(ComponentPropertyRef::Id(id), value)]);
                            }
                            CssRulesetProperty::Transitions(property_transitions) => {
                                for transition_fallible in property_transitions {
                                    match transition_fallible {
                                        Ok(CssTransitionProperty { property, options }) => {
                                            ruleset_builder
                                                .add_property_transition(property, options);
                                        }
                                        Err(error) => {
                                            report_generator.add_error(error);
                                        }
                                    }
                                }
                            }
                            CssRulesetProperty::Animations(animations) => {
                                for animation_fallible in animations {
                                    match animation_fallible {
                                        Ok(CssAnimation { name, options }) => {
                                            ruleset_builder.add_animation(name, options);
                                        }
                                        Err(error) => {
                                            report_generator.add_error(error);
                                        }
                                    }
                                }
                            }
                            CssRulesetProperty::NestedRuleset(nested_ruleset) => {
                                nested_rulesets.push(nested_ruleset);
                            }
                            CssRulesetProperty::Error(error) => {
                                report_generator.add_error(error);
                            }
                        }
                    }
                    for nested_ruleset in nested_rulesets {
                        process_ruleset_recursively(
                            nested_ruleset,
                            Some(&selectors),
                            builder,
                            report_generator,
                        );
                    }
                }
                Err(selectors_error) => {
                    report_generator.add_error(selectors_error);
                    // We just try to find the error properties to report them too
                    report_property_errors_recursively(ruleset.properties, report_generator);
                }
            }
        }

        parse_css(
            &type_registry,
            &self.properties_registry,
            &contents,
            |item| match item {
                CssStyleSheetItem::RuleSet(ruleset) => {
                    process_ruleset_recursively(ruleset, None, &mut builder, &mut report_generator);
                }
                CssStyleSheetItem::FontFace(font_face) => {
                    for error in font_face.errors {
                        report_generator.add_error(error);
                    }
                    builder.register_font_face(font_face.family_name, font_face.source);
                }
                CssStyleSheetItem::AnimationKeyFrames(keyframes) => {
                    let name = keyframes.name;

                    let mut keyframes_per_property =
                        PropertiesHashMap::<AnimationKeyframesBuilder>::default();

                    for k in keyframes.keyframes {
                        match k {
                            AnimationKeyFrame::Valid { times, properties } => {
                                for property in properties {
                                    match property {
                                        CssRulesetProperty::SingleProperty(
                                            property_id,
                                            PropertyValue::Value(sample),
                                        ) => {
                                            for time in &times {
                                                keyframes_per_property
                                                    .entry(property_id)
                                                    .or_default()
                                                    .add_keyframe_reflect_value(
                                                        *time,
                                                        sample.clone(),
                                                    );
                                            }
                                        }
                                        CssRulesetProperty::Error(error) => {
                                            report_generator.add_error(error);
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            AnimationKeyFrame::Error(error) => {
                                report_generator.add_error(error);
                            }
                        }
                    }

                    builder.add_animation_keyframes(
                        name,
                        keyframes_per_property
                            .into_iter()
                            .filter_map(|(p, b)| Some((p, b.build().ok()?))),
                    );
                }
                CssStyleSheetItem::Error(error) => {
                    report_generator.add_error(error);
                }
            },
        );

        if !report_generator.is_empty() {
            match settings.error_mode {
                CssStyleLoaderErrorMode::PrintWarn => {
                    warn!("\n{}", report_generator.into_message());
                }
                CssStyleLoaderErrorMode::PrintError => {
                    error!("\n{}", report_generator.into_message());
                }
                CssStyleLoaderErrorMode::ReturnError => {
                    return Err(CssStyleLoaderError::Report(report_generator.into_message()));
                }
                CssStyleLoaderErrorMode::Ignore => {}
            }
        }

        Ok(builder.build_with_load_context(&self.properties_registry, load_context)?)
    }

    fn extensions(&self) -> &[&str] {
        Self::EXTENSIONS
    }
}
