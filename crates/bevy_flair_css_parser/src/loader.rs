use bevy::asset::io::Reader;
use bevy::asset::{AssetLoader, AsyncReadExt, LoadContext};
use bevy::log::warn;
use bevy::reflect::TypeRegistryArc;
use bevy_flair_core::{ComponentPropertyRef, PropertiesHashMap, PropertiesRegistry};
use bevy_flair_style::{
    AnimationKeyframesBuilder, StyleSheet, StyleSheetBuilder, StyleSheetBuilderError,
};
use thiserror::Error;

use crate::error::ErrorReportGenerator;
use crate::parser::{
    parse_css, AnimationKeyFrame, CssAnimation, CssRulesetProperty, CssStyleSheetItem,
    CssTransitionProperty,
};

/// Error that could happen while loading a CSS stylesheet using [`CssStyleLoaderError`].
#[derive(Debug, Error)]
#[error(transparent)]
pub enum CssStyleLoaderError {
    /// Error that could happen while reading the file.
    Io(#[from] std::io::Error),
    /// Error that could happen while parsing the CSS stylesheet.
    StyleSheetBuilder(#[from] StyleSheetBuilderError),
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
    type Settings = ();
    type Error = CssStyleLoaderError;

    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &Self::Settings,
        load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut contents = String::new();
        reader.read_to_string(&mut contents).await?;

        let path_display = load_context.path().display().to_string();
        let type_registry = self.type_registry_arc.read();

        let mut builder = StyleSheetBuilder::new();
        let mut report_generator = ErrorReportGenerator::new(&path_display, &contents);

        parse_css(
            &type_registry,
            &self.properties_registry,
            &contents,
            |item| {
                match item {
                    CssStyleSheetItem::RuleSet(ruleset) => {
                        match ruleset.selectors {
                            Ok(selectors) => {
                                debug_assert!(!selectors.is_empty());

                                let mut ruleset_builder = builder.new_ruleset();

                                for selector in selectors {
                                    ruleset_builder.add_css_selector(selector);
                                }

                                for property in ruleset.properties {
                                    match property {
                                        CssRulesetProperty::SingleProperty(id, value) => {
                                            ruleset_builder.add_properties([(
                                                ComponentPropertyRef::Id(id),
                                                value,
                                            )]);
                                        }
                                        CssRulesetProperty::Transitions(property_transitions) => {
                                            for transition_fallible in property_transitions {
                                                match transition_fallible {
                                                    Ok(CssTransitionProperty {
                                                        property,
                                                        options,
                                                    }) => {
                                                        ruleset_builder.add_property_transition(
                                                            property, options,
                                                        );
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
                                                        ruleset_builder
                                                            .add_animation(name, options);
                                                    }
                                                    Err(error) => {
                                                        report_generator.add_error(error);
                                                    }
                                                }
                                            }
                                        }
                                        CssRulesetProperty::Error(error) => {
                                            report_generator.add_error(error);
                                        }
                                    }
                                }
                            }
                            Err(selectors_error) => {
                                report_generator.add_error(selectors_error);
                                // We just try to find the error properties to report them too
                                for property in ruleset.properties {
                                    if let CssRulesetProperty::Error(error) = property {
                                        report_generator.add_error(error);
                                    }
                                }
                            }
                        }
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
                            PropertiesHashMap::<AnimationKeyframesBuilder>::new();

                        for k in keyframes.keyframes {
                            match k {
                                AnimationKeyFrame::Valid { times, properties } => {
                                    for property in properties {
                                        match property {
                                            CssRulesetProperty::SingleProperty(
                                                property_id,
                                                sample,
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
                }
            },
        );

        if !report_generator.is_empty() {
            warn!("\n{}", report_generator.into_message());
        }

        Ok(builder.build_with_load_context(&self.properties_registry, load_context)?)
    }

    fn extensions(&self) -> &[&str] {
        Self::EXTENSIONS
    }
}
