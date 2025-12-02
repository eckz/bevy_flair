use crate::parser::{
    CssRuleset, CssRulesetProperty, CssStyleSheetItem, ParserAnimationKeyFrame, parse_css,
};
use crate::utils::ImportantLevel;
use crate::{
    CssStyleLoaderError, CssStyleLoaderErrorMode, ErrorReportGenerator, ShorthandPropertyRegistry,
};
use std::sync::LazyLock;

use bevy_flair_core::{CssPropertyRegistry, PropertyRegistry};
use bevy_flair_style::animations::{AnimationKeyframes, AnimationProperty, AnimationPropertyId};
use bevy_flair_style::css_selector::CssSelector;
use bevy_flair_style::{RulesetBuilder, StyleBuilderProperty, StyleSheet, StyleSheetBuilder};
use bevy_reflect::TypeRegistry;
use rustc_hash::FxHashMap;
use tracing::{error, info, warn};

pub(crate) struct InternalStylesheetLoader<'a> {
    pub(crate) type_registry: &'a TypeRegistry,
    pub(crate) property_registry: &'a PropertyRegistry,
    pub(crate) css_property_registry: &'a CssPropertyRegistry,
    pub(crate) shorthand_property_registry: &'a ShorthandPropertyRegistry,
    pub(crate) error_mode: CssStyleLoaderErrorMode,
    pub(crate) imports: &'a FxHashMap<String, StyleSheet>,
}

// This is temporal until https://github.com/tokio-rs/tracing/issues/3378 is fixed
const NO_COLOR_REPORT_CONFIG: ariadne::Config = ariadne::Config::new().with_color(false);

static EMPTY_PROPERTY_REGISTRY: LazyLock<PropertyRegistry> =
    LazyLock::new(PropertyRegistry::default);

fn report_important_level(report_generator: &mut ErrorReportGenerator, level: ImportantLevel) {
    if let ImportantLevel::Important(location) = level {
        report_generator.add_advice(
            location,
            "!important is not supported",
            "!important token is being ignored, so you can remove it",
        );
    }
}

pub fn process_ruleset_property<B: RulesetBuilder>(
    property: CssRulesetProperty,
    ruleset_builder: &mut B,
    report_generator: &mut ErrorReportGenerator,
) {
    match property {
        CssRulesetProperty::SingleProperty(id, value, important_level) => {
            ruleset_builder.add_property(StyleBuilderProperty::new(id, value));
            report_important_level(report_generator, important_level);
        }
        CssRulesetProperty::MultipleProperties(properties, important_level) => {
            ruleset_builder.add_properties(
                properties
                    .into_iter()
                    .map(|(id, value)| StyleBuilderProperty::new(id, value)),
            );
            report_important_level(report_generator, important_level);
        }
        CssRulesetProperty::DynamicProperty(css_name, parser, tokens, important_level) => {
            ruleset_builder.add_property(StyleBuilderProperty::Dynamic {
                css_name,
                parser,
                tokens,
            });
            report_important_level(report_generator, important_level);
        }
        CssRulesetProperty::AnimationProperty(property) => {
            ruleset_builder.add_animation_property(property);
        }
        CssRulesetProperty::TransitionProperty(property) => {
            ruleset_builder.add_transition_property(property);
        }
        CssRulesetProperty::Var(var_name, tokens) => {
            ruleset_builder.add_var(var_name, tokens);
        }
        CssRulesetProperty::NestedRuleset(_) => {
            unreachable!("`process_property` shouldn't be called with `NestedRuleset`");
        }
        CssRulesetProperty::Error(error) => {
            report_generator.add_error(error);
        }
    }
}

impl InternalStylesheetLoader<'_> {
    pub(crate) fn load_stylesheet(
        &self,
        path_name: &str,
        contents: &str,
    ) -> Result<StyleSheetBuilder, CssStyleLoaderError> {
        let mut builder = StyleSheetBuilder::new();
        let mut report_generator =
            ErrorReportGenerator::new_with_config(path_name, contents, NO_COLOR_REPORT_CONFIG);

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
                            CssRulesetProperty::NestedRuleset(nested_ruleset) => {
                                process_ruleset_recursively(
                                    nested_ruleset,
                                    Some(&selectors),
                                    builder,
                                    report_generator,
                                );
                                ruleset_builder = builder.new_ruleset();
                                for selector in selectors.iter().cloned() {
                                    ruleset_builder.add_css_selector(selector);
                                }
                            }
                            property => {
                                process_ruleset_property(
                                    property,
                                    &mut ruleset_builder,
                                    report_generator,
                                );
                            }
                        }
                    }
                }
                Err(selectors_error) => {
                    report_generator.add_error(selectors_error);
                    // We just try to find the error properties to report them too
                    report_property_errors_recursively(ruleset.properties, report_generator);
                }
            }
        }

        fn processor(
            item: CssStyleSheetItem,
            builder: &mut StyleSheetBuilder,
            report_generator: &mut ErrorReportGenerator,
        ) {
            match item {
                CssStyleSheetItem::EmbedStylesheet(style_sheet, layer) => {
                    builder.embed_style_sheet(style_sheet, layer);
                }
                CssStyleSheetItem::Inner(items) => {
                    for item in items {
                        processor(item, builder, report_generator);
                    }
                }
                CssStyleSheetItem::LayersDefinition(layers) => {
                    builder.define_layers(&layers);
                }
                CssStyleSheetItem::RuleSet(ruleset) => {
                    process_ruleset_recursively(ruleset, None, builder, report_generator);
                }
                CssStyleSheetItem::FontFace(font_face) => {
                    for error in font_face.errors {
                        report_generator.add_error(error);
                    }
                    builder.register_font_face(font_face.family_name, font_face.source);
                }
                CssStyleSheetItem::AnimationKeyFrames(keyframes) => {
                    let mut keyframes_builder = AnimationKeyframes::builder(keyframes.name);

                    for k in keyframes.keyframes {
                        match k {
                            ParserAnimationKeyFrame::Valid { times, properties } => {
                                for property in properties {
                                    match property {
                                        CssRulesetProperty::SingleProperty(
                                            property_id,
                                            property_value,
                                            _,
                                        ) => {
                                            for &time in &times {
                                                keyframes_builder
                                                    .add_keyframe(time)
                                                    .with_properties([StyleBuilderProperty::new(
                                                        property_id,
                                                        property_value.clone(),
                                                    )]);
                                            }
                                        }
                                        CssRulesetProperty::MultipleProperties(properties, _) => {
                                            for &time in &times {
                                                keyframes_builder
                                                    .add_keyframe(time)
                                                    .with_properties(properties.iter().map(
                                                        |(id, value)| {
                                                            StyleBuilderProperty::new(
                                                                *id,
                                                                value.clone(),
                                                            )
                                                        },
                                                    ));
                                            }
                                        }
                                        CssRulesetProperty::DynamicProperty(
                                            css_name,
                                            parser,
                                            tokens,
                                            _,
                                        ) => {
                                            let property = StyleBuilderProperty::Dynamic {
                                                css_name,
                                                parser,
                                                tokens,
                                            };
                                            for &time in &times {
                                                keyframes_builder
                                                    .add_keyframe(time)
                                                    .with_properties([property.clone()]);
                                            }
                                        }
                                        CssRulesetProperty::AnimationProperty(s) => {
                                            let AnimationProperty::SingleProperty {
                                                property_id: AnimationPropertyId::TimingFunction,
                                                values,
                                            } = s
                                            else {
                                                unreachable!(
                                                    "Invalid animation property in keyframes: {s:?}"
                                                );
                                            };
                                            for &time in &times {
                                                keyframes_builder
                                                    .add_keyframe(time)
                                                    .with_animation_timing_function(values.clone());
                                            }
                                        }
                                        CssRulesetProperty::Error(error) => {
                                            report_generator.add_error(error);
                                        }
                                        other => {
                                            unreachable!(
                                                "Got {other:?} on keyframes. This is probably a bug"
                                            );
                                        }
                                    }
                                }
                            }
                            ParserAnimationKeyFrame::Error(error) => {
                                report_generator.add_error(error);
                            }
                        }
                    }

                    builder.add_animation_keyframes(
                        keyframes_builder
                            .build(&EMPTY_PROPERTY_REGISTRY)
                            .expect("No property should fail to resolve"),
                    );
                }
                CssStyleSheetItem::Error(error) => {
                    report_generator.add_error(error);
                }
            }
        }

        parse_css(
            self.type_registry,
            self.property_registry,
            self.css_property_registry,
            self.shorthand_property_registry,
            self.imports,
            contents,
            |item| {
                processor(item, &mut builder, &mut report_generator);
            },
        );

        if report_generator.contains_errors() {
            match self.error_mode {
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
        } else if !report_generator.is_empty() {
            info!("\n{}", report_generator.into_message());
        }

        builder.remove_all_empty_rulesets();
        Ok(builder)
    }
}
