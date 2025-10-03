use crate::parser::{
    AnimationKeyFrame, CssAnimation, CssRuleset, CssRulesetProperty, CssStyleSheetItem,
    CssTransitionProperty, parse_css,
};
use crate::utils::ImportantLevel;
use crate::{
    CssStyleLoaderError, CssStyleLoaderErrorMode, ErrorReportGenerator, ShorthandPropertyRegistry,
};

use bevy_flair_core::{ComponentPropertyRef, PropertiesHashMap, PropertyRegistry, PropertyValue};
use bevy_flair_style::css_selector::CssSelector;
use bevy_flair_style::{
    AnimationKeyframesBuilder, StyleBuilderProperty, StyleSheet, StyleSheetBuilder,
};
use bevy_reflect::TypeRegistry;
use rustc_hash::FxHashMap;
use tracing::{error, info, warn};

pub(crate) struct InternalStylesheetLoader<'a> {
    pub(crate) type_registry: &'a TypeRegistry,
    pub(crate) property_registry: &'a PropertyRegistry,
    pub(crate) shorthand_property_registry: &'a ShorthandPropertyRegistry,
    pub(crate) error_mode: CssStyleLoaderErrorMode,
    pub(crate) imports: &'a FxHashMap<String, StyleSheet>,
}

// This is temporal until https://github.com/tokio-rs/tracing/issues/3378 is fixed
const NO_COLOR_REPORT_CONFIG: ariadne::Config = ariadne::Config::new().with_color(false);

impl InternalStylesheetLoader<'_> {
    pub(crate) fn load_stylesheet(
        &self,
        path_name: &str,
        contents: &str,
    ) -> Result<StyleSheetBuilder, CssStyleLoaderError> {
        let mut builder = StyleSheetBuilder::new();
        let mut report_generator =
            ErrorReportGenerator::new_with_config(path_name, contents, NO_COLOR_REPORT_CONFIG);

        fn report_important_level(
            report_generator: &mut ErrorReportGenerator,
            level: ImportantLevel,
        ) {
            if let ImportantLevel::Important(location) = level {
                report_generator.add_advice(
                    location,
                    "!important is not supported",
                    "!important token is being ignored, so you can remove it",
                );
            }
        }

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
                            CssRulesetProperty::SingleProperty(id, value, important_level) => {
                                ruleset_builder
                                    .add_properties([(ComponentPropertyRef::Id(id), value)]);
                                report_important_level(report_generator, important_level);
                            }
                            CssRulesetProperty::MultipleProperties(properties, important_level) => {
                                ruleset_builder.add_properties(
                                    properties
                                        .into_iter()
                                        .map(|(id, value)| (ComponentPropertyRef::Id(id), value)),
                                );
                                report_important_level(report_generator, important_level);
                            }
                            CssRulesetProperty::DynamicProperty(
                                css_name,
                                parser,
                                tokens,
                                important_level,
                            ) => {
                                ruleset_builder.add_properties([StyleBuilderProperty::Dynamic {
                                    css_name,
                                    parser,
                                    tokens,
                                }]);
                                report_important_level(report_generator, important_level);
                            }
                            CssRulesetProperty::Transitions(property_transitions) => {
                                for transition_fallible in property_transitions {
                                    match transition_fallible {
                                        Ok(CssTransitionProperty {
                                            properties,
                                            options,
                                        }) => {
                                            for property in properties {
                                                ruleset_builder.add_property_transition(
                                                    property,
                                                    options.clone(),
                                                );
                                            }
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
                            CssRulesetProperty::Var(var_name, tokens) => {
                                ruleset_builder.add_var(var_name, tokens);
                            }
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
                            CssRulesetProperty::Error(error) => {
                                report_generator.add_error(error);
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
                                            _,
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
                                        CssRulesetProperty::MultipleProperties(properties, _) => {
                                            for (property_id, sample) in properties {
                                                if let PropertyValue::Value(sample) = sample {
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
        }

        parse_css(
            self.type_registry,
            self.property_registry,
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
