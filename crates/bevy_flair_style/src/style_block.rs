use crate::animations::{
    AnimationConfiguration, AnimationProperties, AnimationPropertyId, TransitionOptions,
    TransitionPropertyId, from_properties_to_animation_configuration,
    from_properties_to_transition_configuration,
};
use bevy_asset::{Asset, AssetId, Assets};
use bevy_flair_core::{
    ComponentPropertyId, ComponentPropertyRef, CssPropertyRegistry, CssResolveError,
    CssResolveResult, PropertiesHashMap, PropertyMap, PropertyRegistry, PropertyValue,
};
use bevy_reflect::TypePath;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use tracing::{error, warn};

use crate::{VarName, VarResolver, VarToken, VarTokens};

/// A parser function for dynamically parsing a list of [`VarToken`]s.
///
/// This parser is used to convert a sequence of var tokens into a list of defined properties.
///
/// The result is a `Vec` of `(ComponentPropertyRef, PropertyValue)` pairs, or an error if parsing fails.
pub type DynamicParseVarTokens = Arc<
    dyn Fn(
            &[VarToken],
        )
            -> Result<Vec<(ComponentPropertyRef, PropertyValue)>, Box<dyn core::error::Error>>
        + Send
        + Sync,
>;

#[derive(derive_more::Debug, Clone)]
pub(crate) enum StyleProperty {
    Specific {
        property_id: ComponentPropertyId,
        value: PropertyValue,
    },
    Dynamic {
        css_name: Arc<str>,
        #[debug(skip)]
        parser: DynamicParseVarTokens,
        tokens: VarTokens,
    },
}

impl StyleProperty {
    pub(crate) fn resolve(
        &self,
        property_registry: &PropertyRegistry,
        var_resolver: &dyn VarResolver,
        mut resolved: impl FnMut(ComponentPropertyId, PropertyValue),
    ) {
        match self {
            StyleProperty::Specific { property_id, value } => {
                resolved(*property_id, value.clone());
            }
            StyleProperty::Dynamic {
                css_name,
                parser,
                tokens,
            } => {
                let tokens_resolved = match tokens
                    .resolve_recursively(|var_name| var_resolver.get_var_tokens(var_name))
                {
                    Ok(v) => v,
                    Err(err) => {
                        let err = err.enhance_error(var_resolver);
                        error!("Error parsing '{css_name}': {err}");
                        return;
                    }
                };
                match parser(&tokens_resolved) {
                    Ok(values) => {
                        for (property_ref, value) in values {
                            let property_id = property_registry.resolve(&property_ref).expect(
                                "Error resolving ref of a dynamic property. This is probably a bug",
                            );
                            resolved(property_id, value);
                        }
                    }
                    Err(err) => {
                        error!("Property '{css_name}' cannot parse var tokens:\n{err}");
                    }
                }
            }
        }
    }
}

/// A group of style properties, including variables, animation and transition properties.
///
/// In css it would be called a [declaration block]:
/// ```css
/// {
///   --my-variable: 10px;
///   color: var(--my-variable);
///   width: 3px;
/// }
/// ```
///
/// [declaration block]: https://drafts.csswg.org/css2/#rule-sets
#[derive(Debug, Clone, Default, Asset, TypePath)]
pub struct StyleBlock {
    #[cfg(test)]
    pub(crate) original_id: Option<crate::builder::StyleSheetBuilderBlockId>,
    pub(super) vars: Vec<(Arc<str>, VarTokens)>,
    pub(super) properties: Vec<StyleProperty>,
    pub(super) transition_properties: AnimationProperties<TransitionPropertyId>,
    pub(super) animation_properties: AnimationProperties<AnimationPropertyId>,
}

/// Resolves one or more `StyleBlock` assets into runtime values.
///
/// This helper iterates the provided assets and exposes methods to
/// collect properties, variables and animation/transition configurations.
pub struct StyleResolver<'a, I> {
    blocks: &'a Assets<StyleBlock>,
    ids: I,
}

impl<'a, I> StyleResolver<'a, I>
where
    I: IntoIterator<Item = AssetId<StyleBlock>> + Clone,
{
    /// Create a new `StyleResolver` for the given `StyleBlock` assets and ids.
    pub fn new(blocks: &'a Assets<StyleBlock>, ids: I) -> Self {
        Self { blocks, ids }
    }

    fn blocks(&self) -> impl Iterator<Item = &'a StyleBlock> {
        cfg_select! {
            debug_assertions => {
                self.ids.clone().into_iter().filter_map(|id| {
                    let opt = self.blocks.get(id);
                    if opt.is_none() {
                        warn!("StyleBlock not available when resolving styles: {:?}", id);
                    }
                    opt
                })
            }
            _ => {
                self.ids.clone().into_iter().map(|id| self.blocks.get(id).unwrap())
            }
        }
    }

    /// Returns all property values defined in the given blocks and inline styles.
    pub fn resolve_property_values<V: VarResolver>(
        &self,
        property_registry: &PropertyRegistry,
        var_resolver: &V,
        output: &mut PropertyMap<PropertyValue>,
    ) {
        for block in self.blocks() {
            for property in block.properties.iter() {
                property.resolve(property_registry, var_resolver, |property_id, value| {
                    output.set_if_neq(property_id, value);
                });
            }
        }
    }

    /// Returns all variables defined in the given blocks and inline styles.
    pub fn resolve_vars(&self) -> FxHashMap<VarName, VarTokens> {
        let mut result = FxHashMap::default();

        for block in self.blocks() {
            result.extend(
                block
                    .vars
                    .iter()
                    .map(|(name, value)| (name.clone(), value.clone())),
            );
        }
        result
    }

    pub(crate) fn resolve_transition_options<V: VarResolver>(
        &self,
        property_registry: &PropertyRegistry,
        css_property_registry: &CssPropertyRegistry,
        var_resolver: &V,
    ) -> PropertiesHashMap<TransitionOptions> {
        let mut output = FxHashMap::default();

        for block in self.blocks() {
            block
                .transition_properties
                .resolve_to_output(var_resolver, &mut output);
        }
        let configurations = from_properties_to_transition_configuration(&output);

        configurations
            .into_iter()
            .flat_map(|config| {
                let property_name = &*config.property_name;

                match css_property_registry.resolve(property_name, property_registry) {
                    Ok(CssResolveResult::Property(property_id)) => {
                        vec![(property_id, config.options)]
                    }
                    Ok(CssResolveResult::Shorthand(properties)) => properties
                        .into_iter()
                        .map(|id| (id, config.options.clone()))
                        .collect(),
                    Err(CssResolveError::CssPropertyNotRegistered(_)) => {
                        warn!("Css property '{property_name}' does not exist, skipping");
                        Vec::new()
                    }
                    Err(err) => {
                        error!("Error resolving css property '{property_name}': {err}, skipping");
                        Vec::new()
                    }
                }
            })
            .collect()
    }

    // Returns the animation configurations defined in the given blocks.
    // This only includes which animations should run, and the config of the animations.
    // It doesn't include the keyframes
    pub(crate) fn resolve_animation_configs<V: VarResolver>(
        &self,
        var_resolver: &V,
    ) -> Vec<AnimationConfiguration> {
        let mut properties = FxHashMap::default();

        for block in self.blocks() {
            block
                .animation_properties
                .resolve_to_output(var_resolver, &mut properties);
        }
        from_properties_to_animation_configuration(&properties)
    }
}
