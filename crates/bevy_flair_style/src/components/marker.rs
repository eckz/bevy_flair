use bevy_ecs::component::Component;
use bevy_ecs::reflect::ReflectComponent;
use bevy_reflect::prelude::*;

#[derive(Debug, Copy, Clone, Default, PartialEq, Component, Reflect)]
#[reflect(Debug, Clone, Default, Component)]
enum StyleSystem {
    None,
    #[default]
    Reset,
    CalculateStyle,
    ResolvePropertyValues,
    ComputePropertyValues,
    ApplyPendingProperties,
}

impl StyleSystem {
    fn into_next_system(self) -> Self {
        match self {
            Self::None => Self::None,
            Self::Reset => Self::CalculateStyle,
            Self::CalculateStyle => Self::ResolvePropertyValues,
            Self::ResolvePropertyValues => Self::ComputePropertyValues,
            Self::ComputePropertyValues => Self::ApplyPendingProperties,
            Self::ApplyPendingProperties => Self::None,
        }
    }
}

/// Helper component that when an entity needs it's style to be recalculated
#[derive(Debug, Clone, Default, Component, Reflect)]
#[reflect(Debug, Clone, Default, Component)]
#[cfg_attr(feature = "detailed_trace", component(on_insert))]
pub struct StyleMarkers {
    #[cfg(feature = "detailed_trace")]
    entity_name: String,
    current_system: StyleSystem,
}

#[cfg(feature = "detailed_trace")]
impl StyleMarkers {
    fn on_insert(
        mut world: bevy_ecs::world::DeferredWorld,
        hook_context: bevy_ecs::lifecycle::HookContext,
    ) {
        let entity = hook_context.entity;
        let entity_name = match world.get::<bevy_ecs::name::Name>(entity) {
            Some(name) => name.to_string(),
            None => entity.to_string(),
        };
        world
            .entity_mut(entity)
            .get_mut::<StyleMarkers>()
            .unwrap()
            .entity_name = entity_name;
    }
}

macro_rules! detailed_trace {
    ($self:expr, $fn_name:literal) => {
        #[cfg(feature = "detailed_trace")]
        tracing::trace!(
            "Calling `{fn_name}` on entity {entity} at {caller}",
            fn_name = $fn_name,
            entity = $self.entity_name,
            caller = bevy_ecs::change_detection::MaybeLocation::caller()
        );
    };
    ($self:expr, $fn_name:expr, $fmt:expr, $($args:tt)*) => {
        #[cfg(feature = "detailed_trace")]
        tracing::trace!(
            "Calling `{fn_name}` on entity {entity} at {caller}: {args}",
            fn_name = $fn_name,
            entity = $self.entity_name,
            caller = bevy_ecs::change_detection::MaybeLocation::caller(),
            args = format_args!($fmt, $($args)*),
        );
    }
}

macro_rules! impl_state {
    ($get_value:ident, $finish_ident:ident, $system:ident) => {
        pub(crate) fn $get_value(&self) -> bool {
            self.current_system == StyleSystem::$system
        }

        #[track_caller]
        pub(crate) fn $finish_ident(&mut self) {
            let expected_system = StyleSystem::$system;
            let next_system = self.current_system.into_next_system();

            detailed_trace!(self, stringify!($finish_ident), "Next: {:?}", next_system);

            debug_assert_eq!(
                self.current_system,
                expected_system,
                "Expected current system to be {expected_system:?} in order to move to {next_system:?} (Current system is {current_system:?})",
                current_system = self.current_system
            );
            self.current_system = next_system;
        }
    };
}

impl StyleMarkers {
    #[cfg(test)]
    pub(crate) fn set_to_none(&mut self) {
        self.current_system = StyleSystem::None;
    }

    /// Marks this entity to be reset. All animations and transitions will be reset. All styles will be recalculated
    #[track_caller]
    pub fn reset(&mut self) {
        detailed_trace!(self, "reset");
        self.current_system = StyleSystem::Reset;
    }

    impl_state!(needs_reset, finish_reset, Reset);

    /// Marks this entity as to get its style recalculated.
    #[track_caller]
    pub fn recalculate_style(&mut self) {
        detailed_trace!(self, "recalculate_style");
        debug_assert!(
            matches!(
                self.current_system,
                StyleSystem::None | StyleSystem::CalculateStyle
            ),
            "Cannot set next system to NeedsCalculateStyle because current state is {:?}",
            self.current_system
        );
        self.current_system = StyleSystem::CalculateStyle;
    }

    impl_state!(
        needs_calculate_style,
        finish_calculate_style,
        CalculateStyle
    );

    /// Marks this entity as to resolve it property values again (Because a var might have changed)
    #[track_caller]
    pub(crate) fn set_needs_resolve_property_values(&mut self) {
        detailed_trace!(self, "set_needs_resolve_property_values");

        // If the next system is `reset` or `calculate_style`, it will end up in `SetPropertyValues` eventually.
        if matches!(
            self.current_system,
            StyleSystem::Reset | StyleSystem::CalculateStyle
        ) {
            return;
        }
        debug_assert!(
            matches!(
                self.current_system,
                StyleSystem::None | StyleSystem::ResolvePropertyValues
            ),
            "Cannot set next system to NeedsSetPropertyValues because current state is {:?}",
            self.current_system
        );

        self.current_system = StyleSystem::ResolvePropertyValues;
    }

    impl_state!(
        needs_resolve_property_values,
        finish_resolve_property_values,
        ResolvePropertyValues
    );

    /// Marks this entity as to compute its properties again (An inherited value might have changed)
    #[track_caller]
    pub(crate) fn set_needs_compute_property_values(&mut self) {
        detailed_trace!(self, "set_needs_compute_property_values");

        // If the next system is `reset` or `calculate_style`, it will end up in `ComputePropertyValues` eventually.
        if matches!(
            self.current_system,
            StyleSystem::Reset | StyleSystem::CalculateStyle
        ) {
            return;
        }
        debug_assert!(
            matches!(
                self.current_system,
                StyleSystem::None | StyleSystem::ComputePropertyValues
            ),
            "Cannot set next system to ComputePropertyValues because current state is {:?}",
            self.current_system
        );

        self.current_system = StyleSystem::ComputePropertyValues;
    }

    impl_state!(
        needs_compute_property_values,
        finish_compute_property_values,
        ComputePropertyValues
    );

    #[track_caller]
    pub(crate) fn set_needs_apply_pending_properties(&mut self) {
        detailed_trace!(self, "set_needs_apply_pending_properties");

        // If the next system is `reset` or `calculate_style`, it will end up in `ApplyPendingProperties` eventually.
        if matches!(
            self.current_system,
            StyleSystem::Reset | StyleSystem::CalculateStyle
        ) {
            return;
        }
        debug_assert!(
            matches!(
                self.current_system,
                StyleSystem::None | StyleSystem::ApplyPendingProperties
            ),
            "Cannot set next system to ApplyPendingProperties because current state is {:?}",
            self.current_system
        );
        self.current_system = StyleSystem::ApplyPendingProperties;
    }
    impl_state!(
        needs_apply_pending_properties,
        finish_apply_pending_properties,
        ApplyPendingProperties
    );
}
