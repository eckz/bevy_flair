use crate::ElementData;
use bevy_ecs::entity::Entity;
use bevy_ecs::name::Name;
use bevy_ecs::query::QueryData;
use std::fmt;

/// Convenient query for giving a human friendly name to an entity that might be an element.
///
/// ```
/// # use bevy_ecs::prelude::*;
/// # use bevy_flair_css_selector::ElementDebugName;
/// # #[derive(Component)] pub struct Score(f32);
/// fn increment_score(mut scores: Query<(ElementDebugName, &mut Score)>) {
///     for (name, mut score) in &mut scores {
///         score.0 += 1.0;
///         if score.0.is_nan() {
///             println!("Score for {name} is invalid");
///         }
///     }
/// }
/// # bevy_ecs::system::assert_is_system(increment_score);
/// ```
///
/// # Implementation
///
/// The `Display` implementation shows the entity's [`Name`] prefixed with `#` when available;
/// otherwise, it shows the entity ID. If the entity has [`ElementData`], that data is appended
/// in parentheses.
#[derive(QueryData)]
#[query_data(derive(Debug))]
pub struct ElementDebugName {
    /// A [`Name`] that the entity might have that is displayed if available.
    pub name: Option<&'static Name>,
    /// A [`ElementData`] that the entity might have that is displayed if available.
    pub element_data: Option<&'static ElementData>,
    /// The unique identifier of the entity as a fallback.
    pub entity: Entity,
}

impl<'w, 's> fmt::Display for ElementDebugNameItem<'w, 's> {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (self.name, self.element_data) {
            (Some(name), Some(element_data)) => {
                write!(f, "#{name} ({element_data})")
            }
            (None, Some(element_data)) => {
                write!(f, "{entity} ({element_data})", entity = self.entity)
            }
            (Some(name), None) => {
                write!(f, "#{name}")
            }
            (None, None) => fmt::Display::fmt(&self.entity, f),
        }
    }
}
