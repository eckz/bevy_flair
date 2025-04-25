use crate::{
    ClassName, IdName, NodePseudoStateSelector, SelectorSpecificity, TypeName,
    components::NodeStyleData,
};
use std::fmt::Write;

/// Simples implementation of a selector that can be used to match nodes
#[derive(Debug, Clone)]
pub enum SimpleSelector {
    /// Default condition that does not match anything, but is not a valid condition
    #[cfg(test)]
    Empty,
    /// Any type, similar to `*` in CSS
    AnyType,
    /// Similar to `:root` in CSS
    IsRoot,
    /// Similar to `#name` in CSS
    HasId(IdName),
    /// Similar to `.name` in CSS
    HasClassName(ClassName),
    /// Similar to `:hover` in CSS
    MatchesPseudoState(NodePseudoStateSelector),
    /// Similar to `type` in CSS
    HasTypeName(TypeName),
    /// Concatenation of different selectors, like `type.class:hover` in CSS.
    And(Vec<SimpleSelector>),
}

impl SimpleSelector {
    /// Creates a [`SimpleSelector`] that matches any type.
    pub fn is_any_type() -> Self {
        Self::AnyType
    }

    /// Creates a [`SimpleSelector`] that matches a root node.
    pub fn is_root() -> Self {
        Self::IsRoot
    }

    /// Creates a [`SimpleSelector`] that matches nodes with the given class.
    pub fn has_class_name(class_name: impl Into<ClassName>) -> Self {
        Self::HasClassName(class_name.into())
    }

    /// Creates a [`SimpleSelector`] that matches nodes with the given pseudo state.
    pub fn has_pseudo_state(selector: NodePseudoStateSelector) -> Self {
        Self::MatchesPseudoState(selector)
    }

    /// Creates a [`SimpleSelector`] that matches nodes with the given type name.
    pub fn has_type(type_name: impl Into<TypeName>) -> Self {
        Self::HasTypeName(type_name.into())
    }

    /// Concatenates this selector with another one.
    pub fn and(self, other: Self) -> Self {
        match self {
            #[cfg(test)]
            Self::Empty => other,
            Self::AnyType => other,
            Self::And(mut conditions) => {
                conditions.push(other);
                Self::And(conditions)
            }
            this => Self::And(vec![this, other]),
        }
    }

    /// Concatenates this selector with a new one that matches a node with a class name.
    pub fn and_has_class_name(self, class_name: impl Into<ClassName>) -> Self {
        self.and(Self::has_class_name(class_name))
    }

    /// Concatenates this selector with a new one that matches a node with a given type name.
    pub fn and_has_type(self, type_name: impl Into<TypeName>) -> Self {
        self.and(Self::has_type(type_name))
    }

    /// Concatenates this selector with a new one that matches a node with a given pseudo state.
    pub fn and_has_pseudo_state(self, selector: NodePseudoStateSelector) -> Self {
        self.and(Self::has_pseudo_state(selector))
    }

    pub(crate) fn specificity(&self) -> SelectorSpecificity {
        match self {
            #[cfg(test)]
            Self::Empty => {
                panic!("Empty selector cannot have specificity")
            }
            Self::AnyType => SelectorSpecificity::ZERO,
            Self::IsRoot | Self::HasClassName(_) | Self::MatchesPseudoState(_) => {
                SelectorSpecificity::ONE_CLASS_COLUMN
            }
            Self::HasId(_) => SelectorSpecificity::ONE_ID_COLUMN,
            Self::HasTypeName(_) => SelectorSpecificity::ONE_TYPE_COLUMN,
            Self::And(selectors) => selectors.iter().map(|s| s.specificity()).sum(),
        }
    }

    pub(crate) fn matches(&self, style_data: &NodeStyleData) -> bool {
        match self {
            #[cfg(test)]
            Self::Empty => false,
            Self::AnyType => true,
            Self::IsRoot => style_data.is_root,
            Self::HasId(name) => style_data.name.as_ref() == Some(name),
            Self::HasClassName(class_name) => style_data.has_class(class_name),
            Self::MatchesPseudoState(selector) => style_data.matches_pseudo_state(*selector),
            Self::HasTypeName(type_name) => style_data.has_type_name(type_name),
            Self::And(and_rules) => and_rules.iter().all(|r| r.matches(style_data)),
        }
    }
}

impl crate::ToCss for SimpleSelector {
    fn to_css<W: Write>(&self, dest: &mut W) -> std::fmt::Result {
        match self {
            #[cfg(test)]
            Self::Empty => dest.write_str(""),
            Self::AnyType => dest.write_str("*"),
            Self::IsRoot => dest.write_str(":root"),
            Self::HasId(name) => write!(dest, "#{name}"),
            Self::HasClassName(class_name) => write!(dest, ".{class_name}"),
            Self::MatchesPseudoState(selector) => crate::ToCss::to_css(selector, dest),
            Self::HasTypeName(type_name) => write!(dest, "{type_name}"),
            Self::And(and_rules) => and_rules.iter().try_for_each(|r| r.to_css(dest)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::testing::{entity, simple_selector};

    macro_rules! selector_matches {
        ($entity:expr, $selector:expr) => {
            $selector.matches(&$entity)
        };
    }

    #[test]
    fn class_selector() {
        let has_some_class = simple_selector! { .some_class };
        let has_no_match_class = simple_selector! { .no_match_class };
        let entity = entity!(Text.other_class.some_class);

        assert!(selector_matches!(entity, has_some_class));
        assert!(!selector_matches!(entity, has_no_match_class));
    }

    #[test]
    fn id_selector() {
        let has_id = simple_selector! { #matches };
        let has_id_and_class = simple_selector! { #matches.some_class };
        let has_another_id = simple_selector! { #no_matches };
        let entity = entity!(Text #matches.some_class);

        assert!(selector_matches!(entity, has_id));
        assert!(selector_matches!(entity, has_id_and_class));
        assert!(!selector_matches!(entity, has_another_id));
    }

    #[test]
    fn is_root_selector() {
        let is_root_and_class = simple_selector! { :root.class };
        let is_root = simple_selector! { :root };

        let root_entity = entity!(:root .class);

        assert!(selector_matches!(root_entity, is_root_and_class));
        assert!(selector_matches!(root_entity, is_root));

        let non_root_entity = entity!(.some_class);

        assert!(!selector_matches!(non_root_entity, is_root_and_class));
        assert!(!selector_matches!(non_root_entity, is_root));
    }

    #[test]
    fn type_selector() {
        let is_text = simple_selector! { Text };
        let is_other_type = simple_selector! { OtherType };
        let entity = entity!(Text OtherComponent .some_class);

        assert!(selector_matches!(entity, is_text));
        assert!(!selector_matches!(entity, is_other_type));
    }

    #[test]
    fn any_type_selector() {
        let is_any_type = simple_selector! { * };
        let entity = entity!(Text OtherComponent .some_class);

        assert!(selector_matches!(entity, is_any_type));
    }

    #[test]
    fn specificity_type_over_any() {
        let any_type = simple_selector! { * };
        let type_selector = simple_selector! { GrandParent };

        assert!(any_type.specificity() < type_selector.specificity());
    }

    #[test]
    fn specificity_root_over_type() {
        let any_type = simple_selector! { * };
        let root_selector = simple_selector! { :root };

        assert!(any_type.specificity() < root_selector.specificity());
    }

    #[test]
    fn specificity_class_over_type() {
        let type_selector = simple_selector! { GrandParent };
        let class_selector = simple_selector! { .class };

        assert!(type_selector.specificity() < class_selector.specificity());
    }

    #[test]
    fn specificity_class_with_pseudo_over_class() {
        let class_selector = simple_selector! { .class };
        let class_selector_with_pseudo = simple_selector! { .class:hover };

        assert!(class_selector.specificity() < class_selector_with_pseudo.specificity());
    }

    #[test]
    fn specificity_id_over_anything_else() {
        let selector_with_id = simple_selector! { #name };
        let complex_selector = simple_selector! { * :root Text.class.another_class:hover };

        assert!(complex_selector.specificity() < selector_with_id.specificity());
    }

    #[test]
    fn specificity_any_does_not_count() {
        let class_selector = simple_selector! { .class };
        let class_selector_with_any_ancestor = simple_selector! { *.class };

        assert_eq!(
            class_selector.specificity(),
            class_selector_with_any_ancestor.specificity()
        );
    }
}
