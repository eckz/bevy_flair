trait DebugOptionExt<T> {
    fn debug_unwrap_or_else(self, f: impl FnOnce() -> T) -> T;
}

impl<T> DebugOptionExt<T> for Option<T> {
    #[cfg(debug_assertions)]
    #[inline(always)]
    fn debug_unwrap_or_else(self, f: impl FnOnce() -> T) -> T {
        self.unwrap_or_else(f)
    }

    #[cfg(not(debug_assertions))]
    #[inline(always)]
    fn debug_unwrap_or_else(self, _f: impl FnOnce() -> T) -> T {
        self.unwrap()
    }
}

#[derive(Debug, Clone, Default)]
struct ParsedLayer<'a> {
    parents: Vec<&'a str>,
    name: &'a str,
}

impl<'a> ParsedLayer<'a> {
    fn parse(full_name: &'a str) -> Self {
        let mut parents = Vec::new();
        let mut consumed = 0;
        while let Some(i) = full_name[consumed..].find('.') {
            parents.push(&full_name[..consumed + i]);
            consumed += i + 1;
        }
        let name = &full_name[consumed..];
        debug_assert!(!name.is_empty(), "Invalid layer name: '{full_name}'");
        Self { parents, name }
    }

    pub fn has_parents(&self) -> bool {
        !self.parents.is_empty()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct LayersHierarchy {
    inner: Vec<String>,
}

impl Default for LayersHierarchy {
    fn default() -> Self {
        Self::new()
    }
}

impl LayersHierarchy {
    pub const fn new() -> Self {
        Self { inner: Vec::new() }
    }

    pub fn define_layer(&mut self, full_name: &str) {
        // Empty layer is always implicit present at the very end
        if full_name.is_empty() {
            return;
        }
        if self.is_layer_defined(full_name) {
            return;
        }
        let layer = ParsedLayer::parse(full_name);

        if !layer.has_parents() {
            self.inner.push(layer.name.into());
        } else {
            for parent in &layer.parents {
                self.define_layer(parent);
            }

            let insert_position = self.find_insert_position(&layer);
            self.inner.insert(insert_position, full_name.into());
        }
    }

    fn is_layer_defined(&self, full_name: &str) -> bool {
        self.inner.iter().any(|a| &**a == full_name)
    }

    /// Gets the layer priority, higher number means higher priority
    pub fn get_layer_priority(&self, full_name: &str) -> usize {
        if full_name.is_empty() {
            // Highest priority
            return self.inner.len();
        }
        self.inner
            .iter()
            .position(|a| &**a == full_name)
            .debug_unwrap_or_else(|| {
                panic!("Layer '{full_name}' not found");
            })
    }

    pub fn cmp_layers(&self, a: &str, b: &str) -> std::cmp::Ordering {
        self.get_layer_priority(a).cmp(&self.get_layer_priority(b))
    }

    fn find_insert_position(&self, layer: &ParsedLayer) -> usize {
        debug_assert!(layer.has_parents());

        let parent = layer.parents.last().unwrap();
        self.inner
            .iter()
            .position(|a| &**a == *parent)
            .debug_unwrap_or_else(|| {
                panic!("Parent layer '{parent}' not found");
            })
    }

    #[cfg(test)]
    fn get_layers(&self) -> impl Iterator<Item = &str> {
        self.inner.iter().map(|v| &**v)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;

    #[test]
    fn parsed_test() {
        let parsed = ParsedLayer::parse("simple");

        assert_eq!(parsed.name, "simple");
        assert!(parsed.parents.is_empty());
        assert_eq!(parsed.parents.capacity(), 0);

        let parsed = ParsedLayer::parse("parent.child");

        assert_eq!(parsed.name, "child");
        assert_eq!(parsed.parents, vec!["parent"]);
        // assert_eq!(parsed.to_string(), "parent.child");

        let parsed = ParsedLayer::parse("multiple.parents.child");

        assert_eq!(parsed.name, "child");
        assert_eq!(parsed.parents, vec!["multiple", "multiple.parents"]);
        // assert_eq!(parsed.to_string(), "multiple.parents.child");
    }

    #[test]
    fn simple_layers() {
        let mut layers = LayersHierarchy::new();
        layers.define_layer("first");
        layers.define_layer("second");
        layers.define_layer("last");

        // These are repeated so they should be ignored
        layers.define_layer("first");
        layers.define_layer("second");

        assert_eq!(
            layers.get_layers().collect::<Vec<_>>(),
            vec!["first", "second", "last"]
        );

        assert_eq!(layers.get_layer_priority("first"), 0);
        assert_eq!(layers.get_layer_priority("last"), 2);
        // Highest priority
        assert_eq!(layers.get_layer_priority(""), 3);
    }

    #[test]
    fn layers_with_one_parent() {
        let mut layers = LayersHierarchy::new();
        layers.define_layer("first");
        layers.define_layer("second");
        layers.define_layer("last");

        layers.define_layer("first.inner_1");
        layers.define_layer("first.inner_2");

        // These are repeated so they should be ignored
        layers.define_layer("first");
        layers.define_layer("second");
        layers.define_layer("first.inner_2");

        // We have not defined `undefined`, but it gets defined automatically
        layers.define_layer("undefined.inner_1");

        assert_eq!(
            layers.get_layers().collect::<Vec<_>>(),
            vec![
                "first.inner_1",
                "first.inner_2",
                "first",
                "second",
                "last",
                "undefined.inner_1",
                "undefined"
            ]
        );
    }

    #[test]
    fn layers_with_multiple_parents() {
        let mut layers = LayersHierarchy::new();
        layers.define_layer("first");
        layers.define_layer("second");

        layers.define_layer("first.middle_1");
        layers.define_layer("first.middle_2");

        layers.define_layer("last");

        layers.define_layer("second.middle_1");
        layers.define_layer("second.middle_2");

        layers.define_layer("second.middle_1.child_1");
        layers.define_layer("second.middle_2.child_1");
        layers.define_layer("first.middle_1.child_1");
        layers.define_layer("first.middle_2.child_1");
        layers.define_layer("first.middle_1.child_2");
        layers.define_layer("first.middle_2.child_2");
        layers.define_layer("second.middle_1.child_2");
        layers.define_layer("second.middle_2.child_2");

        // These are repeated so they should be ignored
        layers.define_layer("first");
        layers.define_layer("second");
        layers.define_layer("first.middle_1.child_2");
        layers.define_layer("last");

        let expected = vec![
            "first.middle_1.child_1",
            "first.middle_1.child_2",
            "first.middle_1",
            "first.middle_2.child_1",
            "first.middle_2.child_2",
            "first.middle_2",
            "first",
            "second.middle_1.child_1",
            "second.middle_1.child_2",
            "second.middle_1",
            "second.middle_2.child_1",
            "second.middle_2.child_2",
            "second.middle_2",
            "second",
            "last",
        ];

        assert_eq!(layers.get_layers().collect::<Vec<_>>(), expected);

        assert_eq!(
            layers
                .get_layers()
                .sorted_by(|a, b| { layers.cmp_layers(a, b) })
                .collect::<Vec<_>>(),
            expected
        );
    }
}
