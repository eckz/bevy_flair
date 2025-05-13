//! Contains implementation of a map that has a value for every possible property.

use crate::ComponentPropertyId;
use bevy_reflect::Reflect;
use std::marker::PhantomData;
use std::ops::{Deref, Index, IndexMut};
use std::ptr::NonNull;
use std::sync::Arc;

/// A type of Map that has a value for every possible property
#[derive(Debug, Clone, PartialEq, Reflect)]
pub struct PropertyMap<T>(pub(crate) Arc<[T]>);

impl<T> PropertyMap<T> {
    /// Returns true if the map is empty and has no contents.
    /// This is generally true only when the map is first created using [`Default::default`].
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Iterates over all properties returning the property id and the value
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            inner: self.0.iter().enumerate(),
        }
    }

    /// Iterates over all values
    pub fn iter_values(&self) -> core::slice::Iter<'_, T> {
        self.0.iter()
    }

    /// Iterates mutably over all values
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        let len = self.0.len();
        IterMut {
            map: NonNull::from(&mut self.0),
            _marker: PhantomData,
            len,
            index: 0,
        }
    }

    /// Iterates mutably over all values
    pub fn iter_values_mut(&mut self) -> IterValuesMut<'_, T> {
        IterValuesMut {
            inner: self.iter_mut(),
        }
    }

    /// Returns true if the two maps share the same underlying [`Arc`]
    #[inline]
    pub fn ptr_eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl<T: PartialEq + Clone> PropertyMap<T> {
    /// Sets the value of a property if it is different from the current value.
    /// Returns true if the value was changed
    pub fn set_if_neq(&mut self, property_id: ComponentPropertyId, new: T) -> bool {
        let current_value = &self[property_id];
        if current_value != &new {
            self[property_id] = new;
            true
        } else {
            false
        }
    }
}

impl<E: Into<T>, T: Clone> Extend<(ComponentPropertyId, E)> for PropertyMap<T> {
    fn extend<I: IntoIterator<Item = (ComponentPropertyId, E)>>(&mut self, iter: I) {
        for (id, value) in iter {
            self[id] = value.into();
        }
    }
}

impl<T> Default for PropertyMap<T> {
    fn default() -> Self {
        PropertyMap(Default::default())
    }
}

impl<T> Index<ComponentPropertyId> for PropertyMap<T> {
    type Output = T;
    fn index(&self, index: ComponentPropertyId) -> &Self::Output {
        let index: usize = index.into();
        &self.0[index]
    }
}

impl<T: Clone> IndexMut<ComponentPropertyId> for PropertyMap<T> {
    fn index_mut(&mut self, index: ComponentPropertyId) -> &mut Self::Output {
        let index: usize = index.into();
        &mut Arc::make_mut(&mut self.0)[index]
    }
}

impl<'a, T> IntoIterator for &'a PropertyMap<T> {
    type Item = (ComponentPropertyId, &'a T);

    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// An iterator over the properties of a [`PropertyMap`]
pub struct Iter<'a, T> {
    inner: core::iter::Enumerate<core::slice::Iter<'a, T>>,
}

impl<T> Clone for Iter<'_, T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

fn map_enumerate<T>((index, value): (usize, &T)) -> (ComponentPropertyId, &T) {
    (ComponentPropertyId(index as u32), value)
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = (ComponentPropertyId, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(map_enumerate)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }

    fn count(self) -> usize {
        self.inner.count()
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.inner.nth(n).map(map_enumerate)
    }
}

impl<T> DoubleEndedIterator for Iter<'_, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.next_back().map(map_enumerate)
    }

    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        self.inner.nth_back(n).map(map_enumerate)
    }
}

impl<T> ExactSizeIterator for Iter<'_, T> {
    fn len(&self) -> usize {
        self.inner.len()
    }
}

/// Lazy mutable proxy for an index in a [`PropertyMap`].
/// If [`DerefMut`] or [`PropertyMut::set_if_neq`] are not called, the internal [`Arc`] would not
/// necessarily be cloned.
pub struct PropertyMut<'a, T> {
    _marker: PhantomData<&'a mut Arc<[T]>>,
    map: NonNull<Arc<[T]>>,
    index: usize,
}

impl<T: Clone + PartialEq> PropertyMut<'_, T> {
    /// Mutates the properties map only if the new value is different that the exiting one.
    pub fn set_if_neq(&mut self, new: T) -> bool {
        let current_value = &**self;
        if current_value != &new {
            let arc_map = unsafe { self.map.as_mut() };
            let map_slice = Arc::make_mut(arc_map);
            map_slice[self.index] = new;
            true
        } else {
            false
        }
    }
}

impl<T> Deref for PropertyMut<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        let map = unsafe { self.map.as_ref() };
        &map[self.index]
    }
}

/// Mutable iterator over pairs of [`ComponentPropertyId`]  and [`PropertyMap`].
/// It does not clone the values of the internal [`Arc`] unless there is a real mutation.
pub struct IterMut<'a, T> {
    _marker: PhantomData<&'a mut Arc<[T]>>,
    map: NonNull<Arc<[T]>>,
    len: usize,
    index: usize,
}

impl<'a, T: 'a> Iterator for IterMut<'a, T> {
    type Item = (ComponentPropertyId, PropertyMut<'a, T>);

    fn next(&mut self) -> Option<(ComponentPropertyId, PropertyMut<'a, T>)> {
        let index = self.index;
        if index < self.len() {
            self.index += 1;
            Some((
                ComponentPropertyId(index as u32),
                PropertyMut {
                    _marker: PhantomData,
                    map: self.map,
                    index,
                },
            ))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}

impl<T> ExactSizeIterator for IterMut<'_, T> {
    fn len(&self) -> usize {
        self.len
    }
}

/// Mutable iterator over values of a [`PropertyMap`].
/// It does not clone the values of the internal [`Arc`] unless there is a real mutation.
pub struct IterValuesMut<'a, T> {
    inner: IterMut<'a, T>,
}

impl<'a, T: 'a> Iterator for IterValuesMut<'a, T> {
    type Item = PropertyMut<'a, T>;

    fn next(&mut self) -> Option<PropertyMut<'a, T>> {
        Some(self.inner.next()?.1)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<T> ExactSizeIterator for IterValuesMut<'_, T> {
    fn len(&self) -> usize {
        self.inner.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ComputedValue;

    macro_rules! create_map {
        ($($v:expr),* $(,)?) => {
            PropertyMap([$($v),*].into_iter().collect())
        };
    }

    #[test]
    fn default_points_to_the_same_place() {
        let map_a: PropertyMap<ComputedValue> = Default::default();
        let map_b: PropertyMap<ComputedValue> = Default::default();

        assert!(map_a.ptr_eq(&map_b));
    }

    #[test]
    fn property_map() {
        let original_map: PropertyMap<i32> = create_map![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let mut map = original_map.clone();

        assert_eq!(map, original_map);
        assert!(map.ptr_eq(&original_map));

        assert_eq!(map.iter().count(), 10);
        assert_eq!(map.iter().len(), 10);
        assert_eq!(
            map.iter_values().copied().collect::<Vec<_>>(),
            vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
        );
        assert_eq!(map.iter_values().len(), 10);
        assert_eq!(map.iter_mut().count(), 10);
        assert_eq!(map.iter_mut().len(), 10);
        assert_eq!(
            map.iter_values_mut().map(|v| *v).collect::<Vec<_>>(),
            vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
        );
        assert_eq!(map.iter_values_mut().len(), 10);

        // Both maps should be the same pointer since no modification has been done.
        assert!(map.ptr_eq(&original_map));

        map.iter_values_mut().next().unwrap().set_if_neq(0);

        // Both maps should be the same pointer since no modification has been done.
        assert!(map.ptr_eq(&original_map));

        let mut iter_mut = map.iter_mut();
        let (id0, mut value0) = iter_mut.next().unwrap();
        let (id1, mut value1) = iter_mut.next().unwrap();

        assert_eq!(id0, ComponentPropertyId(0));
        assert_eq!(id1, ComponentPropertyId(1));

        value0.set_if_neq(100);
        value1.set_if_neq(200);

        // Now the point to different places
        assert!(!map.ptr_eq(&original_map));
    }
}
