# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.7.0] Unreleased

### Added
- Bevy 0.18 support.
- Revamps animations/transition system
  - It adds support for individual properties,
    like `animation-*` and `transition-*` (https://github.com/eckz/bevy_flair/issues/23, https://github.com/eckz/bevy_flair/issues/24).
  - It allows the use of `var()` in animation / transition properties (https://github.com/eckz/bevy_flair/issues/17).
  - Implements `animation-fill-mode` and `animation-play-state`.
  - Allows the use of `var()` inside keyframes (https://github.com/eckz/bevy_flair/issues/26).
  - Support for `animation-timing-function` inside `@keyframes`
  - You can specify `animation-*` and `transition-*` properties inline.
  - Animations and transitions with zero duration are now supported.
  - Missing properties in `from` or `to` keyframes will be filled with the current computed value.
  - In general, bevy_flair is more conformant to the css standard, so if an animation works in a browser,
    it's more probable that it works now.
- Full support for `TextureSlicer` parsing inside `NodeImage`.
- `BoxShadow` and `TextShadow` are interpolable.
- Support for providing `unset` in any css property.
- Immutable components supported has been added.
- New example showing how to spawn sound effects using css.
- `border-left`, `border-right`, etc are supported.
- `border` shorthand properties accept style keywords, like `solid`, they will be ignored since there is no support in bevy.

### Changed
- Most changes are internal refactorings that only affect direct usage of internal APIs.
- Some usages of `SmolStr` have been replaced with `Cow<'static, str>`, like `ClassList`, `AttributeList` or `InlineStyle`.
- Added new `CssPropertyRegistry` struct for managing CSS property name mappings. `PropertyRegistry` is not used for this purpose anymore.
- Some types implemented `Serialize` and `Deserialize`. It has been removed.
- The way to build `AnimationKeyframes` manually has changed.
- `ReflectAnimatable` create_keyframes_animation signature has changed to use `AnimationPropertyKeyframe`.
- `RawInlineStyle` now can only be built with a `Ruleset` and it's an immutable component, most methods have been removed.
- The build method of `StyleSheetBuilder` requires a `TypeRegistry`.
- Some variants of `StyleSheetBuilderError` have been removed
- `*Placeholder` items have been moved to the `placeholder` module.
- `InlineStyle` now only contains a single `set` and `get` that works for properties, vars, animations and transitions.

### Fixed
- Support resolving images and fonts dynamically, even on inline styles (https://github.com/eckz/bevy_flair/issues/36)

## [0.6.0] 5-Nov-2025

### Added
- Support for the css properties:
  - `-bevy-image-rect`
- Support for use of `GhostNode` in the UI hierarchy

### Changed
- Much of the internals have been changed to better support custom properties. 
`ComponentProperty` functionality has been split between `ComponentProperty`,  `ComponentPropertiesRegistration` and `ComponentProperties` trait.
Other internals like `SubProperties` have been removed.
If you were registering custom properties, this most probably is a breaking change.
Check https://github.com/eckz/bevy_flair/blob/main/crates/bevy_flair_core/src/impls.rs on how to define custom properties.
  - This fixes #12 where full components were replaced on every hot reload.
- Ui node iteration is done through `UiChildren` and `UiRootNodes` to support `GhostNode`.
  - This change is behind the `experimental_ghost_nodes` feature, but it could work even without it.
  - This could be a breaking change if had a non-standard UI hierarchies (like not using Node). 
- If a component that have been inserted automatically, doesn't have properties defined, it will be auto removed
  - This affects components like `BackgroundGradient`, `Outline` or `BoxShadow`.
- It's not possible to restart animations by just using css. Previously some animations were restarted.
but this is not correct according the css standard. I might need to introduce an explicit mechanism to restart animations.
[Common tricks](https://css-tricks.com/restart-css-animation/) to restart animations will not work here.
- Added the capabilities to `InlineStyle` to also set vars (https://github.com/eckz/bevy_flair/issues/20)

### Fixed
- Font-faces did not get registered when using "import" (https://github.com/eckz/bevy_flair/issues/15)
- Multiple animations were not supported in the same property (https://github.com/eckz/bevy_flair/issues/18)
- Some animations were restarted when hover (https://github.com/eckz/bevy_flair/issues/6)
- @keyframes does not support `var()`, but it was never reported as an error, now it does. 

## [0.5.1] - 11-Oct-2025

### Added
- Added `AnimationEvent` support.

## [0.5] - 3-Oct-2025

### Added
- Bevy 0.17 support.
- Support for new gradients in bevy 0.17 by using `background` or `background-image` properties, with any of the [`<gradient>`] functions.
- Support for `translate`, `scale` and `rotate`, thanks by the new [`UiTransform`] component.
  - It's possible to use `transform` as a shorthand property of the above ones.
- Support for custom time support. See https://github.com/eckz/bevy_flair/blob/main/examples/animations.rs.
- New `InlineCssStyleSheetParser` that allows parsing stylesheets from strings directly. Fixes [#8](https://github.com/eckz/bevy_flair/issues/8)

### Changed
- [`StyleSystemSets`] has been renamed to [`StyleSystems`] according to bevy recommendations.
- Animations and transitions now follow `Time<Real>` instead of `Time<Virtual>` by default.

### Fixed
- Some spurious panics are fixed when nodes are spawned in the wrong order: https://github.com/eckz/bevy_flair/issues/5


## [0.4.1] - 15-Sep-2025

### Added

- New example that show how to parse custom types, like a custom `Val` type.
- Make some previously private parsing functions public.

## [0.4] - 1-Aug-2025

### Added

- Support for `@layer`.
- Detection of an `!important` token and ignore it with a message being emitted.
- Support for attributes using `AttributeList`.
- Support for inline styles using `InlineStyle`.
- Added some support for `::before` and `::after`.

### Fixed
- Using the latest version of `selectors` makes it possible to use this crate in WASM.
- Compatible with spawning scenes during an initial state change, which effectively spawns before `PreStartup`.

### Changed
- `TrackTypeNameComponentPlugin` has ben replaced by a simpler single component `TypeName`.

## [0.3] - 16-May-2025

### Added

- Support for `@media` queries.
  - The following properties are supported: `prefers-color-scheme`, `width`, `height`, `resolution`, `aspect-ratio`.
- Support for the css properties:
  - `grid`
  - `gap`
  - `grid-template`
  - `place-items`
  - `text-shadow`
  - `overflow-clip-margin`
  - `aspect-ratio`
  - `line-height`
  - `font-smooth`
  - `text-align`
  - `-bevy-line-break`
- Adding transition events. New events are emitted when a transition starts, ends or gets replaced.
- Support for `initial` property value.
  - Things like `flex: initial` should work.

### Fixed
- Some properties were not supporting `inherit`. Now all properties do support it.

### Changed
- `bevy_flair` doesn't depend on `bevy` crate, now it depends directly on individual crates, like `bevy_ui`.
- Color interpolation happens in Oklab color space, independently of the defined color space.
- Removed `SimpleSelector`. Use `CssSelector`.
- All non-standard css properties are prefixed with `-bevy`.
  - For example `background-image-mode` now is called `-bevy-image-mode`

## [0.2] - 30-Apr-2025

### Added

- Support for inherited properties (e.g. `color`, `font-family` are inherited by default).
- Fancy selectors like `:not()`, `:has()`, `:is()` and `:where()`.
- Import other stylesheets using `@import`.
- Nested selectors are supported.
    - You can add `&:hover { .. }` inside a selector and it will work.
- Support for custom properties using `var()`.
    - Fallback is currently not supported
- Basic support for calc expressions using `calc()`.
    - This is currently limited by Bevy support of mixing different types. For example, this cannot not work currently: `calc(100% - 20px)`.
    - Currently, is valuable to do calculations using vars. For example: `calc(var(--spacing) * 2)`.
- Support for some shorthand properties not previously supported like `border` or `flex`.

### Fixed
- `:nth-child()` type selectors are correctly re-calculated when a sibling is added.

### Changed
- Support for Bevy 0.16

## [0.1] - 24-Jan-2025

Bevy Flair enables developers to style Bevy UI interfaces using familiar CSS syntax.

With Bevy Flair, you can define the appearance and layout of Bevy UI components efficiently, leveraging the simplicity and power of CSS.

## Added

- Use CSS assets to apply format to any Bevy UI element.
- Inherited stylesheets. Just specify the stylesheet using [`NodeStyleSheet`] in the root node, and all children will inherit the same stylesheet.
- Support for css reloading. Just edit your .css files and the styles will be re-applied on the fly. This is one of the main advantages over specifying the styles directly.
- Almost All existing UI components and properties are supported:
    - All [`Node`] properties are supported.
    - Components [`BorderColor`], [`BackgroundColor`], [`BorderRadius`], [`Outline`], [`BoxShadow`] and [`ZIndex`]
      are supported, and inserted automatically when the corresponding property is used (e.g. using `background-color: red` will automatically insert the [`BackgroundColor`] component )
- Use of non-standard css to support [`ImageNode`] (e.g: `image-texture: url("panel-border-030.png")`, `image-mode: sliced(20.0px)`).
- Color parsing. (e.g. `red`,`#ff0000`,`rgb(255 0 0)`,`hsl(0 100% 50% / 50%)`,`oklch(40.1% 0.123 21.57)`)
- Most common css selectors works by default (Thanks to [selectors] crate).
    - `:root` selector
    - `#id` selector. Works by using the [`Name`] component
    - `.class` selector. Works by using the [`ClassList`] component
    - `Type` selector. Works by using the [`TrackTypeNameComponentPlugin`] plugin configured by default to track: [`Node`], [`Button`], [`Label`] and [`Text`],
    - `:hover`, `:active` and `:focus` pseudo class selectors. `:hover` and `:active` are automatically tracked for buttons.
    - `:nth-child(_)`, `:first-child` works just fine. e.g: `:nth_child(2n + 1)`
- Most common css selector combinators  (Thanks to [selectors] crate):
    - Descendant: `ul.my-things li`.
    - Child: `ul.my-things > li`.
    - Next sibling: `img + p`.
    - Subsequent-sibling: `img ~ p`.
- Font loading support using [`@font-face`].
- Animated property changes using [`transition`].
- Custom animations using [`@keyframes`].
- Different stylesheets per subtree. With the use of a different [`NodeStyleSheet`] per subtree. It's even possible to not apply any style for a given subtree.
- Supports for custom properties. (Example TBA)
- Supports for custom parsing. (Example TBA)

[`Node`]: https://docs.rs/bevy/0.15.1/bevy/ui/struct.Node.html
[`ImageNode`]: https://docs.rs/bevy/0.15.1/bevy/ui/widget/struct.ImageNode.html
[`Button`]: https://docs.rs/bevy/0.15.1/bevy/ui/widget/struct.Button.html
[`Label`]: https://docs.rs/bevy/0.15.1/bevy/ui/widget/struct.Label.html
[`Text`]: https://docs.rs/bevy/0.15.1/bevy/ui/widget/struct.Text.html
[`Name`]: https://docs.rs/bevy/0.15.1/bevy/core/struct.Name.html
[`BorderColor`]: https://docs.rs/bevy/0.15.1/bevy/ui/struct.BorderColor.html
[`BackgroundColor`]: https://docs.rs/bevy/0.15.1/bevy/ui/struct.BackgroundColor.html
[`BorderRadius`]: https://docs.rs/bevy/0.15.1/bevy/ui/struct.BorderRadius.html
[`Outline`]: https://docs.rs/bevy/0.15.1/bevy/ui/struct.Outline.html
[`BoxShadow`]: https://docs.rs/bevy/0.15.1/bevy/ui/struct.BoxShadow.html
[`ZIndex`]: https://docs.rs/bevy/0.15.1/bevy/ui/struct.ZIndex.html
[`ClassList`]: https://docs.rs/bevy_flair/latest/bevy_flair/style/components/struct.ClassList.html
[`TrackTypeNameComponentPlugin`]: https://docs.rs/bevy_flair/latest/bevy_flair/style/struct.TrackTypeNameComponentPlugin.html
[`NodeStyleSheet`]: https://docs.rs/bevy_flair/latest/bevy_flair/style/components/enum.NodeStyleSheet.html
[selectors]: https://crates.io/crates/selectors
[`transition`]: https://developer.mozilla.org/en-US/docs/Web/CSS/transition
[`@keyframes`]: https://developer.mozilla.org/en-US/docs/Web/CSS/@keyframes
[`@font-face`]: https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face