# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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