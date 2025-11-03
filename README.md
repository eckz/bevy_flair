# About

[![Crates.io](https://img.shields.io/crates/v/bevy_flair.svg)](https://crates.io/crates/bevy_flair)
[![docs.rs](https://img.shields.io/docsrs/bevy_flair/latest)](https://docs.rs/bevy_flair/latest)


**Bevy Flair** brings CSS-like styling to Bevy UI, letting you define appearance and layout using familiar CSS syntax.

It enables you to style UI components, taking advantage of the power of CSS.

## Features

- Apply CSS assets directly to Bevy UI elements.
- Inherited stylesheets. Specify a [`NodeStyleSheet`] on a root node and all children inherit it automatically.
- Property inheritance support (e.g. `color`, `font-family`).
- Font loading with [`@font-face`].
- Animated property changes via [`transition`].
- Custom animations with [`@keyframes`].
- Hot-reloading: edit your `.css` file and see styles re-applied on the fly. 
  - This is one of the main advantages over specifying the styles directly in code.
- Broad support for existing Bevy UI components and properties:
  - All [`Node`] properties are supported.
  - Components [`BorderColor`], [`BackgroundColor`], [`BorderRadius`], [`Outline`], [`BoxShadow`], [`UiTransform`] and [`ZIndex`] 
    are supported, and inserted automatically when the corresponding property is used (e.g. using `background-color: red` will automatically insert the [`BackgroundColor`] component )
  - Support for parsing gradients, like [`linear-gradient()`], [`radial-gradient()`] or [`conic-gradient()`].
- Shorthand properties like [`border`], [`grid`], `margin`, etc.
  - Shorthand properties parse into individual properties, like `margin` becomes `margin-left`, `margin-right`, etc.
  - Transitions and animations are supported for shorthand properties as well.
- Non-standard CSS extensions for [`ImageNode`] 
  - Example: `background-image: url("panel-border-030.png")`, `-bevy-image-mode: sliced(20.0px)`, `-bevy-image-rect: 0 0 64 64`.
- Color parsing. (e.g. `red`,`#ff0000`,`rgb(255 0 0)`,`hsl(0 100% 50% / 50%)`,`oklch(40.1% 0.123 21.57)`)
- Common CSS selectors and combinators (via [selectors] crate):
  - `:root`, `#id` (using [`Name`]), `.class` (using [`ClassList`]), type selectors (via [`TypeName`]), `:hover`, `:active`, `:focus`, `:nth-child`, `:first-child`.
  - descendant (`ul li`), child (`ul > li`), sibling (`img + p`, `img ~ p`).
  - `:not()`, `:has()`, `:is()`, `:where()`.

- Attribute selectors (via [`AttributeList`]).
- Nested selectors: e.g. `&:hover { ... }`.
- Importing other stylesheets with `@import`.
- Custom properties with [`var()`] (Fallback is currently not supported).
- Basic [`calc()`] expressions (mainly useful with variables).
  - This is currently limited by Bevy support of mixing different `Val` types. This wouldn't work: `calc(100% - 20px)`.
  - Is valuable only to do calculations using vars. For example: `calc(var(--spacing) * 2)`.
- [`@media`] queries (`prefers-color-scheme`, `width`, `height`, `resolution`, `aspect-ratio`).
- [`@layer`] support.
- Inline CSS properties.
- Pseudo-elements `::before` and `::after` (enabled with [`PseudoElementsSupport`]).
- Different stylesheets per subtree. With the use of a different [`NodeStyleSheet`] per subtree. It's even possible to not apply any style for a given subtree.
- Use of custom times for transitions and animations (See <https://github.com/eckz/bevy_flair/blob/main/examples/animations.rs>).
- Support for the use of [`GhostNode`] in the hierarchy. Ghost nodes are simply ignored.
  - Enable `experimental_ghost_nodes` feature for better support of Ghost nodes.
- Supports for custom properties. (Example TBA).
- Supports for custom parsing. (See <https://github.com/eckz/bevy_flair/blob/main/examples/custom_parsing.rs>)

[`Node`]: https://docs.rs/bevy/0.17.0-rc.1/bevy/ui/struct.Node.html
[`ImageNode`]: https://docs.rs/bevy/0.17.0-rc.1/bevy/ui/widget/struct.ImageNode.html
[`Button`]: https://docs.rs/bevy/0.17.0-rc.1/bevy/ui/widget/struct.Button.html
[`Label`]: https://docs.rs/bevy/0.17.0-rc.1/bevy/ui/widget/struct.Label.html
[`Text`]: https://docs.rs/bevy/0.17.0-rc.1/bevy/ui/widget/struct.Text.html
[`Name`]: https://docs.rs/bevy/0.17.0-rc.1/bevy/core/struct.Name.html
[`BorderColor`]: https://docs.rs/bevy/0.17.0-rc.1/bevy/ui/struct.BorderColor.html
[`BackgroundColor`]: https://docs.rs/bevy/0.17.0-rc.1/bevy/ui/struct.BackgroundColor.html
[`BorderRadius`]: https://docs.rs/bevy/0.17.0-rc.1/bevy/ui/struct.BorderRadius.html
[`Outline`]: https://docs.rs/bevy/0.17.0-rc.1/bevy/ui/struct.Outline.html
[`BoxShadow`]: https://docs.rs/bevy/0.17.0-rc.1/bevy/ui/struct.BoxShadow.html
[`ZIndex`]: https://docs.rs/bevy/0.17.0-rc.1/bevy/ui/struct.ZIndex.html
[`UiTransform`]: https://docs.rs/bevy/0.17.0-rc.1/bevy/ui/struct.UiTransform.html
[`ClassList`]: https://docs.rs/bevy_flair/latest/bevy_flair/style/components/struct.ClassList.html
[`TypeName`]: https://docs.rs/bevy_flair/latest/bevy_flair/style/struct.TypeName.html
[`PseudoElementsSupport`]: https://docs.rs/bevy_flair/latest/bevy_flair/style/struct.PseudoElementsSupport.html
[`NodeStyleSheet`]: https://docs.rs/bevy_flair/latest/bevy_flair/style/components/enum.NodeStyleSheet.html
[`AttributeList`]: https://docs.rs/bevy_flair/latest/bevy_flair/style/components/enum.AttributeList.html
[`InlineStyle`]: https://docs.rs/bevy_flair/latest/bevy_flair/parser/inline_styles/enum.InlineStyle.html
[selectors]: https://crates.io/crates/selectors
[`transition`]: https://developer.mozilla.org/en-US/docs/Web/CSS/transition
[`@keyframes`]: https://developer.mozilla.org/en-US/docs/Web/CSS/@keyframes
[`@font-face`]: https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face
[`calc()`]: https://developer.mozilla.org/en-US/docs/Web/CSS/calc
[`var()`]: https://developer.mozilla.org/en-US/docs/Web/CSS/var
[`@media`]: https://developer.mozilla.org/en-US/docs/Web/CSS/@media
[`@layer`]: https://developer.mozilla.org/en-US/docs/Web/CSS/@layer
[`linear-gradient()`]: https://developer.mozilla.org/en-US/docs/Web/CSS/gradient/linear-gradient
[`radial-gradient()`]: https://developer.mozilla.org/en-US/docs/Web/CSS/gradient/radial-gradient
[`conic-gradient()`]: https://developer.mozilla.org/en-US/docs/Web/CSS/gradient/conic-gradient
[`border`]: https://developer.mozilla.org/en-US/docs/Web/CSS/border
[`grid`]: https://developer.mozilla.org/en-US/docs/Web/CSS/grid
[`GhostNode`]: https://docs.rs/bevy/latest/bevy/ui/experimental/struct.GhostNode.html

## Missing features and limitations

- Only one stylesheet per entity (workaround: by using `@import`).
- No global stylesheets.
- No real support for `!important`.
  - Currently, `!important` is detected but ignored with a warning.
- Limited font support: only single fonts via `@font-face`. No local or fallback fonts.
- No advanced color functions like `color-mix()` or relative color syntax (e.g. `lch(from blue calc(l + 20) c h)`).
- No individual animation or transition properties like `animation-name`, `transition-duration`, etc.


## Showcase

Example styled entirely with CSS:

https://github.com/user-attachments/assets/792b9cfa-42fb-4e50-a85f-8d21aafeb1e5

([View source CSS](https://github.com/eckz/bevy_flair/blob/main/assets/game_menu.css))


## Getting started

1. Add `bevy_flair` to your `Cargo.toml`.

2. Create your UI structure and attach `NodeStyleSheet` the root:

`main.rs`:

```rust, no_run
use bevy::prelude::*;
use bevy_flair::prelude::*;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, FlairPlugin))
        .add_systems(Startup, setup)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn(Camera2d);
    commands.spawn((
        Node::default(),
        NodeStyleSheet::new(asset_server.load("my_stylesheet.css")),
        children![(Button, children![Text::new("Button")])],
    ));
}

```

Save your css file under `assets/my_stylesheet.css`:

```css
:root {
  display: flex;
  width: 100%;
  height: 100%;
  align-items: center;
  justify-content: center;

  /* font-size and color are inherited */
  font-size: 35px;
  color: rgb(30% 30% 30%);
}

button {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 150px;
  height: 65px;
  background-color: rgb(15%, 15%, 15%);
  border-radius: 10px;
  transition: background-color 0.5s;

  &:hover {
    color: #ddd;
    background-color: rgb(30%, 30%, 25%);
  }

  &:active {
    color: #ddd;
    background-color: rgb(35%, 65%, 35%);
  }

  text {
    /* Color transitions need to happen in the text element */
    transition: color 0.5s;
  }
}
```

Another good place to start are the examples in the [examples folder](https://github.com/eckz/bevy_flair/tree/main/examples).

## Project goals

- Full CSS-based styling for Bevy UI.
- Efficient and reactive when applying styles.
  - No unnecessary style re-application if the UI tree hasn’t changed.
  - When modifications are detected in the UI tree, just the minimum affected nodes should get their style reapplied.
- Strict CSS validation and reporting:
  - Invalid or unknown properties are reported, not ignored.
  - Errors in one rule don’t block others.
  - No panics from malformed CSS.
- Css that would make your application panic should be rejected and reported.
  - If any correctly parsed css can cause a panics in bevy, it should be treated as a bug.

## Non goals

- Dictating how Bevy UI elements are spawned
  - If you want to use any fancy macro to spawn your bevy UI elements, or if you want to do it in a manual way, it should not matter, it should work the same way.
  - Once bsn! macro gets implemented, this crate should keep working as before.
- Define a default style.
  - By default, if a property is not defined, such property will not be modified. This means that is up to the author to set up fallback styling if it's needed.
  - There is support for `initial` values, which uses the component's default value.
- Supporting every CSS feature / property.
  - CSS is a vast specification, so there are plenty of features that might not make sense to support.
- Strict CSS spec compliance (some deviations for practical reasons).
  - CSS is a standard and such it defines certain behaviours very well, for example, current implementation of `animation` or `transition` is quite possible not 100% consistent with the standard.
- Implementing missing Bevy UI features (e.g. unsupported units like `em`, or properties like [`text-decoration`]).

[`text-decoration`]: https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration

## Bevy compatibility


| bevy | bevy_flair    |
|------|---------------|
| 0.17 | 0.5, 0.6      |
| 0.16 | 0.2, 0.3, 0.4 |
| 0.15 | 0.1           |


## Contributing

Contributions are welcome! Feel free to fork the repository and submit a pull request.

## License

This project is licensed under the MIT License. See the LICENSE file for more details.

The assets included in this repository (for our examples) fall under different open licenses.

## Assets

- Poppins font. Designed by Indian Type Foundry, Jonny Pinhorn, Ninad Kale (<https://fonts.google.com/specimen/Poppins>) (SIL Open Font License, Version 1.1: assets/fonts/OFL.txt)
- Kenney Space Font from [Kenney Fonts](https://kenney.nl/assets/kenney-fonts) (CC0 1.0 Universal)
- UI borders from [Kenny's Fantasy UI Borders Kit](https://kenney.nl/assets/fantasy-ui-borders) (CC0 1.0 Universal)
