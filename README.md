# About

[![Crates.io](https://img.shields.io/crates/v/bevy_flair.svg)](https://crates.io/crates/bevy_flair)
[![docs.rs](https://img.shields.io/docsrs/bevy_flair/latest)](https://docs.rs/bevy_flair/latest)

Bevy Flair enables developers to style Bevy UI interfaces using familiar CSS syntax.

With Bevy Flair, you can define the appearance and layout of Bevy UI components efficiently, leveraging the simplicity and power of CSS.

## Features

- Use CSS assets to apply format to any Bevy UI element.
- Inherited stylesheets. Just specify the stylesheet using [`NodeStyleSheet`] in the root node, and all children will inherit the same stylesheet.
- Support for css reloading. Just edit your .css files and the styles will be re-applied on the fly. This is one of the main advantages over specifying the styles directly.
- Almost All existing UI components and properties are supported:
  - All [`Node`] properties are supported.
  - Components [`BorderColor`], [`BackgroundColor`], [`BorderRadius`], [`Outline`], [`BoxShadow`] and [`ZIndex`] 
    are supported, and inserted automatically when the corresponding property is used (e.g. using `background-color: red` will automatically insert the [`BackgroundColor`] component )  
- Use of non-standard css to support [`ImageNode`] (e.g: `background-image: url("panel-border-030.png")`, `-bevy-image-mode: sliced(20.0px)`).
- Color parsing. (e.g. `red`,`#ff0000`,`rgb(255 0 0)`,`hsl(0 100% 50% / 50%)`,`oklch(40.1% 0.123 21.57)`)
- Most common css selectors works by default (Thanks to [selectors] crate).
  - `:root` selector
  - `#id` selector. Works by using the [`Name`] component
  - `.class` selector. Works by using the [`ClassList`] component
  - `Type` selector. Works by using the [`TypeName`] component. 
    - By default, is set to track:  [`Button`] as `button`, [`Label`] as `label`, [`Text`] as `text` and [`TextSpan`] as `span`,
  - `:hover`, `:active` and `:focus` pseudo class selectors. `:hover` and `:active` are automatically tracked for buttons.
  - `:nth-child(_)`, `:first-child` works just fine. e.g: `:nth_child(2n + 1)`
- Most common css selector combinators  (Thanks to [selectors] crate):
  - Descendant: `ul.my-things li`.
  - Child: `ul.my-things > li`.
  - Next sibling: `img + p`.
  - Subsequent-sibling: `img ~ p`.
- Fancy selectors like `:not()`, `:has()`, `:is()` and `:where()`.
- Support for attributes using [`AttributeList`]
- Nested selectors are supported.
  - You can add `&:hover { .. }` inside a selector and it will work.
- Import other stylesheets using `@import`.
- Support for custom properties using [`var()`].
  - Fallback is currently not supported
- Basic support for calc expressions using [`calc()`].
  - This is currently limited by Bevy support of mixing different types. For example, this cannot not work currently: `calc(100% - 20px)`.
  - Currently, is valuable only to do calculations using vars. For example: `calc(var(--spacing) * 2)`. 
- Support for inherited properties (e.g. `color`, `font-family` are inherited by default).
- Font loading support using [`@font-face`].
- Animated property changes using [`transition`].
- Custom animations using [`@keyframes`].
- Support for [`@media`] queries.
  - The following properties are supported: `prefers-color-scheme`, `width`, `height`, `resolution`, `aspect-ratio`.
- Support for [`@layer`].
- Inline css properties.
- Support for `::before` and `::after` elements.
  - This is an opt-in feature, you need to add [`PseudoElementsSupport`] to the element that you are targeting.
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

## Missing features and limitations

- Multiple stylesheets at the same time. Right now it's restricted to a single stylesheet per entity.
  - This is partially mitigated by the use of `@imports`.
- Global stylesheets. It's not possible to define a stylesheet that is applied everywhere.
- Real support for `!important`.
  - I don't expect it to have real usage today, specially with `@layer` support.
  - The only support is the detection of an `!important` token and ignore it with a message being emitted.
- Support for local fonts or support fallback fonts. Right now a single font is specified using `@font-face`. In bevy this should work for the majority of users.
- Advance color parsing like using `color-mix()` or relative colors like `lch(from blue calc(l + 20) c h)`.
  - This should be relatively easy to add.
- Support for pre-processors like `sass`. It should be relatively simple to add crate that generates css from sass code.


## Showcase
This example works by only using CSS (See [example](https://github.com/eckz/bevy_flair/blob/main/assets/game_menu.css))

https://github.com/user-attachments/assets/792b9cfa-42fb-4e50-a85f-8d21aafeb1e5

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

- Support all Bevy UI features from css.
- Efficient and reactive when applying styles.
  - If there are no modifications to the state of the UI tree, the styles should not be re-applied.
  - When modifications are detected in the UI tree, just the minimum affected nodes should get their style reapplied.
- Invalid css should be reported and discarded.
  - Any unrecognized property should be reported, not silently ignored.
  - A badly written property should not stop the parsing of the rest of the css file
  - An invalid css select, should only affect its rule set, and not the rest of the css
  - No panics while parsing css.
- Css that would make your application panic should be rejected and reported.
  - If any correctly parsed css can cause a panics in bevy, it should be treated as a bug.

## Non goals

- Care about how developers spawn the Bevy UI elements.
  - If you want to use any fancy macro to spawn your bevy UI elements, or if you want to do it in a manual way, it should not matter, it should work the same way.
- Define a default style.
  - By default, if a property is not defined, such property will not be modified. This means that is up to the author to set up fallback styling if it's needed.
  - There is support for `initial` values, which uses the components's default value, but it's not the default behaviour.
- Support all css features / properties.
  - CSS is a vast specification, so there are plenty of features that might not make sense to support.
- Being consistent with the css standard.
  - CSS is a standard and such it defines certain behaviours very well, for example, current implementation of `animation` or `transition` is quite possible not 100% consistent with the standard.
- Implement missing Bevy UI features.
  - If a certain feature does not exist in bevy UI, it will not implement, e.g. it will not implement [`transform`] nor [`linear-gradient`] if bevy UI does not implement them.
  - There is always an option to implement custom properties with custom parsers.

[`transform`]: https://developer.mozilla.org/en-US/docs/Web/CSS/transform
[`linear-gradient`]: https://developer.mozilla.org/en-US/docs/Web/CSS/gradient/linear-gradient

## Bevy compatibility


| bevy | bevy_flair    |
|------|---------------|
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
