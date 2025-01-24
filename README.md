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

## Missing features and limitations

- Multiple stylesheets at the same time. Right now it's restricted to a single stylesheet per entity.
- Global stylesheets. It's not possible to define a stylesheet that is applied anywhere.
- Inline css. Right now it's not possible to define css directly in code. It has to be defined directly into an asset with the `.css` extension. 
  I wouldn't be as easy as creating a simple macro for it, but it most definitely something to be considered.
- Support for `@import` other stylesheets.
- Support for `@media` queries. It could be interesting to see what features make sense to implement.
- Inherited properties. This is challenging feature, but it's common in css to set a property in a parent, like `color`, or `font`
  and all children will inherit that property. Right now you need to set properties to all children directly
  with a rule.
- Support for `!important`.
- Support for local fonts or support fallback fonts. Right now a single font is specified using `@font-face`. In bevy this should work for the majority of users.
- Inline properties. There is no technical blocker for this, a new component would be required. Pull request are welcome.
- Fancy selectors like `:has()`, `:is()` or `:where()`. It should be simple to add, but it has not been tested.
- Advance color parsing like using `color-mix()` or relative colors like `lch(from blue calc(l + 20) c h)`.
- Nested selectors. It should be possible to add, pull requests are welcome.
- Support for pre-processors like `sass`. It should be relatively simple to add crate that generates css from sass code.
- Support for [`calc()`]. This is feature that would be needed to be supported by Bevy UI first. Otherwise, things like `calc(100% - 20px)` would be never be possible.
- Support for [`var()`]. This is no small feat, it would require support for inheritance, and access to `calc()` to be somehow useful, see comment above.

[`calc()`]: https://developer.mozilla.org/en-US/docs/Web/CSS/calc
[`var()`]: https://developer.mozilla.org/en-US/docs/Web/CSS/var

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

    commands
        .spawn((
            Node::default(),
            NodeStyleSheet::new(asset_server.load("my_stylesheet.css")),
        ))
        .with_children(|parent| {
            parent
                .spawn((Button, Node::default()))
                .with_child(Text::new("Button"));
        });
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
    flex-direction: column;
}

Button {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 150px;
    height: 65px;
    background-color: rgb(15%, 15.0%, 15.0%);
    border-radius: 30px;
    transition: background-color 0.3s;
}

Button:hover {
    background-color: rgb(25.0%, 25.0%, 25.0%);
}

Button:active {
    background-color: rgb(35.0%, 75.0%, 35.0%);
}

Button Text {
    font-size: 35px;
    color: rgb(30% 30% 30%);
}
```

You can see more examples in the [examples folder](https://github.com/eckz/bevy_flair/tree/main/examples).

## Project goals

- Support all Bevy UI features from css.
- Efficient and reactive when applying styles.
  - If there are no modifications to the state of the UI tree, the styles should not be re-applied.
  - When modifications are detected in the UI tree, just the minimum affected nodes should get their style reapplied.
- Invalid css should be reported and discarded.
  - Any unrecognized property should be reported, not silently ignored.
  - A badly written property, should not stop the parsing of the rest of the css file
  - An invalid css select, should only affect its rule set, and not the rest of the css
  - No panics while parsing css.
- Css that would make your application panic should be rejected and reported.
  - If any correctly parsed css can cause a panics in bevy, it should be treated as a bug.

## Non goals

- Care about how developers spawn the Bevy UI elements.
  - If you want to use any fancy macro to spawn your bevy UI elements, or if you want to do it in a manual way, it should not matter, it should work the same way.
- Mix between manually applied styles and CSS styles.
  - It's not a goal to keep manually applied style. When applying CSS styles, is expected that the only actor in modifying style is this crate.
- Support all css features / properties.
  - Css is a vast specification, so there are plenty of features that might not make sense to support.
- Being consistent with the css standard.
  - Css is a standard and such it defines certain behaviours very well, for example, current implementation of `animation` or `transition` is quite possible not 100% consistent with the standard.
- Implement missing Bevy UI features.
  - If a certain feature does not exist in bevy UI, it will not implement, e.g. it will not implement [`transform`] nor [`linear-gradient`] if bevy UI does not implement them.
  - There is always an option to implement custom properties with custom parsers.

[`transform`]: https://developer.mozilla.org/en-US/docs/Web/CSS/transform
[`linear-gradient`]: https://developer.mozilla.org/en-US/docs/Web/CSS/gradient/linear-gradient

## Contributing

Contributions are welcome! Feel free to fork the repository and submit a pull request.

## License

This project is licensed under the MIT License. See the LICENSE file for more details.

The assets included in this repository (for our examples) fall under different open licenses.

## Assets

- Poppins font. Designed by Indian Type Foundry, Jonny Pinhorn, Ninad Kale (<https://fonts.google.com/specimen/Poppins>) (SIL Open Font License, Version 1.1: assets/fonts/OFL.txt)
- Kenney Space Font from [Kenney Fonts](https://kenney.nl/assets/kenney-fonts) (CC0 1.0 Universal)
- UI borders from [Kenny's Fantasy UI Borders Kit](https://kenney.nl/assets/fantasy-ui-borders) (CC0 1.0 Universal)
