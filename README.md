# About

[![Crates.io](https://img.shields.io/crates/v/bevy_flair.svg)](https://crates.io/crates/bevy_flair) [![docs.rs](https://img.shields.io/docsrs/bevy_flair/latest)](https://docs.rs/bevy_flair/latest)

**Bevy Flair** brings CSS-like styling to Bevy, letting you define appearance and layout using familiar CSS syntax.

It enables you to style UI components, taking advantage of the power of CSS.

## Showcase

Example styled entirely with CSS:

https://github.com/user-attachments/assets/792b9cfa-42fb-4e50-a85f-8d21aafeb1e5

([View source CSS](https://github.com/eckz/bevy_flair/blob/main/assets/game_menu.css))

## Getting started

1. Add `bevy_flair` to your `Cargo.toml`, using the version that matches your Bevy version (see [Bevy compatibility](#bevy-compatibility)).

2. Create your UI structure and attach `Styled` to the root. `main.rs`:

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
        Styled::new(asset_server.load("my_stylesheet.css")),
        children![(ClassList::new("button"), Node::default(), children![Text::new("Button")])],
    ));
}

```

3. Save your CSS file under `assets/my_stylesheet.css`:

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

.button {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 150px;
  height: 65px;
  background-color: rgb(15% 15% 15%);
  border-radius: 10px;
  transition: background-color 0.5s;

  &:hover {
    color: #ddd;
    background-color: rgb(30% 30% 25%);
  }

  &:active {
    color: #ddd;
    background-color: rgb(35% 65% 35%);
  }

  text {
    /* Color transitions need to happen in the text element */
    transition: color 0.5s;
  }
}
```

Another good place to start is the examples in the [examples folder](https://github.com/eckz/bevy_flair/tree/main/examples).

## Features

| Status | Meaning |
| ------ | ------- |
| ✅ | Supported |
| ⚠️ | Partially supported (see notes) |
| 🔜 | Soon |
| ❌ | Not supported yet |
| 🛑 | Not supported, and probably will never be |

### Selectors

Common CSS selectors and combinators, via the [selectors](https://crates.io/crates/selectors) crate.

```css
.button {
  background-color: brown;
  &:hover {
    color: #f69d3c;
  }
}

.card:has(.badge):not(:hover) {
  background-color: pink;
}

.list > :first-child {
  margin: 0;
}


```

| Feature                                                                                                                                        | Status | Notes                                                                                                                        |
|------------------------------------------------------------------------------------------------------------------------------------------------|:------:|------------------------------------------------------------------------------------------------------------------------------|
| `:root`                                                                                                                                        |   ✅   |                                                                                                                              |
| `#id`                                                                                                                                          |   ✅   | Using [`Name`](https://docs.rs/bevy/latest/bevy/prelude/struct.Name.html)                                                    |
| `.class`                                                                                                                                       |   ✅   | Using [`ClassList`](https://docs.rs/bevy_flair/latest/bevy_flair/style/components/struct.ClassList.html)                     |
| Type selectors                                                                                                                                 |   ✅   | Via [`TypeName`](https://docs.rs/bevy_flair/latest/bevy_flair/style/struct.TypeName.html)                                    |
| `:hover`, `:active`, `:focus`, `:nth-child`, `:first-child`                                                                                    |   ✅   |                                                                                                                              |
| `:not()`, `:has()`, `:is()`, `:where()`                                                                                                        |   ✅   |                                                                                                                              |
| Combinators: descendant (`ul li`), child (`ul > li`), sibling (`img + p`, `img ~ p`)                                                           |   ✅   |                                                                                                                              |
| Attribute selectors                                                                                                                            |   ✅   | Via [`AttributeList`](https://docs.rs/bevy_flair/latest/bevy_flair/style/components/enum.AttributeList.html)                 |
| Nested selectors (`&:hover { ... }`)                                                                                                           |   ✅   |                                                                                                                              |
| `::before` and `::after`                                                                                                                       |   ✅   | Enabled with [`PseudoElementsSupport`](https://docs.rs/bevy_flair/latest/bevy_flair/style/struct.PseudoElementsSupport.html) |
| Automatic [`aria-*` attribute selectors](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Selectors/Attribute_selectors)             |   ❌   | ARIA attributes are not in sync with `bevy_a11y`                                                                             |
| Other [pseudo-elements](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Selectors/Pseudo-elements) like `::checkmark`, `::backdrop` |   🛑   | Bevy has no general concept of pseudo-elements to target                                                                     |

### Functions

```css
:root {
  --spacing: 2px;
  --accent: #fd00ab;
}

.tab {
  padding: calc(var(--spacing) * 2);
  background-color: oklch(from var(--accent) 20% calc(c / 2) h);
}
```

| Feature | Status | Notes |
| ------- | :----: | ----- |
| Custom properties with [`var()`](https://developer.mozilla.org/en-US/docs/Web/CSS/var) | ⚠️ | Fallback values are not supported yet (🔜) |
| [`calc()`](https://developer.mozilla.org/en-US/docs/Web/CSS/calc) | ⚠️ | Limited by Bevy's support for mixing `Val` types. `calc(100% - 20px)` will not work, but `calc(2 * var(--spacing))` does <br/> Only `min/max/clamp`, `round` and `pow/sqrt/log/exp` functions are supported currently. More functions are 🔜 |
| [`sibling-index()`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/sibling-index)  and [`sibling-count()`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/sibling-count) | ❌ | |
| [`if()`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/if) | ❌ | |

### At-rules

```css
@import "base.css";

@font-face {
  font-family: "Poppins";
  src: url("fonts/Poppins-Regular.ttf");
}

@keyframes pulse {
  from { background-color: white; }
  to   { background-color: red; }
}

@media (prefers-color-scheme: dark) {
  :root {
    --accent: oklch(70% 0.1 250);
  }
}

.button {
  background-color: white;
  @media (prefers-color-scheme: dark) {
    background-color: #0f0f2b;
  }
}
```

| Feature                                                                                                  | Status | Notes                                                                                                                  |
|----------------------------------------------------------------------------------------------------------|:------:|------------------------------------------------------------------------------------------------------------------------|
| `@import`                                                                                                |   ✅   | Import other stylesheets. Also the workaround for one stylesheet per entity                                            |
| [`@font-face`](https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face)                              |   ⚠️   | Font loading works, but only with single fonts. No local fonts                                                         |
| [`@keyframes`](https://developer.mozilla.org/en-US/docs/Web/CSS/@keyframes)                              |   ✅   |                                                                                                                        |
| [`@layer`](https://developer.mozilla.org/en-US/docs/Web/CSS/@layer)                                      |   ✅   |                                                                                                                        |
| [`@media`](https://developer.mozilla.org/en-US/docs/Web/CSS/@media)                                      |   ⚠️   | `prefers-color-scheme`, `width`, `height`, `resolution`, `aspect-ratio`. Range syntax such as `(width >= 600px)` is 🔜 |
| [`@starting-style`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/At-rules/@starting-style) |   🔜   |                                                                                                                        |
| [`@scope`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/At-rules/@scope)                   |   ❌   |                                                                                                                        |
| [`@container`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/At-rules/@container)           |   ❌   |                                                                                                                        |
| [`@property`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/At-rules/@property)             |   ❌   |                                                                                                                        |
| [`@function`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/At-rules/@function)             |   ❌   |                                                                                                                        |
| [`@supports`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/At-rules/@supports)             |   🛑   | Unnecessary when the supported features are known in advance                                                           |
| [`@charset`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/At-rules/@charset)               |   🛑   | UTF-8 is always assumed                                                                                                |

### Colors

```css
.button {
  color: #dfd;
  background-color: oklch(42.1% 0.123 21.45);
  border-color: hsl(0 90% 20% / 50%);
  outline-color: oklch(from red calc(l / 2) c h / 50%);
  transition: background-color 0.5s;
}
```

| Feature                                                                                                                                                                            | Status | Notes                                                                                                                                                                                                                                                                                                 |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------:|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Named colors                                                                                                                                                                       |   ✅   | e.g. `red`, `lime`                                                                                                                                                                                                                                                                                    |
| Hex colors                                                                                                                                                                         |   ✅   | e.g. `#ff0000`, `#ddd`                                                                                                                                                                                                                                                                                |
| Color functions, [`rgb()`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/rgb), [`oklch()`](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/oklch), ... |   ✅   | e.g. `rgb(255 0 0)`, `oklch(40.1% 0.123 21.57)`                                                                                                                                                                                                                                                       |
| Gradients                                                                                                                                                                          |   ✅   | [`linear-gradient()`](https://developer.mozilla.org/en-US/docs/Web/CSS/gradient/linear-gradient), [`radial-gradient()`](https://developer.mozilla.org/en-US/docs/Web/CSS/gradient/radial-gradient) and [`conic-gradient()`](https://developer.mozilla.org/en-US/docs/Web/CSS/gradient/conic-gradient) |
| Relative color syntax                                                                                                                                                              |   ✅   | e.g. `lch(from blue calc(l + 20) c h)`                                                                                                                                                                                                                                                                |
| [`color-mix()`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/color_value/color-mix)                                                                           |   🔜   |                                                                                                                                                                                                                                                                                                       |
| [`contrast-color()`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/color_value/contrast-color)                                                                 |   ❌   |                                                                                                                                                                                                                                                                                                       |
| [`color-scheme`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Properties/color-scheme)                                                                               |   🛑   | Doesn't fit well with Bevy UI, which has no native UI elements.                                                                                                                                                                                                                                       |
| [`light-dark()`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/color_value/light-dark)                                                                         |   🛑   | Only make sense if `color-scheme` is supported.                                                                                                                                                                                                                                                       |

### Properties, components and animations

| Feature                                                                                                                                                                                                                 | Status | Notes                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------:|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| All [`Node`](https://docs.rs/bevy/latest/bevy/ui/struct.Node.html) properties                                                                                                                                           |   ✅   |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| Other UI components                                                                                                                                                                                                     |   ✅   | [`BorderColor`](https://docs.rs/bevy/latest/bevy/ui/struct.BorderColor.html), [`BackgroundColor`](https://docs.rs/bevy/latest/bevy/ui/struct.BackgroundColor.html), [`BorderRadius`](https://docs.rs/bevy/latest/bevy/ui/struct.BorderRadius.html), [`Outline`](https://docs.rs/bevy/latest/bevy/ui/struct.Outline.html), [`BoxShadow`](https://docs.rs/bevy/latest/bevy/ui/struct.BoxShadow.html), [`UiTransform`](https://docs.rs/bevy/latest/bevy/ui/struct.UiTransform.html) and [`ZIndex`](https://docs.rs/bevy/latest/bevy/ui/struct.ZIndex.html). Inserted automatically when the corresponding property is used (e.g. `background-color: red` inserts `BackgroundColor`). Check all supported properties at [`impls.rs`](https://github.com/eckz/bevy_flair/blob/main/crates/bevy_flair_core/src/impls.rs) |
| Shorthand properties                                                                                                                                                                                                    |   ✅   | [`border`](https://developer.mozilla.org/en-US/docs/Web/CSS/border), [`grid`](https://developer.mozilla.org/en-US/docs/Web/CSS/grid), `margin`, etc. They parse into individual properties (`margin` becomes `margin-left`, `margin-right`, ...), and transitions and animations work with them. Check all supported shorthands at [`shorthand.rs`](https://github.com/eckz/bevy_flair/blob/main/crates/bevy_flair_css_parser/src/shorthand.rs)                                                                                                                                                                                                                                                                                                                                                                    |
| [`ImageNode`](https://docs.rs/bevy/latest/bevy/ui/widget/struct.ImageNode.html) extensions                                                                                                                              |   ✅   | Non-standard. Example: `background-image: url("panel-border-030.png")`, `-bevy-image-mode: sliced(20.0px)`, `-bevy-image-rect: 0 0 64 64`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| Property inheritance                                                                                                                                                                                                    |   ✅   | e.g. `color`, `font-family`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| Inline CSS properties                                                                                                                                                                                                   |   ✅   |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| [`transition`](https://developer.mozilla.org/en-US/docs/Web/CSS/transition)                                                                                                                                             |   ✅   | All sub properties are supported (e.g. [`transition-timing-function`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Properties/transition-timing-function), [`transition-delay`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Properties/transition-delay))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| [`animation`](https://developer.mozilla.org/en-US/docs/Web/CSS/animation)                                                                                                                                               |   ✅   | All sub properties are supported (e.g. [`animation-fill-mode`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Properties/animation-fill-mode), [`animation-delay`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Properties/animation-delay))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| Custom `Time` for transitions and animations                                                                                                                                                                            |   ✅   | See the [custom time example](https://github.com/eckz/bevy_flair/blob/main/examples/custom_time.rs)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| [Logical properties and values](https://developer.mozilla.org/en-US/docs/Web/CSS/Guides/Logical_properties_and_values)                                                                                                  |   🔜   | e.g. `margin-inline` and `padding-block`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Discrete animations for [`display`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Properties/display) and [`visibility`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Properties/visibility) |   🔜   |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| `!important`                                                                                                                                                                                                            |   ⚠️   | Detected but ignored, and a warning is reported                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| Not supported [`<length>`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/length) units (e.g `rex`, `cqmin`, ...)                                                                                    |   🛑   | Missing in `bevy_ui` itself. Implementing missing `bevy_ui` features is a non-goal                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |

### Stylesheets and integration

| Feature                                                                                                | Status | Notes                                                                                                                                           |
|--------------------------------------------------------------------------------------------------------|:------:|-------------------------------------------------------------------------------------------------------------------------------------------------|
| Inherited stylesheets                                                                                  |   ✅   | Specify a [`Styled`](https://docs.rs/bevy_flair/latest/bevy_flair/style/components/enum.Styled.html) on a root node and all children inherit it |
| Different stylesheets per subtree                                                                      |   ✅   | Use a different `Styled` per subtree. It's even possible to apply no style to a given subtree                                                   |
| Hot-reloading                                                                                          |   ✅   | Edit your `.css` file and see styles re-applied on the fly. One of the main advantages over specifying styles directly in code                  |
| [`GhostNode`](https://docs.rs/bevy/latest/bevy/ui/experimental/struct.GhostNode.html) in the hierarchy |   ✅   | Ghost nodes are ignored. Enable the `ghost_nodes` feature for better support                                                                    |
| Custom properties                                                                                      |   ✅   | See the [sound effects example](https://github.com/eckz/bevy_flair/blob/main/examples/sound_effects.rs)                                         |
| Override parsing                                                                                       |   ✅   | See the [custom parsing example](https://github.com/eckz/bevy_flair/blob/main/examples/custom_parsing.rs)                                       |
| More than one stylesheet per entity                                                                    |   ❌   | Use `@import` instead                                                                                                                           |
| Global stylesheets                                                                                     |   ❌   |                                                                                                                                                 |
| Web Components and Shadow DOM                                                                          |   🛑   | Substantial complexity for features outside this crate's focus                                                                                  |

## Project goals

- Provide styling support for all existing `bevy_ui` components.
- Allow custom properties and make the parsing phase customizable.
- Follow CSS standards as much as it's reasonable.
  - One advantage of using `bevy_flair` is the capability of translating CSS that was built for `html` directly into Bevy.
- Only the properties directly defined are applied to the components.
  - If you never defined `display` in your stylesheets, not a single [`Node.display`](https://docs.rs/bevy/latest/bevy/prelude/struct.Node.html#structfield.display) will be touched. You should not worry about it getting overridden.
- Work independently on how entities are spawned.
  - Use `bsn!` or spawn elements manually, it should work in both ways.
- Hot-reloading should always work and not break your UI or `panic!`
- Efficient and reactive application of styles:
  - No unnecessary style re-application if the UI tree hasn't changed.
  - When modifications are detected in the UI tree, only the minimum affected nodes should get their style re-applied.
- CSS validation and reporting:
  - Invalid or unknown properties are reported, not silently ignored.
  - Errors in one rule don't block others.
  - No panics from malformed CSS. CSS that would make your application panic should be rejected and reported.
  - If any correctly parsed CSS can cause a panic in Bevy, it should be treated as a bug.

## Non-goals

- Defining a default style.
  - By default, if a property is not defined, it will not be modified. This means it is up to the author to set up fallback styling if needed.
  - There is support for `initial` values, which use the component's default value.
- Supporting every CSS feature or property.
  - CSS is a vast specification, so there are plenty of features that will not make sense to support in the Bevy context, like [`@supports`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/At-rules/@supports).
- Implementing missing Bevy features (e.g. unsupported units like `ch` not directly supported by Bevy, or properties like [`opacity`](https://developer.mozilla.org/en-US/docs/Web/CSS/opacity)).

## Bevy compatibility

| bevy | bevy_flair    |
| ---- | ------------- |
| 0.20 | 0.9           |
| 0.19 | 0.8           |
| 0.18 | 0.7           |
| 0.17 | 0.5, 0.6      |
| 0.16 | 0.2, 0.3, 0.4 |
| 0.15 | 0.1           |

## Contributing

Contributions are welcome! Feel free to fork the repository and submit a pull request.

## License

This project is licensed under the MIT License. See the LICENSE file for more details.

The assets included in this repository (for our examples) fall under different open licenses.

## Assets

- Poppins font. Designed by Indian Type Foundry, Jonny Pinhorn, Ninad Kale (<https://fonts.google.com/specimen/Poppins>) (SIL Open Font License, Version 1.1: `assets/fonts/OFL.txt`)
- Kenney Space Font from [Kenney Fonts](https://kenney.nl/assets/kenney-fonts) (CC0 1.0 Universal)
- UI borders from [Kenney's Fantasy UI Borders Kit](https://kenney.nl/assets/fantasy-ui-borders) (CC0 1.0 Universal)
