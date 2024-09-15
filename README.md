# Moka-DOM

Moka-DOM is a Haskell library providing bindings for JavaScript DOM functions, designed to work with the new GHC JavaScript backend. It offers a convenient and type-safe way to manipulate the DOM in Haskell when targeting JavaScript as the compilation output.

## Project Status: Alpha

Moka-DOM is currently in alpha stage. This means:

- The API is unstable and may change significantly between versions.
- Some features may be incomplete or missing.
- There may be undiscovered bugs or performance issues.
- Documentation may be incomplete.

Feedback, bug reports, and contributions are very welcome, but please note that this library is not yet ready for production use. Use it at your own risk in non-critical projects.

## Features

- DOM element creation and manipulation
- Event handling
- Cookie management
- Local storage operations
- AJAX requests
- URL manipulation
- Form handling
- Helper functions for common DOM operations

## Requirements

- This library uses the GHC JS backend. While I intend to provide support for setting this up in the near future, for now it's best to install the JS backend following the instructions [here](https://anvilproject.org/guides/content/creating-links).

## Considerations

- There is a lot of documentation for the GHC JS backend. I will provide links below; it's recommended you become familiar with these.

- As both this library and the JS backend are relatively new there are some things that dont work as expected. For example I was unable to get event triggered callbacks to works anywhere except when attached to the document, to work around this I use a global event listener with a unique attribute. I spent a lot of time trying to debug more localized event listeners, I was unable to get them working. They may works and I was unsuccessful, there may be a bug in the haskell runtime or it could be some other minor issue. However I think stuff like this should be expected.

- The result of compilation will be a JS bundle. To view the result, you will need a server and browser. Any will do, but I recommend something like the Quark server and Surf browser from the suckless community. It is my intention to provide modified versions of these in the future that will be configured with Haskell and used for development and testing.

- This project will strive to provide first-class support for Nix and NixOS. If you have the option, I recommend using Nix from the start.

- Using the JS backend requires compiling GHC from source. Unfortunately, HLS and Haddock are not easily available.

## GHC JS Documentation

1. [Wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript-backend)
2. [User guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html)
3. [tutorials](https://engineering.iog.io/2023-01-24-javascript-browser-tutorial/)

## Installation

It's assumed you have the GHC JS compiler set up. You can simply run `cabal build` to get access to the tests. The tests serve as basic examples as well as a sanity check for what works.

When built, the files are generated in a location like this:

```
moka-dom/dist-newstyle/build/javascript-ghcjs/ghc-9.10.0.20240413/moka-dom-0.0.0.1/x/moka-dom-test/build/moka-dom-test/moka-dom-test.jsexe
```

It's a little awkward to find, and when I add supporting build tools and scripts, I will address it. For now, I think it's good to leave it like this because this is the default behavior.

To take a look at the tests, just serve this folder to localhost and take a look. The tests will output to the DOM and there will also be output in the console, which can be accessed from your browsers inspector.

To use Moka-DOM in your project, add it to your project's `.cabal` file:

```
build-depends: moka-dom
```

Note: Moka-DOM requires the GHC JavaScript backend, which is still in active development.

## Usage

Here's a simple example of how to use Moka-DOM:

```haskell
import Moka.DOM.Elements
import Moka.DOM.Events
import Moka.DOM.Manipulation

main :: IO ()
main = do
  maybeButton <- createElement "button"
  case maybeButton of
    Just button -> do
      setInnerHTML button "Click me!"
      appendToBody button
      addEventListener "click" Nothing $ \_ -> do
        consoleLog "Button clicked!"
    Nothing -> consoleLog "Failed to create button"
```

## Haddock Support

Due to the current state of the GHC JavaScript backend, standard Haddock documentation is not yet available (but the code is documented to support Haddock). However, each module and major function is documented in the source code.

## Testing

The project includes tests that demonstrates the usage of various Moka-DOM functions. To run the tests:

1. Compile the test suite with the GHC JavaScript backend.
2. Open the resulting HTML file in a web browser.
3. Check the browser console and the rendered page for test results.

Because this is a alpha library written in Haskell and JS to work with a new backend on the GHC, it's not easy to effectively test it like traditional Haskell. Because of this, I opted to focus on end-to-end tests with the results showing in the browser.

Hopefully, in the future, more robust and even automated testing will become available.

## Contributing

Any and all contributions and feedback are welcome! Please feel free to submit a Pull Request.

You can check CONTRIBUTING.md for more information on contributing.

## License

This project is licensed under the BSD-3-Clause License - see the [LICENSE](LICENSE) file for details.

## Contact

Please feel free to reach out. I try to respond in a timely fashion. At this early stage, all interactions are incredibly valuable, so please contact me with any questions or thoughts that come up.

Shane - <shane@peregrinum.dev>

## Project Philosophy

This project and the framework it supports are named after the Moka pot, a reference to the small stovetop coffee pot popular in Europe. Besides the mandatory caffeine reference with a JS framework, the Moka pot also symbolizes the goals of this project: to provide a simple and robust tool that will produce the same results as much larger and complex systems. On starting the project, it was my intention to provide a lightweight and simple set of tools that a developer could use to achieve their heart's desire when it comes to frontend development.

This library is more about providing you the tools to make your own batteries rather than having batteries included. It was my intention from the start to provide a generic set of tools and, as much as possible, avoid opinionated design decisions. This flexibility unfortunately comes at a cost, and the trade-off is that the developer will have to do a lot more to achieve the same results as other frameworks.

The other thing I wanted to provide is quick and easy access to the underlying JS, HTML, and CSS so a developer could, in theory, quickly convert any examples or designs they came across online.

It is my intention with Moka to provide a full user-friendly and performant framework for the Haskell community, but I want to provide that in isolated parts like this DOM bindings library so users can take as much or as little of the offering as they need.

For me, this library is a means to an end and not the end in itself. I am most excited about working on more complex aspects of frontend development in Haskell, but to do so, I needed to start somewhere, and that is the point of this library.

## Acknowledgements

- The GHC team for their work on the JavaScript backend
- The Haskell community for their continuous support and inspiration
