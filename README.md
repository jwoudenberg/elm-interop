# elm-interop

## Why another Haskell-Elm interop library?

There's a number of different libraries that support generating Elm types, encoders, and decoders from Haskell types. And these often have companion libraries that support generating Elm functions for calling endpoints in Haskell Servant webserver applications.

This library seeks to improve on alternatives in a couple of ways.

- It implements the simplest possible API. It suffices to pass in a Servant API type to generate a complete Elm module, including client functions for all the endpoints defined in the API, Elm types for the values that get passed over the API, and Elm encoders and decoders for these types. It's not necessary to manually define which Elm types you want to generate code for.
- Deriving type classes to generate Elm code for custom types uses GHC's built-in `Generics` mechanism.
- Some libraries generate Elm encoders and decoders that are compatible with the Haskell encoders and decoders that the Aeson library can generate for Haskell types. This works well, unless you define (or generate) a custom Aeson encoder or decoder for one of the types exchanged with Elm. This action will break compatibility between Haskell and Elm encoders and decoders resulting in runtime errors. This library's design does not include this trap.
- Another trap is using complicated types as URL parameters and header values. It's hard to ensure generated Elm code encodes and decoders these structures in the same way that the Haskell code does. To library only allows a small number of primitive types and simple wrappers of a primitive to be used in parameters to help avoid users from falling into this trap.
- This library liberally takes dependencies on common Haskell libraries to ensure commonly used Haskell types support Elm code generation out of the box. PRs adding support for even more common types are welcome.
- This library avoids dependencies on the Elm side, so the generated Elm code is as portable and stable as possible. For example: generated Elm JSON decoders do not make use of the `json-pipeline` library.

## Design goals

Goals:

- Reliability: Don't generate broken Elm code. No runtime errors. No traps.
- Operability: Support a wide range of custom Haskell types, including common types from libraries.
- Ease of use: Pass it a servant API and you're done.
- Discoverability: Generated Elm code is readable, well-structured, and commented.
- Low Elm-dependencies: Avoid Elm dependencies in generated code (a dependency on `elm/json` seems unavoidable).

Non-goals:

- Configurability: For example being able to configure the style of Elm code generated.
- Human-readable wire format: This is a nice-to-have, but not a core objective off the lib.
  We'll compromise on this point if it helps achieve points in the 'goals' section.
  We think this is okay: users can more easily see what's going over the write by putting probes on the Haskell or the Elm side. Decoding errors are bugs in this library.
