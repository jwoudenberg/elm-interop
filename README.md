# elm-interop

Goals:

- Reliability: Don't generate broken Elm code. No runtime errors.
- Operability: Support a wide range of custom Haskell types, including common types from libraries.
- Ease of use: Pass it a servant API and you're done.
- Discoverability: Generated Elm code is readable, well-structured, and commented.
- Low Elm-dependencies: Avoid Elm dependencies in generated code (a dependency on `elm/json` seems unavoidable).

Non-goals:

- Configurability: For example being able to configure the style of Elm code generated.
- Human-readable wire format: This is a nice-to-have, but not a core objective off the lib.
  We'll compromise on this point if it helps achieve points in the 'goals' section.
  We think this is okay: users can more easily see what's going over the write by putting probes on the Haskell or the Elm side. Decoding errors are bugs in this library.
