# Haskell bindings to Google's V8 JavaScript Engine

This is still in a very early stage.

## Getting started with the code

Currently you have to build V8 yourself and copy the libraries and headers
manually.

#### Build V8

See [V8's download and build instructions](https://developers.google.com/v8/build).

After building V8, run the following commands from this repositories root
directory:

```console
$ cp -r <path-to-google-v8>/out/native/obj.target/tools/gyp/ lib
$ cp -r <path-to-google-v8>/include/ .
```

#### Build the bindings and run the test suite

```
$ cabal configure --enable-tests && cabal build && ./dist/build/spec/spec
```
