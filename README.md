# gltf-hs

[![Hackage](https://img.shields.io/hackage/v/gltf-hs.svg)](https://hackage.haskell.org/package/gltf-hs)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/gltf-hs/badge/lts)](http://stackage.org/lts/package/gltf-hs)
[![Stackage Nightly](http://stackage.org/package/gltf-hs/badge/nightly)](http://stackage.org/nightly/package/gltf-hs)
[![Build status](https://secure.travis-ci.org/o1lo01ol1o/gltf-hs.svg)](https://travis-ci.org/o1lo01ol1o/gltf-hs)

Parsing and serialization support for the transfer of 3d assets as described by the by the Khronos Group's [gltf specification (version 2.0)](https://github.com/KhronosGroup/glTF).

This package aims to support loading assets in the `gltf` format and efficient, no-copy conversions to datatypes used in the [vulkan-api](https://github.com/achirkin/vulkan)

## Roadmap

- [x] FromJSON instances (supports loading `.gltf` files)
- [ ] uri ingestion to `PrimBytes` arrays for efficient interop with the `vulkan-api` library
- [ ] ToJSON instances for all data structures.
