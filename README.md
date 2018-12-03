# gltf-easytensor

[![Hackage](https://img.shields.io/hackage/v/gltf-easytensor.svg)](https://hackage.haskell.org/package/gltf-easytensor)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/gltf-easytensor/badge/lts)](http://stackage.org/lts/package/gltf-easytensor)
[![Stackage Nightly](http://stackage.org/package/gltf-easytensor/badge/nightly)](http://stackage.org/nightly/package/gltf-easytensor)
[![Build status](https://secure.travis-ci.org/o1lo01ol1o/gltf-easytensor.svg)](https://travis-ci.org/o1lo01ol1o/gltf-easytensor)

Parsing and serialization support for the transfer of 3d assets as described by the (gltf specification (version 2.0))[https://github.com/KhronosGroup/glTF].

This package aims to support loading assets in the `gltf` format and efficient, no-copy conversions to datatypes used in the (vulkan-api)[https://github.com/achirkin/vulkan]

## Roadmap

- [x] FromJSON instances (supports loading `.gltf` files)
- [ ] uri ingestion to `PrimBytes` arrays for efficient interop with the `vulkan-api` library
- [ ] ToJSON instances for all data structures.
