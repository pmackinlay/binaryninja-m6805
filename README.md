# Motorola 6805 Architecture Plugin (v0.1)
Author: **Patrick Mackinlay**

_A disassembler, lifter and basic loader for the Motorola 6805 architecture._

## Description

[This plugin](m6805.py) disassembles Motorola 6805 assembly code and generates LLIL.

The binary view has the following known issues:

* no distinct signature for Motorola 6805 binaries

## Installation

To install this plugin, navigate to your Binary Ninja plugins directory, and run:

```git clone https://github.com/pmackinlay/binaryninja-m6805.git m6805```

## Minimum Version

This plugin requires the following minimum version of Binary Ninja:

 * 1.1.1689

## License

This plugin is released under a [MIT](LICENSE) license.
