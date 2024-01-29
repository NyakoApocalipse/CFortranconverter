For project introduction and steps to build and install, refer to [CFortranTranslator](https://github.com/CalvinNeo/CFortranTranslator).

The original project implement basic statement-wise translation. We try to complete a few missing features such as:

- Cray `POINTER`, `DATA`, `USE` statement
- `READ` from `string`
- Line continuation
- `=>` pointer assignment

and also add support for `MODULE` and multi program unit translation

