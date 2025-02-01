# CHANGELOG

## [0.1.0] - 2025-01-31
### Added
- Initial implementation of a simplified assembler in Lean 4.
- Basic support for instructions: mov, add, sub, jmp, and label.
- Lowering function to convert instructions to assembly text.
- Evaluation loop using `#eval` with `dbg_trace` for diagnostics.
