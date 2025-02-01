# LAsm

A simple assembler written in Lean 4 and lowered.

This project defines a functional assembly language that uses compositional
and strongly-typed design idioms to construct, manipulate, and lower assembler
instructions into standard textual assembly. The lowering phase generates
assembly code that could, in principle, be run on a target architecture.

To try out the sample program, simply run:
  lake build
and then use Lean's evaluation loop with:
  lean --run src/LAsm.lean
or use the #eval commands in Lean 4.

For more details, see the source in [LAsm/LAsm.lean](LAsm/LAsm.lean).

Inspired in part by the [movfuscator](https://github.com/xoreaxeaxeax/movfuscator) project.