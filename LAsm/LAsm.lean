-- This module serves as the root of the `LAsm` library.
-- We build a simple functional assembler that lowers a custom assembly syntax to textual assembly.
import Std.Internal.Parsec
import LAsm.Basic
set_option relaxedAutoImplicit false

namespace LAsm
open Std.Internal.Parsec

abbrev Parsec := Std.Internal.Parsec String


def Std.Internal.Parsec.choice {α : Type}[Alternative Parsec][OrElse (Parsec α)] (ps : Array (Parsec α)) : Parsec α :=
  let init : Parsec α := failure
  ps.foldl (fun acc p => acc <|> p) init

/-- The set of registers available in our assembly language. -/
inductive Register where
  /-- The general-purpose accumulator register (e.g., for operations like `mov eax, ebx`). -/
  | eax
  /-- The general-purpose base register (e.g., used in instructions such as `mov ebx, [addr]`). -/
  | ebx
  /-- The general-purpose counter register (e.g., often seen in looping as `add ecx, 1`). -/
  | ecx
  /-- The general-purpose data register (e.g., utilized in arithmetic like `sub edx, ecx`). -/
  | edx
  /-- The source index register (e.g., employed in string operations such as `mov esi, edi`). -/
  | esi
  /-- The destination index register (e.g., similarly in string operations as in `mov edi, esi`). -/
  | edi
  deriving Repr, DecidableEq, Inhabited, BEq

instance : ToString Register where
  toString
    | .eax => "eax"
    | .ebx => "ebx"
    | .ecx => "ecx"
    | .edx => "edx"
    | .esi => "esi"
    | .edi => "edi"



/-- Define operands, which can be registers, immediates, or memory addresses. -/
inductive Operand where
  /-- An operand that is a register. -/
  | reg (r : Register)
  /-- An operand that is an immediate value. -/
  | imm (n : Int)
  /-- An operand that is a memory address. -/
  | mem (addr : String)
  deriving Repr, DecidableEq, Inhabited, BEq

instance : ToString Operand where
  toString
    | .reg r  => s!"{r}"
    | .imm n  => s!"{n}"
    | .mem a  => s!"[{a}]"

/-- Define a simplified assembly instruction set. -/
inductive Instruction where
  | mov   (dst : Operand) (src : Operand)
  | add   (dst : Operand) (src : Operand)
  | sub   (dst : Operand) (src : Operand)
  | jmp   (label : String)
  | label (name : String)
  deriving Repr, DecidableEq, Inhabited, BEq

instance : ToString Instruction where
  toString
    | .mov dst src   => s!"mov {dst}, {src}"
    | .add dst src   => s!"add {dst}, {src}"
    | .sub dst src   => s!"sub {dst}, {src}"
    | .jmp lbl       => s!"jmp {lbl}"
    | .label name    => s!"{name}:"

/-- A Program is a list of Instructions. -/
abbrev Program := Array Instruction

def intercalate {ρ : Type} {α : Type} [ForIn Id ρ α] [ToString α] (xs : ρ) (sep : String) : String := Id.run do
  let mut acc  := ""
  for x in xs do
    acc := acc ++ (toString x) ++ sep
  return acc

/--
  Lower a program to a string, where each instruction is on its own line.
  This is the "syntax lowering" phase of our assembler.
-/
def lower (prog : Program) : String :=
  intercalate prog "\n"

/--
  Sample program showcasing our simplified assembler.
  It sets values using mov, manipulates them with add and sub, and finally jumps back to a label.
-/
def sampleProgram : Program :=
  #[ .label "start",
    .mov (.reg .eax) (.imm 10),
    .mov (.reg .ebx) (.imm 20),
    .add (.reg .eax) (.reg .ebx),
    .sub (.reg .eax) (.imm 5),
    .jmp "start"  -- represents an infinite loop for demonstration
  ]

/--
  A simple evaluation loop to lower a sample program and output intermediate diagnostics.
  We use dbg_trace to log the generated assembly.
-/
def runSampleProgram : IO Unit :=
  do
    let asm := lower sampleProgram
    dbg_trace ("Generated Assembly:\n" ++ asm)
    IO.println asm


/-
  Parsing implementation for our assembly language using Parsec.
  We define parsers for registers, operands, and full instructions,
  then combine them to parse entire programs.
-/

section Parsing
open Std.Internal.Parsec

/-- Parse a register name (eax/ebx/ecx/edx/esi/edi) -/
def register : Parsec Register :=
  choice
    [ "eax" *> pure .eax
    , "ebx" *> pure .ebx
    , "ecx" *> pure .ecx
    , "edx" *> pure .edx
    , "esi" *> pure .esi
    , "edi" *> pure .edi
    ]

/-- Parse an operand (register, immediate, or memory address) -/
def operand : Parsec Operand :=
  (mem <$> (ch '[' *> takeWhile1 (· ≠ ']') <* ch ']')) <|>  -- [addr]
  (imm <$> num) <|>                                         -- 42
  (reg <$> register)                                         -- eax
where
  num := do
    let sign ← optional (ch '-' <|> ch '+')
    let digits ← takeWhile1 (·.isDigit)
    let n := digits.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
    pure <| match sign with
      | some '-' => -n
      | _ => n

/-- Parse a full instruction -/
def instruction : Parsec Instruction :=
  (skipString "mov"   *> ws *> Instruction.mov   <$> operand <* ws <* ch ',' <* ws <*> operand) <|>
  (skipString "add"   *> ws *> Instruction.add   <$> operand <* ws <* ch ',' <* ws <*> operand) <|>
  (skipString "sub"   *> ws *> Instruction.sub   <$> operand <* ws <* ch ',' <* ws <*> operand) <|>
  (skipString "jmp"   *> ws *> Instruction.jmp   <$> takeWhile1 (·.isAlphanum)) <|>
  (Instruction.label  <$> (takeWhile1 (·.isAlphanum) <* ch ':'))

/-- Parse a full program (sequence of instructions) -/
def program : Parsec Program :=
  many (ws *> instruction <* ws) <* eof

/-- Parse a string into a Program, returning Except for error handling -/
def parseProgram (input : String) : Except String Program :=
  match program input.mkIterator with
  | .success _ res => .ok res
  | .error it err  => .error s!"Parse error at {it.i.repr}: {err}"

/- Test roundtrip: lower sample program then parse it back -/
#eval parseProgram (lower sampleProgram)

end Parsing

/-
  #eval will run this loop in a Lake build environment, allowing us to see intermediate outputs.
-/
#eval runSampleProgram

/--
  Lints a program to ensure label correctness.
  Currently, it checks if all jump targets are defined labels within the program.
  Returns `Except String Unit`: `ok ()` if the program is valid, `error msg` with an error message otherwise.
-/
def lintProgram (prog : Program) : Except String Unit := Id.run do
  let mut labels : Array String := #[]
  for instr in prog do
    match instr with
    | .label name => labels := labels.push name
    | _ => pure ()

  for instr in prog do
    match instr with
    | .jmp lbl =>
      if !labels.contains lbl then
        return Except.error s!"Jump to undefined label: '{lbl}'"
      else
        pure ()
    | _ => pure ()
  return Except.ok ()

/- Example of linting a valid program. -/
#eval lintProgram sampleProgram

/-- Example of linting an invalid program with an undefined jump target. -/
def invalidProgram : Program :=
  #[ .jmp "nonexistent_label" ]

#eval lintProgram invalidProgram



end LAsm
