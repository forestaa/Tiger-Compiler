module Compiler.Backend.X86.File (writeAssemblyFile) where

import Compiler.Backend.X86.Arch (Assembly (..), Label (..), Memory (..), Register (..), Type (Function, Object))
import Compiler.Backend.X86.Frame qualified as F (ProgramFragmentX86)
import Compiler.Utils.String (unlines)
import RIO hiding (unlines)

writeAssemblyFile :: [F.ProgramFragmentX86 [Assembly Register]] -> Utf8Builder
writeAssemblyFile fragments = mconcat $ writeProgramFragment <$> fragments

writeProgramFragment :: F.ProgramFragmentX86 [Assembly Register] -> Utf8Builder
writeProgramFragment fragment = unlines $ writeAssembly <$> fragment.body

writeAssembly :: Assembly Register -> Utf8Builder
writeAssembly (MovImmediate i target) = fold [delimeter, "movq", delimeter, writeImmediate i, ", ", writeRegister target]
writeAssembly (MovImmediateLabel label target) = fold [delimeter, "movq", writeImmediate label, ", ", writeRegister target]
writeAssembly (MovRegister source target) = fold [delimeter, "movq", delimeter, writeRegister source, ", ", writeRegister target]
writeAssembly (MovLoad memory register) = fold [delimeter, "movq", delimeter, writeMemory memory, ", ", writeRegister register]
writeAssembly (MovLoadIndirect offset base target) = fold [delimeter, "movq", delimeter, writeRegisterIndirectAccess offset base, ", ", writeRegister target]
writeAssembly (MovLoadDisplacement offset base1 base2 scale target) = fold [delimeter, "movq", delimeter, writeDisplacement offset base1 base2 scale, ", ", writeRegister target]
writeAssembly (MovStore source target) = fold [delimeter, "movq", delimeter, writeRegister source, ", ", writeMemory target]
writeAssembly (MovStoreIndirect source offset base) = fold [delimeter, "movq", delimeter, writeRegister source, ", ", writeRegisterIndirectAccess offset base]
writeAssembly (Lea label base target) = fold [delimeter, "leaq", delimeter, writeLabel label, "(", writeRegister base, ")", ", ", writeRegister target]
writeAssembly (AddImmediate offset target) = fold [delimeter, "addq", delimeter, writeImmediate offset, ", ", writeRegister target]
writeAssembly (AddRegister source target) = fold [delimeter, "addq", delimeter, writeRegister source, ", ", writeRegister target]
writeAssembly (SubImmediate offset target) = fold [delimeter, "subq", delimeter, writeImmediate offset, ", ", writeRegister target]
writeAssembly (SubRegister source target) = fold [delimeter, "subq", delimeter, writeRegister source, ", ", writeRegister target]
writeAssembly (MulImmediate offset target) = fold [delimeter, "imul", delimeter, writeImmediate offset, ", ", writeRegister target]
writeAssembly (MulRegister source target) = fold [delimeter, "imul", delimeter, writeRegister source, ", ", writeRegister target]
writeAssembly (CmpImmediate source i) = fold [delimeter, "cmpq", delimeter, writeImmediate i, ", ", writeRegister source]
writeAssembly (CmpRegister left right) = fold [delimeter, "cmpq", delimeter, writeRegister left, ", ", writeRegister right]
writeAssembly (Jump label) = fold [delimeter, "jmp", delimeter, writeLabel label]
writeAssembly (JumpIfEqual label) = fold [delimeter, "je", delimeter, writeLabel label]
writeAssembly (JumpIfNotEqual label) = fold [delimeter, "jne", delimeter, writeLabel label]
writeAssembly (JumpIfGreaterThan label) = fold [delimeter, "jg", delimeter, writeLabel label]
writeAssembly (JumpIfLessThan label) = fold [delimeter, "jl", delimeter, writeLabel label]
writeAssembly (JumpIfEqualOrGreaterThan label) = fold [delimeter, "jge", delimeter, writeLabel label]
writeAssembly (JumpIfEqualOrLessThan label) = fold [delimeter, "jle", delimeter, writeLabel label]
writeAssembly (Call label) = fold [delimeter, "call", delimeter, writeLabel label]
writeAssembly Leave = fold [delimeter, "leave"]
writeAssembly Ret = fold [delimeter, "ret"]
writeAssembly (PushImmediate i) = fold [delimeter, "pushq", delimeter, writeImmediate i]
writeAssembly (PushRegister source) = fold [delimeter, "pushq", delimeter, writeRegister source]
writeAssembly (Pop target) = fold [delimeter, "popq", delimeter, writeRegister target]
writeAssembly (Label label) = fold [writeLabel label, ":"]
writeAssembly (Global label) = fold [delimeter, ".globl", delimeter, writeLabel label]
writeAssembly Data = fold [delimeter, ".data"]
writeAssembly Text = fold [delimeter, ".text"]
writeAssembly (Align i) = fold [delimeter, ".align", delimeter, display i]
writeAssembly (Type label ty) = fold [delimeter, ".type", " ", writeLabel label, ", ", writeType ty]
writeAssembly (Size label size) = fold [delimeter, ".size", " ", writeLabel label, ", ", display size]
writeAssembly (String text) = fold [delimeter, ".string", delimeter, display text]
writeAssembly (Zero n) = fold [delimeter, ".zero", " ", display n]
writeAssembly (Long i) = fold [delimeter, ".long", " ", display i]

delimeter :: Utf8Builder
delimeter = "\t"

writeRegister :: Register -> Utf8Builder
writeRegister RAX = "%rax"
writeRegister RDI = "%rdi"
writeRegister RSI = "%rsi"
writeRegister RDX = "%rdx"
writeRegister RCX = "%rcx"
writeRegister RBP = "%rbp"
writeRegister RSP = "%rsp"
writeRegister RBX = "%rbx"
writeRegister R8 = "%r8"
writeRegister R9 = "%r9"
writeRegister R10 = "%r10"
writeRegister R11 = "%r11"
writeRegister R12 = "%r12"
writeRegister R13 = "%r13"
writeRegister R14 = "%r14"
writeRegister R15 = "%r15"
writeRegister RIP = "%rip"
writeRegister EFLAGS = "%eflags"

writeImmediate :: Display a => a -> Utf8Builder
writeImmediate i = "$" <> display i

writeLabel :: Label -> Utf8Builder
writeLabel (Label' label') = display label'

writeMemory :: Memory -> Utf8Builder
writeMemory (Memory memory) = display memory

writeType :: Type -> Utf8Builder
writeType Function = "@function"
writeType Object = "@object"

writeRegisterIndirectAccess :: Int -> Register -> Utf8Builder
writeRegisterIndirectAccess 0 register = fold ["(", writeRegister register, ")"]
writeRegisterIndirectAccess offset register = fold [display offset, "(", writeRegister register, ")"]

writeDisplacement :: Int -> Register -> Register -> Int -> Utf8Builder
writeDisplacement 0 base1 base2 scale = fold ["(", writeRegister base1, ",", writeRegister base2, ", ", display scale, ")"]
writeDisplacement offset base1 base2 scale = fold [display offset, "(", writeRegister base1, ",", writeRegister base2, ", ", display scale, ")"]
