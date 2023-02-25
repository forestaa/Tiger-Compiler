module Compiler.Backend.X86.File (writeAssemblyFile) where

import Compiler.Backend.X86.Arch (Assembly (..), Label (..), Register (..))
import Compiler.Backend.X86.Frame qualified as F (ProgramFragmentX86)
import Compiler.Utils.String (unlines)
import RIO hiding (unlines)

writeAssemblyFile :: [F.ProgramFragmentX86 [Assembly Register]] -> Utf8Builder
writeAssemblyFile fragments = unlines $ writeProgramFragment <$> fragments

writeProgramFragment :: F.ProgramFragmentX86 [Assembly Register] -> Utf8Builder
writeProgramFragment fragment = unlines $ writeAssembly <$> fragment.body

writeAssembly :: Assembly Register -> Utf8Builder
writeAssembly (MovImmediate i target) = fold [delimeter, "movl", delimeter, writeImmediate i, ", ", writeRegister target]
writeAssembly (MovRegister source target) = fold [delimeter, "movq", delimeter, writeRegister source, ", ", writeRegister target]
writeAssembly (Jump label) = fold [delimeter, "jmp", delimeter, writeLabel label]
writeAssembly (Label label) = fold [writeLabel label, ":"]
writeAssembly (MovLoadIndirect offset base target) = fold [delimeter, "movl", delimeter, writeRegisterIndirectAccess offset base, ", ", writeRegister target]
writeAssembly (SubImmediate offset target) = fold [delimeter, "subq", delimeter, writeImmediate offset, ", ", writeRegister target]
writeAssembly (MulRegister source target) = fold [delimeter, "imul", delimeter, writeRegister source, ", ", writeRegister target]
writeAssembly (Call label) = fold [delimeter, "call" , delimeter, writeLabel label]
writeAssembly (MovStoreIndirect source offset base) = fold [delimeter, "movl", delimeter, writeRegister source, ", ", writeRegisterIndirectAccess offset base]
writeAssembly (CmpImmediate source i) = fold [delimeter, "cmpl", delimeter, writeRegister source, ", ", writeImmediate i]
writeAssembly (JumpIfEqual label) = fold [delimeter, "je", delimeter, writeLabel label]
writeAssembly Leave = fold [delimeter, "leave"]
writeAssembly Ret = fold [delimeter, "ret"]

-- writeAssembly (MovImmediateLabel Label register)
-- writeAssembly MovLoad Memory register
-- writeAssembly MovLoadDisplacement Int register register Integer register
-- writeAssembly MovStore register Memory
-- writeAssembly Lea Label register register
-- writeAssembly AddImmediate Int register
-- writeAssembly AddRegister register register
-- writeAssembly SubRegister register register
-- writeAssembly MulImmediate Int register
-- writeAssembly CmpRegister register register
-- writeAssembly JumpIfNotEqual Label
-- writeAssembly JumpIfLessThan Label
-- writeAssembly JumpIfGreaterThan Label
-- writeAssembly JumpIfEqualOrLessThan Label
-- writeAssembly JumpIfEqualOrGreaterThan Label
-- writeAssembly PushImmediate Int
-- writeAssembly PushRegister register
-- writeAssembly Pop register
-- writeAssembly Label Label
-- writeAssembly Global Label
-- writeAssembly Data
-- writeAssembly Text
-- writeAssembly Align Int
-- writeAssembly Type Label -- TODO:
-- writeAssembly Size Label Int
-- writeAssembly String String
-- writeAssembly Zero Int
-- writeAssembly Long Int

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

writeImmediate :: Int -> Utf8Builder
writeImmediate i = "$" <> display i

writeLabel :: Label -> Utf8Builder
writeLabel (Label' label') = display label'

writeRegisterIndirectAccess :: Int -> Register -> Utf8Builder
writeRegisterIndirectAccess i register = fold [display i, "(", writeRegister register ,")" ]
