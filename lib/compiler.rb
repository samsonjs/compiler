require "zeitwerk"

# Autoload everything under lib/compiler/.  The directory layout is the
# namespace: lib/compiler/asm/x86/registers.rb is Compiler::ASM::X86::Registers.
module Compiler
  Loader = Zeitwerk::Loader.for_gem
  Loader.inflector.inflect(
    "asm" => "ASM",
    "x86" => "X86",
    "elf" => "ELF",
    "macho" => "MachO",
    "cstruct" => "CStruct"
  )
  Loader.setup
end
