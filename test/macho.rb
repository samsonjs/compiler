#!/usr/bin/env ruby

require_relative "../lib/compiler"

# usage: macho.rb <func> [outdir]
#
# Compiles one fixture to a Mach-O object and checks the bytes against the
# format.  Nothing is linked or run: the output is 32-bit x86, which no
# current macOS can execute, so this is as far as the Mach-O back-end can
# be exercised until #11 lands.
#
# The header is unpacked here against the format rather than read back
# through the writer's own structs, so a wrong constant in the writer
# can't agree with itself and pass.

MH_MAGIC = 0xfeedface
CPU_TYPE_X86 = 7
MH_OBJECT = 0x1
HEADER_BYTES = 28 # seven uint32s, with the load commands following

def main
  func = ARGV.shift
  outdir = ARGV.shift || "."
  Dir.mkdir(outdir) unless File.exist?(outdir)
  print "checking macho #{func} ... "

  # Distinct from the ELF build's test_<func>.o in the same directory.
  objfile = File.join(outdir, "test_#{func}.macho.o")
  File.binwrite(objfile, assemble("test_#{func}.code"))
  check(File.binread(objfile))
  puts "ok (#{File.size(objfile)} bytes)"
end

def assemble(source)
  asm = Compiler::ASM::X86::BinaryAssembler.new(
    "darwin",
    Compiler::ASM::MachO::SymbolTable.new,
    Compiler::ASM::MachO::ObjectFile
  )
  File.open(source) { |input| Compiler::Parser.new(input, asm).compile }
end

def check(bytes)
  expect(bytes.bytesize > HEADER_BYTES, "file is too small to hold a header")
  magic, cputype, _cpusubtype, filetype, ncmds, sizeofcmds = bytes.unpack("L<l<l<L<L<L<")

  expect(magic == MH_MAGIC, format("magic is %#x, expected %#x", magic, MH_MAGIC))
  expect(cputype == CPU_TYPE_X86, "cputype is #{cputype}, expected #{CPU_TYPE_X86}")
  expect(filetype == MH_OBJECT, "filetype is #{filetype}, expected #{MH_OBJECT}")
  expect(ncmds > 0, "no load commands")
  expect(HEADER_BYTES + sizeofcmds <= bytes.bytesize,
    "load commands claim #{sizeofcmds} bytes, past the end of a #{bytes.bytesize} byte file")
  expect(bytes.include?("__text"), "no __text section")
end

def expect(condition, message)
  return if condition
  puts "FAIL!"
  warn "  #{message}"
  exit(1)
end

main if $0 == __FILE__
