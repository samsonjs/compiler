#!/usr/bin/env ruby

require "../build"

# usage: compare.rb <func> [outdir] [binformat]
#
# Builds one fixture both ways and compares the instructions that come
# out.  The two back-ends should agree on what to emit even though they
# don't agree on how to encode it: we always use rel32 jumps where nasm
# picks rel8, which shifts every address after the first short jump.
# So addresses are normalized away and the instructions compared.

# Instructions in objdump's listing are "  <addr>:\t<mnemonic> <operands>".
INSTRUCTION = /^\s*[0-9a-f]+:\t(.+)$/

# The symbol name objdump appends to a jump or call target.
SYMBOL = /\s*<[^>]+>\s*$/

# An address in the text segment, whether a bare jump target or an 0x
# memory reference.  Immediates are prefixed with $ and don't match.
ADDRESS = /(?<![$\w])(?:0x)?0?8[0-9a-f]{6}\b/

def main
  func = ARGV.shift
  outdir = ARGV.shift || "."
  Dir.mkdir(outdir) unless File.exist?(outdir)
  binformat = (ARGV.shift || "elf").downcase
  platform = `uname -s`.chomp.downcase
  print "comparing #{func} ... "

  listings = %w[bin asm].map do |format|
    disassemble(builder(format).call("test_#{func}.code", outdir, platform, binformat))
  end

  if listings[0] == listings[1]
    puts "same (#{listings[0].length} instructions)"
  else
    puts "DIFFER!"
    report(*listings)
    exit(1)
  end
end

def disassemble(exefile)
  output = `objdump -d --no-show-raw-insn #{exefile}`
  raise "objdump failed: #{$?.exitstatus}" unless $?.exitstatus == 0

  output.lines.filter_map do |line|
    next unless line =~ INSTRUCTION
    normalize($1)
  end
end

# Drop the symbol names objdump appends and the addresses that shift
# when the two back-ends choose different encodings.
def normalize(instruction)
  instruction
    .sub(SYMBOL, "")
    .gsub(ADDRESS, "ADDR")
    .squeeze(" ")
    .strip
end

def report(from_bin, from_asm)
  width = [from_bin.length, from_asm.length].max
  (0...width).each do |i|
    next if from_bin[i] == from_asm[i]
    puts "  #{i}: bin #{from_bin[i].inspect} != asm #{from_asm[i].inspect}"
  end
end

main if $0 == __FILE__
