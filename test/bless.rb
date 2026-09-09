#!/usr/bin/env ruby

require_relative "../lib/compiler"

# usage: bless.rb <func> [outdir] [binformat] [format]
#
# Builds one fixture, runs it, and records what it printed in
# test_<func>.expected for review as a diff.  A fixture that prints
# nothing gets no file at all, which is the absence test.rb reads as
# "expected to print nothing".
#
# A fixture that exits non-zero is refused rather than recorded: its
# output is whatever it managed to print before going wrong, and
# blessing that would bake the failure into the expectations.

def main
  func = ARGV.shift
  outdir = ARGV.shift || "."
  Dir.mkdir(outdir) unless File.exist?(outdir)
  binformat = (ARGV.shift || "elf").downcase
  format = (ARGV.shift || "bin").downcase
  platform = `uname -s`.chomp.downcase
  print "blessing #{func} ... "

  result = Compiler::Build.run(Compiler::Build.builder(format).call("test_#{func}.code", outdir, platform, binformat))
  if result.status != 0
    puts "REFUSED! exited #{result.status}"
    exit(1)
  end

  filename = "test_#{func}.expected"
  if result.output.empty?
    File.delete(filename) if File.exist?(filename)
    puts "prints nothing"
  else
    File.write(filename, result.output)
    puts "#{result.output.lines.length} lines"
  end
end

main if $0 == __FILE__
