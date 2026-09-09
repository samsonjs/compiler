#!/usr/bin/env ruby

require '../build'

# usage: test.rb <func> [outdir] [binformat] [format]

def main
  func = ARGV.shift
  outdir = ARGV.shift || '.'
  Dir.mkdir(outdir) unless File.exist?(outdir)
  binformat = (ARGV.shift || 'elf').downcase
  format = (ARGV.shift || 'bin').downcase
  platform = `uname -s`.chomp.downcase
  print "testing #{func} ... "
  result = run(builder(format).call("test_#{func}.code", outdir, platform, binformat))

  if result.status != 0
    puts "FAIL! (#{result.status})"
    exit(1)
  end

  # A test without a .expected file is expected to print nothing.
  expected = expected_output(func)
  if result.output != expected
    puts "FAIL! (output)"
    report(expected, result.output)
    exit(1)
  end

  puts "pass"
end

def expected_output(func)
  filename = "test_#{func}.expected"
  File.readable?(filename) ? File.read(filename) : ''
end

def report(expected, got)
  puts "--- expected ---"
  print expected
  puts "--- got ---"
  print got
end

main if $0 == __FILE__
