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
  exefile = builder(format).call("test_#{func}.code", outdir, platform, binformat)
  success = run(exefile)
  if success == 0
    puts "pass"
  else
    puts "FAIL! (#{success})"
  end
  exit(success.to_i)
end

main if $0 == __FILE__
