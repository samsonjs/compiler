#!/usr/bin/env ruby

require '../build'

# usage: test.rb <func> [outdir] [binformat] [format]

def main
  func = ARGV.shift
  outdir = ARGV.shift || '.'
  Dir.mkdir(outdir) unless File.exists?(outdir)
  binformat = (ARGV.shift || 'elf').downcase
  format = (ARGV.shift || 'asm').downcase
  platform = `uname -s`.chomp.downcase
  print "testing #{func} ... "
  success = run( build("test_#{func}.code", outdir, platform, binformat) )
  if success == 0
    puts "pass"
  else
    puts "FAIL! (#{success})"
  end
  exit(success.to_i)
end

main if $0 == __FILE__
