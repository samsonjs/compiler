#!/usr/bin/env ruby

ROOT = Dir.pwd.sub(/\/test.*$/, '')
$LOAD_PATH << ROOT

require 'build'

# usage: test.rb <func> [binformat] [format]

def main
  func = ARGV[0].to_s
  binformat = ARGV[1] ? ARGV[1].downcase : 'elf'
  format = ARGV[2] ? ARGV[2].downcase : 'asm'
  platform = `uname -s`.chomp.downcase
  print "testing #{func} ... "
  success = run( build("test_#{func}.code", platform, binformat) )
  if success == 0
    puts "pass"
  else
    puts "FAIL! (#{success})"
  end
  exit(success.to_i)
end

main if $0 == __FILE__
