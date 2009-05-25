#!/usr/bin/env ruby

ROOT = Dir.pwd.sub(/\/test.*$/, '')
$LOAD_PATH << ROOT

require 'build'

# usage: build.rb <func> [binformat]
#
# ([format] will go before [binformat])

def main
  func = ARGV[0].to_s
  format = 'asm'                # 'bin' only assembles one or two
                                # instructions right now, but support
                                # is in place
  binformat = (ARGV[1] ? ARGV[1] : 'elf').downcase
  platform = `uname -s`.chomp.downcase
  print "testing #{func} ... "
  success = run( build("test_#{func}.code", platform, format, binformat) )
  puts success == 0 ? "pass" : "FAIL! (#{success})"
  exit(success.to_i)
end

main if $0 == __FILE__
