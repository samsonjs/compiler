#!/usr/bin/env ruby

ROOT = Dir.pwd.sub(/\/test.*$/, '')
$LOAD_PATH << ROOT

require 'build'

def main
  func = ARGV[0].to_s
  print "testing #{func} ... "
  success = run( build("test_#{func}.code") )
  puts success == 0 ? "pass" : "FAIL! (#{success})"
  exit(success)
end

main if $0 == __FILE__
