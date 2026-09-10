require "standard/rake"

# Run a command with Ruby warnings enabled, echoing its output, and fail if
# anything warned.  Warnings are only worth having if they stay at zero, so
# a new one has to break the build rather than scroll past.  Matches Ruby's
# own "warning:" and the assembler's "[warning]" diagnostics.
def sh_warning_free(command)
  seen = []
  IO.popen({"RUBYOPT" => "-w"}, "#{command} 2>&1") do |io|
    io.each_line do |line|
      $stdout.print(line)
      seen << line
    end
  end
  raise "command failed: #{command}" unless $?.success?

  warned = seen.grep(/warning:|\[warning\]/)
  raise "#{warned.length} warnings from #{command}" unless warned.empty?
end

task default: %i[eager_load test compare standard]

desc "Check every file defines the constant Zeitwerk expects from its path"
task :eager_load do
  sh %(ruby -Ilib -e 'require "compiler"; Compiler::Loader.eager_load')
end

desc "Run the test suite through both back-ends"
task test: %w[test:asm test:bin]

namespace :test do
  desc "Run the test suite through nasm"
  task :asm do
    sh_warning_free "make -C test all FORMAT=asm"
  end

  desc "Run the test suite through the homegrown assembler"
  task :bin do
    sh_warning_free "make -C test all FORMAT=bin"
  end

  desc "Record what each fixture prints in its .expected file, to review as a diff"
  task :bless do
    sh "make -C test bless"
  end
end

desc "Check that both back-ends emit the same instructions"
task :compare do
  sh_warning_free "make -C test compare"
end

desc "Remove build products"
task :clean do
  sh "make -C test clean"
end
