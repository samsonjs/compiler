require "standard/rake"

task default: %i[test standard]

desc "Run the test suite through both back-ends"
task test: %w[test:asm test:bin]

namespace :test do
  desc "Run the test suite through nasm"
  task :asm do
    sh "make -C test all FORMAT=asm"
  end

  desc "Run the test suite through the homegrown assembler"
  task :bin do
    sh "make -C test all FORMAT=bin"
  end
end

desc "Remove build products"
task :clean do
  sh "make -C test clean"
end
