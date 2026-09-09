require "standard/rake"

task default: %i[test compare standard]

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

  desc "Record what each fixture prints in its .expected file, to review as a diff"
  task :bless do
    sh "make -C test bless"
  end
end

desc "Check that both back-ends emit the same instructions"
task :compare do
  sh "make -C test compare"
end

desc "Remove build products"
task :clean do
  sh "make -C test clean"
end
