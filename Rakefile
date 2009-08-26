JAVA_CLASS_DIR = "build/classes"
JAVA_UPTODATE = "#{JAVA_CLASS_DIR}/.uptodate"
BUILD_DIRS = ["build", JAVA_CLASS_DIR, "build/test", "build/test/grammars", "build/runtime"]
JAVA_CLASSPATH = FileList["lib/*.jar"] + ["lib", JAVA_CLASS_DIR, "src/template"]
CP_SEP = PLATFORM =~ /win32/ ? ";" : ":"
JAVA_SRCPATH = "src/java"
JAVA_SOURCE_FILES = FileList["src/**/*.java"]
GRAMMARS = FileList["test/grammars/*.g"]
TEMPLATES = FileList["src/template/**/*.stg"]
RUNTIME_SRC = "src/runtime/ELisp/a3el-runtime.el"
RUNTIME_TARGET = "build/runtime/a3el-runtime.el"

def local_path_seps(path)
  if PLATFORM =~ /win32/
    path.gsub("/", "\\")
  else
    path.gsub("\\", "/")
  end
end


BUILD_DIRS.each{|d| directory d }


file RUNTIME_TARGET => [RUNTIME_SRC] do
  cp RUNTIME_SRC, RUNTIME_TARGET
end


file JAVA_UPTODATE => [:prepare] + JAVA_SOURCE_FILES do
  sh "javac -d #{JAVA_CLASS_DIR} -sourcepath #{JAVA_SRCPATH} -cp #{JAVA_CLASSPATH.join(CP_SEP)} #{JAVA_SOURCE_FILES.join(" ")}"
  touch JAVA_UPTODATE
end

def grammar_targets_for(src)
  if src =~ /\/([^\/]+_elisp).g$/
    ["build/test/grammars/#{$1}Lexer.el"]
  elsif src =~ /\/([^\/]+_java).g$/
    ["build/test/grammars/#{$1}Lexer.java"]
  end
end
GRAMMARS.each{|g| 
  grammar_targets_for(g).each{|t|
    file t => [g] + BUILD_DIRS + TEMPLATES do
      sh "java -cp #{JAVA_CLASSPATH.join(CP_SEP)} org.antlr.Tool -o build #{local_path_seps(g)}"
    end    
  }
}




# public tasks

task :compile => JAVA_UPTODATE

task :gen_parsers => GRAMMARS.collect{|g| grammar_targets_for(g) }.flatten

# Run elisp tests in batch mode.
task :test => [:compile, :gen_parsers] do
  sh "emacs -batch -q -l test/run_tests.el"
end

# Run elisp tests, in interactive emacs instance.
task :itest => [:compile, :gen_parsers] do
  sh "emacs -q -l test/run_tests.el"
end

# Byte compile the css lexer/parser and run it on a big css file.
# Dump profiling data afterwards.
task :css_perf => [:compile, :gen_parsers] do
  sh "emacs -batch -q -l test/css_stress_test.el"
end

# Byte compile the as3 lexer/parser and run it on a big as3 file.
# Dump profiling data afterwards.
task :as3_perf => [:compile, :gen_parsers] do
  sh "emacs -batch -q -l test/as3_stress_test.el"
end

task :prepare => BUILD_DIRS + [RUNTIME_TARGET]

task :clean => [] do
  rm_rf BUILD_DIRS
end

task :default => :test

