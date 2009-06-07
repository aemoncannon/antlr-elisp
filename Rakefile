JAVA_CLASS_DIR = "build/classes"
JAVA_UPTODATE = "#{JAVA_CLASS_DIR}/.uptodate"
BUILD_DIRS = ["build", JAVA_CLASS_DIR, "build/test", "build/test/grammars", "build/runtime"]
JAVA_CLASSPATH = FileList["lib/*.jar"] + ["lib", JAVA_CLASS_DIR, "src/template"]
JAVA_SRCPATH = "src/java"
JAVA_SOURCE_FILES = FileList["src/**/*.java"]
GRAMMARS = FileList["test/grammars/*.g"]
TEMPLATES = FileList["src/template/**/*.stg"]
RUNTIME_SRC = "src/runtime/ELisp/a3el-runtime.el"
RUNTIME_TARGET = "build/runtime/a3el-runtime.el"


BUILD_DIRS.each{|d| directory d }


file RUNTIME_TARGET => [RUNTIME_SRC] do
  cp RUNTIME_SRC, RUNTIME_TARGET
end


file JAVA_UPTODATE => [:prepare] + JAVA_SOURCE_FILES do
  sh "javac -d #{JAVA_CLASS_DIR} -sourcepath #{JAVA_SRCPATH} -cp #{JAVA_CLASSPATH.join(":")} #{JAVA_SOURCE_FILES.join(" ")}"
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
    file t => [g] + [:prepare] + TEMPLATES do
      sh "java -cp #{JAVA_CLASSPATH.join(":")} org.antlr.Tool -o build #{g}"
    end    
  }
}




# public tasks

task :compile => JAVA_UPTODATE

task :gen_parsers => GRAMMARS.collect{|g| grammar_targets_for(g) }.flatten

task :test => [:compile, :gen_parsers] do
  sh "emacs -batch -q -l test/run_tests.el"
end

task :prepare => BUILD_DIRS + [RUNTIME_TARGET]

task :clean => [] do
  rm_rf BUILD_DIRS
end

task :default => :test

