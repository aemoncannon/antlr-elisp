(setq debug-on-error t)
(setq max-specpdl-size 5000)
(setq max-lisp-eval-depth 1000)
(setq load-path (cons (expand-file-name "build/test/grammars") load-path))
(setq load-path (cons (expand-file-name "build/runtime") load-path))
(setq load-path (cons (expand-file-name "test") load-path))


(byte-compile-file (expand-file-name "build/runtime/a3el-runtime.el"))
(require 'a3el-runtime)
(byte-compile-file (expand-file-name "build/test/grammars/css_elispLexer.el"))
(load "css_elispLexer")
(byte-compile-file (expand-file-name "build/test/grammars/css_elispParser.el"))
(load "css_elispParser")


(find-file "test/data/big.css")
(elp-instrument-package "a3el")

(message "\n")
(dotimes (i 3)
  (message "Run #%s...." i)
  (a3el-parse-buffer 'css_elispLexer 'css_elispParser 'stylesheet (current-buffer)))
(message "\n")

(elp-results)
(kill-buffer nil)


;; Clean all these up
(delete-file (expand-file-name "build/runtime/a3el-runtime.elc"))
(delete-file (expand-file-name "build/test/grammars/css_elispParser.elc"))
(delete-file (expand-file-name "build/test/grammars/css_elispLexer.elc"))

(message "Done!")


