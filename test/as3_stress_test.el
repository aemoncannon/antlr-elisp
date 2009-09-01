(setq debug-on-error t)
(setq max-specpdl-size 5000)
(setq max-lisp-eval-depth 5000)
(setq load-path (cons (expand-file-name "build/test/grammars") load-path))
(setq load-path (cons (expand-file-name "build/runtime") load-path))
(setq load-path (cons (expand-file-name "test") load-path))


(byte-compile-file (expand-file-name "build/runtime/a3el-runtime.el"))
(require 'a3el-runtime)
(byte-compile-file (expand-file-name "build/test/grammars/as3_elispLexer.el"))
(load "as3_elispLexer")
(byte-compile-file (expand-file-name "build/test/grammars/as3_elispParser.el"))
(load "as3_elispParser")


(find-file "test/data/BasicObservable.as")
(elp-instrument-package "a3el")

(unwind-protect
    (progn
      (message "\n")
      (dotimes (i 3)
	(message "Run #%s...." i)
	(a3el-parse-buffer 'as3_elispLexer 'as3_elispParser 'compilationUnit (current-buffer)))
      (message "\n")

      (elp-results)
      (kill-buffer nil)
      )

  ;; Clean all these up
  (message "Cleaning up..")
  (delete-file (expand-file-name "build/runtime/a3el-runtime.elc"))
  (delete-file (expand-file-name "build/test/grammars/as3_elispParser.elc"))
  (delete-file (expand-file-name "build/test/grammars/as3_elispLexer.elc"))
)

(message "Done!")


