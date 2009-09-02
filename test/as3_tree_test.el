(setq debug-on-error t)
(setq max-specpdl-size 5000)
(setq max-lisp-eval-depth 5000)
(setq load-path (cons (expand-file-name "build/test/grammars") load-path))
(setq load-path (cons (expand-file-name "build/runtime") load-path))
(setq load-path (cons (expand-file-name "test") load-path))

(require 'a3el-runtime)
(load "as3_elispLexer")
(load "as3_elispParser")

(find-file "test/data/BasicObservable.as")

(unwind-protect
    (progn
      (message "\n")
      (let* ((result (a3el-parse-buffer 'as3_elispLexer 'as3_elispParser 'compilationUnit (current-buffer)))
	     (tree (a3el-retval-tree result)))
	(a3el-common-tree-pretty-print tree)))
      (kill-buffer nil)
      )



