(load "full_calc_ast_rewrite_elispLexer.el")
(load "full_calc_ast_rewrite_elispParser.el")

(test "Full calc parsing with AST generation"


      (let* ((src "42")
	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match '("EXPR" ("FOO" ("42"))) tree)
	)

      (let* ((src "1 + 2 + 3 + 4")
	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match '("EXPR" ("FOO" ("1") ("2") ("3") ("4"))) tree)
	)

      (let* ((src "ln 42")
	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match '("EXPR" ("FOO" ("NATURAL_LOG" ("42")))) tree)
	)


      ;;      (let* ((src "42 + 64")
      ;;	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
      ;;	     (tree (a3el-retval-tree result)))
      ;;	(assert-tree-match '("+" (("42") ("64"))) tree)
      ;;	)
      ;;
      ;;
      ;;      (let* ((src "42 * 64 + 23 * 0.1")
      ;;	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
      ;;	     (tree (a3el-retval-tree result)))
      ;;	(assert-tree-match '("+" ( ("*" (("42")("64"))) ("*" (("23")("0.1"))) ) ) tree)
      ;;	)
      ;;
      ;;      (let* ((src "(42 * 64) + (23 * 0.1)")
      ;;	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
      ;;	     (tree (a3el-retval-tree result)))
      ;;	(assert-tree-match '("+" ( ("*" (("42")("64"))) ("*" (("23")("0.1"))) ) ) tree)
      ;;	)
      ;;
      ;;      (let* ((src "25^2")
      ;;	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
      ;;	     (tree (a3el-retval-tree result)))
      ;;	(assert-tree-match '("EXP" (("^" (("25"))))) tree 'full_calc_ast_rewrite_elispParser)
      ;;	)
      ;;
      ;;      (let* ((src "ln 2.0123^E")
      ;;	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
      ;;	     (tree (a3el-retval-tree result)))
      ;;	(assert-tree-match '("ln" (("^" ( ("2.0123") ("E"))))) tree)
      ;;	)


      )


