(load "full_calc_ast_operators_elispLexer.el")
(load "full_calc_ast_operators_elispParser.el")

(test "Full calc parsing with AST generation"


      (let* ((src "42")
	     (result (do-parse 'full_calc_ast_operators_elispLexer 'full_calc_ast_operators_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match src '("42") tree)
	)


      (let* ((src "42 + 64")
	     (result (do-parse 'full_calc_ast_operators_elispLexer 'full_calc_ast_operators_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match src '("+" (("42") ("64"))) tree)
	)


      (let* ((src "42 * 64 + 23 * 0.1")
	     (result (do-parse 'full_calc_ast_operators_elispLexer 'full_calc_ast_operators_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match src '("+" ( ("*" (("42")("64"))) ("*" (("23")("0.1"))) ) ) tree)
	)

      (let* ((src "(42 * 64) + (23 * 0.1)")
	     (result (do-parse 'full_calc_ast_operators_elispLexer 'full_calc_ast_operators_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match src '("+" ( ("*" (("42")("64"))) ("*" (("23")("0.1"))) ) ) tree)
	)

      (let* ((src "((42 * 64) + (23 * 0.1))^PI")
	     (result (do-parse 'full_calc_ast_operators_elispLexer 'full_calc_ast_operators_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match src '("^" (("+" ( ("*" (("42")("64"))) ("*" (("23")("0.1"))) ) ) ("PI"))) tree)
	)

      (let* ((src "ln 2.0123^E")
	     (result (do-parse 'full_calc_ast_operators_elispLexer 'full_calc_ast_operators_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match src '("ln" (("^" ( ("2.0123") ("E"))))) tree)
	)


      )


