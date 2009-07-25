(load "full_calc_ast_rewrite_elispLexer.el")
(load "full_calc_ast_rewrite_elispParser.el")

(test "Full calc parsing with AST generation"


      (let* ((src "42")
	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match '("EXPR" ("SUM" ("42"))) tree)
	)

      (let* ((src "1 + 2 + 3 + 4")
	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match '("EXPR" ("SUM" ("1") ("2") ("3") ("4"))) tree)
	)

      (let* ((src "ln 42")
	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match '("EXPR" ("SUM" ("NATURAL_LOG" ("42")))) tree)
	)

      (let* ((src "GOOSE")
	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match '("EXPR" ("SUM" ("GEESE" ("GOOSE") ("GOOSE")))) tree)
	)

      (let* ((src "2 + FROG[1,32,7]")
	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match '("EXPR" ("SUM" ("2") ("FROG_LIST" 
						  ("FROG" ("1")) 
						  ("FROG" ("32")) 
						  ("FROG" ("7"))))) tree)
	)

      (let* ((src "2 + jack[1,32,7]")
	     (result (do-parse 'full_calc_ast_rewrite_elispLexer 'full_calc_ast_rewrite_elispParser 'evaluate src))
	     (tree (a3el-retval-tree result)))
	(assert-tree-match '("EXPR" ("SUM" ("2") ("TOAD_LIST" 
						  ("ENTRY" ("TOAD" ("jack")) ("1")) 
						  ("ENTRY" ("TOAD" ("jack")) ("32"))
						  ("ENTRY" ("TOAD" ("jack")) ("7")) ))) tree)
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


