(load "full_calc_ast_operators_elispLexer.el")
(load "full_calc_ast_operators_elispParser.el")

(test "Full calc parsing with AST generation"

      (let* ((result (do-parse 'full_calc_ast_operators_elispLexer 'full_calc_ast_operators_elispParser 'evaluate "42"))
	     (tree (a3el-retval-tree result)))
	(assert-false "Should not result in nil tree. We hoist expression with ^ operator."
		     (a3el-common-tree-is-nil tree))

	(assert-equal "Should have 0 children, the 42 token should be the root. EOF is ommitted with a ! operator."
		      0 (length (a3el-common-tree-children tree)))
	)

      (let* ((result (do-parse 'full_calc_ast_operators_elispLexer 'full_calc_ast_operators_elispParser 'evaluate "42 + 64"))
	     (tree (a3el-retval-tree result)))

	(assert-false "Result should not be a nil tree."
		      (a3el-common-tree-is-nil tree))

	(assert-equal "Should have 2 children, the two number tokens."
		      2
		      (length (a3el-common-tree-children tree)))

	)


      ;;      (assert-equal "One operator"
      ;;                    nil
      ;;                    (do-parse 'full_calc_elispLexer 'full_calc_elispParser 'evaluate "10 + 2"))
      ;;
      ;;      (assert-equal "Two operators.."
      ;;                    nil
      ;;                    (do-parse 'full_calc_elispLexer 'full_calc_elispParser 'evaluate "10 + 2 * 4"))
      ;;
      ;;      (assert-equal "Paren nesting"
      ;;                    nil
      ;;                    (do-parse 'full_calc_elispLexer 'full_calc_elispParser 'evaluate "(10 + (2 + 2)) * 4"))
      ;;
      ;;      (assert-equal "More functions"
      ;;                    nil
      ;;                    (do-parse 'full_calc_elispLexer 'full_calc_elispParser 'evaluate "(ln 10 + (PI + 2)) * 3.2"))
      ;;
      )


