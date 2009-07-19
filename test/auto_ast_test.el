(load "full_calc_ast_operators_elispLexer.el")
(load "full_calc_ast_operators_elispParser.el")

(test "Full calc parsing with AST generation"
      (assert-true "Basic number should result in nil tree."
                    (let ((result (do-parse 'full_calc_ast_operators_elispLexer 'full_calc_ast_operators_elispParser 'evaluate "42")))
		      (a3el-common-tree-is-nil (a3el-retval-tree result))
		      ))

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


