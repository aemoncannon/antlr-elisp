(load "full_calc_elispLexer.el")
(load "full_calc_elispParser.el")

(test "Error recovery.."

      (assert-equal "A control. Should parse silently."
                    nil (do-parse 'full_calc_elispLexer 'full_calc_elispParser 'evaluate "(1)"))

      (assert-equal "Should expect an expression after the first '('.. then go into recovery mode.. then resync on the next ')'."
                    nil
                    (progn 
		      (message "\nShould print no-viable-alt error:")
		      (do-parse 'full_calc_elispLexer 'full_calc_elispParser 'evaluate "()")))

      )


