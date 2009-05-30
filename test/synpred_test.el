(load "syntactic_pred_elispLexer.el")
(load "syntactic_pred_elispParser.el")

(test "Syntactic predicates"

      (assert-equal "Should parse successfully."
                    nil
                    (do-parse 'syntactic_pred_elispLexer 'syntactic_pred_elispParser 's "(((123)))%"))

      (assert-equal "Should parse successfully."
                    nil
                    (do-parse 'syntactic_pred_elispLexer 'syntactic_pred_elispParser 's "(((123)))!"))

      )


