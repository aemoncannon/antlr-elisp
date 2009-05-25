(load "full_calc_elispLexer.el")
(load "full_calc_elispParser.el")

(test "Full calc parsing"
      (assert-equal "Basic number"
                    nil
                    (do-parse 'full_calc_elispLexer 'full_calc_elispParser 'evaluate "42"))

      (assert-equal "One operator"
                    nil
                    (do-parse 'full_calc_elispLexer 'full_calc_elispParser 'evaluate "10 + 2"))

      (assert-equal "Two operators.."
                    nil
                    (do-parse 'full_calc_elispLexer 'full_calc_elispParser 'evaluate "10 + 2 * 4"))

      (assert-equal "Paren nesting"
                    nil
                    (do-parse 'full_calc_elispLexer 'full_calc_elispParser 'evaluate "(10 + (2 + 2)) * 4"))

      (assert-equal "More functions"
                    nil
                    (do-parse 'full_calc_elispLexer 'full_calc_elispParser 'evaluate "(ln 10 + (PI + 2)) * 3.2"))
      )


