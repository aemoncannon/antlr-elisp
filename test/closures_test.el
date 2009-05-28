(load "closures_elispLexer.el")
(load "closures_elispParser.el")

(test "Rule closures"

      (assert-equal "One from first."
                    nil
                    (do-parse 'closures_elispLexer 'closures_elispParser 'evaluate "cow"))

      (assert-equal "Two from each."
                    nil
                    (do-parse 'closures_elispLexer 'closures_elispParser 'evaluate "cow horse"))

      (assert-equal "One from first, one from second"
                    nil
                    (do-parse 'closures_elispLexer 'closures_elispParser 'evaluate "cow goose"))

      (assert-equal "Many from first."
		    nil
		    (do-parse 'closures_elispLexer 'closures_elispParser 'evaluate "cow horse cow cow cow"))

      (assert-equal "Many from first, many from second."
		    nil
		    (do-parse 'closures_elispLexer 'closures_elispParser 'evaluate "cow horse cow cow cow goose duck goose duck"))

      )


