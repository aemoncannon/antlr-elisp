(load "css_elispLexer.el")
(load "css_elispParser.el")

(test "CSS parsing tests"

      (assert-equal "Should parse successfully."
                    nil
                    (do-parse 'css_elispLexer 'css_elispParser 'stylesheet "#dude{}"))

      )


