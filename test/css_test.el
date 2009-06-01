(load "css_elispLexer.el")
(load "css_elispParser.el")

(test "CSS parsing tests"

      (assert-equal "Empty declaration."
                    nil
                    (do-parse 'css_elispLexer 'css_elispParser 'stylesheet "#dude{}"))

      (assert-equal "A couple properties."
                    nil
                    (do-parse 'css_elispLexer 'css_elispParser 'stylesheet "#dude{ color: #ffffff; width: 200px }"))

      )


