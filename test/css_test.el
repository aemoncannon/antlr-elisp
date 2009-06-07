(load "css_elispLexer.el")
(load "css_elispParser.el")

(test "CSS parsing tests"

      (assert-equal "Empty declaration."
                    nil
                    (do-parse 'css_elispLexer 'css_elispParser 'stylesheet "#dude{}"))

      (assert-equal "A couple properties."
                    nil
                    (do-parse 'css_elispLexer 'css_elispParser 'stylesheet "#dude{ color: #ffffff; width: 200px }"))

      (assert-equal "A more complicated sheet."
                    nil
                    (do-parse 'css_elispLexer 'css_elispParser 'stylesheet (concat "#dude{ color: #ffffff; width: 200px }\r\n"
										   ".hello{ margin: 0px 2px 3px 3px; }\r\n"
										   "#hello li .dude{ background-image: '/dude/face.jpg' }\r\n"
										   )))

      (assert-equal "Try the url function (should invoke a synpred)."
                    nil
                    (do-parse 'css_elispLexer 'css_elispParser 'stylesheet (concat "#dude{ background-image: url(../dude/face.jpg) }\r\n")))

      (assert-equal "A more complicated sheet."
                    nil
                    (do-parse 'css_elispLexer 'css_elispParser 'stylesheet (concat "#dude{ color: #ffffff; width: 200px }\r\n"
										   ".hello{ margin: 0px 2px 3px 3px; }\r\n"
										   "#hello li .dude{ background-image: '/dude/face.jpg' }\r\n"
										   )))


      )


