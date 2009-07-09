(load "css_elispLexer.el")
(load "css_elispParser.el")

(test "CSS parsing tests"

      (assert-true "Empty declaration."
		   (a3el-retval-p
                    (do-parse 'css_elispLexer 'css_elispParser 'stylesheet "#dude{}")))

      (assert-true "A couple properties."
		   (a3el-retval-p
                    (do-parse 'css_elispLexer 'css_elispParser 'stylesheet "#dude{ color: #ffffff; width: 200px }")))

      (assert-true "A more complicated sheet."
		   (a3el-retval-p
                    (do-parse 'css_elispLexer 'css_elispParser 'stylesheet (concat "#dude{ color: #ffffff; width: 200px }\r\n"
										   ".hello{ margin: 0px 2px 3px 3px; }\r\n"
										   "#hello li .dude{ background-image: '/dude/face.jpg' }\r\n"
										   ))))

      (assert-true "Try the url function (should invoke a synpred)."
		   (a3el-retval-p
                    (do-parse 'css_elispLexer 'css_elispParser 'stylesheet (concat "#dude{ background-image: url(../dude/face.jpg) }\r\n"))))

      (assert-true "A more complicated sheet."
		   (a3el-retval-p
                    (do-parse 'css_elispLexer 'css_elispParser 'stylesheet (concat "#dude{ color: #ffffff; width: 200px }\r\n"
										   ".hello{ margin: 0px 2px 3px 3px; }\r\n"
										   "#hello li .dude{ background-image: '/dude/face.jpg' }\r\n"
										   ))))


      )


