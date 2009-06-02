(load "full_calc_elispLexer.el")
(load "full_calc_elispParser.el")

(test "Error recovery.."

      (assert-equal "A control. Should parse silently."
                    nil (do-parse 'full_calc_elispLexer 'full_calc_elispParser 'evaluate "(1)"))


      (let ((errors (swallowing-recognition-errors 
		     (do-parse 
		      'full_calc_elispLexer 
		      'full_calc_elispParser 
		      'evaluate "()"))))

	(assert-equal (concat "Should result in 1 recognition error. "
			      "Should expect an expression after the first '('.. then go into recovery mode.. "
			      "then resync on the next ')'.")
		      t
		      (and (= 1 (length errors))
			   (equal 'no-viable-alt (car (first errors))))))
      )


