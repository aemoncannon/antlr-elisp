(test "Bitset operations"

      (assert-equal "Basic add" t
		    (let ((set (make-bitset)))
		      (bitset-add set 1492)
		      (bitset-member set 1492)))


      ;; Test adding and removing.
      (let ((set (make-bitset)))
	(bitset-add set 1492)
	(assert-equal "Should be a member" t (bitset-member set 1492))
	(bitset-remove set 1492)
	(assert-equal "Should NOT be a member" nil (bitset-member set 1492)))


      ;; Test adding and removing, more involved.
      (let ((set (make-bitset)))
	(bitset-add set 1492)
	(bitset-add set 23)
	(bitset-add set 2300932)
	(assert-equal "Should be a member" t (bitset-member set 1492))
	(assert-equal "Should be a member" t (bitset-member set 23))
	(assert-equal "Should be a member" t (bitset-member set 2300932))
	(bitset-remove set 1492)
	(assert-equal "Should NOT be a member" nil (bitset-member set 1492))
	(assert-equal "Should be a member" t (bitset-member set 23))
	(assert-equal "Should be a member" t (bitset-member set 2300932)))


      ;; Test set-size call (shouldn't be called directly)
      (let ((set (make-bitset)))
	(assert-equal "Length should be 1" t (= 1 (bitset-word-len set)))
	(bitset-set-size set 5)
	(assert-equal "Length should be 5" t (= 5 (bitset-word-len set)))
	(bitset-set-size set 3)
	(assert-equal "Length should be 5, shouldn't shrink.'" t (= 5 (bitset-word-len set))))


      ;; Test that word-len grows as expected (remember our word length is 29)
      (let ((set (make-bitset)))
	(bitset-add set 27)
	(assert-equal "Length should be 1" t (= 1 (bitset-word-len set)))
	(bitset-add set 50)
	(assert-equal "Length should be 2" t (= 2 (bitset-word-len set)))
	(bitset-add set 65)
	(assert-equal "Length should be 4 (should be doubling)" t (= 4 (bitset-word-len set))))


      ;; Big randomized test. 
      (let* ((set (make-bitset))
	     (iterations 500)
	     (nums (make-vector iterations 0)))

	(dotimes (i iterations)
	  (aset nums i (random most-positive-fixnum)))
	
	(dotimes (i iterations)
	  (bitset-add set (aref nums i)))

	(dotimes (i iterations)
	  (assert-equal "Should include every i" t (bitset-member set (aref nums i))))

	(dotimes (i iterations)
	  (bitset-remove set (aref nums i)))

	(dotimes (i iterations)
	  (assert-equal "Should NOT include any i" nil (bitset-member set (aref nums i)))))




      )
