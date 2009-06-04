(test "Bitset operations"

      (assert-equal "Basic add" t
		    (let ((set (make-a3el-bitset)))
		      (a3el-bitset-add set 1492)
		      (a3el-bitset-member set 1492)))


      ;; Test adding and removing.
      (let ((set (make-a3el-bitset)))
	(a3el-bitset-add set 1492)
	(assert-equal "Should be a member" t (a3el-bitset-member set 1492))
	(a3el-bitset-remove set 1492)
	(assert-equal "Should NOT be a member" nil (a3el-bitset-member set 1492)))


      ;; Test adding and removing, more involved.
      (let ((set (make-a3el-bitset)))
	(a3el-bitset-add set 1492)
	(a3el-bitset-add set 23)
	(a3el-bitset-add set 2300932)
	(assert-equal "Should be a member" t (a3el-bitset-member set 1492))
	(assert-equal "Should be a member" t (a3el-bitset-member set 23))
	(assert-equal "Should be a member" t (a3el-bitset-member set 2300932))
	(a3el-bitset-remove set 1492)
	(assert-equal "Should NOT be a member" nil (a3el-bitset-member set 1492))
	(assert-equal "Should be a member" t (a3el-bitset-member set 23))
	(assert-equal "Should be a member" t (a3el-bitset-member set 2300932)))


      ;; Test set-size call (shouldn't be called directly)
      (let ((set (make-a3el-bitset)))
	(assert-equal "Length should be 1" t (= 1 (a3el-bitset-word-len set)))
	(a3el-bitset-set-size set 5)
	(assert-equal "Length should be 5" t (= 5 (a3el-bitset-word-len set)))
	(a3el-bitset-set-size set 3)
	(assert-equal "Length should be 5, shouldn't shrink.'" t (= 5 (a3el-bitset-word-len set))))


      ;; Test that word-len grows as expected (remember our word length is 29)
      (let ((set (make-a3el-bitset)))
	(a3el-bitset-add set 27)
	(assert-equal "Length should be 1" t (= 1 (a3el-bitset-word-len set)))
	(a3el-bitset-add set 50)
	(assert-equal "Length should be 2" t (= 2 (a3el-bitset-word-len set)))
	(a3el-bitset-add set 65)
	(assert-equal "Length should be 4 (should be doubling)" t (= 4 (a3el-bitset-word-len set))))


      ;; Big randomized test. 
      (let* ((set (make-a3el-bitset))
	     (iterations 500)
	     (nums (make-vector iterations 0)))

	(dotimes (i iterations)
	  (aset nums i (random most-positive-fixnum)))
	
	(dotimes (i iterations)
	  (a3el-bitset-add set (aref nums i)))

	(dotimes (i iterations)
	  (assert-equal "Should include every i" t (a3el-bitset-member set (aref nums i))))

	(dotimes (i iterations)
	  (a3el-bitset-remove set (aref nums i)))

	(dotimes (i iterations)
	  (assert-equal "Should NOT include any i" nil (a3el-bitset-member set (aref nums i)))))




      )
