(test "Bitset operations"

      (assert-equal "Basic add" t
		    (let ((set (make-bitset)))
		      (bitset-add set 1492)
		      (bitset-member set 1492)))

      (let ((set (make-bitset)))
	(bitset-add set 1492)
	(assert-equal "Should be a member" t (bitset-member set 1492))
	(bitset-remove set 1492)
	(assert-equal "Should NOT be a member" nil (bitset-member set 1492)))

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

      (let ((set (make-bitset)))
	(assert-equal "Length should be 1" t (= 1 (bitset-word-len set)))
	(bitset-set-size set 5)
	(assert-equal "Length should be 5" t (= 5 (bitset-word-len set)))
	(bitset-set-size set 3)
	(assert-equal "Length should be 5, shouldn't shrink.'" t (= 5 (bitset-word-len set))))

      )
