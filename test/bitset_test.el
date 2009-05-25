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


      )
