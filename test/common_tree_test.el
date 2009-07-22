(defun add-child (adaptor parent child)
  (funcall (a3el-tree-adaptor-add-child adaptor) parent child))

(defun become-root (adaptor new old)
  (funcall (a3el-tree-adaptor-become-root adaptor) new old))

(defun create (adaptor token)
  (funcall (a3el-tree-adaptor-create adaptor) token))

(defun new-nil (adaptor)
  (funcall (a3el-tree-adaptor-new-nil adaptor)))


(test "Common tree test"

      ;; Test simple nil tree
      (let* ((adaptor (make-a3el-tree-adaptor))
	     (tree (new-nil adaptor)))
	(assert-true "should be nil" (a3el-common-tree-is-nil tree)))

      ;; Test simple non-nil tree
      (let* ((adaptor (make-a3el-tree-adaptor))
	     (tree (create adaptor'tok)))
	(assert-false "should not be nil" (a3el-common-tree-is-nil tree))
	(assert-equal "should have correct token" 'tok (a3el-common-tree-token tree)))

      ;; Test adding to nil tree
      (let* ((adaptor (make-a3el-tree-adaptor))
	     (tree (new-nil adaptor))
	     (child (create adaptor 'tok)))
	(add-child adaptor tree child)
	(assert-true "should still be nil" (a3el-common-tree-is-nil tree))
	(assert-equal "should have 1 child" 1 (length (a3el-common-tree-children tree)))
	(assert-false "that 1 child should not be nil" (a3el-common-tree-is-nil (nth 0 (a3el-common-tree-children tree))))
	)

      ;; Test adding nil child with 2 children to nil tree
      (let* ((adaptor (make-a3el-tree-adaptor))
	     (tree (new-nil adaptor))
	     (child (new-nil adaptor)))
	(add-child adaptor child (create adaptor 'tok))
	(add-child adaptor child (create adaptor 'tok))
	(add-child adaptor tree child)
	(assert-true "should still be nil" (a3el-common-tree-is-nil tree))
	(assert-equal "should have 2 children" 2 (length (a3el-common-tree-children tree)))
	(assert-false "the 1st child should not be nil" (a3el-common-tree-is-nil (nth 0 (a3el-common-tree-children tree))))
	(assert-false "the 2nd child should not be nil" (a3el-common-tree-is-nil (nth 1 (a3el-common-tree-children tree))))
	)


      ;; Test adding nil child with 2 children to non-nil tree
      (let* ((adaptor (make-a3el-tree-adaptor))
	     (tree (create adaptor 'tok))
	     (child (new-nil adaptor)))
	(add-child adaptor child (create adaptor 'tok))
	(add-child adaptor child (create adaptor 'tok))
	(add-child adaptor tree child)
	(assert-false "should still not be nil" (a3el-common-tree-is-nil tree))
	(assert-equal "should have 2 children" 2 (length (a3el-common-tree-children tree)))
	(assert-false "the 1st child should not be nil" (a3el-common-tree-is-nil (nth 0 (a3el-common-tree-children tree))))
	(assert-false "the 2nd child should not be nil" (a3el-common-tree-is-nil (nth 1 (a3el-common-tree-children tree))))
	)


      ;; Test become root, with nil old root
      (let* ((adaptor (make-a3el-tree-adaptor))
	     (old (new-nil adaptor))
	     (new (create adaptor 'tok)))
	(add-child adaptor old (create adaptor 'tok))
	(add-child adaptor old (create adaptor 'tok))
	(become-root adaptor new old)
	(assert-false "should still Not be nil" (a3el-common-tree-is-nil new))
	(assert-equal "should have 2 children" 2 (length (a3el-common-tree-children new)))
	)




      )
