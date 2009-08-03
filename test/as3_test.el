(load "as3_elispLexer.el")
(load "as3_elispParser.el")

(test "AS3 parsing tests"


      (let* ((src (concat
		   "package com.as3.test{"
		   "import com.animals.colors.*;"
		   "public class Monkey{"
		   "}"
		   "}"
		   ))
	     (result (do-parse 'as3_elispLexer 'as3_elispParser 'compilationUnit src))
	     (tree (a3el-retval-tree result)))
	(assert-true "Should have a return value." (a3el-retval-p result))
	(assert-false "Tree shouldn't be nil." (null tree))
	(assert-equal "Root of tree should be a compilation unit token." 
		      (a3el-common-token-text (a3el-common-tree-token tree))
		      "COMPILATION_UNIT")
	)

      )


