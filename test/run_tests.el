;;; run_tests.el --- Run all tests for Antlr lexers and parsers

;; Copyright (C) 2008  Ola Bini

;; Author: Ola Bini <ola.bini@gmail.com>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(setq debug-on-error t)
(setq max-specpdl-size 5000)
(setq max-lisp-eval-depth 1000)
(setq load-path (cons (expand-file-name "build/test/grammars") load-path))
(setq load-path (cons (expand-file-name "build/runtime") load-path))
(setq load-path (cons (expand-file-name "test") load-path))

(require 'a3el-runtime)

(require 'el_test)

(defun collect-lex-tokens (name str)
  (let ((all-tokens ()))
    (a3el-lex-string name str #'(lambda (token) 
				  (when (= (a3el-common-token-channel token) 0)
				    (setq all-tokens (cons (cons (a3el-lexer-token-type token) (a3el-lexer-token-text token)) all-tokens)))))
    (reverse all-tokens)))


(defun do-parse (lexerName parserName start-rule str)
  (a3el-parse-string lexerName parserName start-rule str))

  
(defun assert-tree-match (text pattern tree)
  "Given a pattern and a a3el-common-tree, check that the pattern is a satisfactory 
   abbreviation for the tree, with respect to text."
  (if (null (car pattern))
      (if (a3el-common-tree-is-nil tree)
	  (assert-trees-match text (cadr pattern) (a3el-common-tree-children tree))
	(signal 'test-failed (format "Expecting nil tree, found %s." tree)))
    (let* ((tok (a3el-common-tree-token tree))
	   (start (a3el-common-token-start tok))
	   (stop (a3el-common-token-stop tok))
	   (str (substring text (- start 1) (- stop 1))))
      (if (equal str (car pattern))
	  (assert-trees-match text (cadr pattern) (a3el-common-tree-children tree))
	(signal 'test-failed (format "Expecting node with text '%s', found %s." (car pattern) str))))))

(defun assert-trees-match (text patterns trees)
  "Helper for assert-tree-match. Match a list of trees."
  (catch 'return
    (dotimes (i (length patterns))
      (let ((pattern (nth i patterns))
	    (tree (nth i trees)))
	(if (not (assert-tree-match text pattern tree))
	    (throw 'return nil))
	))
    t))


(load "simple_lexer_test.el")
(load "calc_lexer_test.el")
(load "full_calc_test.el")
(load "bitset_test.el")
(load "common_tree_test.el")
(load "error_recovery_test.el")
(load "closures_test.el")
(load "synpred_test.el")
(load "css_test.el")
(load "auto_ast_test.el")
(load "ast_rewrite_test.el")

;;; run_tests.el ends here
