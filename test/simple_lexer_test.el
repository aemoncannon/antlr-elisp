(load "test_lexer_elispLexer.el")

(defun print-token (token)
  (message "Token: [%d] {%d -> %d} %s" (a3el-lexer-token-type token) (a3el-common-token-start token) (a3el-common-token-stop token) (a3el-lexer-token-text token)))

(test "Simple lexing"
      (assert-equal "Basic match"
                    '((4 . "0")
                      (-1 . nil))
                    (collect-lex-tokens 'test_lexer_elispLexer "0"))
      (assert-equal "Basic double match"
                    '((4 . "0")
                      (4 . "0")
                      (-1 . nil))
                    (collect-lex-tokens 'test_lexer_elispLexer "00"))

      ;; TODO: No longer signaling recognition errors. Need a different mechanism for testing
      ;; for error output.
;;      (assert-error "Basic failure"
;;                    'mismatched-token
;;                    (collect-lex-tokens 'test_lexer_elispLexer "1"))
      )
