(load "calc_elispLexer.el")

(test "Calc lexing"
      (assert-equal "Basic match"
                    '((4 . "123") ; 4 == integer
                      (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "123"))
      (assert-equal "Basic match2"
                    '((4 . "321") ; 4 == integer
                      (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "321"))
      (assert-equal "Match decimal"
                    '((10 . "2.2") ; 10 == decimal
                      (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "2.2"))
      (assert-equal "Match single token E"
                    '((8 . "E") ; 8 == E
                      (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "E"))
      (assert-equal "Match single token PI"
                    '((7 . "PI") ; 7 == integer
                      (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "PI"))
      (assert-equal "Basic match2"
                    '((4 . "1") ; 4 == integer
                      (4 . "1") ; 4 == integer
                      (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "1 1"))

      (assert-equal "Really complicated string"
                    '(
                      (5 . "(") 
                      (4 . "1")
                      (4 . "2")
                      (4 . "3443")
                      (10 . "232434353.1")
                      (6 . ")") 
                      (7 . "PI") 
                      (10 . "0.0")
                      (8 . "E") 
                      (8 . "E") 
                      (8 . "E") 
                      (8 . "E") 
                      (8 . "E") 
                     (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "(1 2   3443 232434353.1 ) PI 0.0EEEEE"))
      )
