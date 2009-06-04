lexer grammar calc_elisp;
options { language = ELisp; }

LPAREN: '(';
RPAREN: ')';
PI: 'PI';
E: 'E';
INTEGER: DIGIT+;
DECIMAL: DIGIT+ '.' DIGIT+;

fragment
DIGIT: '0'..'9';
WS: (' ' | '\n' | '\t')+ { (a3el-lexer-set-channel 99) };
