grammar calc_elisp;
options { language = ELisp; }

evaluate : INTEGER EOF; // Just so a parser will be created

LPAREN: '(';
RPAREN: ')';
PI: 'PI';
E: 'E';
INTEGER: DIGIT+;
DECIMAL: DIGIT+ '.' DIGIT+;

fragment
DIGIT: '0'..'9';
WS: (' ' | '\n' | '\t')+ { (a3el-lexer-set-channel 99) };
