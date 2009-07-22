grammar full_calc_ast_rewrite_java;
options { 
    language = Java; 
	output = AST;
}

tokens {
    EXPR;
    NATURAL_LOG;
}

evaluate : expression EOF -> ^(EXPR expression);

expression : 
    mult (
    '+'^ mult
  | '-'^ mult
  )*
    ;

mult : 
    log (
    '*'^ log
  | '/'^ log 
  | '%'^ log 
  )* 
  ;

log : 
    'ln' exp -> ^(NATURAL_LOG exp)
  | exp 
  ;

exp : atom ('^'^ atom )? ;

atom :
    INTEGER
  | DECIMAL
  | '('! expression ')'!
  | 'PI'
  | 'E'
  ;

INTEGER: DIGIT+;
DECIMAL: DIGIT+ '.' DIGIT+;

fragment
DIGIT: '0'..'9';
WS: (' ' | '\n' | '\t')+ { (a3el-lexer-set-channel 99) };
