grammar full_calc_ast_operators_elisp;
options { 
    language = ELisp; 
	output = AST;
}

evaluate : expression EOF;

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
    'ln'^ exp
  | exp 
  ;

exp : atom ('^'^ atom )? ;

atom :
    INTEGER
  | DECIMAL
  | '(' expression ')'
  | 'PI'
  | 'E'
  ;

INTEGER: DIGIT+;
DECIMAL: DIGIT+ '.' DIGIT+;

fragment
DIGIT: '0'..'9';
WS: (' ' | '\n' | '\t')+ { (a3el-lexer-set-channel 99) };
