grammar full_calc_ast_rewrite_java;
options { 
    language = Java; 
	output = AST;
}

tokens {
    EXPR;
    NATURAL_LOG;
    SUM;
    GEESE;
    FROG_LIST;
    TOAD_LIST;
    ENTRY;
    TOAD;
}

evaluate : expression EOF -> ^(EXPR expression);

expression : 
    mult ('+' mult )* -> ^(SUM mult*);

mult : 
    log ('*'^ log | '/'^ log | '%'^ log )*
  ;

log : 
    'ln' exp -> ^(NATURAL_LOG exp)
  | exp
  ;

exp : atom ('^'^ atom )?;

atom :
    INTEGER
  | DECIMAL
  | '('! expression ')'!
  | 'PI'
  | 'E'
  | GOOSE -> ^(GEESE GOOSE GOOSE)
  | FROG '[' atom (',' atom)* ']' -> ^(FROG_LIST ^(FROG atom)+)
  | toad '[' atom (',' atom)* ']' -> ^(TOAD_LIST ^(ENTRY toad atom)+)
  ;

toad : 'jack' -> ^(TOAD 'jack');

FROG: 'FROG';
GOOSE: 'GOOSE';

INTEGER: DIGIT+;
DECIMAL: DIGIT+ '.' DIGIT+;

fragment
DIGIT: '0'..'9';
WS: (' ' | '\n' | '\t')+ { (a3el-lexer-set-channel 99) };

