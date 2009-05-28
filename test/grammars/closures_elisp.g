grammar closures_elisp;
options { language = ELisp; }

evaluate : expression EOF;

expression : (mammal)+ (bird)*;

mammal: HORSE | COW;

bird: GOOSE | DUCK;

HORSE: 'horse';
COW: 'cow';

GOOSE: 'goose';
DUCK: 'duck';

WS: (' ' | '\n' | '\t')+ { (lexer-set-channel 99) };
