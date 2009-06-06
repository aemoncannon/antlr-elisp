grammar closures_java;
options { language = Java; }

evaluate : expression EOF;

expression : (HORSE | COW)+;


HORSE: 'horse';
COW: 'cow';

GOOSE: 'goose';
DUCK: 'duck';

fragment
WS: (' ' | '\n' | '\t')+ { (a3el-lexer-set-channel 99) };