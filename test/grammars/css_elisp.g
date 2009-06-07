grammar css_elisp;

options {
	language=ELisp;
	k=4;
}

tokens{
    STYLESHEET;RULE_SET;DECLARATION;VALUE_LIST;COLOR;SELECTOR;
}


stylesheet
  : 
        (CHARSET_SYM STRING SEMI)?
        (WS|CDO|CDC)* (importCSS (WS|CDO|CDC)* )*
        ((ruleset | media | page) (WS|CDO|CDC)* )*
		EOF
  ;

ruleset
  : selector (COMMA selector)*
    LBRACE 
        declaration? (SEMI declaration)* SEMI? 
    RBRACE
  ;

importCSS
  : IMPORT_SYM
    (STRING|URI) (IDENT (COMMA IDENT)* )? SEMI
  ;
  
media
  : MEDIA_SYM IDENT (COMMA IDENT)* LBRACE (ruleset)* RBRACE
  ;
  
page
  : PAGE_SYM (pseudo_page)?
    LBRACE declaration (SEMI declaration)* RBRACE
  ;
  
pseudo_page
  : COLON IDENT
  ;
  
operator
  : '/' | COMMA | /* empty */
  ;

unary_operator
  : '-' | PLUS
  ;

combinator
  : PLUS S*
  | GREATER S*
  | S
  ;

selector
  : simpleSelector (combinator simpleSelector)*
  ;
  
simpleSelector
  :  (element_name | idDecl | classDecl | attrib | pseudo)+
  ;
  
idDecl
  : HASH
  ;

classDecl
  : CLASSDECL
  ;

attrib
  : '[' IDENT (('=' | INCLUDES | DASHMATCH) (IDENT | STRING))? ']'
  ;

pseudo
  : COLON (IDENT | (IDENT '(') => IDENT '(' (IDENT)? ')')
  ;

prio
  : IMPORTANT_SYM
  ;
	
declaration
  : IDENT  ':'	expr prio?
  ;

expr
  :  term (operator term)*
  ;
  
term
	:	(IDENT '(') => function
    |   simpleTerm
	;

simpleTerm
    : (color | TERM | IDENT | STRING)
    ;

function
  : IDENT '(' expr ')'
  ;

color
    : HASH
    ;
        
element_name
  : IDENT | '*'
  ;
  
CDO			:	'<!--';
CDC			:	'-->';
INCLUDES	:	'~=';
DASHMATCH	:	'|=';
LBRACE		:	'{';
RBRACE		:	'}';
PLUS		:	'+';
GREATER		:	'>';
COMMA		:	',';
DOT			:	'.';
COLON		:	':';
SEMI		:	';';
STAR		:	'*';

IMPORT_SYM
	:	'@import'
	;

PAGE_SYM
	:	'@page'
	;

MEDIA_SYM
	:	'@media'
	;

CHARSET_SYM
	:	'@charset'
	;

IMPORTANT_SYM
	:	'!' WS* 'important'
	;

CLASSDECL
	: DOT IDENT
	;

// Whitespace -- ignored
WS	:	(	' '
		|	'\t'
		|	'\f'
			// handle newlines
		|	(	
                ('\r\n') => '\r\n'	// Evil DOS
			|	'\r'	// Macintosh
			|	'\n'	// Unix (the right way)
			)
		)+
		{ (a3el-lexer-set-channel 99) }
	;

// string literals
STRING
	:	'"'! (ESC|~('"'|'\\'|'\n'|'\r'))* '"'!
	|	'\''! (ESC|~('\''|'\\'|'\n'|'\r'))* '\''!
	;

// Single-line comments
SL_COMMENT
	:	'//'
		(~('\n'|'\r'))* ('\n'|'\r'('\n')?)
		{ (a3el-lexer-set-channel 99) }
	;
	
// multiple-line comments
COMMENT
	:	'/*' ( options {greedy=false;} : . )* '*/' { (a3el-lexer-set-channel 99)}
	;

IDENT
	:	NMSTART (NMCHAR)*
	;

TERM
	:    
        ('url(') => URI
    |   (NUM (PX | PERCENT | CM | MM | IN | PT | PC)) => LENGTH
 	|   (NUM (MS | S)) => TIME
 	|   (NUM (DEG | RAD | GRAD)) => ANGLE
 	|	EMS
 	|	EXS
 	|	(NUM (HZ | KHZ)) => FREQ
 	|	NUM
    ;

HASH
    : '#' NAME
    ;



// hexadecimal digit (again, note it's protected!)
fragment
HEX_DIGIT
	:	('0'..'9'|'A'..'F'|'a'..'f')
	;

// escape sequence -- note that this is protected; it can only be called
// from another lexer rule -- it will not ever directly return a token to
// the parser
// There are various ambiguities hushed in this rule. The optional
// '0'...'9' digit matches should be matched here rather than letting
// them go back to STRING_LITERAL to be matched. ANTLR does the
// right thing by matching immediately; hence, it's ok to shut off
// the FOLLOW ambig warnings.
fragment
ESC
	:	'\\'
		(	'n'
		|	'r'
		|	't'
		|	'b'
		|	'f'
		|	'"'
		|	'\''
		|	'\\'
		|	('u')+ HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
		|	'0'..'3'
			(
			:	'0'..'7'
				(
				:	'0'..'7'
				)?
			)?
		|	'4'..'7'
			(
			:	'0'..'7'
			)?
		)
	;
fragment
NONASCII
	:	'\u0200'..'\u0377'
	;

fragment
NMSTART
	:	'_' | '-' | 'a'..'z'| 'A'..'Z' | NONASCII | ESC
	;

fragment
NMCHAR
	:	NMSTART | '0'..'9'
	;

fragment
NAME
	:	(NMCHAR)+
	;

fragment
NUM
	:	('0'..'9')+ | ('0'..'9')* '.' ('0'..'9')+
	;

fragment
EMS	:	('em') => 'em';

fragment
EXS	:	'ex';

fragment
PX	:	'px';

fragment
CM	:	'cm';

fragment
MM	:	'mm';

fragment
IN	:	'in';

fragment
PT	:	'pt';

fragment
PC	:	'pc';

fragment
PERCENT
	: '%'
	;

fragment
LENGTH
	:	NUM (PX | PERCENT | CM | MM | IN | PT | PC)
	;
	
fragment
DEG	:	'deg';

fragment
RAD	:	'rad';

fragment
GRAD:	'grad';

fragment
ANGLE
	:	NUM (DEG | RAD | GRAD)
	;
	
fragment
MS	:	'ms';

fragment
S	:	's';

fragment
TIME
	:	NUM (MS | S)
	;

fragment
HZ	:	'hz';

fragment
KHZ	:	'khz';

fragment
FREQ
	: NUM (HZ | KHZ)
	;

fragment
URI
	: 'url(' (STRING | URL)? ')'
	;

fragment
URL
	:	
     (ESC|~('#'|'('|')'|'"'|'\''|'\\'|'\n'|'\r'))+
	;
