grammar test_lexer_java;
options { language = Java; }

evaluate : ZERO EOF; // Just so a parser will be created

ZERO: '0';
