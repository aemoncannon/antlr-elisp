grammar test_lexer_elisp;
options { language = ELisp; }

evaluate : ZERO EOF; // Just so a parser will be created

ZERO: '0';
