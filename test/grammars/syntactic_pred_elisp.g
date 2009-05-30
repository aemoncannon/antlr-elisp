grammar syntactic_pred_elisp;

options { language = ELisp; }

s : (e '%')=> e '%' 
    | e '!';

e : '(' e ')' | INT;

INT : '0'..'9'+;

WS: (' ' | '\n' | '\t')+ { (lexer-set-channel 99) };
