package org.antlr.codegen;

import org.antlr.Tool;
import org.antlr.stringtemplate.StringTemplate;
import org.antlr.tool.Grammar;

public class ELispTarget extends Target {
    public String getTargetCharLiteralFromANTLRCharLiteral(CodeGenerator generator,
                                                           String literal){
	
	// Escape all these characters so elisp reader and editing commands 
	// don't get confused.
        if(literal.equals("' '") ||
	   literal.equals("'\"'")|| 
	   literal.equals("'('") || 
	   literal.equals("')'") || 
	   literal.equals("'|'") || 
	   literal.equals("';'") || 
	   literal.equals("'''") || 
	   literal.equals("'`'") || 
	   literal.equals("'#'") || 
	   literal.equals("'.'") || 
	   literal.equals("','")) {
            return "?\\" + literal.substring(1, literal.length() - 1);
        }
        return "?" + literal.substring(1, literal.length()-1);
    }

    public int getMaxCharValue(CodeGenerator generator) {
	return 0xFF;
    }
}
