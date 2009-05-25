;;; antlr-runtime.el --- Antlr runtime

;; Copyright (C) 2008  Ola Bini

;; Author: Ola Bini <ola.bini@gmail.com>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(require 'cl)

(defvar *antlr-runtime-lexers* (make-hash-table)
  "Keeps track of all lexers defined in the system, including their definitions") 

(defvar *antlr-runtime-parsers* (make-hash-table)
  "Keeps track of all parsers defined in the system, including their definitions") 

(defconst *antlr-token-default-channel* 0)
(defconst *antlr-token-invalid-token-type* 0)

(defmacro deflexer (name)
  `(puthash ',name 
            (make-antlr-lexer :name ',name) 
            *antlr-runtime-lexers*))

(defmacro defparser (name)
  `(puthash ',name 
            (make-antlr-parser :name ',name) 
            *antlr-runtime-parsers*))

(defstruct antlr-lexer
  "An Antlr lexer"
  name
  (tokens (make-hash-table))
  (rules (make-hash-table))
  (dfas (make-hash-table)))

(defstruct common-token
  "A Common Antlr token"
  input
  type
  channel
  (index -1)
  (start -1)
  (stop -1)
  line
  text
  char-position-in-line)

(defstruct DFA
  recognizer
  decision-number
  eot
  eof
  min
  max
  accept
  special
  transition
  description)

(defun lexer-input-LA (n)
  (let ((at (+ (point) (- n 1))))
    (if (= at (point-max))
	-1
      (char-after at))))
  
(defun lexer-input-consume ()
  (goto-char (+ (point) 1)))

(defun dfa-special-state-transition (state)
  -1)

(defun predict-DFA-with (dfa)
  (save-excursion
    (catch 'return
      (let ((s 0))
        (while t
          (catch 'continue
            (let ((special-state (aref (DFA-special dfa) s)))
              (when (>= special-state 0)
                (setq s (dfa-special-state-transition special-state))
                (lexer-input-consume)
                (throw 'continue nil))
              (when (>= (aref (DFA-accept dfa) s) 1)
                (throw 'return (aref (DFA-accept dfa) s)))
              (let ((c (lexer-input-LA 1)))
                (when (and (>= c (aref (DFA-min dfa) s)) (<= c (aref (DFA-max dfa) s)))
                  (let ((snext (aref (aref (DFA-transition dfa) s) (- c (aref (DFA-min dfa) s)) )))
                    (when (< snext 0)
                      (when (>= (aref (DFA-eot dfa) s) 0)
                        (setq s (aref (DFA-eot dfa) s))
                        (lexer-input-consume)
                        (throw 'continue nil))
                      (dfa-no-viable-alt s)
                      (throw 'return 0))
                    (setq s snext)
                    (lexer-input-consume)
                    (throw 'continue nil)))
                (when (>= (aref (DFA-eot dfa) s) 0)
                  (setq s (aref (DFA-eot dfa) s))
                  (lexer-input-consume)
                  (throw 'continue nil))
                (when (and (eq c *antlr-token-eof-token*) (>= (aref (DFA-eof dfa) s) 0))
                  (throw 'return (aref (DFA-accept dfa) (aref (DFA-eof dfa) s))))
                (dfa-no-viable-alt s)
                (throw 'return 0)))))))))


(defun dfa-no-viable-alt (s)
  (signal 'no-viable-alt (list s)))

(defmacro predictDFA (name)
  `(predict-DFA-with (gethash ',name (antlr-lexer-dfas (antlr-lexer-context-lexer context)))))

(defmacro setDFA (name value)
  `(puthash ',name ,value (antlr-lexer-dfas current-lexer)))

(defmacro defDFA (name value)
  `(set (intern (concat "*" (format "%s" (antlr-lexer-name current-lexer)) "-" (format "%s" ',name) "*")) ,value))

(defmacro getDFA (name)
  `(symbol-value (intern (concat "*" (format "%s" (antlr-lexer-name current-lexer)) "-" (format "%s" ',name) "*"))))

(defmacro defDFAstruct (name &rest defaults)
  `(progn 
     (fset (intern (concat (format "%s" (antlr-lexer-name current-lexer)) "-" (format "%s" ',name))) 
           #'(lambda (reco)
               (make-DFA
                :recognizer reco
                ,@defaults
                )))
     (unless (fboundp (intern (concat "make-DFAstruct-" (format "%s" ',name))))
       (fset (intern (concat "make-DFAstruct-" (format "%s" ',name))) 
             #'(lambda ()
                 (funcall (intern (concat (format "%s" (antlr-lexer-name current-lexer)) "-" (format "%s" ',name))) nil))))))

(defun lexer-token-type (token)
  (common-token-type token))

(defun lexer-token-text (token)
  (cond
   ((not (null (common-token-text token))) 
    common-token-text token)
   ((not (null (common-token-input token)))
    (let ((text (with-current-buffer (common-token-input token)
		  (buffer-substring (common-token-start token) 
				    (common-token-stop token)))))
      (setf (common-token-text token) text)
      text))
   (t nil)))

(defconst *antlr-token-eof-token-type* -1)
(defconst *antlr-token-eor-token-type* 1)
(defconst *antlr-token-eof-token* (make-common-token :type *antlr-token-eof-token-type* :channel 0))
(defconst *antlr-token-invalid-token* (make-common-token :type *antlr-token-invalid-token-type* :channel 0))
(defconst *antlr-token-skip-token* (make-common-token :type *antlr-token-invalid-token-type* :channel 0))

(defstruct antlr-lexer-context
  "Context used for a lexing"
  (lexer nil)
  (input nil)
  (input-start -1)
  (input-end -1)
  (token nil)
  (token-start-char-index -1)
  (token-start-line -1)
  (token-start-char-position-in-line -1)
  (channel nil)
  (type nil)
  (text nil)
  (failed nil))

(put 'mismatched-token 'error-conditions
     '(error antlr-error))
(put 'mismatched-token 'error-message "Mismatched token")

(put 'no-viable-alt 'error-conditions
     '(error antlr-error))
(put 'no-viable-alt 'error-message "No viable alternative")

(defun lexer-set-type (type)
  (setf (antlr-lexer-context-type context) type))

(defun lexer-set-channel (c)
  (setf (antlr-lexer-context-channel context) c))

(defmacro lexer-call-rule (name)
  `(progn 
					;(message (concat "calling rule " (format "%s" ',name))) 
     (funcall (gethash ',name (antlr-lexer-rules (antlr-lexer-context-lexer context))) context)))

(defmacro lexer-token-id (name)
  `(gethash ',name (antlr-lexer-tokens (antlr-lexer-context-lexer context))))

(defmacro deftoken (name value)
  `(puthash ',name ,value 
            (if (boundp 'current-lexer) 
                (antlr-lexer-tokens current-lexer) 
	      (antlr-parser-tokens current-parser))))

(defmacro defrule (name params &rest body)
  (let ((tokens (if (boundp 'current-lexer)
                    (antlr-lexer-tokens current-lexer)
		  (antlr-parser-tokens current-parser)))
	(bitsets (if (boundp 'current-parser)
		     (antlr-parser-bitsets current-parser)))
        (current-buffer (if (boundp 'current-lexer)
                            '(antlr-lexer-context-input context)
			  '(antlr-lexer-context-input (antlr-parser-context-input context))))
        (let-bindings '()))
    
    ;; Create let-bindings for tokens
    (maphash (lambda (key value)
               (setq let-bindings (cons (list key value) let-bindings))) tokens)

    ;; Create let-bindings for bitsets
    (if bitsets
	(maphash (lambda (key value)
		   (setq let-bindings (cons (list key value) let-bindings))) bitsets))

    `(puthash ',name (lambda (context ,@params) 
                       (with-current-buffer ,current-buffer
                         (let (,@let-bindings)
                           ,@body)))
              (if (boundp 'current-lexer) 
                  (antlr-lexer-rules current-lexer) 
		(antlr-parser-rules current-parser)))))

(defmacro lexer-match-range (a b)
  (let ((la (lexer-input-LA 1)))
    (when (or (< la a) (> la b))
      (signal 'mismatched-range (list a b)))
    (lexer-input-consume)
    (setf (antlr-lexer-context-failed context) nil)))

(defmacro lexer-match (s)
  (cond
   ((numberp s) 
    `(if (= (lexer-input-LA 1) ,s)
	 (goto-char (+ (point) 1))
       (signal 'mismatched-token (list ,(char-to-string s) context))))
   ((stringp s) 
    `(let ((i 0)
	   (str ,s))
       (while (< i (length str))
	 (unless (= (lexer-input-LA 1) (elt str i))
	   (signal 'mismatched-token (list (point) (char-to-string (lexer-input-LA 1)) (lexer-input-LA 1) (char-to-string (elt str i)) (elt str i) str context)))
	 (incf i)
	 (lexer-input-consume)
	 (setf (antlr-lexer-context-failed context) nil))))
   (t (signal 'error "Implement t case"))))

(defmacro with-lexer (name &rest body)
  `(progn 
     (let ((current-lexer (gethash ',name *antlr-runtime-lexers*)))
       ,@body)))

(defun lex-string (name str method)
  (let ((buffer (generate-new-buffer (generate-new-buffer-name "*antlr string lexing*"))))
    (save-excursion
      (with-current-buffer buffer
        (insert str)
        (goto-char (point-min))
        (lex-buffer name method buffer 0 (buffer-size buffer))
        (set-buffer-modified-p nil)
        (kill-buffer buffer)))))

(defun lex-emit (token)
  (setf (antlr-lexer-context-token context) token))

(defun lex-emit-token ()
  (let ((token (make-common-token
                :input (current-buffer)
                :type (antlr-lexer-context-type context)
                :channel (antlr-lexer-context-channel context)
                :start (antlr-lexer-context-token-start-char-index context)
                :stop (point)
                :line (antlr-lexer-context-token-start-line context)
                :text (antlr-lexer-context-text context)
                :char-position-in-line (antlr-lexer-context-token-start-char-position-in-line context))))
    (lex-emit token)
    token))

(defun lexer-for-buffer (lexer-name buffer start end)
  (let* ((current-lexer (gethash lexer-name *antlr-runtime-lexers*))
         (context (make-antlr-lexer-context 
                   :lexer current-lexer
                   :input buffer
                   :input-start start
                   :input-end end)))
    (save-excursion
      (with-current-buffer (antlr-lexer-context-input context)
        (goto-char (antlr-lexer-context-input-start context))
	(setf (antlr-lexer-context-token context) nil
	      (antlr-lexer-context-channel context) *antlr-token-default-channel*
	      (antlr-lexer-context-token-start-char-index context) (point)
	      (antlr-lexer-context-token-start-char-position-in-line context) (current-column)
	      (antlr-lexer-context-token-start-line context) (line-number-at-pos)
	      (antlr-lexer-context-text context) nil)))
    context))

(defun lex-buffer (lexer-name method buffer start end)
  (let ((context (lexer-for-buffer lexer-name buffer start end)))
    (lex-with-lexer context method)))

(defun lex-with-lexer (context method)
  (save-excursion
    (with-current-buffer (antlr-lexer-context-input context)
      (catch 'at-end
	(while t
	  (setf (antlr-lexer-context-token context) nil
		(antlr-lexer-context-channel context) *antlr-token-default-channel*
		(antlr-lexer-context-token-start-char-index context) (point)
		(antlr-lexer-context-token-start-char-position-in-line context) (current-column)
		(antlr-lexer-context-token-start-line context) (line-number-at-pos)
		(antlr-lexer-context-text context) nil)
	  (when (= (point) (point-max))
	    (funcall method *antlr-token-eof-token*)
	    (throw 'at-end nil))
	  (condition-case nil
	      (progn
		(funcall (gethash 'Tokens (antlr-lexer-rules (antlr-lexer-context-lexer context))) context)
		(when (null (antlr-lexer-context-token context))
		  (lex-emit-token))
		(unless (eq (antlr-lexer-context-token context) *antlr-token-skip-token*)
		  (funcall method (antlr-lexer-context-token context))))
	    (message "TODO handling error here")))))))
  

(defstruct antlr-parser
  "An Antlr parser"
  name
  (token-names nil)
  (tokens (make-hash-table))
  (rules (make-hash-table))
  (bitsets (make-hash-table))
  )


(defmacro with-parser (name &rest body)
  `(progn 
     (let ((current-parser (gethash ',name *antlr-runtime-parsers*)))
       ,@body)))

(defun parser-token-names (&rest names)
  (setf (antlr-parser-token-names current-parser) names))

(defmacro parser-initialization (&rest body))

(defmacro parser-bitset (name bitsets)
  `(puthash ',name 
	    (make-bitset :bits ',bitsets)
	    (antlr-parser-bitsets current-parser)))



(defstruct bitset
  "An Antlr bitset"
  (bits (make-vector 1 0)))

(defconst *bitset-word-size* 29)
(defconst *bitset-mod-mask* (- *bitset-word-size* 1))

(defun bitset-member (set el)
  (if (< el 0) nil
    (let ((n (bitset-word-number el)))
      (if (>= n (bitset-word-len set)) nil
	(/= (logand (bitset-word-at set n) (bitset-bitmask el)) 0)))))

(defun bitset-or-in-place (set a)
  (unless (null a)

    ;;If this is smaller than a, grow this first
    (if (> (bitset-word-len a) (bitset-word-len set))
	(bitset-set-size set (bitset-word-len a)))

    (let ((m (min (bitset-word-len set) 
		  (bitset-word-len a)))
	  (i -1))
      (setf i (- m 1))
      (while (>= i 0)
	(aset (bitset-bits set) i
	      (logior (bitset-word-at set i) 
		      (bitset-word-at a i)))
	(decf i))
      )))

(defun bitset-add (set el)
  (let ((n (bitset-word-number el)))
    (if (>= n (bitset-word-len set))
	(bitset-grow-to-include set el))
    (aset (bitset-bits set) n
	  (logior (bitset-word-at set n) 
		  (bitset-bitmask el)))
    ))

(defun bitset-remove (set el)
  (let ((n (bitset-word-number el)))
    (if (< n (bitset-word-len set))
	(aset (bitset-bits set) n 
	      (logand (bitset-word-at set n) (lognot (bitset-bitmask el)))))))

(defun bitset-bitmask (el)
  (lsh 1 (logand el *bitset-mod-mask*)))

(defun bitset-word-number (el)
  (/ el *bitset-word-size*))

(defun bitset-word-len (set)
  (length (bitset-bits set)))

(defun bitset-word-at (set n)
  (aref (bitset-bits set) n))

(defun bitset-set-size (set nwords)
  "Sets the size of a set.
   @param nwords how many words the new set should be"
  (let* ((n (max nwords (bitset-word-len set)))
	 (growth (- n (bitset-word-len set)))
	 (newbits (vconcat (bitset-bits set) (make-vector growth 0))))
    (setf (bitset-bits set) newbits)))

(defun bitset-grow-to-include (set bit)
  "Grows the set to a larger number of bits.
   @param bit element that must fit in set"
  (let* ((new-size (max (lsh (bitset-word-len set) 1)
			(bitset-num-words-to-hold bit)))
	 (growth (- new-size (bitset-word-len set)))
	 (newbits (vconcat (bitset-bits set) (make-vector growth 0))))
    (setf (bitset-bits set) newbits)))

(defun bitset-num-words-to-hold (el)
  (+ (/ el *bitset-word-size*) 1))





(defstruct antlr-parser-context
  "An instance of a running parser"
  input
  parser
  (fsp -1)
  (following (make-vector 100 nil))
  (token-buffer (make-vector 100 nil))
  (pos -1)
  (channel *antlr-token-default-channel*)
  (discard-off-channel-tokens nil)
  (backtracking 0)
  (failed nil)
  (last-error-index -1)
  (error-recovery nil)
  )


(defun parser-input-LA (k)
  (common-token-type (parser-input-LT k)))


(defun parser-input-LT (k)
  "Get the ith token from the current position 1..n where k=1 is the first symbol of lookahead."
  (catch 'return
    (if (= (antlr-parser-context-pos context) -1) (parser-fill-buffer))
    (if (= k 0) (throw 'return nil))
    (if (< k 0) (throw 'return (parser-input-LB (- k))))
    (if (>= (+ (antlr-parser-context-pos context) k (- 1)) (length (antlr-parser-context-token-buffer context)))
	(throw 'return *antlr-token-eof-token*))
    (let ((i (antlr-parser-context-pos context))
	  (n 1))
      (while (< n k)
	;;skip off-channel tokens
	(setf i (skip-off-token-channels (+ i 1)))
	(incf n)
	)
      (let ((token-buffer (antlr-parser-context-token-buffer context)))
	(if (>= i (length token-buffer))
	    (throw 'return *antlr-token-eof-token*))
	(aref token-buffer i))
      )))


(defun parser-input-LB (k)
  "Look backwards k tokens on-channel tokens"
  (throw "Not implemented!")
  )


(defun parser-match (ttype follow)
  "Match current input symbol against ttype.  Upon error, do one token
  insertion or deletion if possible.  You can override to not recover
  here and bail out of the current production to the normal error
  exception catch (at the end of the method) by just throwing
  MismatchedTokenException upon input.LA(1)!=ttype."
  (cond 

   ((eql (parser-input-LA 1) ttype)
    (progn
      (parser-consume)
      (setf (antlr-parser-context-error-recovery context) nil)
      (setf (antlr-parser-context-failed context) nil)))

   ((> (antlr-parser-context-backtracking 0))
    (setf (antlr-parser-context-failed context) t))

   (t (parser-mismatch(ttype, follow)))))


(defun parser-mismatch (ttype follow)
  "Factor out what to do upon token mismatch so tree parsers can behave
   differently.  Override this method in your parser to do things
   like bailing out after the first error; just throw the mte object
   instead of calling the recovery method."
  (signal 'mismatched-token (list ttype context))
  ;; TODO: recoverFromMismatchedToken(input, mte, ttype, follow)	;
  )


(defun parser-consume ()
  "Move the input pointer to the next incoming token.  The stream
   must become active with LT(1) available.  consume() simply
   moves the input pointer so that LT(1) points at the next
   input symbol. Consume at least one token.
   Walk past any token not on the channel the parser is listening to."
  (if (< (antlr-parser-context-pos context) 
	 (length (antlr-parser-context-token-buffer context)))
      (progn
	(incf (antlr-parser-context-pos context))
	;; leave p on valid token
	(setf (antlr-parser-context-pos context) 
	      (skip-off-token-channels (antlr-parser-context-pos context))))))



(defun parser-consume-until-type (token-type)
  "Consume tokens until one matches the given token set"
  (let ((ttype (parser-input-LA 1)))
    (while (and (/= ttype *antlr-token-eof-token-type*) (/= ttype token-type))
      (parser-consume)
      (setf ttype (parser-input-LA 1))
      )
    ))


(defun parser-consume-until-in-set (set)
  "Consume tokens until one matches the given token set"
  (let ((ttype (parser-input-LA 1)))
    (while (and (/= ttype *antlr-token-eof-token-type*) (not (bitset-member set ttype)))
      (parser-consume)
      (setf ttype (parser-input-LA 1))
      )
    ))


(defun parser-fill-buffer ()
  "Load all tokens from the token source and put in token-buffer.
   This is done upon first LT request because you might want to
   set some token type / channel overrides before filling buffer."
  (let* ((lexer-context (antlr-parser-context-input context))
	 (index 0)
	 (tokens nil))
    (lex-with-lexer 
     lexer-context 
     #'(lambda (token) 
	 (if (and token (/= (common-token-type token) *antlr-token-eof-token-type*))
	     (let ((discard nil))
	       ;;// is there a channel override for token type?
	       ;;if ( channelOverrideMap!=null ) {
	       ;;	Integer channelI = (Integer)
	       ;;		channelOverrideMap.get(new Integer(t.getType()));
	       ;;	if ( channelI!=null ) {
	       ;;		t.setChannel(channelI.intValue());
	       ;;	}
	       ;;}
	       ;;if ( discardSet!=null &&
	       ;;	 discardSet.contains(new Integer(t.getType())) )
	       ;;{
	       ;;	discard = true;
	       ;;}
	       ;;else if ( discardOffChannelTokens && t.getChannel()!=this.channel ) {
	       ;;	discard = true;
	       ;;}
	       (if (not discard)
		   (progn
		     (setf (common-token-index token) index)
		     (setf tokens (cons token tokens)) ;;TODO This should be more efficient.
		     (incf index)))
	       ))))
    (setf (antlr-parser-context-token-buffer context) (vconcat (reverse tokens)))
    (setf (antlr-parser-context-pos context) 0)
    (setf (antlr-parser-context-pos context) (skip-off-token-channels 0))
    ))



(defun skip-off-token-channels (i)
  "Given a starting index, return the index of the first on-channel token."
  (let* ((token-buffer (antlr-parser-context-token-buffer context))
	 (channel (antlr-parser-context-channel context))
	 (n (length token-buffer)))
    (while (and (< i n) (/= (common-token-channel (aref token-buffer i)) channel))
      (incf i))
    i))


(defun buffer-from-string (str)
  (let ((buffer (generate-new-buffer (generate-new-buffer-name "*antlr string lexing*"))))
    (save-excursion
      (with-current-buffer buffer
	(insert str)
	(goto-char (point-min))
	(set-buffer-modified-p nil)))
    buffer))

(defun parse-string (lname pname start-rule str) 
  (let* ((buffer  (buffer-from-string str))
	 (context (make-antlr-parser-context 
		   :input (lexer-for-buffer lname buffer 0 (buffer-size buffer))
		   :parser (gethash pname *antlr-runtime-parsers*))))
    (funcall (gethash start-rule (antlr-parser-rules (antlr-parser-context-parser context))) context)))

(defmacro parser-push-follow (rule-name)
  `(progn 
     (if (>= (+ (antlr-parser-context-fsp context) 1)
	     (length (antlr-parser-context-following context)))
	 (let ((f (vconcat (antlr-parser-context-following context) 
			   (make-vector (length (antlr-parser-context-following context)) nil))))
	   (setf (antlr-parser-context-following context) f)))
     (incf (antlr-parser-context-fsp context))
     (aset (antlr-parser-context-following context)
	   (antlr-parser-context-fsp context)
	   ,rule-name)
     ))

(defmacro parser-call-rule (name)
  `(progn 
     ;;(message (concat "calling rule " (format "%s" ',name))) 
     (funcall (gethash ',name (antlr-parser-rules (antlr-parser-context-parser context))) context)))



(defun parser-report-error (re)
  "Report a recognition problem.
	
   This method sets errorRecovery to indicate the parser is recovering
   not parsing.  Once in recovery mode, no errors are generated.
   To get out of recovery mode, the parser must successfully match
   a token (after a resync).  So it will go:
   		1. error occurs
   		2. enter recovery mode, report error
   		3. consume until token found in resynch set
   		4. try to resume parsing
   		5. next match() will reset errorRecovery mode
   "
  (unless (antlr-parser-context-error-recovery context)
    (setf (antlr-parser-context-error-recovery context) t)
    (message "%s : %s" (car re) (cdr re))
    )
  )

(defun parser-recover (re)
  "Recover from an error found on the input stream.  Mostly this is
   NoViableAlt exceptions, but could be a mismatched token that
   the match() routine could not recover from."
	
  (if (= (antlr-parser-context-last-error-index context) 
	 (antlr-parser-context-pos context))
      ;; uh oh, another error at same token index ; must be a case
      ;; where LT(1) is in the recovery token set so nothing is
      ;; consumed	       ; consume a single token so at least to prevent
      ;; an infinite loop  ; this is a failsafe.
      (parser-consume)
    )

  (setf (antlr-parser-context-last-error-index context)
	(antlr-parser-context-pos context))

  (let ((follow-set (parser-compute-error-recover-set)))
    (parser-consume-until-in-set follow-set)
    ))


(defun parser-compute-error-recovery-set () 
  "Compute the error recovery set for the current rule.  During
   rule invocation, the parser pushes the set of tokens that can
   follow that rule reference on the stack; this amounts to
   computing FIRST of what follows the rule reference in the
   enclosing rule. This local follow set only includes tokens
   from within the rule; i.e., the FIRST computation done by
   ANTLR stops at the end of a rule....(see java source for examples)"
  (parser-combine-follows nil))

(defun parser-compute-context-sensitive-rule-follow ()
  "Compute the context-sensitive FOLLOW set for current rule.
   This is set of token types that can follow a specific rule
   reference given a specific call chain.  You get the set of
   viable tokens that can possibly come next (lookahead depth 1)
   given the current call chain....(see java source for examples)"
  (parser-combine-follows t))


(defun parser-combine-follows (exact)
  (let ((top (antlr-parser-context-fsp context))
	(follow-set (make-bitset)))
    (let ((i top))
      (catch 'break
	(while (>= i 0)
	  (let ((local-follow-set (aref (antlr-parser-context-following context) i)))
	    (bitset-or-in-place follow-set local-follow-set)
	    (if (and exact (not (bitset-member local-follow-set *antlr-token-eor-token-type*)))
		(throw 'break nil))
	    (decf i)
	    ))))
    (bitset-remove follow-set *antlr-token-eor-token-type*)
    follow-set
    ))



;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;


(defmacro antlr-alt-case (expr-form &rest clauses)
  "A case statement that doesn't quote the heads of its clauses."
  ;; Compare the following:
  ;; (message "%s" (macroexpand '(antlr-alt-case (+ 1 2) (5 1 3) (3 t) (2) (t nil))))
  ;; (message "%s" (macroexpand '(case           (+ 1 2) (5 1 3) (3 t) (2) (t nil))))
  (let ((val-sym (gensym)))
    `(let ((,val-sym ,expr-form))
       (cond ,@(mapcar (lambda (c)
			 (let ((tail (cond 
				      ((> (length (cdr c)) 0) (cdr c))
				      (t (list (car c))))))
			   (if (memq (car c) '(t otherwise))
			       `(t ,@tail)
			     `((eql ,val-sym ,(car c)) ,@tail))))
		       clauses)))))


(provide 'antlr-runtime)
;;; antlr-runtime.el ends here
