;;; a3el-runtime.el --- Antlr runtime

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

(defvar *a3el-runtime-lexers* (make-hash-table)
  "Keeps track of all lexers defined in the system, including their definitions") 

(defvar *a3el-runtime-parsers* (make-hash-table)
  "Keeps track of all parsers defined in the system, including their definitions") 


;; Recognizer error types

(put 'a3el-mismatched-token 'error-conditions
     '(error a3el-re-error))
(put 'a3el-mismatched-token 'error-message "Mismatched token")

(put 'a3el-no-viable-alt 'error-conditions
     '(error a3el-re-error))
(put 'a3el-no-viable-alt 'error-message "No viable alternative")

(put 'a3el-early-exit 'error-conditions
     '(error a3el-re-error))
(put 'a3el-early-exit 'error-message "Early exit")

(put 'a3el-mismatched-set 'error-conditions
     '(error a3el-re-error))
(put 'a3el-mismatched-set 'error-message "Mismatched set")

(put 'a3el-mismatched-range 'error-conditions
     '(error a3el-re-error))
(put 'a3el-mismatched-range 'error-message "Mismatched range")



(defstruct a3el-common-token
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

(defconst *a3el-token-default-channel* 0)

(defconst *a3el-token-invalid-token-type* 0)

(defconst *a3el-token-eof-token-type* -1)

(defconst *a3el-token-eor-token-type* 1)

(defconst *a3el-token-eof-token* (make-a3el-common-token :type *a3el-token-eof-token-type* :channel 0))

(defconst *a3el-token-invalid-token* (make-a3el-common-token :type *a3el-token-invalid-token-type* :channel 0))

(defconst *a3el-token-skip-token* (make-a3el-common-token :type *a3el-token-invalid-token-type* :channel 0))






(defstruct a3el-DFA
  recognizer
  decision-number
  eot
  eof
  min
  max
  accept
  special
  transition
  description
  special-state-transition
  )


(defun a3el-predict-DFA-with (dfa)
  (let ((mark (a3el-lexer-input-mark)))
    (unwind-protect
	(catch 'return
	  (let ((s 0))
	    (while t
	      (catch 'continue
		(let ((special-state (aref (a3el-DFA-special dfa) s)))
		  (when (>= special-state 0)
		    (setq s (funcall (a3el-DFA-special-state-transition dfa) special-state))
		    (a3el-lexer-input-consume)
		    (throw 'continue nil))
		  (when (>= (aref (a3el-DFA-accept dfa) s) 1)
		    (throw 'return (aref (a3el-DFA-accept dfa) s)))
		  (let ((c (a3el-lexer-input-LA 1)))
		    (when (and (>= c (aref (a3el-DFA-min dfa) s)) (<= c (aref (a3el-DFA-max dfa) s)))
		      (let ((snext (aref (aref (a3el-DFA-transition dfa) s) (- c (aref (a3el-DFA-min dfa) s)) )))
			(when (< snext 0)
			  (when (>= (aref (a3el-DFA-eot dfa) s) 0)
			    (setq s (aref (a3el-DFA-eot dfa) s))
			    (a3el-lexer-input-consume)
			    (throw 'continue nil))
			  (a3el-dfa-no-viable-alt s)
			  (throw 'return 0))
			(setq s snext)
			(a3el-lexer-input-consume)
			(throw 'continue nil)))
		    (when (>= (aref (a3el-DFA-eot dfa) s) 0)
		      (setq s (aref (a3el-DFA-eot dfa) s))
		      (a3el-lexer-input-consume)
		      (throw 'continue nil))
		    (when (and (eq c *a3el-token-eof-token*) (>= (aref (a3el-DFA-eof dfa) s) 0))
		      (throw 'return (aref (a3el-DFA-accept dfa) (aref (a3el-DFA-eof dfa) s))))
		    (a3el-dfa-no-viable-alt s)
		    (throw 'return 0)))))))
      (a3el-lexer-input-rewind-to mark))
    ))

(defun a3el-dfa-no-viable-alt (s)
  (signal 'a3el-no-viable-alt (list s)))

(defmacro a3el-lexer-predict-DFA (name)
  `(a3el-predict-DFA-with (gethash ',name (a3el-lexer-dfas (a3el-lexer-context-lexer context)))))

(defmacro a3el-lexer-set-DFA (name value)
  "Install a newly instantiated DFA into the lexer (during lexer definition)."
  `(puthash ',name ,value (a3el-lexer-dfas current-lexer)))

(defmacro a3el-defDFAstruct (name &rest defaults)
  "Define a custom DFA creation function"
  `(progn 
     (fset ',name
	   #'(lambda (reco)
	       (make-a3el-DFA
		:recognizer reco
		,@defaults
		)))
     (unless (fboundp (intern (concat "make-a3el-DFAstruct-" (format "%s" ',name))))
       (fset (intern (concat "make-a3el-DFAstruct-" (format "%s" ',name)))
	     #'(lambda ()
		 (funcall ',name nil))))))










(defmacro a3el-deftoken (name value)
  `(puthash ',name ,value 
	    (if (boundp 'current-lexer)
		(a3el-lexer-tokens current-lexer)
	      (a3el-parser-tokens current-parser))))


(defmacro a3el-defrule (name params &rest body)
  `(puthash ',name (lambda (context ,@params)
		     (with-current-buffer current-buffer
		       ,@body))
	    (if (boundp 'current-lexer) 
		(a3el-lexer-rules current-lexer)
	      (a3el-parser-rules current-parser))))



(defstruct a3el-char-stream-state
  "The state of a character stream.
   For elisp lexers, this amounts to 
   tracking the point in the current 
   buffer."
  (pos -1)
  (line -1)
  (char-position-in-line -1))


(defstruct a3el-lexer
  "An Antlr lexer"
  name
  (tokens (make-hash-table))
  (rules (make-hash-table))
  (dfas (make-hash-table))
  (entry-func nil)
  )


(defstruct a3el-lexer-context
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
  (failed nil)
  (backtracking 0)

  ;; Track the last mark() call result value for use in rewind().
  (last-marker -1)
  
  ;; Tracks how deep a3el-lexer-input-mark calls are nested
  (mark-depth 0)
	
  ;; A list of a3el-char-stream-state objects that tracks the stream state
  ;; values line, char-position-in-line, and pos that can change as you
  ;; move through the input stream.  Indexed from 1..markDepth.
  ;; A nil is kept @ index 0.  Create upon first call to a3el-lexer-input-mark.
  (markers '())

  ;; Line number 1..n within the input
  (line -1)

  ;; The index of the character relative to the beginning of the line 0..n-1
  (char-position-in-line -1)
  )

(defun a3el-lexer-context-pos (context) (point))

(defmacro a3el-deflexer (name)
  `(puthash ',name 
	    (make-a3el-lexer :name ',name) 
	    *a3el-runtime-lexers*))

(defun a3el-lexer-input-LA (n)
  (let ((at (+ (point) (- n 1))))
    (if (= at (point-max))
	-1
      (char-after at))))
  
(defun a3el-lexer-input-consume ()
  (goto-char (+ (point) 1)))

(defun a3el-lexer-match-any ()
  (a3el-lexer-input-consume))

(defun a3el-lexer-token-type (token)
  (a3el-common-token-type token))

(defun a3el-lexer-token-text (token)
  (cond
   ((not (null (a3el-common-token-text token))) 
    a3el-common-token-text token)
   ((not (null (a3el-common-token-input token)))
    (let ((text (with-current-buffer (a3el-common-token-input token)
		  (buffer-substring (a3el-common-token-start token) 
				    (a3el-common-token-stop token)))))
      (setf (a3el-common-token-text token) text)
      text))
   (t nil)))


(defun a3el-lexer-input-mark ()
  (progn
    (if (null (a3el-lexer-context-markers context))
	;; depth 0 means no backtracking, leave blank
	(setf (a3el-lexer-context-markers context) (list nil)))
    (incf (a3el-lexer-context-mark-depth context))
    (let ((state nil))
      (if (>= (a3el-lexer-context-mark-depth context)
	      (length (a3el-lexer-context-markers context)))
	  (progn
	    (setq state (make-a3el-char-stream-state))
	    (setf (a3el-lexer-context-markers context) 
		  (append (a3el-lexer-context-markers context) (list state))))
	(progn
	  (setq state (nth (a3el-lexer-context-mark-depth context)
			   (a3el-lexer-context-markers context)))))

      (setf (a3el-char-stream-state-pos state) 
	    (a3el-lexer-context-pos context))

      (setf (a3el-char-stream-state-line state)
	    (a3el-lexer-context-line context))

      (setf (a3el-char-stream-state-char-position-in-line state) 
	    (a3el-lexer-context-char-position-in-line context))

      (setf (a3el-lexer-context-last-marker context) 
	    (a3el-lexer-context-mark-depth context))

      (a3el-lexer-context-mark-depth context))))



(defun a3el-lexer-input-rewind-to (m)
  (let ((state (nth m (a3el-lexer-context-markers context))))

    ;; restore stream state

    (a3el-lexer-input-seek (a3el-char-stream-state-pos state))
    (setf (a3el-lexer-context-line context)
	  (a3el-char-stream-state-line state))

    (setf (a3el-lexer-context-char-position-in-line context)
	  (a3el-char-stream-state-char-position-in-line state))

    (a3el-lexer-input-release m)))


(defun a3el-lexer-input-rewind ()
  (a3el-lexer-input-rewind-to (a3el-lexer-context-last-marker context)))


(defun a3el-lexer-input-release (m)
  ;;unwind any other markers made after m and release m
  (setf (a3el-lexer-context-mark-depth context) m)
  ;;release this marker
  (decf (a3el-lexer-context-mark-depth context))
  )


(defun a3el-lexer-input-seek (index)
  "consume() ahead until p==index; can't just set p=index as we must
   update line and charPositionInLine."
  (if (<= index (point))
      (goto-char index) ;; just jump; don't update stream state (line, ...)
    ;; seek forward, consume until p hits index
    (while (< (point) index) 
      (a3el-lexer-input-consume))))

(defun a3el-lexer-set-type (type)
  (setf (a3el-lexer-context-type context) type))

(defun a3el-lexer-set-channel (c)
  (setf (a3el-lexer-context-channel context) c))

(defmacro a3el-lexer-call-rule (name)
  `(progn 
     ;;(message (concat "calling rule " (format "%s" ',name))) 
     (funcall (gethash ',name (a3el-lexer-rules (a3el-lexer-context-lexer context))) context)))

(defmacro a3el-lexer-token-id (name)
  `(gethash ',name (a3el-lexer-tokens (a3el-lexer-context-lexer context))))


(defmacro a3el-lexer-match-range (a b)
  `(let ((la (a3el-lexer-input-LA 1)))
     (when (or (< la ,a) (> la ,b))
       (signal 'a3el-mismatched-range (list ,a ,b)))
     (a3el-lexer-input-consume)
     (setf (a3el-lexer-context-failed context) nil)))

(defmacro a3el-lexer-match (s)
  (cond
   ((numberp s) 
    `(if (= (a3el-lexer-input-LA 1) ,s)
	 (goto-char (+ (point) 1))
       (signal 'a3el-mismatched-token (list ,(char-to-string s) context))))
   ((stringp s) 
    `(let ((i 0)
	   (str ,s))
       (while (< i (length str))
	 (unless (= (a3el-lexer-input-LA 1) (elt str i))
	   (signal 'a3el-mismatched-token (list 
					   (point) 
					   (char-to-string (a3el-lexer-input-LA 1)) 
					   (a3el-lexer-input-LA 1) 
					   (char-to-string (elt str i)) 
					   (elt str i) str context)))
	 (incf i)
	 (a3el-lexer-input-consume)
	 (setf (a3el-lexer-context-failed context) nil))))
   (t (signal 'error "Implement t case"))))

(defmacro a3el-with-lexer (name &rest body)
  `(progn 
     (let ((current-lexer (gethash ',name *a3el-runtime-lexers*)))
       ,@body)))

(defun a3el-lex-string (name str method)
  (let ((buffer (generate-new-buffer (generate-new-buffer-name "*antlr string lexing*"))))
    (save-excursion
      (with-current-buffer buffer
	(insert str)
	(goto-char (point-min))
	(a3el-lex-buffer name method buffer 0 (buffer-size buffer))
	(set-buffer-modified-p nil)
	(kill-buffer buffer)))))

(defun a3el-lex-emit (token)
  (setf (a3el-lexer-context-token context) token))

(defun a3el-lex-emit-token ()
  (let ((token (make-a3el-common-token
		:input (current-buffer)
		:type (a3el-lexer-context-type context)
		:channel (a3el-lexer-context-channel context)
		:start (a3el-lexer-context-token-start-char-index context)
		:stop (point)
		:line (a3el-lexer-context-token-start-line context)
		:text (a3el-lexer-context-text context)
		:char-position-in-line (a3el-lexer-context-token-start-char-position-in-line context))))
    (a3el-lex-emit token)
    token))

(defun a3el-lexer-for-buffer (lexer-name buffer start end)
  (let* ((current-lexer (gethash lexer-name *a3el-runtime-lexers*))
	 (context (make-a3el-lexer-context 
		   :lexer current-lexer
		   :input buffer
		   :input-start start
		   :input-end end)))
    (save-excursion
      (with-current-buffer (a3el-lexer-context-input context)
	(goto-char (a3el-lexer-context-input-start context))
	(setf (a3el-lexer-context-token context) nil
	      (a3el-lexer-context-channel context) *a3el-token-default-channel*
	      (a3el-lexer-context-token-start-char-index context) (point)
	      (a3el-lexer-context-token-start-char-position-in-line context) (current-column)
	      (a3el-lexer-context-token-start-line context) (line-number-at-pos)
	      (a3el-lexer-context-text context) nil)))
    context))

(defun a3el-lex-buffer (lexer-name method buffer start end)
  (let ((context (a3el-lexer-for-buffer lexer-name buffer start end)))
    (a3el-lex-with-lexer context method)))

(defun a3el-lex-with-lexer (context method)
  (save-excursion
    (with-current-buffer (a3el-lexer-context-input context)
      (catch 'at-end
	(while t
	  (setf (a3el-lexer-context-token context) nil
		(a3el-lexer-context-channel context) *a3el-token-default-channel*
		(a3el-lexer-context-token-start-char-index context) (point)
		(a3el-lexer-context-token-start-char-position-in-line context) (current-column)
		(a3el-lexer-context-token-start-line context) (line-number-at-pos)
		(a3el-lexer-context-text context) nil)
	  (when (= (point) (point-max))
	    (funcall method *a3el-token-eof-token*)
	    (throw 'at-end nil))
	  (condition-case re
	      (progn
		(funcall (a3el-lexer-entry-func (a3el-lexer-context-lexer context)) context 'Tokens)
		(when (null (a3el-lexer-context-token context))
		  (a3el-lex-emit-token))
		(unless (eq (a3el-lexer-context-token context) *a3el-token-skip-token*)
		  (funcall method (a3el-lexer-context-token context))))
	    (a3el-re-error
	     (a3el-lexer-report-error re)
	     (a3el-lexer-recover re)
	     )))))))

(defmacro a3el-lexer-call-synpred (synpred-rule-name)
  `(progn
     (incf (a3el-lexer-context-backtracking context))
     (let ((start (a3el-lexer-input-mark))
	   (success nil))
       (condition-case er
	   (a3el-lexer-call-rule ,synpred-rule-name) ;; can never throw exception
	 (a3el-re-error
	  (throw er "Illegal state! synpreds cannot throw exceptions.")))
       (setq success (not (a3el-lexer-context-failed context)))
       (a3el-lexer-input-rewind-to start)
       (decf (a3el-lexer-context-backtracking context))
       (setf (a3el-lexer-context-failed context) nil)
       success)))


(defun a3el-lexer-report-error (re)
  (if (boundp '*a3el-swallowed-recogition-errors*)
      (push re *a3el-swallowed-recogition-errors*)
    (a3el-display-recognition-error re)))

(defun a3el-lexer-recover (re)
  "Lexers can normally match any char in it's vocabulary after matching
   a token, so do the easy thing and just kill a character and hope
   it all works out.  You can instead use the rule invocation stack
   to do sophisticated error recovery if you are in a fragment rule."
  (a3el-lexer-input-consume))
  






(defstruct a3el-bitset
  "An Antlr bitset"
  (bits (make-vector 1 0)))

(defconst *a3el-bitset-word-size* 29)

(defconst *a3el-bitset-mod-mask* (- *a3el-bitset-word-size* 1))

(defun a3el-bitset-member (set el)
  (if (< el 0) nil
    (let ((n (a3el-bitset-word-number el)))
      (if (>= n (a3el-bitset-word-len set)) nil
	(/= (logand (a3el-bitset-word-at set n) (a3el-bitset-bitmask el)) 0)))))

(defun a3el-bitset-or-in-place (set a)
  (unless (null a)

    ;;If this is smaller than a, grow this first
    (if (> (a3el-bitset-word-len a) (a3el-bitset-word-len set))
	(a3el-bitset-set-size set (a3el-bitset-word-len a)))

    (let ((m (min (a3el-bitset-word-len set) 
		  (a3el-bitset-word-len a)))
	  (i -1))

      (setf i (- m 1))
      (while (>= i 0)
	(aset (a3el-bitset-bits set) i
	      (logior (a3el-bitset-word-at set i) 
		      (a3el-bitset-word-at a i)))
	(decf i))
      )))

(defun a3el-bitset-add (set el)
  (let ((n (a3el-bitset-word-number el)))
    (if (>= n (a3el-bitset-word-len set))
	(a3el-bitset-grow-to-include set el))
    (aset (a3el-bitset-bits set) n
	  (logior (a3el-bitset-word-at set n) 
		  (a3el-bitset-bitmask el)))
    ))

(defun a3el-bitset-remove (set el)
  (let ((n (a3el-bitset-word-number el)))
    (if (< n (a3el-bitset-word-len set))
	(aset (a3el-bitset-bits set) n 
	      (logand (a3el-bitset-word-at set n) (lognot (a3el-bitset-bitmask el)))))))

(defun a3el-bitset-bitmask (el)
  (lsh 1 (logand el *a3el-bitset-mod-mask*)))

(defun a3el-bitset-word-number (el)
  (/ el *a3el-bitset-word-size*))

(defun a3el-bitset-word-len (set)
  (length (a3el-bitset-bits set)))

(defun a3el-bitset-word-at (set n)
  (aref (a3el-bitset-bits set) n))

(defun a3el-bitset-set-size (set nwords)
  "Sets the size of a set.
   @param nwords how many words the new set should be"
  (let* ((n (max nwords (a3el-bitset-word-len set)))
	 (growth (- n (a3el-bitset-word-len set)))
	 (newbits (vconcat (a3el-bitset-bits set) (make-vector growth 0))))
    (setf (a3el-bitset-bits set) newbits)))

(defun a3el-bitset-grow-to-include (set bit)
  "Grows the set to a larger number of bits.
   @param bit element that must fit in set"
  (let* ((new-size (max (lsh (a3el-bitset-word-len set) 1)
			(a3el-bitset-num-words-to-hold bit)))
	 (growth (- new-size (a3el-bitset-word-len set)))
	 (newbits (vconcat (a3el-bitset-bits set) (make-vector growth 0))))
    (setf (a3el-bitset-bits set) newbits)))

(defun a3el-bitset-num-words-to-hold (el)
  (+ (/ el *a3el-bitset-word-size*) 1))





(defstruct a3el-parser
  "An Antlr parser"
  name
  (token-names nil)
  (tokens (make-hash-table))
  (rules (make-hash-table))
  (bitsets (make-hash-table))
  (entry-func nil)
  )


(defstruct a3el-parser-context
  "An instance of a running parser"
  input
  parser
  (fsp -1)
  (following (make-vector 100 nil))
  (token-buffer (make-vector 100 nil))
  (pos -1)
  (channel *a3el-token-default-channel*)
  (discard-off-channel-tokens nil)
  (backtracking 0)
  (failed nil)
  (last-error-index -1)
  (error-recovery nil)
  (last-marker -1)
  )

(defmacro a3el-with-parser (name &rest body)
  `(progn 
     (let ((current-parser (gethash ',name *a3el-runtime-parsers*)))
       ,@body)))

(defun a3el-parser-init-token-names (&rest names)
  (setf (a3el-parser-token-names current-parser) names))

(defmacro a3el-parser-initialization (&rest body))

(defmacro a3el-parser-bitset (name bitsets)
  `(puthash ',name 
	    (make-a3el-bitset :bits ,bitsets)
	    (a3el-parser-bitsets current-parser)))

(defmacro a3el-defparser (name)
  `(puthash ',name 
	    (make-a3el-parser :name ',name) 
	    *a3el-runtime-parsers*))

(defun a3el-parser-input-mark ()
  (if (= (a3el-parser-context-pos context) -1) (a3el-parser-fill-buffer))
  (setf (a3el-parser-context-last-marker context) (a3el-parser-context-pos context))
  (a3el-parser-context-last-marker context))

(defun a3el-parser-input-rewind-to (p)
  (a3el-parser-input-seek p))

(defun a3el-parser-input-rewind ()
  (a3el-parser-input-seek (a3el-parser-context-last-marker context)))

(defun a3el-parser-input-seek (p)
  (setf (a3el-parser-context-pos context) p))

(defun a3el-parser-input-LA (k)
  (a3el-common-token-type (a3el-parser-input-LT k)))


(defun a3el-parser-input-LT (k)
  "Get the ith token from the current position 1..n where k=1 is the first symbol of lookahead."
  (catch 'return
    (if (= (a3el-parser-context-pos context) -1) (a3el-parser-fill-buffer))
    (if (= k 0) (throw 'return nil))
    (if (< k 0) (throw 'return (a3el-parser-input-LB (- k))))
    (if (>= (+ (a3el-parser-context-pos context) k (- 1)) (length (a3el-parser-context-token-buffer context)))
	(throw 'return *a3el-token-eof-token*))
    (let ((i (a3el-parser-context-pos context))
	  (n 1))
      (while (< n k)
	;;skip off-channel tokens
	(setf i (a3el-parser-skip-off-token-channels (+ i 1)))
	(incf n))
      (let ((token-buffer (a3el-parser-context-token-buffer context)))
	(if (>= i (length token-buffer))
	    (throw 'return *a3el-token-eof-token*))
	(aref token-buffer i))
      )))


(defun a3el-parser-input-LB (k)
  "Look backwards k tokens on-channel tokens"
  (throw 'error "Not implemented!"))


(defun a3el-parser-match (ttype follow)
  "Match current input symbol against ttype.  Upon error, do one token
  insertion or deletion if possible.  You can override to not recover
  here and bail out of the current production to the normal error
  exception catch (at the end of the method) by just throwing
  MismatchedTokenException upon input.LA(1)!=ttype."
  (cond 

   ((eql (a3el-parser-input-LA 1) ttype)
    (progn
      (a3el-parser-input-consume)
      (setf (a3el-parser-context-error-recovery context) nil)
      (setf (a3el-parser-context-failed context) nil)))

   ((> (a3el-parser-context-backtracking context) 0)
    (setf (a3el-parser-context-failed context) t))

   (t (a3el-parser-mismatch ttype follow))))


(defun a3el-parser-match-any ()
  (setf (a3el-parser-context-error-recovery context) nil)
  (setf (a3el-parser-context-failed context) nil)
  (a3el-parser-input-consume))


(defun a3el-parser-mismatch (ttype follow)
  "Factor out what to do upon token mismatch so tree parsers can behave
   differently.  Override this method in your parser to do things
   like bailing out after the first error; just throw the mte object
   instead of calling the recovery method."
  (a3el-parser-recover-from-mismatched-token 'a3el-mismatched-token ttype follow))


(defun a3el-parser-recover-from-mismatched-token (error-type ttype follow)
  ;;if next token is what we are looking for then "delete" this token
  (if (= (a3el-parser-input-LA 2) ttype)
      (progn
	(a3el-parser-report-error e)
	;;(begin-resync)
	;;simply delete extra token
	(a3el-parser-input-consume) 
	;;(end-resync)
	;;move past ttype token as if all were ok
	(a3el-parser-input-consume))
    (progn
      ;; if (!recoverFromMismatchedElement(input,e,follow) ) {
      ;;			throw e;
      ;; }
      (signal error-type nil)
      )
    ))


(defun a3el-parser-recover-from-mismatched-token-no-type (error-type follow)
  ;;TODO do single token deletion like above for Token mismatch
  ;; if ( !recoverFromMismatchedElement(input,e,follow) ) {
  ;;	throw e;
  ;; }
  (signal error-type nil)
  )


(defun a3el-parser-input-consume ()
  "Move the input pointer to the next incoming token.  The stream
   must become active with LT(1) available.  consume() simply
   moves the input pointer so that LT(1) points at the next
   input symbol. Consume at least one token.
   Walk past any token not on the channel the parser is listening to."
  (if (< (a3el-parser-context-pos context) 
	 (length (a3el-parser-context-token-buffer context)))
      (progn
	(incf (a3el-parser-context-pos context))
	;; leave p on valid token
	(setf (a3el-parser-context-pos context) 
	      (a3el-parser-skip-off-token-channels (a3el-parser-context-pos context))))))



(defun a3el-parser-consume-until-type (token-type)
  "Consume tokens until one matches the given token set"
  (let ((ttype (a3el-parser-input-LA 1)))
    (while (and (/= ttype *a3el-token-eof-token-type*) (/= ttype token-type))
      (a3el-parser-input-consume)
      (setf ttype (a3el-parser-input-LA 1))
      )
    ))


(defun a3el-parser-consume-until-in-set (set)
  "Consume tokens until one matches the given token set"
  (let ((ttype (a3el-parser-input-LA 1)))
    (while (and (/= ttype *a3el-token-eof-token-type*) (not (a3el-bitset-member set ttype)))
      (a3el-parser-input-consume)
      (setf ttype (a3el-parser-input-LA 1))
      )
    ))


(defun a3el-parser-fill-buffer ()
  "Load all tokens from the token source and put in token-buffer.
   This is done upon first LT request because you might want to
   set some token type / channel overrides before filling buffer."
  (let* ((lexer-context (a3el-parser-context-input context))
	 (index 0)
	 (tokens nil))
    (a3el-lex-with-lexer 
     lexer-context 
     #'(lambda (token) 
	 (if (and token (/= (a3el-common-token-type token) *a3el-token-eof-token-type*))
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
		     (setf (a3el-common-token-index token) index)
		     (setf tokens (cons token tokens)) ;;TODO This should be more efficient.
		     (incf index)))
	       ))))
    (setf (a3el-parser-context-token-buffer context) (vconcat (reverse tokens)))
    (setf (a3el-parser-context-pos context) 0)
    (setf (a3el-parser-context-pos context) (a3el-parser-skip-off-token-channels 0))

    ))


(defun a3el-parser-skip-off-token-channels (i)
  "Given a starting index, return the index of the first on-channel token."
  (let* ((token-buffer (a3el-parser-context-token-buffer context))
	 (channel (a3el-parser-context-channel context))
	 (n (length token-buffer)))
    (while (and (< i n) (/= (a3el-common-token-channel (aref token-buffer i)) channel))
      (incf i))
    i))


(defun a3el-parse-buffer (lname pname start-rule buffer)
  "Invoke the given lexer,parser and start-rule on the given buffer."
  (let* ((context (make-a3el-parser-context 
		   :input (a3el-lexer-for-buffer lname buffer 0 (buffer-size buffer))
		   :parser (gethash pname *a3el-runtime-parsers*))))
    (funcall (a3el-parser-entry-func (a3el-parser-context-parser context)) context start-rule)))


(defun a3el-parse-string (lname pname start-rule str) 
  "Create a temporary buffer containing only str and invoke the given lexer,parser and start-rule."
  (let* ((buffer  (a3el-buffer-from-string str)))
    (a3el-parse-buffer lname pname start-rule buffer)))


(defmacro a3el-parser-push-follow (rule-name)
  `(progn 
     (if (>= (+ (a3el-parser-context-fsp context) 1)
	     (length (a3el-parser-context-following context)))
	 (let ((f (vconcat (a3el-parser-context-following context) 
			   (make-vector (length (a3el-parser-context-following context)) nil))))
	   (setf (a3el-parser-context-following context) f)))
     (incf (a3el-parser-context-fsp context))
     (aset (a3el-parser-context-following context)
	   (a3el-parser-context-fsp context)
	   ,rule-name)
     ))

(defmacro a3el-parser-call-rule (name)
  `(progn 
     ;;(message (concat "calling rule " (format "%s" ',name))) 
     (funcall (gethash ',name (a3el-parser-rules (a3el-parser-context-parser context))) context)))

(defmacro a3el-parser-call-synpred (synpred-rule-name)
  `(progn
     (incf (a3el-parser-context-backtracking context))
     (let ((start (a3el-parser-input-mark))
	   (success nil))
       (condition-case er
	   (a3el-parser-call-rule ,synpred-rule-name) ;; can never throw exception
	 (a3el-re-error
	  (throw er "Illegal state! synpreds cannot throw exceptions.")))
       (setq success (not (a3el-parser-context-failed context)))
       (a3el-parser-input-rewind-to start)
       (decf (a3el-parser-context-backtracking context))
       (setf (a3el-parser-context-failed context) nil)
       success)))


(defun a3el-parser-report-error (re)
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
  (unless (a3el-parser-context-error-recovery context)
    (setf (a3el-parser-context-error-recovery context) t)

    (if (boundp '*a3el-swallowed-recogition-errors*)
	(push re *a3el-swallowed-recogition-errors*)
      (a3el-display-recognition-error re))))

(defun a3el-parser-recover (re)
  "Recover from an error found on the input stream.  Mostly this is
   NoViableAlt exceptions, but could be a mismatched token that
   the match() routine could not recover from."

  (if (= (a3el-parser-context-last-error-index context) 
	 (a3el-parser-context-pos context))
      ;; uh oh, another error at same token index ; must be a case
      ;; where LT(1) is in the recovery token set so nothing is
      ;; consumed	       ; consume a single token so at least to prevent
      ;; an infinite loop  ; this is a failsafe.
      (a3el-parser-input-consume)
    )

  (setf (a3el-parser-context-last-error-index context)
	(a3el-parser-context-pos context))

  (let ((follow-set (a3el-parser-compute-error-recovery-set)))
    ;;(begin-resync)
    (a3el-parser-consume-until-in-set follow-set)
    ;;(end-resync)
    ))


(defun a3el-parser-compute-error-recovery-set () 
  "Compute the error recovery set for the current rule.  During
   rule invocation, the parser pushes the set of tokens that can
   follow that rule reference on the stack; this amounts to
   computing FIRST of what follows the rule reference in the
   enclosing rule. This local follow set only includes tokens
   from within the rule; i.e., the FIRST computation done by
   ANTLR stops at the end of a rule....(see java source for examples)"
  (a3el-parser-combine-follows nil))

(defun a3el-parser-compute-context-sensitive-rule-follow ()
  "Compute the context-sensitive FOLLOW set for current rule.
   This is set of token types that can follow a specific rule
   reference given a specific call chain.  You get the set of
   viable tokens that can possibly come next (lookahead depth 1)
   given the current call chain....(see java source for examples)"
  (a3el-parser-combine-follows t))


(defun a3el-parser-combine-follows (exact)
  (let ((top (a3el-parser-context-fsp context))
	(follow-set (make-a3el-bitset)))
    (let ((i top))
      (catch 'break
	(while (>= i 0)
	  (let ((local-follow-set (aref (a3el-parser-context-following context) i)))
	    (a3el-bitset-or-in-place follow-set local-follow-set)
	    (if (and exact (not (a3el-bitset-member local-follow-set *a3el-token-eor-token-type*)))
		(throw 'break nil))
	    (decf i)
	    ))))
    (a3el-bitset-remove follow-set *a3el-token-eor-token-type*)
    follow-set
    ))


(defun a3el-display-recognition-error (re)
  (message "%s : %s" (car re) (cdr re)))




;; Utilities 


(defmacro a3el-swallowing-recognition-errors (&rest body)
  "Collect all recognition errors output withing the dynamic extent of body
   and return them in a list."
  `(let ((*a3el-swallowed-recogition-errors* '()))
     ,@body
     (reverse *a3el-swallowed-recogition-errors*)
     ))


(defun a3el-buffer-from-string (str)
  (let ((buffer (generate-new-buffer (generate-new-buffer-name "*antlr string lexing*"))))
    (save-excursion
      (with-current-buffer buffer
	(insert str)
	(goto-char (point-min))
	(set-buffer-modified-p nil)))
    buffer))


(defmacro a3el-alt-case (expr-form &rest clauses)
  "A case statement that doesn't quote the heads of its clauses."
  ;; Compare the following:
  ;; (message "%s" (macroexpand '(a3el-alt-case (+ 1 2) (5 1 3) (3 t) (2) (t nil))))
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

(defmacro a3el-lookahead-let-bindings (prefix max-k &rest body)
  "A helper macro for generating the bindings for temporary lookahead
   storage."
  ;; (macroexpand '(a3el-lookahead-let-bindings "LA0_" 10 'hello))
  (let ((bindings '())
	(num-bindings (max max-k 100)))
    (dotimes (i num-bindings)
      (let ((var-name (intern (concat prefix (number-to-string i)))))
	(setq bindings (append bindings `((,var-name nil))))))
    `(let (,@bindings)
       ,@body)))


(provide 'a3el-runtime)
;;; a3el-runtime.el ends here