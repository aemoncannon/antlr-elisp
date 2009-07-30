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


;; Error types

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

(put 'a3el-failed-predicate 'error-conditions
     '(error a3el-re-error))
(put 'a3el-failed-predicate 'error-message "Failed predicate")


(put 'a3el-tree-adaptor-error 'error-conditions
     '(error a3el-runtime-error))

(put 'a3el-rewrite-empty-stream-error 'error-conditions
     '(error a3el-runtime-error))

(put 'a3el-rewrite-cardinality-error 'error-conditions
     '(error a3el-runtime-error))

(put 'a3el-rewrite-early-exit-error 'error-conditions
     '(error a3el-runtime-error))







(defstruct a3el-common-token
  "A Common Antlr token"
  input
  type
  channel
  (index -1)
  (start -1)
  (stop -1)
  text
  line
  char-position-in-line
  name)

(defconst *a3el-token-default-channel* 0)

(defconst *a3el-token-invalid-token-type* 0)

(defconst *a3el-token-down-token-type* 2)

(defconst *a3el-token-up-token-type* 3)

(defconst *a3el-token-eof-token-type* -1)

(defconst *a3el-token-eor-token-type* 1)

(defconst *a3el-token-eof-token* (make-a3el-common-token :type *a3el-token-eof-token-type* :channel 0))

(defconst *a3el-token-invalid-token* (make-a3el-common-token :type *a3el-token-invalid-token-type* :channel 0))

(defconst *a3el-token-skip-token* (make-a3el-common-token :type *a3el-token-invalid-token-type* :channel 0))

(defun a3el-common-token-is-imaginary (tok)
  "Is tok an imaginary token? fabricated in a rewrite rule?"
  (= -1 (a3el-common-token-index tok)))


(defun a3el-common-token-get-text (token)
  "Retrieve the corresponding text from the input buffer. As opposed to 
   a3el-common-token-text, which just returns the stored text."
  (cond
   ((not (null (a3el-common-token-text token)))
    (a3el-common-token-text token))
   ((not (null (a3el-common-token-input token)))
    (let ((text (with-current-buffer (a3el-common-token-input token)
		  (buffer-substring-no-properties (a3el-common-token-start token)
						  (a3el-common-token-stop token)))))
      (setf (a3el-common-token-text token) text)
      text))
   (t nil)))

(defun a3el-common-token-get-name (token parser-name)
  "Retrieve the name of the given token. Useful for debugging."
  (if (null parser-name) nil
    (let ((parser (gethash parser-name *a3el-runtime-parsers*)))
      (nth (a3el-common-token-type token) (a3el-parser-token-names parser)))))





(defstruct a3el-rewrite-stream
  "A rewrite rule stream.

   WARNING: This is an abstract structure! 
   Make instances of a3el-rewrite-token-stream or
   a3el-rewrite-subtree-stream instead."

  ;; The list of tokens or subtrees we are tracking
  (elements '())

  ;; Cursor 0..n-1
  (cursor 0)

  ;; Once a node / subtree has been used in a stream, it must be dup'd
  ;; from then on.  Streams are reset after subrules so that the streams
  ;; can be reused in future subrules.  So, reset must set a dirty bit.
  ;; If dirty, then next() always returns a dup.
  (dirty nil)

  ;; The element or stream description; usually has name of the token or
  ;; rule reference that this list tracks.  Can include rulename too, but
  ;; the exception would track that info.
  ;;
  (element-description nil)

  ;; The tree adaptor.
  ;;
  (adaptor nil)
  )

(defstruct (a3el-rewrite-token-stream (:include a3el-rewrite-stream))
  "A rewrite rule token stream.")

(defstruct (a3el-rewrite-subtree-stream (:include a3el-rewrite-stream))
  "A rewrite rule subtree stream.")



(defun a3el-rewrite-stream-reset (s)
  "Reset the condition of this stream so that it appears we have
   not consumed any of its elements.  Elements themselves are untouched.
   Once we reset the stream, any future use will need duplicates.  Set
   the dirty bit."
  (setf (a3el-rewrite-stream-cursor s) 0)
  (setf (a3el-rewrite-stream-dirty s) t))


(defun a3el-rewrite-stream-add (s el)
  "Add an element to this stream."
  (unless (null el)
    (setf (a3el-rewrite-stream-elements s)
	  (append 
	   (a3el-rewrite-stream-elements s)
	   (list el)
	   ))))


(defun a3el-rewrite-stream-next-tree (s)
  "Return the next element in the stream.  If out of elements, throw
   an exception unless size()==1.  If size is 1, then return elements[0].
   Return a duplicate node/subtree if stream is out of elements and
   size==1.  If we've already used the element, dup (dirty bit set)."
  (if (a3el-rewrite-token-stream-p s) (a3el-rewrite-stream_next s)
    (let ((n (length (a3el-rewrite-stream-elements s)))
	  (dirty (a3el-rewrite-stream-dirty s))
	  (cursor (a3el-rewrite-stream-cursor s)))
      (if (or dirty (and (>= cursor n) (= n 1)))
	  (let ((el (a3el-rewrite-stream_next s)))
	    ;;if out of elements and size is 1, dup
	    (a3el-rewrite-stream-dup s el))
	(a3el-rewrite-stream_next s)))))


(defun a3el-rewrite-stream_next (s)
  "Do the work of getting the next element, making sure that it's
   a tree node or subtree. Throw an exception
   if the stream is empty or we're out of elements and size>1.
   protected so you can override in a subclass if necessary."
  (let ((n (length (a3el-rewrite-stream-elements s)))
	(cursor (a3el-rewrite-stream-cursor s)))
    (if (= n 0)
	(signal 'a3el-rewrite-empty-stream-error (a3el-rewrite-stream-element-description s)))
    (if (>= cursor n) ;;out of elements?
	(if (= n 1)   ;; if size is 1, it's ok; return and we'll dup
	    (progn
	      (a3el-rewrite-stream-to-tree s (nth 0 (a3el-rewrite-stream-elements s))))
	  (signal 'a3el-rewrite-cardinality-error (a3el-rewrite-stream-element-description s)))
      (progn
	(let ((o (a3el-rewrite-stream-to-tree 
		  s (nth cursor (a3el-rewrite-stream-elements s)))))
	  (incf (a3el-rewrite-stream-cursor s))
	  o
	  )))))


(defun a3el-rewrite-stream-next-node (s)
  "Treat next element as a single node even if it's a subtree.
   This is used instead of next() when the result has to be a
   tree root node.  Also prevents us from duplicating recently-added
   children; e.g., ^(type ID)+ adds ID to type and then 2nd iteration
   must dup the type node, but ID has been added.
   
   Referencing a rule result twice is ok; dup entire tree as
   we can't be adding trees as root; e.g., expr expr.
   
   Hideous code duplication here with super.next().  Can't think of
   a proper way to refactor.  This needs to always call dup node
   and super.next() doesn't know which to call: dup node or dup tree."

  (let ((adaptor (a3el-rewrite-stream-adaptor s)))
    (cond 
     ((a3el-rewrite-token-stream-p s)
      (funcall (a3el-tree-adaptor-create adaptor) 
	       (a3el-rewrite-stream_next s)))

     ((a3el-rewrite-subtree-stream-p s)
      (let ((n (length (a3el-rewrite-stream-elements s)))
	    (dirty (a3el-rewrite-stream-dirty s))
	    (cursor (a3el-rewrite-stream-cursor s)))
	(if (or dirty (and (>= cursor n) (= n 1)))
	    ;;if out of elements and size is 1, dup (at most a single node
	    ;;since this is for making root nodes).
	    (let ((el (a3el-rewrite-stream_next s)))
	      (funcall (a3el-tree-adaptor-dup-node
			adaptor) el))
	  (a3el-rewrite-stream_next s)))))))

(defun a3el-rewrite-stream-next-token (s)
  (a3el-rewrite-stream_next s))

(defun a3el-rewrite-stream-to-tree (s el)
  "Ensure stream emits trees; tokens must be converted to AST nodes.
   AST nodes can be passed through unmolested."
  el)


(defun a3el-rewrite-stream-dup (s el)
  "When constructing trees, sometimes we need to dup a token or AST
   subtree.  Dup'ing a token means just creating another AST node
   around it.  For trees, you must call the adaptor.dupTree() unless
   the element is for a tree root; then it must be a node dup."
  (cond 
   ((a3el-rewrite-token-stream-p s)
    (signal 'error "Dup can't be called for a token stream"))

   ((a3el-rewrite-subtree-stream-p s)	
    (funcall (a3el-tree-adaptor-dup-tree
	      (a3el-rewrite-stream-adaptor s)) el))))


(defun a3el-rewrite-stream-has-next (s)
  "Is there another element to be had?"
  (< (a3el-rewrite-stream-cursor s) 
     (length (a3el-rewrite-stream-elements s))
     ))




(defstruct a3el-common-tree
  "The default tree representation."
  (token nil)
  (is-nil nil)
  (children '()))


(defstruct (a3el-common-error-tree (:include a3el-common-tree))
  "Extend a3el-common-tree to store error information"
  (input nil)
  (start -1)
  (stop -1)
  (re nil))

(defun a3el-common-tree-pretty-print (tree &optional indent)
  ;; (a3el-common-tree-pretty-print (make-a3el-common-tree :token (make-a3el-common-token :type 1)))
  (let ((offset (make-string (or indent 0) ?\ ))
	(token (a3el-common-tree-token tree)))
    (message "%s('%s'" offset (if token (a3el-common-token-get-text token) "nil"))
    (dolist (ea (a3el-common-tree-children tree))
      (a3el-common-tree-pretty-print ea (+ (or indent 0) 2)))
    (message "%s)" offset)
    nil
    ))


(defun a3el-common-tree-dup-node (tree)
  "Duplicate a tree, see adaptor for further specification."
  (if tree
      (make-a3el-common-tree :is-nil (a3el-common-tree-is-nil tree)
			     :token (a3el-common-tree-token tree))))


(defun a3el-common-tree-dup-tree (tree)
  "Duplicate a tree, see adaptor for further specification."
  (let ((new-tree
	 (a3el-common-tree-dup-node tree)))
    (dolist (ea (a3el-common-tree-children tree))
      (a3el-common-tree-add-child new-tree 
				  (a3el-common-tree-dup-tree ea)))
    new-tree))

(defun a3el-common-tree-add-child (p c)
  "Add child to tree, see adaptor for further specification."
  (if (and p c)
      (if (a3el-common-tree-is-nil c)
	  (setf (a3el-common-tree-children p)
		(append (a3el-common-tree-children p)
			(a3el-common-tree-children c)))
	(setf (a3el-common-tree-children p)
	      (append (a3el-common-tree-children p)
		      (list c))))))


(defstruct a3el-tree-adaptor
  "A simple tree adaptor. Creates trees of common-trees.
   e.g. (aToken (child1 child2 child3))."

  (create
   #'(lambda (arg1 &optional arg2 arg3)

       (cond 

	;; (arg1:token)
	((a3el-common-token-p arg1)
	 (make-a3el-common-tree :token arg1))
	     
	;; (arg1:token-type, arg2:token-text)
	((and (integerp arg1) (stringp arg2))
	 (make-a3el-common-tree 
	  :token (make-a3el-common-token :type arg1 :text arg2)))

	;; (arg1:token-type, arg2:token) | 
	;; (arg1:token-type, arg2:token, arg3:token-text) | 
	((and (integerp arg1) (a3el-common-token-p arg2))
	 (progn
	   (setf (a3el-common-token-text arg2) arg3)
	   (make-a3el-common-tree :token arg2)))

	;; Otherwise, just stuff arg1 into the token slot. Useful for testing
	(t (make-a3el-common-tree :token arg1)))
       
       ))


  ;; Return a nil node (an empty but non-null node) that can hold
  ;; a list of element as the children.  If you want a flat tree (a list)
  ;; use "t=adaptor.nil(); t.addChild(x); t.addChild(y);"
  (new-nil 
   #'(lambda () 
       (make-a3el-common-tree :is-nil t)))


  ;; Create tree node that holds the start and stop tokens associated
  ;; with an error.
  (error-node 
   #'(lambda (input start stop re) 
       (make-a3el-common-error-tree :input input :start start
				    :stop stop :re re)))


  ;; Add a child to the tree t.  If child is a flat tree (a list), make all
  ;; in list children of t.  Warning: if t has no children, but child does
  ;; and child isNil then you can decide it is ok to move children to t via
  ;; t.children = child.children; i.e., without copying the array.  Just
  ;; make sure that this is consistent with have the user will build
  ;; ASTs. 
  ;; Do nothing if t or child is null.
  (add-child #'a3el-common-tree-add-child)


  ;; Return the i'th child of tree.
  (get-child 
   #'(lambda (tree i) 
       (if tree
	   (nth i (a3el-common-tree-children tree)))))


  ;; Return the count of tree's chidren.
  (get-child-count
   #'(lambda (tree) 
       (if tree
	   (length (a3el-common-tree-children tree))
	 0)))


  ;; Return tree's text. Returns the text of the common-tree's token.
  (get-text
   #'(lambda (tree)
       (if tree
	   (a3el-common-token-get-text (a3el-common-tree-token tree)))))


  ;; If oldRoot is a nil root, just copy or move the children to newRoot.
  ;; If not a nil root, make oldRoot a child of newRoot.
  ;; 
  ;;    old=^(nil a b c), new=r yields ^(r a b c)
  ;;    old=^(a b c), new=r yields ^(r ^(a b c))
  ;; 
  ;; If newRoot is a nil-rooted single child tree, use the single
  ;; child as the new root node.
  ;; 
  ;;    old=^(nil a b c), new=^(nil r) yields ^(r a b c)
  ;;    old=^(a b c), new=^(nil r) yields ^(r ^(a b c))
  ;; 
  ;; If oldRoot was null, it's ok, just return newRoot (even if isNil).
  ;; 
  ;;    old=null, new=r yields r
  ;;    old=null, new=^(nil r) yields ^(nil r)
  ;; 
  ;; Return newRoot.  Throw an exception if newRoot is not a
  ;; simple node or nil root with a single child node--it must be a root
  ;; node.  If newRoot is ^(nil x) return x as newRoot.
  ;; 
  ;; Be advised that it's ok for newRoot to point at oldRoot's
  ;; children; i.e., you don't have to copy the list.  We are
  ;; constructing these nodes so we should have this control for
  ;; efficiency.
  (become-root
   #'(lambda (new-root old-root)
       (if (null old-root) new-root
	 (let ((children-to-add 
		(if (a3el-common-tree-is-nil old-root)
		    (a3el-common-tree-children old-root)
		  (list old-root))))
	   (if (a3el-common-tree-is-nil new-root)
	       (if (/= (length (a3el-common-tree-children new-root)) 1)
		   (signal 'a3el-tree-adaptor-error 
			   "new-root is not a simple node of nil root with a single child node.")
		 (setq new-root (nth 0 (a3el-common-tree-children new-root)))))

	   (setf (a3el-common-tree-children new-root)
		 (append
		  (a3el-common-tree-children new-root)
		  children-to-add))))
       new-root
       ))


  ;; Duplicate tree recursively, using dup-node for each node
  (dup-tree #'a3el-common-tree-dup-tree)


  ;;Duplicate a single tree node
  (dup-node #'a3el-common-tree-dup-node)

        
  ;; Given the root of the subtree created for this rule, post process
  ;; it to do any simplifications or whatever you want.  A required
  ;; behavior is to convert ^(nil singleSubtree) to singleSubtree
  ;; as the setting of start/stop indexes relies on a single non-nil root
  ;; for non-flat trees.
  ;; 
  ;; Flat trees such as for lists like "idlist : ID+ ;" are left alone
  ;; unless there is only one ID.  For a list, the start/stop indexes
  ;; are set in the nil node.
  ;; 
  ;; This method is executed after all rule tree construction and right
  ;; before setTokenBoundaries().
  (rule-post-processing
   #'(lambda (root) 
       (if (and root 
		(a3el-common-tree-is-nil root)
		(= 1 (length (a3el-common-tree-children root))))
	   (nth 0 (a3el-common-tree-children root))
	 root)))
	   

  ;; Where are the bounds in the input token stream for this node and
  ;; all children?  Each rule that creates AST nodes will call this
  ;; method right before returning.  Flat trees (i.e., lists) will
  ;; still usually have a nil root node just to hold the children list.
  ;; That node would contain the start/stop indexes then.
  (set-token-boundaries
   #'(lambda (token start-token stop-token) ))
  )



(defstruct a3el-retval
  "A rule's return value."
  start
  stop
  st
  tree)



(defmacro a3el-deftoken (name value)
  `(progn
     (if (boundp 'current-lexer)
	 (puthash ',name ,value (a3el-lexer-tokens current-lexer))
       (progn
	 (puthash ',name ,value (a3el-parser-tokens current-parser))))))


(defmacro a3el-defrule (name params &rest body)
  `(puthash ',name (lambda (context ,@params)
		     (with-current-buffer current-buffer
		       ,@body))
	    (if (boundp 'current-lexer) 
		(a3el-lexer-rules current-lexer)
	      (a3el-parser-rules current-parser))))



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


(defmacro a3el-predict-DFA (name is-lexer)
  "From the input stream, predict what alternative will succeed
   using this DFA (representing the covering regular approximation
   to the underlying CFL).  Return an alternative number 1..n.  Signal
   an exception upon error."
  ;;(message "%S" (macroexpand '(a3el-predict-DFA puppy t)))
  (let* ((lookup-form (if is-lexer
			  `(gethash ',name (a3el-lexer-dfas (a3el-lexer-context-lexer context)))
			`(gethash ',name (a3el-parser-dfas (a3el-parser-context-parser context)))))
	 
	 (input-consume (if is-lexer
			    'a3el-lexer-input-consume
			  'a3el-parser-input-consume))
	 (input-mark (if is-lexer
			 'a3el-lexer-input-mark
		       'a3el-parser-input-mark))
	 (input-rewind-to (if is-lexer
			      'a3el-lexer-input-rewind-to
			    'a3el-parser-input-rewind-to))
	 (input-LA (if is-lexer
		       'a3el-lexer-input-LA
		     'a3el-parser-input-LA)))
  
    `(let* ((dfa ,lookup-form)
	    (mark (,input-mark)))
       (unwind-protect
	   (catch 'return
	     (let ((s 0))
	       (while t
		 (catch 'continue
		   (let ((special-state (aref (a3el-DFA-special dfa) s)))
		     (when (>= special-state 0)
		       (setq s (funcall (a3el-DFA-special-state-transition dfa) special-state))
		       (if  (= s -1)
			   (progn
			     (a3el-predict-DFA-no-viable-alt s (a3el-DFA-description dfa) ,is-lexer)
			     (throw 'return 0)))
		       (,input-consume)
		       (throw 'continue nil))
		     (when (>= (aref (a3el-DFA-accept dfa) s) 1)
		       (throw 'return (aref (a3el-DFA-accept dfa) s)))
		     (let ((c (,input-LA 1)))
		       (when (and (>= c (aref (a3el-DFA-min dfa) s)) (<= c (aref (a3el-DFA-max dfa) s)))
			 (let ((snext (aref (aref (a3el-DFA-transition dfa) s) (- c (aref (a3el-DFA-min dfa) s)) )))
			   (when (< snext 0)
			     (when (>= (aref (a3el-DFA-eot dfa) s) 0)
			       (setq s (aref (a3el-DFA-eot dfa) s))
			       (,input-consume)
			       (throw 'continue nil))
			     (a3el-predict-DFA-no-viable-alt s (a3el-DFA-description dfa) ,is-lexer)
			     (throw 'return 0))
			   (setq s snext)
			   (,input-consume)
			   (throw 'continue nil)))
		       (when (>= (aref (a3el-DFA-eot dfa) s) 0)
			 (setq s (aref (a3el-DFA-eot dfa) s))
			 (,input-consume)
			 (throw 'continue nil))
		       (when (and (eq c *a3el-token-eof-token*) (>= (aref (a3el-DFA-eof dfa) s) 0))
			 (throw 'return (aref (a3el-DFA-accept dfa) (aref (a3el-DFA-eof dfa) s))))
		       (a3el-predict-DFA-no-viable-alt s (a3el-DFA-description dfa) ,is-lexer)
		       (throw 'return 0)))))))
	 (,input-rewind-to mark))
       )))


(defmacro a3el-predict-DFA-no-viable-alt (s description is-lexer)
  "Helper for throwing no-viable-alt during prediction."
  (if is-lexer
      `(if (> (a3el-lexer-context-backtracking context) 0)
	   (setf (a3el-lexer-context-failed context) t)
	 (signal 'a3el-no-viable-alt ,description))

    `(if (> (a3el-parser-context-backtracking context) 0)
	 (setf (a3el-parser-context-failed context) t)
       (signal 'a3el-no-viable-alt ,description))))


(defmacro a3el-lexer-predict-DFA (name)
  `(a3el-predict-DFA ,name t))

(defmacro a3el-lexer-set-DFA (name value)
  "Install a newly instantiated DFA into the lexer (during lexer definition)."
  `(puthash ',name ,value (a3el-lexer-dfas current-lexer)))


(defmacro a3el-parser-predict-DFA (name)
  `(a3el-predict-DFA ,name nil))

(defmacro a3el-parser-set-DFA (name value)
  "Install a newly instantiated DFA into the lexer (during lexer definition)."
  `(puthash ',name ,value (a3el-parser-dfas current-parser)))


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

(defmacro a3el-lexer-context-pos () `(point))

(defmacro a3el-deflexer (name)
  `(puthash ',name 
	    (make-a3el-lexer :name ',name) 
	    *a3el-runtime-lexers*))

(defmacro a3el-lexer-input-LA (n)
  `(let ((at (+ (point) (- ,n 1))))
     (if (= at (point-max))
	 -1
       (char-after at))))
  
(defmacro a3el-lexer-input-consume ()
  `(goto-char (+ (point) 1)))

(defmacro a3el-lexer-match-any ()
  `(a3el-lexer-input-consume))


(defmacro a3el-lexer-input-mark ()
  `(progn
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
	     (a3el-lexer-context-pos))

       (setf (a3el-char-stream-state-line state)
	     (a3el-lexer-context-line context))

       (setf (a3el-char-stream-state-char-position-in-line state) 
	     (a3el-lexer-context-char-position-in-line context))

       (setf (a3el-lexer-context-last-marker context) 
	     (a3el-lexer-context-mark-depth context))

       (a3el-lexer-context-mark-depth context))))



(defmacro a3el-lexer-input-rewind-to (m)
  `(let ((state (nth ,m (a3el-lexer-context-markers context))))

     ;; restore stream state

     (a3el-lexer-input-seek (a3el-char-stream-state-pos state))
     (setf (a3el-lexer-context-line context)
	   (a3el-char-stream-state-line state))

     (setf (a3el-lexer-context-char-position-in-line context)
	   (a3el-char-stream-state-char-position-in-line state))

     (a3el-lexer-input-release ,m)))


(defmacro a3el-lexer-input-rewind ()
  `(a3el-lexer-input-rewind-to (a3el-lexer-context-last-marker context)))


(defmacro a3el-lexer-input-release (m)
  `(progn
     ;;unwind any other markers made after m and release m
     (setf (a3el-lexer-context-mark-depth context) ,m)
     ;;release this marker
     (decf (a3el-lexer-context-mark-depth context))
     ))


(defmacro a3el-lexer-input-seek (index)
  "consume() ahead until p==index; can't just set p=index as we must
   update line and charPositionInLine."
  `(if (<= ,index (point))
       (goto-char ,index) ;; just jump; don't update stream state (line, ...)
     ;; seek forward, consume until p hits index
     (while (< (point) ,index) 
       (a3el-lexer-input-consume))))

(defmacro a3el-lexer-set-type (type)
  `(setf (a3el-lexer-context-type context) ,type))

(defmacro a3el-lexer-set-channel (c)
  `(setf (a3el-lexer-context-channel context) ,c))

(defmacro a3el-lexer-call-rule (name)
  `(funcall (gethash ',name (a3el-lexer-rules (a3el-lexer-context-lexer context))) context))

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


(defmacro a3el-lex-emit-token ()
  `(let ((token (make-a3el-common-token
		 :input (current-buffer)
		 :type (a3el-lexer-context-type context)
		 :channel (a3el-lexer-context-channel context)
		 :start (a3el-lexer-context-token-start-char-index context)
		 :stop (point)
		 :line (a3el-lexer-context-token-start-line context)
		 :text (a3el-lexer-context-text context)
		 :char-position-in-line (a3el-lexer-context-token-start-char-position-in-line context))))
     (setf (a3el-lexer-context-token context) token)
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

(defmacro a3el-lexer-each-buffer-token (token-sym body)
  "Execute body with token-sym bound to each token in the buffer in sequence.
   We assume context is bound to an active lexer context."
  `(let ((lexer-entry-func (a3el-lexer-entry-func (a3el-lexer-context-lexer context))))
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
	     (when (= (point) (point-max))(throw 'at-end nil))
	     (condition-case re
		 (progn
		   (funcall lexer-entry-func context 'Tokens)
		   (when (null (a3el-lexer-context-token context))
		     (a3el-lex-emit-token))
		   (unless (eq (a3el-lexer-context-token context) *a3el-token-skip-token*)
		     (let ((,token-sym (a3el-lexer-context-token context)))
		       ,body
		       )
		     ))
	       (a3el-re-error
		(a3el-lexer-report-error re)
		(a3el-lexer-recover re)
		)))
	   )))))


(defmacro a3el-lexer-call-synpred (synpred-rule-name)
  `(progn
     (incf (a3el-lexer-context-backtracking context))
     (let ((start (a3el-lexer-input-mark))
	   (success nil))
       (condition-case er
	   (a3el-lexer-call-rule ,synpred-rule-name) ;; can never throw exception
	 (a3el-re-error
	  (signal er "Illegal state! synpreds cannot throw exceptions.")))
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
  (dfas (make-hash-table))
  (bitsets (make-hash-table))
  (entry-func nil)
  (tree-adaptor (make-a3el-tree-adaptor))
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

(defmacro a3el-parser-input-mark ()
  `(progn
     (if (= (a3el-parser-context-pos context) -1) (a3el-parser-fill-buffer))
     (setf (a3el-parser-context-last-marker context) (a3el-parser-context-pos context))
     (a3el-parser-context-last-marker context)))

(defmacro a3el-parser-input-rewind-to (p)
  `(a3el-parser-input-seek ,p))

(defmacro a3el-parser-input-rewind ()
  `(a3el-parser-input-seek (a3el-parser-context-last-marker context)))

(defmacro a3el-parser-input-seek (p)
  `(setf (a3el-parser-context-pos context) ,p))


(defmacro a3el-parser-input-LT (k)
  "Get the ith token from the current position 1..n where k=1 is the first symbol of lookahead."
  `(catch 'return
     (if (= (a3el-parser-context-pos context) -1) (a3el-parser-fill-buffer))
     (if (= ,k 0) (throw 'return nil))
     (if (< ,k 0) (throw 'return (a3el-parser-input-LB (- ,k))))
     (if (>= (+ (a3el-parser-context-pos context) ,k -1)
	     (length (a3el-parser-context-token-buffer context)))
	 (throw 'return *a3el-token-eof-token*))
     (let ((i (a3el-parser-context-pos context))
	   (n 1))
       (while (< n ,k)
	 ;;skip off-channel tokens
	 (setq i (a3el-parser-skip-off-token-channels (+ i 1)))
	 (setq n (+ n 1)))
       (let ((token-buffer (a3el-parser-context-token-buffer context)))
	 (if (>= i (length token-buffer))
	     (throw 'return *a3el-token-eof-token*))
	 (aref token-buffer i))
       )))

(defmacro a3el-parser-input-LA (k)
  `(a3el-common-token-type (a3el-parser-input-LT ,k)))


(defmacro a3el-parser-input-LB (k)
  "Look backwards k tokens on-channel tokens"
  `(catch 'return
     (if (= (a3el-parser-context-pos context) -1) (a3el-parser-fill-buffer))
     (if (= ,k 0) (throw 'return nil))
     (if (< (- (a3el-parser-context-pos context) ,k) 0) (throw 'return nil))
     (let ((i (a3el-parser-context-pos context))
	   (n 1))
       ;;find k good tokens looking backwards
       (while (<= n ,k)
	 ;;skip off-channel tokens
	 (setq i (a3el-parser-skip-off-token-channels-reverse (- i 1)))
	 (incf n))
       (if (< i 0) (throw 'return nil))
       (aref (a3el-parser-context-token-buffer context) i))))


(defmacro a3el-parser-input-consume ()
  "Move the input pointer to the next incoming token.  The stream
   must become active with LT(1) available.  consume() simply
   moves the input pointer so that LT(1) points at the next
   input symbol. Consume at least one token.
   Walk past any token not on the channel the parser is listening to."
  `(if (< (a3el-parser-context-pos context)
	  (length (a3el-parser-context-token-buffer context)))
       (progn
	 (incf (a3el-parser-context-pos context))
	 ;; leave p on valid token
	 (setf (a3el-parser-context-pos context)
	       (a3el-parser-skip-off-token-channels (a3el-parser-context-pos context))))))



(defmacro a3el-parser-consume-until-type (token-type)
  "Consume tokens until one matches the given token set"
  `(let ((ttype (a3el-parser-input-LA 1)))
     (while (and (/= ttype *a3el-token-eof-token-type*) (/= ttype ,token-type))
       (a3el-parser-input-consume)
       (setf ttype (a3el-parser-input-LA 1))
       )
     ))


(defun a3el-parser-match (ttype follow)
  "Match current input symbol against ttype.  Upon error, do one token
  insertion or deletion if possible.  You can override to not recover
  here and bail out of the current production to the normal error
  exception catch (at the end of the method) by just throwing
  MismatchedTokenException upon input.LA(1)!=ttype."
  (let ((LT_1 (a3el-parser-input-LT 1)))
    (cond 
     ((= (a3el-common-token-type LT_1) ttype)
      (progn
	(a3el-parser-input-consume)
	(setf (a3el-parser-context-error-recovery context) nil)
	(setf (a3el-parser-context-failed context) nil)
	LT_1
	))

     ((> (a3el-parser-context-backtracking context) 0)
      (progn
	(setf (a3el-parser-context-failed context) t)
	LT_1
	))

     (t (a3el-parser-mismatch ttype follow)))
    ))


`(defmacro a3el-parser-match-any ()
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
  (let ((LT_2 (a3el-parser-input-LT 2)))
    (if (= (a3el-common-token-type LT_2) ttype)
	(progn
	  (a3el-parser-report-error e)
	  ;;(begin-resync)
	  ;;simply delete extra token
	  (a3el-parser-input-consume) 
	  ;;(end-resync)
	  ;;move past ttype token as if all were ok
	  (a3el-parser-input-consume)
	  LT_2)
      (progn
	;; if (!recoverFromMismatchedElement(input,e,follow) ) {
	;;			throw e;
	;; }
	(signal error-type nil)
	)
      )))


(defun a3el-parser-recover-from-mismatched-token-no-type (error-type follow)
  ;;TODO do single token deletion like above for Token mismatch
  ;; if ( !recoverFromMismatchedElement(input,e,follow) ) {
  ;;	throw e;
  ;; }
  (signal error-type nil)
  )



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
  (let* ((index 0)
	 (tokens nil))

    ;; Setup the lexer's dynamic environment
    (let* ((context (a3el-parser-context-input context)))
		      
      (a3el-lexer-each-buffer-token 
       token
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
		   (setq tokens (cons token tokens)) ;;TODO This should be more efficient.
		   (incf index)))))))

    (setf (a3el-parser-context-token-buffer context) (vconcat (nreverse tokens)))
    (setf (a3el-parser-context-pos context) 0)
    (setf (a3el-parser-context-pos context) (a3el-parser-skip-off-token-channels 0))
    ))


(defun a3el-parser-skip-off-token-channels (i)
  "Given a starting index, return the index of the first on-channel token."
  (let* ((token-buffer (a3el-parser-context-token-buffer context))
	 (channel (a3el-parser-context-channel context))
	 (n (length token-buffer)))
    (while (and (< i n) (/= (a3el-common-token-channel (aref token-buffer i)) channel))
      (setq i (+ i 1)))
    i))


(defun a3el-parser-skip-off-token-channels-reverse (i)
  "Given a starting index, return the index of the first on-channel token."
  (let* ((token-buffer (a3el-parser-context-token-buffer context))
	 (channel (a3el-parser-context-channel context)))
    (while (and (>= i 0) (/= (a3el-common-token-channel (aref token-buffer i)) channel))
      (decf i))
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
  `(funcall (gethash ',name (a3el-parser-rules (a3el-parser-context-parser context))) context))

(defmacro a3el-parser-call-synpred (synpred-rule-name)
  `(progn
     (incf (a3el-parser-context-backtracking context))
     (let ((start (a3el-parser-input-mark))
	   (success nil))
       (condition-case er
	   (a3el-parser-call-rule ,synpred-rule-name) ;; can never throw exception
	 (a3el-re-error
	  (signal er "Illegal state! synpreds cannot throw exceptions.")))
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
	(num-bindings (max max-k 10)))
    (dotimes (i num-bindings)
      (let ((var-name (intern (concat prefix (number-to-string i)))))
	(setq bindings (append bindings `((,var-name nil))))))
    `(let (,@bindings)
       ,@body)))


(provide 'a3el-runtime)
;;; a3el-runtime.el ends here
