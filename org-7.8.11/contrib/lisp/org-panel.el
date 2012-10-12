;;; semantic-wisent.el --- Wisent - Semantic gateway

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 30 Aug 2001
;; Keywords: syntax
;; X-RCS: $Id: semantic-wisent.el,v 1.4 2007/01/23 15:41:09 ponced Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Here are functions necessary to use the Wisent LALR parser from
;; Semantic environment.

;;; History:
;;

;;; Code:

(require 'semantic)
(require 'wisent)

;;; Lexical analysis
;;
(defvar wisent-lex-istream nil
  "Input stream of `semantic-lex' syntactic tokens.")

(defvar wisent-lex-lookahead nil
  "Extra lookahead token.
When non-nil it is directly returned by `wisent-lex-function'.")

;; Maintain this alias for compatibility until all WY grammars have
;; been translated again to Elisp code.
(semantic-alias-obsolete 'wisent-lex-make-token-table
                         'semantic-lex-make-type-table)

(defmacro wisent-lex-eoi ()
  "Return an End-Of-Input lexical token.
The EOI token is like this: ($EOI "" POINT-MAX . POINT-MAX)."
  `(cons ',wisent-eoi-term
         (cons ""
               (cons (point-max) (point-max)))))

(defmacro define-wisent-lexer (name doc &rest body)
  "Create a new lexical analyzer with NAME.
DOC is a documentation string describing this analyzer.
When a token is available in `wisent-lex-istream', eval BODY forms
sequentially.  BODY must return a lexical token for the LALR parser.

Each token in input was produced by `semantic-lex', it is a list:

  (TOKSYM START . END)

TOKSYM is a terminal symbol used in the grammar.
START and END mark boundary in the current buffer of that token's
value.

Returned tokens must have the form:

  (TOKSYM VALUE START . END)

where VALUE is the buffer substring between START and END positions."
  `(defun
     ,name () ,doc
     (cond
      (wisent-lex-lookahead
       (prog1 wisent-lex-lookahead
         (setq wisent-lex-lookahead nil)))
      (wisent-lex-istream
       ,@body)
      ((wisent-lex-eoi)))))

(define-wisent-lexer wisent-lex
  "Return the next available lexical token in Wisent's form.
The variable `wisent-lex-istream' contains the list of lexical tokens
produced by `semantic-lex'.  Pop the next token available and convert
it to a form suitable for the Wisent's parser."
  (let* ((tk (car wisent-lex-istream)))
    ;; Eat input stream
    (setq wisent-lex-istream (cdr wisent-lex-istream))
    (cons (semantic-lex-token-class tk)
          (cons (semantic-lex-token-text tk)
                (semantic-lex-token-bounds tk)))))

;;; Syntax analysis
;;
(defvar wisent-error-function nil
  "Function used to report parse error.
By default use the function `wisent-message'.")
(make-variable-buffer-local 'wisent-error-function)

(defvar wisent-lexer-function 'wisent-lex
  "Function used to obtain the next lexical token in input.
Should be a lexical analyzer created with `define-wisent-lexer'.")
(make-variable-buffer-local 'wisent-lexer-function)

;; Tag production
;;
(defsubst wisent-raw-tag (semantic-tag)
  "Return raw form of given Semantic tag SEMANTIC-TAG.
Should be used in semantic actions, in grammars, to build a Semantic
parse tree."
  (nconc semantic-tag
         (if (or $region
                 (setq $region (nthcdr 2 wisent-input)))
             (list (car $region) (cdr $region))
           (list (point-max) (point-max)))))

(defsubst wisent-cook-tag (raw-tag)
  "From raw form of Semantic tag RAW-TAG, return a list of cooked tags.
Should be used in semantic actions, in grammars, to build a Semantic
parse tree."
  (let* ((cooked (semantic--tag-expand raw-tag))
         (l cooked))
    (while l
      (semantic--tag-put-property (car l) 'reparse-symbol $nterm)
      (setq l (cdr l)))
    cooked))

;; Unmatched syntax collector
;;
(defun wisent-collect-unmatched-syntax (nomatch)
  "Add lexical token NOMATCH to the cache of unmatched tokens.
See also the variable `semantic-unmatched-syntax-cache'.

NOMATCH is in Wisent's form: (SYMBOL VALUE START . END)
and will be collected in `semantic-lex' form: (SYMBOL START . END)."
  (let ((region (cddr nomatch)))
    (and (number-or-marker-p (car region))
         (number-or-marker-p (cdr region))
         (setq semantic-unmatched-syntax-cache
               (cons (cons (car nomatch) region)
                     semantic-unmatched-syntax-cache)))))

;; Parser plug-ins
;;
;; The following functions permit to plug the Wisent LALR parser in
;; Semantic toolkit.  They use the standard API provided by Semantic
;; to plug parsers in.
;;
;; Two plug-ins are available, BUT ONLY ONE MUST BE USED AT A TIME:
;;
;; - `wisent-parse-stream' designed to override the standard function
;;   `semantic-parse-stream'.
;;
;; - `wisent-parse-region' designed to override the standard function
;;   `semantic-parse-region'.
;;
;; Maybe the latter is faster because it eliminates a lot of function
;; call.
;;
(defun wisent-parse-stream (stream goal)
  "Parse STREAM using the Wisent LALR parser.
GOAL is a nonterminal symbol to start parsing at.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tags found.
The LALR parser automaton must be available in buffer local variable
`semantic--parse-table'.

Must be installed by `semantic-install-function-overrides' to override
the standard function `semantic-parse-stream'."
  (let (wisent-lex-istream wisent-lex-lookahead la-elt cache)

    ;; IMPLEMENTATION NOTES:
    ;; `wisent-parse' returns a lookahead token when it stopped
    ;; parsing before encountering the end of input.  To re-enter the
    ;; parser it is necessary to push back in the lexical input stream
    ;; the last lookahead token issued.  Because the format of
    ;; lookahead tokens and tokens in STREAM can be different the
    ;; lookahead token is put in the variable `wisent-lex-lookahead'
    ;; before calling `wisent-parse'.  Wisent's lexers always pop the
    ;; next lexical token from that variable when non nil, then from
    ;; the lexical input stream.
    ;;
    ;; The first element of STREAM is used to keep lookahead tokens
    ;; across successive calls to `wisent-parse-stream'.  In fact
    ;; what is kept is a stack of lookaheads encountered so far.  It
    ;; is cleared when `wisent-parse' returns a valid semantic tag,
    ;; or twice the same lookahead token!  The latter indicates that
    ;; there is a syntax error on that token.  If so, tokens currently
    ;; in the lookahead stack have not been used, and are moved into
    ;; `semantic-unmatched-syntax-cache'.  When the parser will be
    ;; re-entered, a new lexical token will be read from STREAM.
    ;;
    ;; The first element of STREAM that contains the lookahead stack
    ;; has this format (compatible with the format of `semantic-lex'
    ;; tokens):
    ;;
    ;; (LOOKAHEAD-STACK START . END)
    ;;
    ;; where LOOKAHEAD-STACK is a list of lookahead tokens.  And
    ;; START/END are the bounds of the lookahead at top of stack.

    ;; Retrieve lookahead token from stack
    (setq la-elt (car stream))
    (if (consp (car la-elt))
        ;; The first elt of STREAM contains a lookahead stack
        (setq wisent-lex-lookahead (caar la-elt)
              stream (cdr stream))
      (setq la-elt nil))
    ;; Parse
    (setq wisent-lex-istream stream
          cache (semantic-safe "wisent-parse-stream: %s"
                  (condition-case error-to-filter
                      (wisent-parse semantic--parse-table
                                    wisent-lexer-function
                                    wisent-error-function
                                    goal)
                    (args-out-of-range
                     (if (and (not debug-on-error)
                              (= wisent-parse-max-stack-size
                                 (nth 2 error-to-filter)))
                         (progn
                           (message "wisent-parse-stream: %s"
                                    (error-message-string error-to-filter))
                           (message "wisent-parse-max-stack-size \
might need to be increased"))
                       (apply 'signal error-to-filter))))))
    ;; Manage returned lookahead token
    (if wisent-lookahead
        (if (eq (caar la-elt) wisent-lookahead)
            ;; It is already at top of lookahead stack
            (progn
              (setq cache nil
                    la-elt (car la-elt))
              (while la-elt
                ;; Collect unmatched tokens from the stack
                (run-hook-with-args
                 'wisent-discarding-token-functions (car la-elt))
                (setq la-elt (cdr la-elt))))
          ;; New lookahead token
          (if (or (consp cache) ;; Clear the stack if parse succeeded
                  (null la-elt))
              (setq la-elt (cons nil nil)))
          ;; Push it into the stack
          (setcar la-elt (cons wisent-lookahead (car la-elt)))
          ;; Update START/END
          (setcdr la-elt (cddr wisent-lookahead))
          ;; Push (LOOKAHEAD-STACK START . END) in STREAM
          (setq wisent-lex-istream (cons la-elt wisent-lex-istream))))
    ;; Return (STREAM SEMANTIC-STREAM)
    (list wisent-lex-istream
          (if (consp cache) cache '(nil))
          )))

(defun wisent-parse-region (start end &optional goal depth returnonerror)
  "Parse the area between START and END using the Wisent LALR parser.
Return the list of semantic tags found.
Optional arguments GOAL is a nonterminal symbol to start parsing at,
DEPTH is the lexical depth to scan, and RETURNONERROR is a flag to
stop parsing on syntax error, when non-nil.
The LALR parser automaton must be available in buffer local variable
`semantic--parse-table'.

Must be installed by `semantic-install-function-overrides' to override
the standard function `semantic-parse-region'."
  (if (or (< start (point-min)) (> end (point-max)) (< end start))
      (error "Invalid bounds [%s %s] passed to `wisent-parse-region'"
             start end))
  (let* ((case-fold-search semantic-case-fold)
         (wisent-lex-istream (semantic-lex start end depth))
         ptree tag cooked lstack wisent-lex-lookahead)
    ;; Loop while there are lexical tokens available
    (while wisent-lex-istream
      ;; Parse
      (setq wisent-lex-lookahead (car lstack)
            tag (semantic-safe "wisent-parse-region: %s"
                    (wisent-parse semantic--parse-table
                                  wisent-lexer-function
                                  wisent-error-function
                                  goal)))
      ;; Manage returned lookahead token
      (if wisent-lookahead
          (if (eq (car lstack) wisent-lookahead)
              ;; It is already at top of lookahead stack
              (progn
                (setq tag nil)
                (while lstack
                  ;; Collect unmatched tokens from lookahead stack
                  (run-hook-with-args
                   'wisent-discarding-token-functions (car lstack))
                  (setq lstack (cdr lstack))))
            ;; Push new lookahead token into the stack
            (setq lstack (cons wisent-lookahead lstack))))
      ;; Manage the parser result
      (cond
       ;; Parse succeeded, cook result
       ((consp tag)
        (setq lstack nil ;; Clear the lookahead stack
              cooked (semantic--tag-expand tag)
              ptree (append cooked ptree))
        (while cooked
          (setq tag    (car cooked)
                cooked (cdr cooked))
          (or (semantic--tag-get-property tag 'reparse-symbol)
              (semantic--tag-put-property tag 'reparse-symbol goal)))
        )
       ;; Return on error if requested
       (returnonerror
        (setq wisent-lex-istream nil)
        ))
      ;; Work in progress...
      (if wisent-lex-istream
	  (if (eq semantic-working-type 'percent)
	      (working-status
               (/ (* 100 (semantic-lex-token-start
                          (car wisent-lex-istream)))
                  (point-max)))
	    (working-dynamic-status))))
    ;; Return parse tree
    (nreverse ptree)))

;;; Interfacing with edebug
;;
(add-hook
 'edebug-setup-hook
 #'(lambda ()

     (def-edebug-spec define-wisent-lexer
       (&define name stringp def-body)
       )

     ))

(provide 'semantic-wisent)

;;; semantic-wisent.el ends here
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ;;; wisent-calc.el --- Infix notation calculator

;; Copyright (C) 2001, 2002, 2003, 2004, 2009 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 11 Sep 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-calc.el,v 1.20 2009/12/26 21:38:46 zappo Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This is a port of the Bison 1.28d Infix Calc sample program to the
;; elisp LALR parser Wisent.  It illustrates usage of operator
;; precedence and contextual precedence.  The grammar is generated
;; from the WY file wisent-calc.wy.
;;
;; To run the calculator use M-x wisent-calc and at "calc:" prompt
;; enter expressions separated by semicolons.  Here is a sample run of
;; `wisent-calc':
;;
;;   calc: 4 + 4.5 - (34.0/(8*3+-3));
;;   -> 6.880952380952381;
;;   calc: -56 + 2;
;;   -> -54;
;;   calc: 3 ^ 2;
;;   -> 9;
;;   calc: 2*2*2 = 2^3;
;;   -> t;
;;   calc: 2*2*2; 2^3;
;;   -> 8; 8;

;;; History:
;; 

;;; Code:
(require 'semantic-wisent)
(require 'wisent-calc-wy)

(define-lex-simple-regex-analyzer wisent-calc-lex-number
  "Detect and create number tokens."
  semantic-lex-number-expression 'NUM)

(define-lex-simple-regex-analyzer wisent-calc-lex-punctuation
  "Detect and create punctuation tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)" (char-after))

(define-lex wisent-calc-lexer
  "Calc lexical analyzer."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  wisent-calc-lex-number
  wisent-calc-lex-punctuation
  semantic-lex-default-action)

;;;###autoload
(defun wisent-calc (input)
  "Infix desktop calculator.
Parse INPUT string and output the result of computation."
  (interactive "scalc: ")
  (or (string-match ";\\s-*$" input)
      (setq input (concat input ";")))
  (with-temp-buffer
    (wisent-calc-setup-parser)
    (semantic-lex-init)
    (insert input)
    (let* ((wisent-lex-istream (semantic-lex-buffer))
	   (answer (wisent-parse semantic--parse-table 'wisent-lex)))
      (if (interactive-p)
	  (message "%s -> %s" input answer))
      answer)))

;; Misc handy fcns
(defun wisent-calc-factorial (integer)
  "Compute factorial of an INTEGER.
Borrowed from the Emacs manual."
  (if (= 1 integer) 1
    (* integer (wisent-calc-factorial (1- integer)))))

(defun wisent-calc-not (num)
  "Compute a NOT operation of an NUMber.
If NUM is 0, return 1. If NUM is not 0, return 0."
  (if (= 0 num) 1 0))

(defun wisent-calc-= (num1 num2)
  "Compute a if NUM1 equal NUM2.
Return 1 if equal, 0 if not equal."
  (if (= num1 num2) 1 0))

;;; TEST SUITE:
(defvar wisent-calc-test-expressions
  '(
    ;; Basic
    ("1+1" . 2) ("2*2" . 4) ("12/3" . 4) ("3-2" . 1)
    ("2^3" . 8) ("!4" . 24) ("2=3" . 0)  ("2=2" . 1)
    ("~0" . 1)  ("~1" . 0)
    ;; Precidence
    ("1+2*3" . 7) ("1+2-1" . 2) ("6/2+1" . 4) ("2^2+1" . 5)
    ("-3+2" . -1) ("-3*2" . -6) ("!3*2" . 12) ("2+2=4" . 1)
    ("~2=0" . 1)
    ;; grouping
    ("(2+3)*2" . 10) ("2*(4-3)" . 2) ("1+2^(2+1)" . 9) ("~(2=0)" . 1)
    ;; Misc goofy
    ("1+2*4-7/4^3" . 9)
    )
  "List of expressions and answers to test all the features.")

;;;###autoload
(defun wisent-calc-utest ()
  "Test the wisent calculator."
  (interactive)
  (dolist (X wisent-calc-test-expressions)
    (let* ((exp (car X))
	   (ans (cdr X))
	   (act  (string-to-number (wisent-calc exp))))
      (when (not (eq act ans))
	(error "Failed: %S == %d, but should be %d"
	       exp act ans))))
  )

(provide 'wisent-calc)

;;; wisent-calc.el ends here
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 GIF89a0��  ���33=,    0� ���������ڋ�޼���H�扦�ʶ���L��������
�Ģ�L*�̦�	�J�Ԫ���j�ܮ��O�Cy�N��I��͎��t����W����Ч0X��q��8��PH�!�Xi�u�@��i�y):j��*x���ں
�����
yᘫQk��)|;Hk�wjK�<S�k�=,���IX��
\(	��K�GMX����}��:]���L��N�������_@}���F�U�	��M���9AM]÷̠@���:��g��@�� L�xr�6T	S��B��@�4%����/�\3�ǡ����Je-j6�]����:�ʌ5��խ�=
O�5�0�}����'�%��q�ɷn_:��L��,GR���"\�oInfةK��΢��62c��	F*V�Ͻ{�½�`Y�~Ӛ�D�k\��?[v�:�A��pJ����N���%��[�lѳc{9�ެ^�������āC�Y�9���R�-�!sp˩_y����e��e^x��y+�O�U�����ߏ�.͑#Ts�w_p�sڀia��`	�w����_������ �8W���eЍ�,���S�X"w0b�(��HÌ8ަ�~pԎ�(d�=jv�����Ljx�*>Ie�V^�e�Zn�e�^~	��If�f��f�j��f�g�	gI�8g�v��f�z��g�~�yg��Jh���h��.�h��>
i��NJi��^�i��n�i��~
j���Jj����j����j���
k���Jk��ފk�u��+SVRZQ�ज��Z�¯��v�j=�7b�Ȣ��Z�`RU�Q�&�6�b�Nk�Lߊ0�Aj���-u�5u���X䊙���n6Վ�n<�;���~�.��F�V�e֐܅ ~D���x���.�nh݉���P"����T��es���h�|�t$/�1|�y�R���M�p�t(%�Q��
|U�	���)+Xs�9c�S�=7h0�iW�깉�b�IXu��l2�c�]��A^|��sӇ`��,h�-gC�t���s�V�-p�G�|TEw������ie)�wx��e9�[�}s;��d�Ȟ�e���	>��ucMV}-�t2͏���o�����8��`{��5D��"������w�o�����ˆ����-�����}Ȫ��}ɿUgef��{��l��4���
�w��uҁG�4M=N�q2����|���^2tj{`�Ć�)}�n�+N�t�_��i�~z�F�^|�����po�C��vU���F9 CM�"]�g��ᆰ1R*#B'�D&QOܗ�¢��&"�@�"�.0��	U�ψ�4��^!b�OE?+��"g�с�n�#�������s �h��2`"sX`TFDeq����&���D8[��$)��h��Xd��v�Jj2Ev|���R<
��!O�X&A����<�)-�376������mI���1v�J\^�p<#e�Y5���kc�Ҕ�fŮ;U�V�UNݘp��D��M�A�%�e3'5a�R6�c&=�!�(�s��J;'y:��M!���w��8��0���	XZ�N�M��Xg��΃N&��g'q'��2�}\�8y�b�*%�=�Q�n|"Eh5Q���\�'��HA7�~�tB�[�G)�>�Үx�\jOG�Ov�rR%*1�)�M.��Ɖ�er�},%$��w�^uH� *7�I�̡j�۔�>�uH�+I��R����^�k�1ÎG�Y�Ϛ�Ȧ2���ZsZQ�9�ul�J��H�P��5jM������Qlg���Ff����n�b�����-�q�����*w��m�s���Jw�ԭ�u����jw���w����w��-�yϋ���w��m�{����w��e�Mш�J��N�-��_�V��!�'�%���bd`n���p)@���v�����w����r���Ίnng�fB:o���,k�Jǲ�.�1�A�#��k`��`���d�����&c֮J�ꈹ?fZs��ѦY��U�q�^&��WZOW����[R��T%>��`V��?Ƶf�e�ۙ�	�g����;�W׉`����(SݳESU�4M����R5�3K��2�9�xZu�-IA��64y)m�gʲ�;jU�<>���uVgiM;�l2Sz�����tVW'N�}�&Q��P��f'uѢyEy��t�'T�b!��ʖ�����\��6����rx.p[���������Bu�$�/��, \x�^�.�6���sp��ʑ����q<y��
9�O��|�,o��_��|�4���o�����|�<����}�D/�я���+}�Lo���4r+E=�Qd�!��B�6�^X����GA��6�m#��f�_k�x�$���u]j񣩾l;�i_�ۢg��ျZ��'h�>�R�]�rb�L"�����7%�6'��nw`��I^2�/=��ʦݮ�;*�fg�e��Zآ�]������b�ׇ������}�IY����D�+WS��K󱇙�������,�����M��R�����|b\_�����9xK`��9���]�4�J'��ޠ7��~�EF!w�oy�J�dSjVgٶ1x�..�����M��n�~,�g�q�7��w�>X3��f�#Qvf��o��9Xpu��Âֳg��gv�g��F���V=�UTwY�P�hrcb_Cj�:��U�k�#U����h�d�&�o,U��YO惞tj��j��4ȦSFu_K�VIƄ������ixTxy (v�S|5m0�!X����7�nUP�Ko�|d8U�f~ك��gIއ2lU�K���S8�qfZcx�Shh�SK2�����|8H>E
��������v?�k�!?�(K���@d	t?8lcq��6��sAOX>b%5If���Lauu��[��{%�72��W_�|�ay��`cAn2ji�e	����z��}�sN�(D�M�,�8
��W��Sw~M|�ep/S��I�$B0�u['{ف@�؉o�=:�nb���)d�3y޳B)9M%ɒ
�wE�y�r+��eh8ْ*w�-��t���Փ�2���5�O��I��KɔM�O	�Q)�SI�Ui�W��Y��[ɕ]�_	�a)�cI�ei�g��i��k	y2�`��&	$o��i�wהr�bA)b=4R|Y_sre4"�Hm����"��g��oX�6o�;HV\H�C�hE�.�P��/�	+��I���Jt�.�=�tZ$���_�|9��؇<(X����a3b�A��Y\�=����z��*�X�hx��<��K�8Y�䜙����|2�}w9��g�Ռ�ւ	�O�i}�	 驞��Ũ���IM�	 �9L��Q��Y�)�p��"E�f���h8��2೟a�7�)-�Y�;�x� �Ġʠ�F:M�V�ٞtT���ڡMؠ}�d�7T�R����T�R JP�i6��v�봢z����=
�2�;>:|C�y���w�Ep{�
�LXP.jj1�=���)Z�MJ��ȥ�H�#���|����d�L��N�>@�VoU��8T_J�o���ӣ���k*�IX����l:�T�kr�}�ǉ����	�I�p���&@�馋zn�j���t�DZ��(�9�T������J�޸���A.T��ڥnzP�ʑ��0�s��P���I��fU�j��71�C��W�_��Ex�E��O���Ė��ɲ�׊-��"�ʭ��}��$��	�媮�ʮ���
��*��J��j��������ʯ���
�+�K�k���	˘����v��Ұ��6`Fɪ{(����^<ԩ~�}K,o'��u<�*�����`~�1!�)kE+��H�9u�GE�s������:+�Ӳ[��F�1�Ț�Zn�z�u�m�4+��I�R�!Y�(뭇����I�����'Y���T��w|�	�h�@g�p�<��oL�ga[~�I�q8$��{+�I��w9)��ᴶ0�N�U�7e�*���xW�mKf��eHh����;���Tk>�iR��UJOt�Z�=fJ��+�|���8g�[&�:����~c)�ۤVhg�[��۷�˚O�����A����H�y
���!�K�f��%�������
���Ž(�����W�h{�j��j��Jw��GC���k�����ہ$T�2��������������ȁ���N�G��|��h���U�[)ƛ���<���a�,�	}'�-'<@�����V��<e��������u���h��;ý�A<��K��g�놐�ɸ%+�:�;��Uڪj*�e�)O6���ٴ�`l1�ڧ�Z�T��s˽�Xr(���m̷e\�?�b3�6\�F|�K�NF"�vJȃK��ˡ�ܚ��`�����[u���!<�
�ɟʡ,ʣLʥlʧ�ʩ�ʫ�ʭ�ʯ˱�tW�ܙ����D.bL�T�?����!�v��˧�������\�˼*\��0K#��y�t5�L�a� ��l����7l�1)�$k�zW��l(˜�I<z�*��<}ؚb@����v�
dz|���/۸.�����̰�8B��V���<�fk�i(�g��i����M�;��8Ɔ�KtL��{�J��f��l������8��(�@��%]���T��x�3l�<-Ȕ7�;f�ي�J����+��ÿ&Ȼ幩���;`#��N�����F�«�1�d��]�ip�u�v\�~�
��9��Z�� ���}�H�1���Mx=�m�s-�?�;!�ְ����H�G�C�S�h䙉�l:��W��=�ec=ԌK���x�+{,򋽢��Z*�0|�$
�YzQM���G�9� ڃcJb~�MǗ]�f<�\-����a#�ǫ�x*�P*����M۲�lk�;�۽ٽ�!�����p�ܣk�#�2̝,��{�t����	��$mR�
ׯx�5s
������6��\�����y8���� Ǟh]�jꨕ��~����^��ϸ�%U�"��bZ!�H�w���h%�p5��UHK���]��9�z4�o|���{-����xe�v���vތ��<k�5>��v��J.\�[|�h�|��K�`��ԋ��O���SV�qmK~�V��c����2�x�I�����a}+��x�#�,+���~.��R���J������5�-p�ujl�ͬ�a�&�Z|~\���%����Y]�٫,�ς^`�:�p],�7�X�$�@a���]�m�Z�pz�����M�q ���~븮u mK ={���q����,M}�����ɴ������d+��b�=���@����f�þ[��~JE��&]I�>�J�կ���>�G���~x�Nm���Zx�C�c/D�X���;A��A�k*��̎�=.o��tw�������~]�7h��N؈}�Q�K���H���k�d�C��h�zrMÝ轫۸*P�����8���R�����Co۔>*w�{���+�1ő�Ƴ՘����Q�>��I>S���[�CM��F����E/����#�L��M����~^��k&�������:��_�_���(}|��'>�y��=�
�DV����Ξ����A�݂K�5���4Ϋ�_��g�I��J�8��Q>ڧ������ȱTo����T�ı"�[n����Z.��oޜ��s�������D+�����"/�s����Rĭ~�@)�B n�b�?��.�$�D9��+�O �1u��a��V{q֛w��P�8�R]��}�X���6�[�����A�X4�I��t> hO:�^��&*��q�^�\nq�+p��v�a�H>�����no����d֖
K��r�)+-7R"�65&'2C@9HIK=?sV/ceg]qBO�p��zm]Gk�wo�L�i�������m��5=yqXSs����?�����¹ǿS����ǻk�ŋ��ۡ����ɬ�%Mۯl���+u� ����g`E�V�b�1�=v�@=T�)��h��I���%�[�����安I�X� Ds-������������T��׎�KhU�1�*+�*�V� �Dsg�)�^g%v���m�#Qʔ;W�ø8�>�걫ʶ#�im���?stlpbǏ6�<�2:��1gnvYsgύ8=�tiӧQ�V��ukׯaǖ=�vm۷q�ֽ�wo߿�>�xq�Ǒ'W>�r|�[m��9�`[�b5[]���.��	d�$�.
�~q�{�� �G�$�~�#�?���,��DH8��LHK-���? �H?&���Ĥ��o�=X��9��@,�~�@�4�p�B&tD��jEDa�P���P�(�B�)G�t���e��kG��1�Dc.+�b��F*���i��D�8*����)�8�	�\l�HH�z��+i�ќ9c��A�TK��|HK���	���񓢬\��*�9��k�0Lu����Lvj�5P�tx:��C�I�xV-i�)��*ES�A�K)iE�QUT��Lj}g�COM�IA����J7�RGV��K63���c�Tp��XVǌ��S.Y����f5�Y"ͅSL���&wy7_�����o�t�^s��*Q:�ҏZ�m݂�6Ҵ����;���#���5�2�,�қ(4�z�vax<6s�m��`����#�Yةܩ(�cZ3�:����w�lTɅ���d��L�LN��t�i�e���z�Yq�G���΃��왝�������4�7mX)��+�oJŊR�!�qlO�
ko|F<7�8��:���oQ�[v���k����<���Q�j��Yo���a�]��i����q�]��y���߁^��/���O^��o��硏^�驯����^�������_���/����O_}@
  ;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <html>
<!-- page generated 3-1-2000 23:17 by make_pages.pl -->
	<head>
		<title>
		page 11
		</title>
	</head>

<body bgcolor="#33333D">

	<!-- navigation bar at top.  Later, will be a bottom -->
	<img src="nav-bb.gif" border=0 usemap="#navmap">

	<hr>

	<!-- main picture goes here -->
	<img src="l08p11.gif" border=0 >

	<hr>
	<font size=-1 color="gray">
	page 11 of 16
	&nbsp;&nbsp;&nbsp;
	from <i>Understanding Unix/Linux Programming</i> by Bruce Molay
	</font>
	<!-- map for navigation goes here -->

<map name="navmap">

	<area shape=rect coords="35,0,100,25"
		alt="First screen"
		href="javascript:window.location.replace('page000.html')">
	<area shape=rect coords="100,0,155,25"
		alt="page010.html"
		href="javascript:window.location.replace('page010.html')">
	<area shape=rect coords="190,0,243,25"
		alt="page012.html"
		href="javascript:window.location.replace('page012.html')">
	<area shape=rect coords="243,0,300,25"
		alt="Last screen"
		href="javascript:window.location.replace('page015.html')">
	<area shape=rect coords="335,0,380,25"
		alt="Lecture Information"
		>
</map>
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <html>
<!-- page generated 3-1-2000 23:17 by make_pages.pl -->
	<head>
		<title>
		page 12
		</title>
	</head>

<body bgcolor="#33333D">

	<!-- navigation bar at top.  Later, will be a bottom -->
	<img src="nav-bb.gif" border=0 usemap="#navmap">

	<hr>

	<!-- main picture goes here -->
	<img src="l08p12.gif" border=0 >

	<hr>
	<font size=-1 color="gray">
	page 12 of 16
	&nbsp;&nbsp;&nbsp;
	from <i>Understanding Unix/Linux Programming</i> by Bruce Molay
	</font>
	<!-- map for navigation goes here -->

<map name="navmap">

	<area shape=rect coords="35,0,100,25"
		alt="First screen"
		href="javascript:window.location.replace('page000.html')">
	<area shape=rect coords="100,0,155,25"
		alt="page011.html"
		href="javascript:window.location.replace('page011.html')">
	<area shape=rect coords="190,0,243,25"
		alt="page013.html"
		href="javascript:window.location.replace('page013.html')">
	<area shape=rect coords="243,0,300,25"
		alt="Last screen"
		href="javascript:window.location.replace('page015.html')">
	<area shape=rect coords="335,0,380,25"
		alt="Lecture Information"
		>
</map>
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            work, complete source
code means all the source code for all modules it contains, plus any
associated interface definition files, plus the scripts used to
control compilation and installation of the executable.  However, as a
special exception, the source code distributed need not include
anything that is normally distributed (in either source or binary
form) with the major components (compiler, kernel, and so on) of the
operating system on which the executable runs, unless that component
itself accompanies the executable.

If distribution of executable or object code is made by offering
access to copy from a designated place, then offering equivalent
access to copy the source code from the same place counts as
distribution of the source code, even though third parties are not
compelled to copy the source along with the object code.

  4. You may not copy, modify, sublicense, or distribute the Program
except as expressly provided under this License.  Any attempt
otherwise to copy, modify, sublicense or distribute the Program is
void, and will automatically terminate your rights under this License.
However, parties who have received copies, or rights, from you under
this License will not have their licenses terminated so long as such
parties remain in full compliance.

  5. You are not required to accept this License, since you have not
signed it.  However, nothing else grants you permission to modify or
distribute the Program or its derivative works.  These actions are
prohibited by law if you do not accept this License.  Therefore, by
modifying or distributing the Program (or any work based on the
Program), you indicate your acceptance of this License to do so, and
all its terms and conditions for copying, distributing or modifying
the Program or works based on it.

  6. Each time you redistribute the Program (or any work based on the
Program), the recipient automatically receives a license from the
original licensor to copy, distribute or modify the Program subject to
these terms and conditions.  You may not impose any further
restrictions on the recipients' exercise of the rights granted herein.
You are not responsible for enforcing compliance by third parties to
this License.

  7. If, as a consequence of a court judgment or allegation of patent
infringement or for any other reason (not limited to patent issues),
conditions are imposed on you (whether by court order, agreement or
otherwise) that contradict the conditions of this License, they do not
excuse you from the conditions of this License.  If you cannot
distribute so as to satisfy simultaneously your obligations under this
License and any other pertinent obligations, then as a consequence you
may not distribute the Program at all.  For example, if a patent
license would not permit royalty-free redistribution of the Program by
all those who receive copies directly or indirectly through you, then
the only way you could satisfy both it and this License would be to
refrain entirely from distribution of the Program.

If any portion of this section is held invalid or unenforceable under
any particular circumstance, the balance of the section is intended to
apply and the section as a whole is intended to apply in other
circumstances.

It is not the purpose of this section to induce you to infringe any
patents or other property right claims or to contest validity of any
such claims; this section has the sole purpose of protecting the
integrity of the free software distribution system, which is
implemented by public license practices.  Many people have made
generous contributions to the wide range of software distributed
through that system in reliance on consistent application of that
system; it is up to the author/donor to decide if he or she is willing
to distribute software through any other system and a licensee cannot
impose that choice.

This section is intended to make thoroughly clear what is believed to
be a consequence of the rest of this License.

  8. If the distribution and/or use of the Program is restricted in
certain countries either by patents or by copyrighted interfaces, the
original copyright holder who places the Program under this License
may add an explicit geographical distribution limitation excluding
those countries, so that distribution is permitted only in or among
countries not thus excluded.  In such case, this License incorporates
the limitation as if written in the body of this License.

  9. The Free Software Foundation may publish revised and/or new versions
of the General Public License from time to time.  Such new versions will
be similar in spirit to the present version, but may differ in detail to
address new problems or concerns.

Each version is given a distinguishing version number.  If the Program
specifies a version number of this License which applies to it and "any
later version", you have the option of following the terms and conditions
either of that version or of any later version published by the Free
Software Foundation.  If the Program does not specify a version number of
this License, you may choose any version ever published by the Free Software
Foundation.

  10. If you wish to incorporate parts of the Program into other free
programs whose distribution conditions are different, write to the author
to ask for permission.  For software which is copyrighted by the Free
Software Foundation, write to the Free Software Foundation; we sometimes
make exceptions for this.  Our decision will be guided by the two goals
of preserving the free status of all derivatives of our free software and
of promoting the sharing and reuse of software generally.

			    NO WARRANTY

  11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS
TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE
PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,
REPAIR OR CORRECTION.

  12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,
INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING
OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED
TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY
YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER
PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGES.

		     END OF TERMS AND CONDITIONS

	Appendix: How to Apply These Terms to Your New Programs

  If you develop a new program, and you want it to be of the greatest
possible use to the public, the best way to achieve this is to make it
free software which everyone can redistribute and change under these terms.

  To do so, attach the following notices to the program.  It is safest
to attach them to the start of each source file to most effectively
convey the exclusion of warranty; and each file should have at least
the "copyright" line and a pointer to where the full notice is found.

    <one line to give the program's name and a brief idea of what it does.>
    Copyright (C) 19yy  <name of author>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA

Also add information on how to contact you by electronic and paper mail.

If the program is interactive, make it output a short notice like this
when it starts in an interactive mode:

    Gnomovision version 69, Copyright (C) 19yy name of author
    Gnomovision comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
    This is free software, and you are welcome to redistribute it
    under certain conditions; type `show c' for details.

The hypothetical commands `show w' and `show c' should show the appropriate
parts of the General Public License.  Of course, the commands you use may
be called something other than `show w' and `show c'; they could even be
mouse-clicks or menu items--whatever suits your program.

You should also get your employer (if you work as a programmer) or your
school, if any, to sign a "copyright disclaimer" for the program, if
necessary.  Here is a sample; alter the names:

  Yoyodyne, Inc., hereby disclaims all copyright interest in the program
  `Gnomovision' (which makes passes at compilers) written by James Hacker.

  <signature of Ty Coon>, 1 April 1989
  Ty Coon, President of Vice

This General Public License does not permit incorporating your program into
proprietary programs.  If your program is a subroutine library, you may
consider it more useful to permit linking proprietary applications with the
library.  If this is what you want to do, use the GNU Library General
Public License instead of this License.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        