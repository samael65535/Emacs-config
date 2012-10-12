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
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 GIF89a0∏Ä  ‘–œ33=,    0∏ ˛åè©ÀÌ£ú¥⁄ã≥ﬁº˚Ü‚HñÊâ¶Í ∂Ó«ÚL◊ˆçÁ˙Œ˜˛
áƒ¢ÒàL*óÃ¶Û	çJß‘™ıäÕj∑‹Æ˜ãOÄCyåN´◊IÄ€ÕéÀÁtŸ˚º≈WÓ˙∫ˇ–ß0Xı∆q®∏8∆˜PH◊!…Xi©uÁ@¥”iÒy):jóà*xòö¿⁄∫
…öÈ˙ä
y·ò´Qkπ‹)|;Hk¨wjK <S∂kÜ=,˝àóIXº¨
\(	Ω‡KÈªGMXçÕ›˜}öÆ:]≠ﬁ›Lˇ“NÃæé˚ÀæÕ_@}ØŒÙFéUø	ÛÚM˚ıÓù9AM]√∑Ã†@˛áı:í˘gÊ‹@äÎ L‰xr£6T	Sê¡BÜ¸@Ü4%êù∂É/é\3§«°∑•‰÷Je-j6˝]¨ô¥§:ó å5ì¶’≠«=
Oﬂ5Æ0ü}©í®⁄'Ö%ã∏qú…∑n_:•õLªº,GR˝ª¡"\íoInfÿ©KƒƒŒ¢•∏62c˘“	F*VóœΩ{ß¬Ωã`Yü~”öûD∫k\Øê?[vÃ:ÁA≥˝pJæç®©NùÚŒ%‰Ÿ[ﬁl—≥c{9óﬁ¨^Øæ˜îó≈ËƒÅCÉYê9ÓÌ˚Rﬂ-û!spÀ©_y∞£¶ÉeÒÌe^xáÂy+ñO„UÌ‹˛˚„ﬂèû.Õë#Ts±w_pπs⁄Äia˜ÿ`	ÍwÑ•ıÂ_Üö•‡ÉÂ ˆ8W®°¸e–çà,ßÜÖSêX"w0bê(√ƒH√å8ﬁ¶„ä~p‘é˛(då=jv¢çâÇãLjx‰*>IeïV^âeñZn…eó^~	ÊÓçIfôfûâföjÆ…fõgÜ	gIÓ8gúvÍ‡fûzÓ…gü~éyg†ÇJh°Üäh¢ä. h£é>
i§íNJi•ñ^äi¶ön ißû~
j®¢éJj©¶ûäj™™Æ j´Ææ
k¨≤ŒJk≠∂ﬁäkòuÊ +SVRZQ™‡§ú¿ıZ˛¬ØäÏvÏj=ÿ7b≥»¢¯≈Zã`RUëQê&ª6Áb€Nk„Lﬂä0¨Aj€⁄ÿ-uÆ5u∆˚◊X‰äôΩ§´n6’éãn<Ë;â ~».¡˜F»Væe÷ê‹Ö ~DÊÖ¸“x≠ºÄ.Ïnh›â√ﬁ˙P"Ó«‡∫÷TÜÃesΩú›h¬|⁄t$/…1|èyõRØ¸ìMÿp∂t(%âQ“Ô
|U∑	∫±∞)+Xs∂9c¸SÑ=7h0”iWÉÍπâ‰bﬁIXu¡«l2¿cÉ]◊”A^|µÑs”á`◊˚,hõ-gC∂tΩÛÁs–V„-pﬂG€|TEw›Ï…˛åôie)…wxéŸe9à[£}s;Çód‡»ûÛeÆ∂Œ	>˙ÃucMV}-≥t2Õè∑óòoÙ·¸˘ë8…ı`{ıŸ5DÊæ«"Î¡«˝∫Œwñoõé∞Ò”ÀÜï–Ô·-±∑†•ˆ}»™Å≈}…øUgef˜Ñ{ÛÔl∞º4‡÷ù
Âwç±u“ÅG•4M=N⁄q2íí˘·è|ÓÛë…^2tj{`∞ƒÜ∂)}ŒnÉ+NÃtù_µÖi‹~zÁéFÓ^|€ÏÍÁ¿po∫Cô◊vUªåçF9 CM≤"]ÎgÑ“·Ü∞¬Ü1R*#B'≥D&QO‹ó˛¬¢ò•&"Òä@‚"ø.0ä±	U£œà∆4∫ ^!b†OE?+ƒÒç†"gÒº—Åàn£#ìï£“ıëÜs ˝h≠Ü2`"sX`TFDeq¶ãû∫&≤ó∏D8[Çô$)Ö‰®hìπXd⁄∆v≥Jj2Ev|ú„ÏR<
ñ¶!O˚X&AπÇ≈Œ<Ñ)-´376ÚÄá◊ˇé≥ΩmI˘ã«1vˆJ\^“p<#eÂY5∏ÂÕkcÂ“î©f≈Æ;UÅVÑUN›òpê“DóüMâAÆ%˙e3'5añR6Öc&=Ôâ!ø(ÆsÆîJ;'y:ÆùM!Çúéw˛8≥“0ñ‰Ã	XZòN˛M°ÌXg†«ŒÉN&öåg'q'Æ‰ù2è}\¥8yìbﬁ*%å=◊Q·n|"Eh5QÈ∆⁄\Ø'⁄√HA7¡~ûtBç[ﬂG)«>°“Æx‡\jOG Ov‘rR%*1Å)äM.¿ßº∆âôerÖ},%$ı˛wÕ^uH˜ *7™I∂Ã°j•€î∫>‚uH›+I•˜R˘ú∞î^úk˚1√éG‚YÎœö◊»¶2ÉÖ·ZsZQÓ9çulìJõ°H˛Pµ¶5jMÙ⁄∂ˆ¨ïQlgãÖ⁄Ffé∏˝Én◊b ﬁ ·∑¬-Æqè˛ã‹‰*wπÃmÆsü›ËJw∫‘≠ÆuØã›Ïjwª‹ÌÆwøﬁäwº‰-ØyœãﬁÙ™wΩÏmØ{ﬂﬂ¯ wæÙe»M—à˙J©ïN‡-€¿_ÅVø!Æ'ú%≤Übd`nïÎ∂•p)@≤Ö¿vÜü†·‹wºê§Ìr©∑˛ŒänngŸfB:o¨ìﬁ,k¨J«≤ô.À1èAÊ∑#Ä¨k`ªò`ü—Úd»⁄´Í◊&c÷ÆJõÍàπ?fZsÉÅ—¶YÀ«UÅqÃ^&†ÄWZOW”¬…Ì[RÖ«T%>î…`VÌ¥ ?∆µfÇeÂ€ôç	˛g´ñ∑¶;¶W◊â`ÙÆ™€(S›≥ESU—4M™êÖ´R5Ê3K¶Ê2À9πxZuï-IA¡±64y)mœg ≤È;jU•<>Ò§¨uVgiM;ßl2SzÏ∂Üî÷«tVW'NØÓïí}Ω&QùÕP„–f'u—¢yEy‰ï¸t∆'T·b!õü ñ∞∞à°ã\ÊÓ6Œû¨Örx.p[õï°µˆøº∑°Bu¢$Á±/›‡, ¬ë\x¬^Ï.£6‚ÔÇ√spÒäÎ ë†∏∆Âòq<y¸„ó
9…OéÚî´|Â,oπÀ_ÛòÀ|Ê4ØπÕoéÛ˛úÎ|Á<ÔπœÙ†}ËD/∫—èéÙ§+}ÈLo∫”„4r+E=†Qd„!Ü‡Bç6Í^XºßŒÍGA´Îù6πm#ÆÈ∂fΩ_k‡xú$éóÕu]jÒ£©æl;‡i_ò€¢gÉ‚·ÄªZÍ∑'h¡>∞R¨]≥rbÍL"˜øõùü7%Òµ6'Ô¿nw`æˆI^2Û¢/=±Ñ ¶›ÆÛ;*˜fg≠e¡ßZÿ¢ﬂ]É∏∞ÃÙˇb·Ö◊á⁄ÙµáΩÓ}»IYÁÿ¯ÃDµ+WS¿ñKÛ±áôà∂Ü˝ßÕ”,É˜ª¢ÃM˙ÈR‘›ü•Á|b\_ ◊◊˛â9xK`˛ï9õÌﬂ]„´4ÉJ'Õ·ﬁ†7Çè~ÄEF!wÁoy∂JËdSjVgŸ∂1x◊..ı¸Å€‘Mà«nÊ~,ÛgÖqÖ7•Åw÷>X3˘§fì#Qvf∑Áoú∑9XpuÊÇ˚√Ç÷≥g—‘gvÖgòÅFˆÇıV=ÚUTwYËPìhrcb_Cj’:Ï∆U∑kº#U»”§±hÿdÄ&∏o,UÑ«YOÊÉûtjæÜj¶◊4»¶SFu_KÿVI∆Ñ¯∂ÄÉåÒixTxy (v∏S|5m0‘!Xá◊÷—7ÜnUPıKo÷|d8U∂f~ŸÉÜ√gIﬁá2lU˛K∏◊ÍS8ªqfZcxÆShh»SK2Ω¶á´à|8H>E
˙≥É¥Ö´µÿv?¢kÛ!?Ω(K àË@d	t?8lcqã˝6á—sAOX>b%5Ifã¬ÿLauuãÁ[ÜÖ{%´72»‘W_∑|Îay·»`cA¬în2ji≈e	¯çÀ«z®£}≤sNÎ(DÚ∂M®,Ûñé8
˙ÊWñµSw~M|èep/S™ÿIæ$B0òu['{ŸÅ@òÿâo”=:§nbËå˜Ü)d◊3yﬁ≥B)9M%…í
«wE–yÉr+áíeh8Ÿí*wì-¢ìtıíÃ’ìö2ìÌ˛5îOáîI©îK…îMÈîO	ïQ)ïSIïUiïWâïY©ï[…ï]Èï_	ña)ñcIñeiñgâñi©ñk	y2¬`≥î&	$o¯Éiów◊îr…bA)b=4R|Y_sre4"ƒHm˘ñ·Úì¥"òÑgëoX•6oÈ;HV\HÉC†hE„.áPûß/Ä	+ÜÑI∞ìëJtò.ÿ=ÿtZ$¿çÌ¶ó_§|9ë ÿá<(Xëıà⁄a3b¶AÚˆY\ò=ØÈÖœˆz¢ô*ÍX¨hx„Á<Ì«KÌ8Yµ‰úôÜÖôÙ|2∏}w9ıÁgé’åÅ÷Ç	ÖOèi}≈	 È©û°Ü≈®û˛ÈIMÎ	 9LÛÁQö…Yˆ)üpàõ"EüfÜûˇh8Ê˘2‡≥üaˆ7Ù)-ˇYå;ìx∫ ˇƒ† †ƒF:MÖV„ŸûtT°™†⁄°Mÿ†}Òüd†7T¢Rë¢óàT“R JPÔi6Ø£vÜÎ¥¢zµò≠¢=
£2∫;>:|CÖyØòˆwÅEp{ü
¢LXP.jj1¶=ìúÌ¥¢)Z£MJ£≠»•îHì#ô†ô|˙ß•ŒdåLï£Nä>@ÍVoUè≥8T_JóoÙ°Ò”£∏òåk*åIX§Õˆòl:åT˙kr }¨«âÇ˙ûÚ	°Iùp˙§“&@˛È¶ãznàj˛©ªt¶DZ£Á(£9üT†˙£ãä£J°ﬁ∏üç˙A.T©£⁄•nzPÇ ëâÊ0•sß™P˘®öIõÔî©≠fU¥jä∂71ßC¨Í´W™_¡öExÖEß¨Oí™ÄƒñÅ‡°…≤£◊ä-´â"€ ≠ä¶}Æè$´Æ	ÆÂ™ÆÎ ÆÌÍÆÔ
ØÒ*ØÛJØıjØ˜äØ˘™Ø˚ Ø˝ÍØˇ
∞+∞K∞k∞ã∞	ÀòúíÆØvÆ€“∞ó‡¨6`F…™{(´≠”Í^<‘©~∞}K,o'≤˚u<•*ôâƒ©û`~ß1!⁄)kE+´òH™9uÖGE«sÉô˛Ú∞‰ë:+ô”≤[™ìF€1ˆ»ö„Znçz™u¥mÊ4+µ√IäR∏!YÄ(Î≠áâ©¥µIßî©õø'Yéµ˙Tí‡w|—	©hä@g⁄p˚<†∫oLªga[~‹I≥q8$˘≤{+∑I˙Åw9)´©·¥∂0¶N‚U˙7e’*ÄÖ∂xW¢mKfÈÙeHhôÆ∏¢;∫âùTk>⁄iR∑£UJOtÍ∫Z¡=fJÖî+û|˚ùì8g´[&ß:µµÜ¯~c)•€§VhgÅ[≠å€∑†ÀöOäÖ¯®çAä∏Ö⁄H™y
µ’Ç! K·f´≤%ßÄ¯¶óà±
öÉ˛≈Ω(ı´î•ˇWçh{®j§≤jΩﬂJwπöGC´∑ıkΩùöø≈€Å$T∏2òÜÖøãòÜäùºõô±»Åˆª∑N≠G˚∞|ö¨høÖ⁄U„[)∆õπÑπ<ë™Êaπ,¬	}'¸-'<@®™ΩË√V¨ø<e®å™∑˜˘¸±u∑√Ïh˙™;√Ω¿A<úÀK§òg™ÎÜê∞…∏%+ë:¨;‹¬U⁄™j*≤e˚)O6ÎßÈ≈Ÿ¥°`l1ä⁄ßﬂZ≤T¨¥sÀΩ¸Xr(•™¨mÃ∑e\≥?åb3≈6\åF|úK˚NF"«vJ»ÉKæ∂À°ã‹ö‰Î∑`˛»—‚í€[uê¸ô!<¥
Î…ü °, £L •l ßå ©¨ ´Ã ≠Ï ØÀ±ºtW¥‹ô˝°…ÓD.bLôT‹?ïÃºå!πvªÏÀßÃíÃÀ“…≈\óÀº*\ªª0K#ÅÁy°t5“L†aˆ »«l≠‡¢ÕÕ7l∫1)æ$közWÕÂl(Àú«I<z¶*ŒÌ<}ÿöb@ÄªÁ‹v±
dz|è¡∂/€∏.ÎµÈ∂”⁄Ã∞î8B¨ÃVõ∏Î<œfk¨i(ƒgÎ¥‚i∏˛ªûMÃ;˘©8∆ÜªKtLøÏ{êJå“fïæl∑È£—¸˘ø8†ˆ(√@ã∑%]”–˛T∫æx£3lπ<-»î7∑;fÒŸä∫J∏¨°⁄+•¸√ø&»ªÂπ©À—Í;`#Ã√N’˚«—…F®¬´°1πdú¡]±ipºu¿v\¨~†
ôΩ9àæZΩ“ Ñ¨¥}ÅH•1¯º⁄Mx=ämºs-«?ù;!’÷∞ßø˚£H∫GõC¡Sÿh‰ôâél:´™WÄä=•ec=‘åKá±…x⁄+{,ÚãΩ¢˝´Z*Ö0|§$
”YzQM¶¸πGÓ9« ⁄ÉcJb~üM«ó]Çf<£\-§ôÒ–a#∏«´”x*úP*Çùá«M€≤Ëlkù;Ã€ΩŸΩ“!çÃ‹¡˛p»‹£k√#ÌÖ2Ãù,Í∫ {±tø±à€	Â∆$mR¥
◊Øx∂5s
Ωïã°ãËâ6ç…\êµ¿∏ßy8Ÿ¿˙’ «ûh]µjÍ®ï——~Äπ ·^‹‹œ∏¨%U“"Æ∆bZ!ΩH€w⁄·≤h%µp5πáUHKº©ü]◊ı9”z4∆o|©Ù˜{-öè˝«xeƒvåøˆvﬁåÁ„<kœ5>–Ëv‰Í¶J.\â[|≈hÏ|À¬K `æ…‘ãç‰OÎ´‡—SV÷qmK~ﬁVÃÿcíåÌ—2ûxπIÄÛ˝è‰ùa}+¬‹xé#˜,+ÄÆ‡~.À¯RËßÁáJ˛¨ËçÓËˆ5˛-pÈöujl˘Õ¨€aÀ&˚Z|~\¬‹ﬂ%¬È∆ËÜY]€Ÿ´,∆œÇ^`È:Íöp],Ê7ÎX‹$ú@aÑË∑Î]ˆmÓZúpz÷—√À∆Méq åö≥~Î∏Æu mK ={ß©§qÿœÍ‘,M}Î†õòÌÜ…¥≤˝æßé‹d+¡Øb∑=›ûŒ@ù∑˙çfﬁ√æ[Å∞~JEæ∫&]Iâ>’J≈’Øç¶ü>öGç››~x’Nm‹÷÷ZxÊCÏc/DƒX˝ÓÃ;A÷‘Aõk*¯¥Ãéà=.oΩÇtwÍˇÙÒ‚¯”~]«7hÚÙNÿà}ÚπQπKΩ˚ËæHËÚ¨k˛dŸC∏€h˝zrM√ùËΩ´€∏*PèèËÙ´8©◊€RæÑ¥Œ€Co€î>*wı{®£“+Ì1≈ëÏÜ∆≥’òã√§ÖQ⁄>ö¶I>S‹—Û[±CM°¶Fºûí–E/©ø∂Ô#ÑLˆêMê¸’æ~^˙®k&¡˝äëçàé:£ç_˜_›‡º(}|üı'>Ωy«Ú=‚
√DV¡û¡•Œû¿º»–Aæ›ÇK·5ÂèÀ4Œ´à_˚ƒgÍ∞IèŒJÊ8≠–Q>⁄ßø©–€ƒÒ»±ToœŒËÂT√ƒ±"‰[n‚„˚ÌZ.¶πoﬁúòÊsá”‡é—ÿË±D+¨˛Ä∑Î"/‚s¸ßÍœRƒ≠~¥@)˚B n´b≤?ˇÚ.Æ$ˇD9ˇÎû+˝O Ò1uπ˝aîìV{q÷õwˇ¡P…8œR]Ÿ÷}·XûÈ⁄6–[ﬂ˘ﬁˇÅA·êX4ëIÂíŸt> hO:µ^±Ÿ&*•Öq´^Òò\nqÕ+pö›vøa∏H>∑ﬂÒ”˙noÏÁ©˛d÷ñ
K∫¯rî)+-7R"–65&'2C@9HIK=?sV/ceg]qBOÂp∫§zm]GkÖwo√LÖiôõÎ¿íÉâm´¨5=yqXSs˜éΩ©?ñùœ—≈¬π«øSªë¡˛«ªk°≈ãΩÌ€°Â”˝ˇ…¨„%M€Øl˙¶Ì+uØ ΩÖ∫Àg`EãV∂b•1‚=vÛ@=T÷)òÆhùæIª∏í%í[Û™ıÈËÒÂÆâI¬XÓ Ds-Å•°≤†¡î¯à≠€ŒTº¶◊éÓ§KhU´1§*+ä*°Vî ˇDsg“)™^g%vΩ∫ñmÆ#Q î;W£√∏8•>˝Í±´ ∂#˙im›¿á?stlpb«è6Ü<ô2:™ï1gnvYsgœç8=öti”ßQßVΩöuk◊Øa«ñ=õvm€∑qÁ÷ΩõwoﬂøÅ>úxq„«ë'W>ôr|ñ[mÆÊ9˛`[ﬁb5[]«ÙË.≥”	dº$√.
á~q˝{ëÚ ﬁGÅ$π~Í#∞?ó˚ˆ,Ë√DH8êôLHK-ËÛÎ? øH?&Ù≈¿ƒ§üÏo√=X•¬úÚªê9…ª@,≥~Ò@¶4¥p≈B&tDû‚jEDa≤Pî∫–PÂ(ªBõ)GøtÒÒ»eåºkGæÚ1àDc.+∆b≥ÒF*π¨ i¸ÛêD¢8*Ã¯†“)Ö8‘	ó\l⁄HH˛zÎÕ+i¢—ú9c≤ãA∑TKÕÎ§|HK∞Úƒ	Øè∂Òì¢¨\ëö*˘9Ú—kÂ0Lu¬ëØê⁄ÃLvj˛5P¬Ü¸tx:ù‡C’IúxV-i©)ç§*ESÎAÙK)iE QUTÌë◊Lj}gõCOMËIA˚Ñ÷’J7ÇRGVÔí÷K63ÕıœcıTp◊ÁXV«åƒ›S.Y˝¸ÂÃf5ÂY"ÕÖSLØ∫‰&wy7_ü÷Ìıîoøt¥^sâÑ*Q:π“èZÇm¬ì›Ç·ù6“¥öÙ©Ç;˚•µ#√Œı5©2©,î“õ(4ÕzÆvax<6s mÒÇ»`˚ı¥‚é#≠Yÿ©‹©(†cZ3Á:ˇï≥–w∫lT…Öàï±dì”LıLNÛ¬tË£iÿeüˇ¥zÁYqîG•ñÒŒÉ∂’Ïôùª∆˘¶¯îÊ4Ì7mX)≠“+ÀoJ≈äR¡!òqlOü
ko|F<7÷8Ù‰:Â©úÂ¢oQΩ[vÛæ¡¸kË«∆¬<ºÙ√QØjø‘Yo›ı◊aè]ˆŸiØ›ˆ€qœ]˜›yÔ›˜ﬂÅ^¯·â/ﬁ¯„ëO^˘Âôoﬁ˘Á°è^˙È©Øﬁ˙Î±œ^˚ÌπÔﬁ˚Ô¡_¸Ò…/ﬂ¸Û—O_}@
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