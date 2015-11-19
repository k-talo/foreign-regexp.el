;;; foreign-regexp.el --- search and replace by foreign regexp.

;; Copyright (C) 2010-2014 K-talo Miyazaki, all rights reserved.

;; Author: K-talo Miyazaki <Keitaro dot Miyazaki at gmail dot com>
;; Created: Sun Nov 28 23:50:45 2010 JST
;; Keywords: convenience emulations matching tools unix wp
;; Revision: $Id$
;; Version: 2.0.0
;; URL: 
;; GitHub: http://github.com/k-talo/foreign-regexp.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; NOTE
;;
;; This library is just tested on Emacs 24.3.1 on Ubuntu 10.04
;; and Mac OS X 10.8.3, and won't be run with any version of XEmacs.

;;; Commentary:
;;
;; CAUTION
;; =======
;; THIS LIBRARY IS VERY EXPERIMENTAL, SO YOU MAY HAVE MANY
;; PROBLEM WITH THIS LIBRARY. AND THIS LIBRARY COMES WITH
;; NO WARRANTY.
;;
;;
;; OVERVIEW
;; ========
;; This library provides a feature, processing regular
;; expressions in manner of other programming languages
;; (we call it `foreign regexp' for convenience here),
;; to Emacs.
;;
;; In particular, this library provides features corresponds to
;; such as `isearch-forward-regexp', `query-replace-regexp' and
;; `occur'.
;;
;; Currently, regular expressions of Perl, Ruby, JavaScript and
;; Python can be used as foreign regexp.
;;
;;
;; THE GUTS OF THIS LIBRARY
;; ========================
;;
;; This library works like below:
;;
;;   1. Make a search/replace operation with foreign regexp
;;      through the user-interface of Emacs.
;;  
;;   2. A search/replace operation will be executed by external
;;      commands (they are implemented in Perl, Ruby, JavaScript
;;      or Python).
;;  
;;   3. Apply the result of search/replace operations to the buffer
;;      through the user-interface of Emacs.
;;
;;
;; REQUIREMENTS
;; ============
;; You need to have an Emacs which running on UNIX-like operating
;; system (*BSD/Linux/MacOSX) or Windows+Cygwin.
;;
;; perl (>= 5.8), ruby (>= 1.9) node (Node.js, for JavaScript) or
;; python (only tested on 2.x), choose one of them as your taste,
;; is required as external command.
;;
;; Also features `cl', `menu-bar' and `re-builder' are required.
;;
;; For better multilingual support, Emacs (>= 21) may be required.
;;
;; NOTE (for Windows users):
;;   In some cases, virus scanner program makes each `foreign-regexp'
;;   command running extremely slow.
;;   On such case, turn off virus scanner program, or exclude the
;;   path which is specified by a variable `foreign-regexp/tmp-dir'
;;   from virus scanning.
;;   This may improve the response of each `foreign-regexp' command.
;;
;;
;; INSTALLING
;; ==========
;; To install this library, save this file to a directory in your
;; `load-path' (you can view the current `load-path' using
;; `C-h v load-path <RET>' within Emacs), then add following
;; lines to your `.emacs':
;;
;;    (require 'foreign-regexp)
;;
;;    (custom-set-variables
;;    '(foreign-regexp/regexp-type 'perl) ;; Choose your taste of foreign regexp
;;                                        ;; from 'perl, 'ruby, 'javascript or
;;                                        ;; 'python.
;;    '(reb-re-syntax 'foreign-regexp))   ;; Tell re-builder to use foreign regexp.
;;
;;
;; USAGE EXAMPLE
;; =============
;; In these examples, we suppose the contents of curent buffer are:
;;
;;    123---789
;;
;; [Example-1] Query Replace in manner of Perl.
;;
;;   STEP-1: Set `foreign-regexp/regexp-type' to Perl. 
;;
;;        `M-x foreign-regexp/regexp-type/set <RET> perl <RET>'
;;
;;        NOTE: Once you choose REGEXP-TYPE, Emacs will remember it
;;              until exit. You can also set and save REGEXP-TYPE for
;;              next Emacs session by setting value via customize.
;;              See "COMMANDS (1) SETTING REGEXP-TYPE" section in
;;              this document.
;;
;;   STEP-2: Run query replace 
;;
;;        `M-s M-% (\d+)---(\d+) <RET> ${1}456${2} <RET>'
;;
;;        This command replaces the text in buffer:
;;
;;           123---789
;;
;;        with text:
;;
;;           123456789
;;
;;        NOTE: Variables in replacement string are interpolated by Perl.
;;
;;
;; [Example-2] Query Replace in manner of Ruby.
;;
;;   STEP-1: Set regexp-type to Ruby.
;;
;;        `M-x foreign-regexp/regexp-type/set <RET> ruby <RET>'
;;
;;   STEP-2: Run query replace 
;;
;;        `M-s M-% (\d+)---(\d+) <RET> #{$1}456#{$2} <RET>'
;;
;;        This command replaces text in buffer:
;;
;;           123---789
;;
;;        with text:
;;
;;           123456789
;;
;;        Variables in replacement string are interpolated by ruby
;;        as if they are in the replacement string inside of the
;;        `String#gsub' method.
;;
;;
;; [Example-3] Query Replace in manner of JavaScript.
;;
;;   STEP-1: Set regexp-type to JavaScript.
;;
;;        `M-x foreign-regexp/regexp-type/set <RET> javascript <RET>'
;;
;;   STEP-2: Run query replace 
;;
;;        `M-s M-% (\d+)---(\d+) <RET> $1456$2 <RET>'
;;
;;        This command replaces text in buffer:
;;
;;           123---789
;;
;;        with text:
;;
;;           123456789
;;
;;        Variables in replacement string are interpolated
;;        as if they are in `String.prototype.replace' method.
;;
;;
;; [Example-4] Query Replace in manner of Python.
;;
;;   STEP-1: Set regexp-type to Python.
;;
;;        `M-x foreign-regexp/regexp-type/set <RET> python <RET>'
;;
;;   STEP-2: Run query replace 
;;
;;        `M-s M-% (\d+)---(\d+) <RET> \g<1>456\g<2> <RET>'
;;
;;        This command replaces text in buffer:
;;
;;           123---789
;;
;;        with text:
;;
;;           123456789
;;
;;        Backreferences in replacement string are interpolated
;;        as if they are in `re.sub' method.
;;
;;
;; COMMANDS(1): SETTING REGEXP-TYPE
;; ================================
;;
;; `M-x foreign-regexp/regexp-type/set <RET> REGEXP-TYPE <RET>'
;;
;;      Set type of regexp syntax to REGEXP-TYPE.
;;      By default, four regexp-types `perl', `ruby', `javascript' and
;;      `python' are provided.
;;
;;      You can also set REGEXP-TYPE via customization interface:
;;
;;      `M-x customize-apropos <RET> foreign-regexp/regexp-type <RET>'
;;
;;
;; COMMANDS(2): SEARCH AND REPLACEMENT
;; ===================================
;;
;; NOTE: While editing a regular expression on the minibuffer prompt
;;       of `foreign-regexp' commands below, you can switch to another
;;       `foreign-regexp' command without losing current editing state.
;;
;; `M-s M-o REGEXP <RET>'
;; `M-x foreign-regexp/occur <RET> REGEXP <RET>'
;;
;;      Show all lines in the current buffer containing a match
;;      for foreign REGEXP.
;;
;; `M-s M-% REGEXP <RET> REPLACEMENT <RET>'
;; `M-x foreign-regexp/query-replace <RET> REGEXP <RET> REPLACEMENT <RET>'
;;
;;      Replace some matches for foreign REGEXP with REPLACEMENT.
;;      Note that notation of REPLACEMENT is different for
;;      each REGEXP-TYPE.
;;
;; `M-s M-s'
;; `M-x foreign-regexp/isearch-forward <RET>'
;;
;;      Begin incremental search for a foreign regexp.
;;
;; `M-s M-r'
;; `M-x foreign-regexp/isearch-backward <RET> REGEXP'
;;
;;      Begin reverse incremental search for a foreign regexp.
;;
;; `M-s M-f REGEXP <RET>'
;; `M-x foreign-regexp/non-incremental/search-forward <RET> REGEXP <RET>'
;;
;;      Search for a foreign REGEXP.
;;
;; `M-s M-F REGEXP <RET>'
;; `M-x foreign-regexp/non-incremental/search-backward <RET> REGEXP <RET>'
;;
;;      Search for a foreign REGEXP backward.
;;
;; `M-s M-g'
;; `M-x nonincremental-repeat-search-forward'
;;
;;      Search forward for the previous search string or regexp.
;;
;; `M-s M-G'
;; `M-x nonincremental-repeat-search-backward'
;;
;;      Search backward for the previous search string or regexp.
;;
;;
;; COMMANDS(3): WORKING WITH SEARCH OPTIONS
;; ========================================
;;
;; NOTE: The status of each search option will be displayed by an
;;       indicator which is put on the minibuffer prompt of each
;;       `foreign-regexp' command, or put on the mode-line of a
;;       buffer `*RE-Builder*'. The indicator will be displayed
;;       like these: `[isxe]' for Perl, `[imxe]' for Ruby,
;;       `[ie]' for JavaScript and [ISXe] for Python.
;;       
;; `M-s M-i'
;; `M-x foreign-regexp/toggle-case-fold <RET>'
;;
;;      Toggle search option `case-fold-search'.
;;
;; `M-s M-m'
;; `M-x foreign-regexp/toggle-dot-match <RET>'
;;
;;      Toggle search option `foreign-regexp/dot-match-a-newline-p'.
;;
;; `M-s M-x'
;; `M-x foreign-regexp/toggle-ext-regexp <RET>'
;;
;;      Toggle search option `foreign-regexp/use-extended-regexp-p'.
;;
;; `M-s M-e'
;; `M-x foreign-regexp/toggle-eval-replacement <RET>'
;;
;;      Toggle search option `foreign-regexp/eval-replacement-p'.
;;
;;      When this search option is on, the replacement string for
;;      a command `foreign-regexp/query-replace' will be evaluated
;;      as expression. For example, these commands:
;;
;;        For `Perl':
;;          `M-s M-% ^ <RET> no strict 'vars';sprintf('%05d: ', ++$LINE) <RET>'
;;            NOTE:
;;              Replacement will be evaluated like REPLACEMENT in replacement
;;              operator with `e' option (like: `s/pattern/REPLACEMENT/e').
;;              In the replacement string, you can refer to special variables
;;              `$&', `$1', `&2', ... and so on.
;;
;;        For `Ruby':
;;          `M-s M-% ^ <RET> { $LINE||=0;sprintf('%05d: ', $LINE+=1) } <RET>'
;;            NOTE:
;;              Replacement will be evaluated like a block passed to
;;              `String#gsub' method.
;;              In the block form, the current match string is passed as a
;;              parameter, and you can refer to built-in variables `$&', `$1',
;;              `&2', ... and so on.
;;
;;        For `JavaScript':
;;          `M-s M-% ^ <RET> function (m) {if(typeof(i)=='undefined'){i=0};return ('0000'+(++i)).substr(-5)+': '} <RET>'
;;            NOTE:
;;              Replacement will be evaluated like a function in the 2nd
;;              argument of the method =String.prototype.replace=.
;;              In the function, the current match string, captured strings
;;              (1 .. nth, if exits), the position where the match occurred, and
;;              the strings to be searched are passed as arguments, and you can
;;              refer to properties `RegExp.lastMatch', `RegExp.$1', ... and
;;              so on.
;;
;;        For `Python':
;;          `M-s M-% ^ <RET> i = 0  C-q C-j def f (m): C-q C-j <SPC> global i
;;                     C-q C-j <SPC> i=i+1  C-q C-j <SPC> return '%05d: ' % i <RET>'
;;
;;            NOTE:
;;              You can specify a function which takes match object as argument
;;              and returns replacement string, by `lambda' expression or `def'
;;              statement.
;;              And you can refer match and sub groups through match object,
;;              for example: `lambda m: m.group(0)'.
;;
;;              When you specify a function by `def' statement, you can use
;;              arbitrary function name and you can put statements around the
;;              function.
;;              In this case, the first `def' statement will be called for each
;;              matches, and the other statements will be called only once
;;              before search/replacement operation has began.
;;
;;              The first implementation of this library accepts only `lambda'
;;              expression as the replacement.
;;              Because of inconvenience of =lambda= expression, that it does
;;              not accept any statement like assignment operation, so we make
;;              this library to accept =def= statement.
;;              Additionally, we can't assign to uninitialized global variable
;;              in function defined by =def= statement, so we make it to accept
;;              statements around the =def= statement which can initialize
;;              global variables, for our convenience.
;;
;;      put line number to beginning of each lines.
;;
;;
;; COMMANDS(4): CONSTRUCTING REGEXP WITH RE-BUILDER
;; ================================================
;;
;; `M-x reb-change-syntax <RET> foreign-regexp <RET>'
;;
;;      Set the syntax used by the `re-builder' to foreign regexp.
;;
;; `M-s M-l'
;; `M-x re-builder <RET>'
;;
;;      Start an interactive construction of a foreign regexp with
;;      `re-builder'.
;;      (See also documents of `re-builder')
;;
;;      NOTE-1: To apply the foreign regexp, which was constructed
;;              with `re-builder', to the `foreign-regexp' commands,
;;              call commands below in `*RE-Builder*' buffer:
;;
;;              `M-s M-o'
;;              `M-x foreign-regexp/re-builder/occur-on-target-buffer'
;;
;;                   Run `foreign-regexp/occur' in `reb-target-buffer'
;;                   with a foreign regexp in the buffer `*RE-Builder*'.
;;
;;              `M-s M-%'
;;              `M-x foreign-regexp/re-builder/query-replace-on-target-buffer'
;;
;;                   Run `foreign-regexp/query-replace' in `reb-target-buffer'
;;                   with a foreign regexp in the buffer `*RE-Builder*'.
;;
;;              `M-s M-s'
;;              `M-x foreign-regexp/re-builder/isearch-forward-on-target-buffer'
;;
;;                   Run `foreign-regexp/isearch-forward' in `reb-target-buffer'
;;                   with a foreign regexp in the buffer `*RE-Builder*'.
;;
;;              `M-s M-r'
;;              `M-x foreign-regexp/re-builder/isearch-backward-on-target-buffer'
;;
;;                   Run `foreign-regexp/isearch-backward' in `reb-target-buffer'
;;                   with a foreign regexp in the buffer `*RE-Builder*'.
;;
;;              `M-s M-f'
;;              `M-x foreign-regexp/re-builder/non-incremental-search-forward-on-target-buffer'
;;
;;                   Run `foreign-regexp/non-incremental/search-forward' in `reb-target-buffer'
;;                   with a foreign regexp in the buffer `*RE-Builder*'.
;;
;;              `M-s M-F'
;;              `M-x foreign-regexp/re-builder/non-incremental-search-backward-on-target-buffer'
;;
;;                   Run `foreign-regexp/non-incremental/search-backward' in `reb-target-buffer'
;;                   with a foreign regexp in the buffer `*RE-Builder*'.
;;
;;      NOTE-2: You can switch search options of the
;;              `reb-target-buffer' with commands below:
;;
;;              `M-s M-i'
;;              `M-x foreign-regexp/re-builder/toggle-case-fold-on-target-buffer'
;;
;;                   Toggle search option `case-fold-search' of `reb-target-buffer'.
;;
;;              `M-s M-m'
;;              `M-x foreign-regexp/re-builder/toggle-dot-match-on-target-buffer'
;;
;;                   Toggle search option `foreign-regexp/dot-match-a-newline-p'
;;                   of `reb-target-buffer'.
;;
;;              `M-s M-x'
;;              `M-x foreign-regexp/re-builder/toggle-ext-regexp-on-target-buffer'
;;
;;                   Toggle search option `foreign-regexp/use-extended-regexp-p'
;;                   of `reb-target-buffer'..
;;
;; `M-\'
;; `M-x foreign-regexp/quote-meta-in-region <RET>'
;;
;;      Escape characters in region, that would have special meaning
;;      in foreign regexp.
;;
;;
;; COMMANDS(5): ALIGNMENT USING FOREIGN REGEXP
;; ===========================================
;;
;; `C-M-|'
;; `M-x align'
;;
;;      Align region according to pre-defined alignment rules.
;;
;;      Foreign regexp can be used in a rule by putting an
;;      `regexp-type' attribute on the alignment rule.
;;
;;      Example)
;;
;;        (add-to-list
;;         'align-rules-list
;;         '(perl-and-ruby-hash-form
;;
;;           ;; This rule will be applied when `regexp-type'
;;           ;; is `perl' or `ruby'.
;;           (regexp-type . '(perl ruby))
;;
;;           (regexp . "([ \\t]*)=>[ \\t]*[^# \\t\\n]") ;; Foreign Regexp
;;           (group  . 1)
;;           (repeat . t)
;;           (modes  . '(perl-mode cperl-mode ruby-mode))))
;;
;;      See also `align-rules-list' and help document of an advice
;;      of `align-region' for more information about alignment rules.
;;
;; `M-s M-a REGEXP <RET>'
;; `M-x foreign-regexp/align <RET> REGEXP <RET>'
;;
;;      Align the current region using a partial foreign regexp
;;      read from the minibuffer.
;;
;;      The foreign regexp read from the minibuffer will be
;;      supposed to be placed after whitespaces.
;;
;;      See also `align-regexp'.
;;
;; `C-u M-s M-a REGEXP <RET> GROUP <RET> SPACING <RET> REPEAT <RET>'
;; `C-u M-x foreign-regexp/align <RET> REGEXP <RET> GROUP <RET> SPACING <RET> REPEAT <RET>'
;;
;;      Align the current region using an ad-hoc rule read from the minibuffer.
;;
;;      Example)
;;
;;        < Use perl-style foreign regexp in this example. >
;;
;;        When texts in region is:
;;
;;             (one 1)
;;             (ten 10)
;;             (hundred 100)
;;             (thousand 1000)
;;
;;        Run command on the region with options:
;;
;;             REGEXP: ([ \t]+)\d
;;                          |
;;                          +--- GROUP: 1
;;                               Alignment will be applied to each
;;                               lines by inserting white-spaces to
;;                               the place where the capture group
;;                               specified by `GROUP' is matched to.
;;             SPACING: 1
;;             REPEAT:  y
;;
;;        Result is:
;;
;;             (one      1)
;;             (ten      10)
;;             (hundred  100)
;;             (thousand 1000)
;;                      |
;;                      +---- Aligned using SPACING spaces.
;;
;;      See also `align-regexp'.
;;
;;
;; FOR HACKERS
;; ===========
;; You can use regexp syntax of your choice of language, if you
;; write four external commands below with the language:
;; 
;;   `foreign-regexp/replace/external-command'
;;   `foreign-regexp/occur/external-command'
;;   `foreign-regexp/search/external-command'
;;   `foreign-regexp/quote-meta/external-command'
;;
;; and install these commands with the function
;; `foreign-regexp/regexp-type/define'.
;;
;; See help documents of these variables and functions
;; for more information.
;;
;;
;; KNOWN PROBLEMS
;; ==============
;; Codes aside, this document should be rewritten.
;; My English sucks :-(
;;
;;
;; WISH LIST
;; =========
;; - History for `re-builder'.
;; - `grep' with foreign regexp?
;; - `tags-search', `tags-query-replace', `dried-do-search' and
;;   `dired-do-query-replace-regexp' with foreign regexp?
;; - `multi-isearch-buffers-regexp', `multi-occur',
;;   `multi-occur-in-matching-buffers', `how-many', `flush-lines',
;;   and `keep-lines' with foreign regexp?
;; - Better error messages.
;; - Write Tests.

;;; Change Log:

;;; Code:

(provide 'foreign-regexp)
(defconst foreign-regexp/version "0.0")


(eval-and-compile
  (require 'cl)
  (require 'menu-bar)
  (require 'easymenu))
(require 're-builder)


;;; ===========================================================================
;;;
;;;  For Debugging.
;;;
;;; ===========================================================================

(eval-and-compile
  (defvar foreign-regexp/search/debug-cache nil)
  (defvar foreign-regexp/transition/debug-advices nil))


;;; ===========================================================================
;;;
;;;  Macros
;;;
;;; ===========================================================================

(defmacro foreign-regexp/.debug (class &rest args)
  "Display debugging message when CLASS has non-nil value."
  (declare (indent 0) (debug t))
  (when (and (boundp class)
             (symbol-value class))
    `(condition-case c
         (with-current-buffer (get-buffer-create "*Messages*")
           (let ((buffer-undo-list t)
                 (orig-deactivate-mark deactivate-mark)
                 (inhibit-modification-hooks t)
                 (inhibit-point-motion-hooks t))
             (goto-char (point-max))
             (insert (format-time-string "[DEBUG %p%H:%M:%S] " (current-time)))
             (insert (format ,@args))
             (insert "\n")
             (setq deactivate-mark orig-deactivate-mark)))
       (error
        (message "%s%s\n%s"
                 (format-time-string "[DEBUG %p%H:%M:%S] " (current-time))
                 (quote ,args)
                 c)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/alambda parms &body body) => RESULT OF BODYFORM
;; ----------------------------------------------------------------------------
(defmacro* foreign-regexp/alambda (parms &body body)
  "An utility macro.
This macro allows anonymous functions to refer themselves
through a symbol `self'."
  (declare (indent 1))
  `(labels ((self ,parms ,@body))
     #'self))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/catch-case var bodyform &rest handlers) => RESULT OF BODYFORM
;;                                                           OR HANDLERS
;; ----------------------------------------------------------------------------
(defmacro foreign-regexp/catch-case (var bodyform &rest handlers)
  "Regain control when a tag is thrown.
Executes BODYFORM and returns its value if no tag is thrown.
Each element of HANDLERS looks like (TAG-NAME BODY...)
where the BODY is made of Lisp expressions.

A handler is applicable to an tag if TAG-NAME is one of
the tag names.
If an tag is thrown, the first applicable handler is run.

The car of a handler may be a list of tag names
instead of a single tag name.  Then it handles all of them.

When a handler handles an error, control returns to the
`foreign-regexp/catch-case' and it executes the handler's
BODY... with VAR bound to (TAG-NAME . THROWNED-VALUE).
Then the value of the last BODY form is returned from the
`foreign-regexp/catch-case' expression.

See also the function `throw' for more info."
  (declare (indent 2))
  (let* ((g-retval (gensym))
         (bodyform (list 'setq g-retval bodyform))
         var-lst
         clause-lst)
    (dolist (h handlers)
      (let* ((tag-lst
              (cond
               ((listp (car h)) (car h))
               (t (list (car h)))))
             (var-tag-val (gensym)))
        (dolist (tag tag-lst)
          (push (list var-tag-val `(quote ,var-tag-val)) var-lst)
          (setq bodyform
                `(setq ,var-tag-val (catch (quote ,tag)
                                      ,bodyform
                                      ,var-tag-val)))
          (push `((not (eq ,var-tag-val (quote ,var-tag-val)))
                  (let ((,var (cons (quote ,tag) ,var-tag-val))) ,@(cdr h)))
                clause-lst))))
    `(let (,g-retval ,@var-lst)
       ,bodyform
       (cond
        ,@clause-lst
        (t ,g-retval)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/read-from-minibuf/with-search-option-indicator &rest body)
;;                                                       => RESULT OF BODYFORM
;; ----------------------------------------------------------------------------
(defmacro foreign-regexp/read-from-minibuf/with-search-option-indicator (&rest body)
  "Run body with search option indicator on prompt of `read-from-minibuffer'. "
  `(unwind-protect
       (progn
         (foreign-regexp/ad-enable 'read-from-minibuffer 'around 'foreign-regexp/read-from-minibuf/with-search-option-indicator)
         (foreign-regexp/ad-activate 'read-from-minibuffer)
         ,@body)
     (foreign-regexp/ad-disable 'read-from-minibuffer 'around 'foreign-regexp/read-from-minibuf/with-search-option-indicator)
     (foreign-regexp/ad-activate 'read-from-minibuffer)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/read-from-minibuf/with-initial-contents
;;                          initial-contents &rest body) => RESULT OF BODYFORM
;; ----------------------------------------------------------------------------
(defmacro foreign-regexp/read-from-minibuf/with-initial-contents (initial-contents &rest body)
  "Run body with setting initial contents to `read-from-minibuffer'."
  (declare (indent 1))
  ;; Set initial contents for `read-from-minibuffer'.
  `(unwind-protect
       (progn
         (foreign-regexp/ad-enable 'read-from-minibuffer 'before 'foreign-regexp/read-from-minibuf/with-initial-contents)
         (foreign-regexp/ad-activate 'read-from-minibuffer)
         (let ((foreign-regexp/.initial-contents ,initial-contents))
           ,@body))
     (foreign-regexp/ad-disable 'read-from-minibuffer 'before 'foreign-regexp/read-from-minibuf/with-initial-contents)
     (foreign-regexp/ad-activate 'read-from-minibuffer)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/search/with-regarding-these-string-as-foreign-regexp
;;             (string-list &optional limit) &rest body) => RESULT OF BODYFORM
;; ----------------------------------------------------------------------------
(defmacro foreign-regexp/search/with-regarding-these-string-as-foreign-regexp (args &rest body)
  "Run BODY with applying `foreign-regexp/search/forward' and
`foreign-regexp/search/backward' to member of STRING-LIST in
substitution for `re-search-forward' and `re-search-backward'.

Note that the equivalence of the STRING are tested in `eq'.

When LIMIT is a number, match will be limited to the LIMIT.
When LIMIT is NIL, match won't be limited.

\(FN (STRING-LIST &OPTIONAL LIMIT) &REST BODY)"
  (declare (indent 1))
  (let ((g-orig-re-fwd-fn  (gensym))
        (g-orig-re-bkwd-fn (gensym))
        (g-regexp-lst      (gensym))
        (g-limit           (gensym))
        (g-args            (gensym)))
    `(lexical-let ((,g-orig-re-fwd-fn  (symbol-function 're-search-forward))
                   (,g-orig-re-bkwd-fn (symbol-function 're-search-backward)))
       (unwind-protect
           (lexical-let ((,g-regexp-lst ,(nth 0 args))
                         (,g-limit      ,(nth 1 args)))
             (setf (symbol-function 're-search-forward)
                   (lambda (regexp &optional bound noerror count)
                     (cond
                      ((memq regexp ,g-regexp-lst)
                       (funcall 'foreign-regexp/search/forward
                                regexp bound noerror count ,g-limit))
                      (t
                       (funcall ,g-orig-re-fwd-fn 
                                regexp bound noerror count)))))
             (setf (symbol-function 're-search-backward)
                   (lambda (regexp &optional bound noerror count)
                     (cond
                      ((memq regexp ,g-regexp-lst)
                       (funcall 'foreign-regexp/search/backward
                                regexp bound noerror count ,g-limit))
                      (t
                       (funcall ,g-orig-re-bkwd-fn
                                regexp bound noerror count)))))
             ,@body)
         (setf (symbol-function 're-search-forward)  ,g-orig-re-fwd-fn)
         (setf (symbol-function 're-search-backward) ,g-orig-re-bkwd-fn)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/search/with-regarding-string-as-foreign-regexp
;;                  (string &optional limit) &rest body) => RESULT OF BODYFORM
;; ----------------------------------------------------------------------------
(defmacro foreign-regexp/search/with-regarding-string-as-foreign-regexp (args &rest body)
  "Run BODY with applying `foreign-regexp/search/forward' and
`foreign-regexp/search/backward' to STRING in substitution for
`re-search-forward' and `re-search-backward'.

Note that the equivalence of the STRING are tested in `eq'.

When LIMIT is a number, match will be limited to the LIMIT.
When LIMIT is NIL, match won't be limited.

\(FN (STRING &OPTIONAL LIMIT) &REST BODY)"
  (declare (indent 1))
  `(foreign-regexp/search/with-regarding-these-string-as-foreign-regexp
    ((list ,(nth 0 args))
     ,(nth 1 args))
    ,@body))



;;; ===========================================================================
;;;
;;;  Variables and functions common to each `foreign-regexp' operation.
;;;
;;; ===========================================================================

(defcustom foreign-regexp/tmp-dir temporary-file-directory
  "Directory in which temporally files should be written."
  :type 'string
  :group 'foreign-regexp)

(defcustom foreign-regexp/tmp-file-prefix "foreign-regexp-"
  "Prefix name of temporally files."
  :type 'string
  :group 'foreign-regexp)

(defvar foreign-regexp/input-coding-system  'utf-8-unix
  "Coding system to be used for decoding files which
contains texts passed from external commands to Emacs.")

(defvar foreign-regexp/output-coding-system 'utf-8-unix
  "Coding system to be used for encoding files which
contains texts passed from Emacs to external commands.")

(defvar foreign-regexp/history  nil
  "History list for some commands that runs foreign-regexp.")

;; Search options.
;;
(defvar foreign-regexp/dot-match-a-newline-p nil
  "Non-nil if searches and muches in foreign regexp
should match a new line character by \".\".")
(make-variable-buffer-local 'foreign-regexp/dot-match-a-newline-p)

(defvar foreign-regexp/use-extended-regexp-p nil
  "Non-nil if search and much in foreign regexp
should use extended regexp.")
(make-variable-buffer-local 'foreign-regexp/use-extended-regexp-p)

(defvar foreign-regexp/eval-replacement-p nil
  "Non-nil if replace with foreign regexp
should eval replacement string.")
(make-variable-buffer-local 'foreign-regexp/eval-replacement-p)


;; ----------------------------------------------------------------------------
;;
;;  Hooks
;;
;; ----------------------------------------------------------------------------
(defvar foreign-regexp/case-fold-will-change-hook nil
  "Normal hook run before toggle `case-fold-search'.")

(defvar foreign-regexp/ext-regexp-will-change-hook nil
  "Normal hook run before toggle `foreign-regexp/use-extended-regexp-p'.")

(defvar foreign-regexp/dot-match-will-change-hook nil
  "Normal hook run before toggle `foreign-regexp/dot-match-a-newline-p'.")

(defvar foreign-regexp/eval-replacement-will-change-hook nil
  "Normal hook run before toggle `foreign-regexp/eval-replacement-p'.")

(defvar foreign-regexp/case-fold-changed-hook nil
  "Normal hook run after toggle `case-fold-search'.")

(defvar foreign-regexp/ext-regexp-changed-hook nil
  "Normal hook run after toggle `foreign-regexp/use-extended-regexp-p'.")

(defvar foreign-regexp/dot-match-changed-hook nil
  "Normal hook run after toggle `foreign-regexp/dot-match-a-newline-p'.")

(defvar foreign-regexp/eval-replacement-changed-hook nil
  "Normal hook run after toggle `foreign-regexp/use-extended-regexp-p'")


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/case-fold-available-p) => BOOL
;; ----------------------------------------------------------------------------
(defun foreign-regexp/case-fold-available-p ()
  "Test if the search option `Case Insensitive Search' is \
supported by current foreign regexp."
  (and (not (string-equal "" foreign-regexp/search-option-indicator/case-fold-str))
       (not (string-equal "" foreign-regexp/search-option-indicator/no-case-fold-str))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/ext-regexp-available-p) => BOOL
;; ----------------------------------------------------------------------------
(defun foreign-regexp/ext-regexp-available-p ()
  "Test if the search option `Extended Regular Expression' is \
supported by current foreign regexp."
  (and (not (string-equal "" foreign-regexp/search-option-indicator/ext-regexp-str))
       (not (string-equal "" foreign-regexp/search-option-indicator/no-ext-regexp-str))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/dot-match-available-p) => BOOL
;; ----------------------------------------------------------------------------
(defun foreign-regexp/dot-match-available-p ()
  "Test if the search option `. Matches a Newline' is \
supported by current foreign regexp."
  (and (not (string-equal "" foreign-regexp/search-option-indicator/dot-match-str))
       (not (string-equal "" foreign-regexp/search-option-indicator/no-dot-match-str))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/eval-replacement-available-p) => BOOL
;; ----------------------------------------------------------------------------
(defun foreign-regexp/eval-replacement-available-p ()
  "Test if the replacement option `Eval Replacement' is \
supported by current foreign regexp."
  (and (not (string-equal "" foreign-regexp/search-option-indicator/eval-replacement-str))
       (not (string-equal "" foreign-regexp/search-option-indicator/no-eval-replacement-str))))


;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/toggle-case-fold &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/toggle-case-fold (&optional no-message)
  "Toggle `case-fold-search'."
  (interactive)
  
  (when (not (foreign-regexp/case-fold-available-p))
    (error "[foreign-regexp] Search option `Case Insensitive' is not supported by current foreign regexp."))
  
  (run-hooks 'foreign-regexp/case-fold-will-change-hook)
  (cond
   (isearch-mode
    ;; FIXME: Turn off an annoying message.
    (isearch-toggle-case-fold)
    (setq case-fold-search isearch-case-fold-search))
   (t
    (setq case-fold-search
          (not case-fold-search))))
  (run-hooks 'foreign-regexp/case-fold-changed-hook)
  
  (when (not no-message)
    (minibuffer-message
     (format "[foreign-regexp] Turned %s search option `Case Insensitive'."
             (if case-fold-search "on" "off")))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/toggle-dot-match &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/toggle-dot-match (&optional no-message)
  "Toggle `foreign-regexp/dot-match-a-newline-p'."
  (interactive)
  
  (when (not (foreign-regexp/dot-match-available-p))
    (error "[foreign-regexp] Search option `. Matches a Newline' is not supported by current foreign regexp."))
  
  (run-hooks 'foreign-regexp/dot-match-will-change-hook)
  (setq foreign-regexp/dot-match-a-newline-p
        (not foreign-regexp/dot-match-a-newline-p))
  (run-hooks 'foreign-regexp/dot-match-changed-hook)
  
  (when (not no-message)
    (minibuffer-message
     (format "[foreign-regexp] Turned %s search option `. Matches newline'."
             (if foreign-regexp/dot-match-a-newline-p "on" "off")))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/toggle-ext-regexp &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/toggle-ext-regexp (&optional no-message)
  "Toggle `foreign-regexp/use-extended-regexp-p'."
  (interactive)
  
  (when (not (foreign-regexp/ext-regexp-available-p))
    (error "[foreign-regexp] Search option `Extended Regular Expression' is not supported by current foreign regexp."))
  
  (run-hooks 'foreign-regexp/ext-regexp-will-change-hook)
  (setq foreign-regexp/use-extended-regexp-p
        (not foreign-regexp/use-extended-regexp-p))
  (run-hooks 'foreign-regexp/ext-regexp-changed-hook)
  
  (when (not no-message)
    (minibuffer-message
     (format "[foreign-regexp] Turned %s search option `Extended Regexp'."
             (if foreign-regexp/use-extended-regexp-p "on" "off")))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/toggle-eval-replacement &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/toggle-eval-replacement (&optional no-message)
  "Toggle `foreign-regexp/eval-replacement-p'."
  (interactive)
  
  (when (not (foreign-regexp/eval-replacement-available-p))
    (error "[foreign-regexp] Replacement option `Eval Replacement' is not supported by current foreign regexp."))
  
  (run-hooks 'foreign-regexp/eval-replacement-will-change-hook)
  (setq foreign-regexp/eval-replacement-p
        (not foreign-regexp/eval-replacement-p))
  (run-hooks 'foreign-regexp/eval-replacement-changed-hook)
  
  (when (not no-message)
    (minibuffer-message
     (format "[foreign-regexp] Turned %s replacement option `Eval Replacement'."
             (if foreign-regexp/eval-replacement-p "on" "off")))))


;; ----------------------------------------------------------------------------
;;
;;  Utility Functions
;;
;; ----------------------------------------------------------------------------


;; ----------------------------------------------------------------------------
;;  (foreign-regexp/.format-external-command-arg arg) => STRING
;; ----------------------------------------------------------------------------
(defun foreign-regexp/.format-external-command-arg (arg)
  (cond
   ((null     arg) "")
   ((stringp  arg) arg)
   ((integerp arg) (format "%s" arg))
   ((markerp  arg) (format "%s" (marker-position arg)))
   (t
    (error "[foreign-regexp] The type of command arg `%s' is invalid." arg))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/run-external-command cmd-path shell-script body
;;                                       regexp replacement &rest other-args)
;;                                                                   => RESULT
;; ----------------------------------------------------------------------------
(defun foreign-regexp/.w32-call-process (program &optional infile buffer display &rest args)
  "On MS Windows, `call-process' won't run shell scripts properly.
This is workaround for this behavior."
  (call-process "sh" ;; XXX: Shoud we use `shell-file-name'?
                nil buffer nil "-c"
                (apply #'concat
                       (shell-quote-argument program) " "
                       (mapcar #'(lambda (arg)
                                   (concat (shell-quote-argument arg) " "))
                               args))))

(defun foreign-regexp/run-external-command (cmd-path shell-script
                                                     body
                                                     regexp
                                                     replacement
                                                     &rest other-args)
  "Run external command to execute operations regarding to search.

NOTES FOR DEVELOPERS: Variables in REPLACEMENT should be interpolated
                      on each match by external command."
  (let* ((base               (expand-file-name
                              foreign-regexp/tmp-file-prefix
                              foreign-regexp/tmp-dir))
         (fn-out-body        (make-temp-name base))
         (fn-out-regexp      (make-temp-name base))
         (fn-out-replacement (make-temp-name base))
         (fn-in-result       (make-temp-name base))
         (fn-cmd         (make-temp-name base))
         (cmd-basename       (if cmd-path
                                 (file-name-nondirectory cmd-path)
                               "shell-script"))
         (proc-output-buf    (get-buffer-create " *foreign-regexp*"))
         (cur-buf            (current-buffer))
         (orig-file-modes    (default-file-modes))
         (coding-sys-out     foreign-regexp/output-coding-system)
         (coding-sys-in      foreign-regexp/input-coding-system)
         result)
    (unwind-protect
        (progn

          (let ((enable-local-variables nil) ;; Ignore `code' variable.
                (coding-system-for-write coding-sys-out))
            
            ;; Save information, which have to be passed to 
            ;; external command, to temporally files.
            (unwind-protect 
                (progn
                  (set-default-file-modes #o0600)
                  
                  (when body
                    (with-temp-file fn-out-body
                      (insert body)))
                  (with-temp-file fn-out-regexp
                    (insert regexp))
                  (when replacement
                    (with-temp-file fn-out-replacement
                      (insert replacement))))
              (set-default-file-modes orig-file-modes))

            ;; Save shell-script to file when required.
            (when (not cmd-path)
              (with-temp-file fn-cmd
                (insert shell-script))
              (set-file-modes fn-cmd #o0700)
              (setq cmd-path fn-cmd)))
          
          ;;(message "[foreign-regexp] Running...")
          
          ;; Do search by external command.
          (let ((status (apply (if (eq window-system 'w32)
                                   #'foreign-regexp/.w32-call-process
                                 #'call-process)
                               `(,cmd-path
                                 nil ,(buffer-name proc-output-buf) nil
                                 ,@(if body (list fn-out-body) nil)
                                 ,fn-in-result
                                 ,fn-out-regexp
                                 ,@(if replacement (list fn-out-replacement) nil)
                                 ,@(mapcar #'foreign-regexp/.format-external-command-arg
                                           other-args)))))
            (when (not (and (numberp status)
                            (zerop status)))
              (error "[foreign-regexp] ERROR(%s): %s"
                     status
                     (with-current-buffer proc-output-buf
                       (buffer-substring (point-min) (point-max))))))
          
          ;;(message "[foreign-regexp] Running...done")
          
          (with-current-buffer proc-output-buf
            (when (/= (point-min) (point-max))
              (message "[foreign-regexp] messages from %s:\n%s"
                       cmd-basename
                       (buffer-substring (point-min) (point-max)))))
          
          ;; Parse result from external command.
          (let ((coding-system-for-read coding-sys-in))
            ;; Loaded data will be stored to the local variable `result'.
            (load (expand-file-name fn-in-result) nil t t))
          result)
      
      ;; Cleanup.
      (and (file-exists-p fn-out-regexp     ) (delete-file fn-out-regexp     ))
      (and (file-exists-p fn-out-replacement) (delete-file fn-out-replacement))
      (and (file-exists-p fn-out-body       ) (delete-file fn-out-body       ))
      (and (file-exists-p fn-in-result      ) (delete-file fn-in-result      ))
      (and (file-exists-p fn-cmd            ) (delete-file fn-cmd            ))
      (kill-buffer proc-output-buf))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/read-minibuf-contents) => (STRING . POSITION)
;; ----------------------------------------------------------------------------
(defun foreign-regexp/read-minibuf-contents ()
  "Returns a cons (STRING . POSITION) where
STRING is minibuffer contents and POSITION is
current cursor position in minibuffer."
  (save-excursion
    ;; Current buffer must be a *Minibuf-N*.
    (when (minibufferp (current-buffer))
      ;; Preserve current input string and cursor
      ;; position to restart `read-from-minibuffer'.
      (let* ((cur-pt (point))
             (eol-pt (progn
                       (end-of-line)
                       (point)))
             (bol-pt (progn
                       (beginning-of-line)
                       (point)))
             (offset (1+ (- cur-pt bol-pt))))
        (setq deactivate-mark nil)
        (cons (buffer-substring bol-pt eol-pt)
              offset)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/ad-enable function class name) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/ad-enable (&rest args)
  "`ad-enable-advice' runs `string-match' and breaks
current match-data as a side effect.
This is a side effect free version of `ad-enable-advice'."
  (let ((orig-match-data (match-data)))
    (apply 'ad-enable-advice args)
    (set-match-data orig-match-data)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/ad-disable function class name) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/ad-disable (&rest args)
  "`ad-enable-advice' runs `string-match' and breaks
current match-data as a side effect.
This is a side effect free version of `ad-disable-advice'."
  (let ((orig-match-data (match-data)))
    (apply 'ad-disable-advice args)
    (set-match-data orig-match-data)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/ad-activate function &optional compile) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/ad-activate (&rest args)
  "`ad-activate' runs `string-match' and breaks
current match-data as a side effect.
This is a side effect free version of `ad-activate'."
  (let ((orig-match-data (match-data)))
    (apply 'ad-activate args)
    (set-match-data orig-match-data)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/which command) => STRING or NIL
;; ----------------------------------------------------------------------------
(defun foreign-regexp/which (command)
  "Locate a program file named COMMAND.
Returns NIL when COMMAND is not found."
  (with-temp-buffer
    (call-process "which" nil (current-buffer) nil command)
    (let* ((eol
           (save-excursion
             (goto-char (point-min))
             (end-of-line)
             (point)))
           (retval (buffer-substring (point-min) eol)))
      (if (zerop (length retval)) nil retval))))


;;; ===========================================================================
;;;
;;;  A "Search Option" indicator.
;;;
;;; ===========================================================================

(defvar foreign-regexp/search-option-indicator/case-fold-str ""
  "A string displayed when the search option
`case-fold-search' is on.

Set empty string to this variable when the foreign
regexp does not support this search option.")

(defvar foreign-regexp/search-option-indicator/no-case-fold-str "Case"
  "A string displayed when the search option
`case-fold-search' is off.

Set empty string to this variable when the foreign
regexp does not support this search option.")

(defvar foreign-regexp/search-option-indicator/dot-match-str ".=~\\n"
  "A string displayed when the search option
`foreign-regexp/dot-match-a-newline-p' is on.

Set empty string to this variable when the foreign
regexp does not support this search option.")

(defvar foreign-regexp/search-option-indicator/no-dot-match-str ""
  "A string displayed when the search option
`foreign-regexp/dot-match-a-newline-p' is off.

Set empty string to this variable when the foreign
regexp does not support this search option.")

(defvar foreign-regexp/search-option-indicator/ext-regexp-str "Ext"
  "A string displayed when the search option
`foreign-regexp/use-extended-regexp-p' is on.

Set empty string to this variable when the foreign
regexp does not support this search option.")

(defvar foreign-regexp/search-option-indicator/no-ext-regexp-str ""
  "A string displayed when the search option
`foreign-regexp/use-extended-regexp-p' is off.

Set empty string to this variable when the foreign
regexp does not support this search option.")

(defvar foreign-regexp/search-option-indicator/eval-replacement-str "Eval"
  "A string displayed when the search option
`foreign-regexp/eval-replacement-p' is on.

Set empty string to this variable when the foreign
regexp does not support this replacement option.")

(defvar foreign-regexp/search-option-indicator/no-eval-replacement-str ""
  "A string displayed when the search option
`foreign-regexp/eval-replacement-p' is off.

Set empty string to this variable when the foreign
regexp does not support this replacement option.")

(defvar foreign-regexp/search-option-indicator/separator-str " "
  "A string displayed between search option strings.")


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/search-option-indicator/make-indicator) => STRING
;; ----------------------------------------------------------------------------
(defun foreign-regexp/search-option-indicator/make-indicator ()
  "Make a string which shows current search option status."
  (let ((indicator (foreign-regexp/search-option-indicator/make-indicator-aux)))
    (if (not (zerop (length indicator)))
        (concat "[" indicator "]")
      "")))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/search-option-indicator/make-indicator-aux) => STRING
;; ----------------------------------------------------------------------------
(defun foreign-regexp/search-option-indicator/make-indicator-aux ()
  "Auxiliary function of `foreign-regexp/search-option-indicator/make-indicator'."
  (reduce '(lambda (str elm)
             (concat str
                     (cond
                      ((zerop (length str)) "")
                      ((zerop (length elm)) "")
                      (t foreign-regexp/search-option-indicator/separator-str))
                     elm))
          (list ""
                (cond
                 ((and isearch-mode
                       isearch-case-fold-search)
                  foreign-regexp/search-option-indicator/case-fold-str)
                 (isearch-mode
                  foreign-regexp/search-option-indicator/no-case-fold-str)
                 (case-fold-search
                  foreign-regexp/search-option-indicator/case-fold-str)
                 (t 
                  foreign-regexp/search-option-indicator/no-case-fold-str))
                (if foreign-regexp/dot-match-a-newline-p
                    foreign-regexp/search-option-indicator/dot-match-str
                  foreign-regexp/search-option-indicator/no-dot-match-str)
                (if foreign-regexp/use-extended-regexp-p
                    foreign-regexp/search-option-indicator/ext-regexp-str 
                  foreign-regexp/search-option-indicator/no-ext-regexp-str)
                (cond
                 ((and (symbolp foreign-regexp/transition/.running-cmd)
                       (get foreign-regexp/transition/.running-cmd 'foreign-regexp/replace-command-p))
                  (if foreign-regexp/eval-replacement-p
                      foreign-regexp/search-option-indicator/eval-replacement-str
                    foreign-regexp/search-option-indicator/no-eval-replacement-str))
                 (t
                  "")))))


;;; ===========================================================================
;;;
;;;  Patches for `read-from-minibuffer'.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;
;;  Advices
;;
;; ----------------------------------------------------------------------------

;; XXX: Should be defined by flet
;;      in `foreign-regexp/read-from-minibuf/with-search-option-indicator'?
(defun foreign-regexp/.signal-option-changed ()
  (let ((contents (foreign-regexp/read-minibuf-contents)))
    (when contents
      (throw 'foreign-regexp/.option-changed
             contents))))

(defadvice read-from-minibuffer (around foreign-regexp/read-from-minibuf/with-search-option-indicator
                                        (prompt &optional initial-contents keymap read
                                                hist defalut-value inherit-input-method))
  "Run `read-from-minibuffer' with displaying search option indicator
on prompt string.

Search option indicator will be updated whenever options are changed via
commands `foreign-regexp/toggle-case-fold',
`foreign-regexp/toggle-dot-match' and
`foreign-regexp/toggle-ext-regexp'.

This advice will be enabled by `foreign-regexp/query-replace'
and `foreign-regexp/occur'."
  ;; Preserve current foreign-regexp/*-will-change-hooks.
  (let ((foreign-regexp/case-fold-will-change-hook        foreign-regexp/case-fold-will-change-hook)
        (foreign-regexp/dot-match-will-change-hook        foreign-regexp/dot-match-will-change-hook)
        (foreign-regexp/ext-regexp-will-change-hook       foreign-regexp/ext-regexp-will-change-hook)
        (foreign-regexp/eval-replacement-will-change-hook foreign-regexp/eval-replacement-will-change-hook)
        (orig-prompt      (copy-sequence prompt))
        (initial-contents initial-contents))
    (unwind-protect
        (progn
          ;; Do not call this `read-from-minibuffer' recursively.
          (foreign-regexp/ad-disable 'read-from-minibuffer 'around 'foreign-regexp/read-from-minibuf/with-search-option-indicator)
          (foreign-regexp/ad-activate 'read-from-minibuffer)
          
          ;; Do not toggle search options of *Minibuf-N* while reading
          ;; regexps, toggle re-options of CURRENT BUFFER instead.
          (lexical-let ((cur-buf (current-buffer)))
            (mapcar
             (lambda (lst)
               (lexical-let ((hook (nth 0 lst)) (toggle-fn (nth 1 lst)))
                 (add-hook hook
                           (foreign-regexp/alambda ()
                                                   (with-current-buffer cur-buf
                                                     (progv (list hook) (list (symbol-value hook))
                                                       ;; Do not call this hook recursively.
                                                       (remove-hook hook #'self)
                                                       ;; Run `foreign-regexp/toggle-*' in current buffer.
                                                       (funcall toggle-fn t)))
                                                   ;; Exit from function `foreign-regexp/toggle-*'
                                                   ;; in *Minibuf-N*.
                                                   (foreign-regexp/.signal-option-changed)))))
             '((foreign-regexp/case-fold-will-change-hook        foreign-regexp/toggle-case-fold)
               (foreign-regexp/dot-match-will-change-hook        foreign-regexp/toggle-dot-match)
               (foreign-regexp/ext-regexp-will-change-hook       foreign-regexp/toggle-ext-regexp)
               (foreign-regexp/eval-replacement-will-change-hook foreign-regexp/toggle-eval-replacement))))
          
          ;; Whenever search option is changed, restart `read-from-minibuffer' to
          ;; redisplay prompt.
          (while (setq initial-contents
                       (foreign-regexp/catch-case data
                                                  (progn
                                                    ;; Put indicator on prompt.
                                                    (setq prompt
                                                          (concat (substring orig-prompt
                                                                             0
                                                                             (string-match "\\( with\\)?: $" orig-prompt))
                                                                  (foreign-regexp/search-option-indicator/make-indicator)
                                                                  (match-string 1 orig-prompt)
                                                                  ": "))
                                                    ;; Call read-from-minibuffer.
                                                    ad-do-it
                                                    ;; Break when nothing has been thrown.
                                                    nil)
                                                  (foreign-regexp/.option-changed
                                                   ;; initial-contents is thrown with
                                                   ;; a tag `foreign-regexp/.option-changed'.
                                                   (cdr data))))))
      (foreign-regexp/ad-enable 'read-from-minibuffer 'around 'foreign-regexp/read-from-minibuf/with-search-option-indicator)
      (foreign-regexp/ad-activate 'read-from-minibuffer))))
(foreign-regexp/ad-disable 'read-from-minibuffer
                           'around
                           'foreign-regexp/read-from-minibuf/with-search-option-indicator)
(foreign-regexp/ad-activate 'read-from-minibuffer)

(defadvice read-from-minibuffer (before foreign-regexp/read-from-minibuf/with-initial-contents
                                        (prompt &optional initial-contents keymap read
                                                hist default-value inherit-input-method))
  "Set value of a variable `foreign-regexp/.initial-contents'
to INITIAL-CONTENTS of `read-from-minibuffer'.

Value should be assigned to `foreign-regexp/.initial-contents'
by caller function with `LET' form.

This advice will be enabled by `foreign-regexp/re-builder/query-replace-on-target-buffer'
and `foreign-regexp/re-builder/occur-on-target-buffer'."
  (foreign-regexp/ad-disable 'read-from-minibuffer 'before 'foreign-regexp/read-from-minibuf/with-initial-contents)
  (foreign-regexp/ad-activate 'read-from-minibuffer)
  
  (when (and (boundp 'foreign-regexp/.initial-contents)
             (stringp foreign-regexp/.initial-contents))
    (setq initial-contents
          (cons foreign-regexp/.initial-contents
                (1+ (length foreign-regexp/.initial-contents)))))
  
  (setq foreign-regexp/.initial-contents nil))
(foreign-regexp/ad-disable 'read-from-minibuffer
                           'before
                           'foreign-regexp/read-from-minibuf/with-initial-contents)
(foreign-regexp/ad-activate 'read-from-minibuffer)


;;; ===========================================================================
;;;
;;;  Menu Definition.
;;;
;;; ===========================================================================

(defvar foreign-regexp/menu/use-menu-p t
  "A flag if menu items for foreign regexp will be installed or not.")


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/menu/install) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/menu/install ()
  "Install menu items for foreign regexp."
  (easy-menu-add-item menu-bar-search-menu
                      nil
                      ["Foreign Regexp Forward..." foreign-regexp/non-incremental/search-forward
                       :active (foreign-regexp/non-incremental/available-p)
                       :help "Search forward for a foreign regular expression"]
                      "separator-repeat-search")
  (easy-menu-add-item menu-bar-search-menu
                      nil
                      ["Foreign Regexp Backward..." foreign-regexp/non-incremental/search-backward
                       :active (foreign-regexp/non-incremental/available-p)
                       :help "Search backward for a foreign regular expression"]
                      "separator-repeat-search")
  
  (easy-menu-add-item menu-bar-i-search-menu
                      nil
                      ["Forward Foreign Regexp..." foreign-regexp/isearch-forward
                       :active (foreign-regexp/isearch/available-p)
                       :help "Search forward for a foreign regular expression as you type it"]
                      nil)

  (easy-menu-add-item menu-bar-i-search-menu
                      nil
                      ["Backward Foreign Regexp..." foreign-regexp/isearch-backward
                       :active (foreign-regexp/isearch/available-p)
                       :help "Search backwards for a foreign regular expression as you type it"]
                      "separator-repeat-search")

  (easy-menu-add-item menu-bar-replace-menu
                      nil
                      ["Replace Foreign Regexp..." foreign-regexp/query-replace
                       :active (foreign-regexp/replace/available-p)
                       :help "Replace foreign regular expression interactively, ask about each occurrence"]
                      "separator-replace-tags")

  (easy-menu-add-item menu-bar-edit-menu
                      nil
                      '("Foreign Regexp Options"
                        ("Regexp Type"
                         :filter (lambda (&rest args)
                                   (foreign-regexp/menu/regexp-type-menu-gen)))
                        ["--" 'ignore]
                        ["Case Insensitive" foreign-regexp/toggle-case-fold
                         :style    radio
                         :selected (and (foreign-regexp/case-fold-available-p)
                                        (if isearch-mode
                                            (insert )search-case-fold-search
                                            case-fold-search))
                         :active   (foreign-regexp/case-fold-available-p)]
                        [". Matches a Newline" foreign-regexp/toggle-dot-match
                         :style    radio
                         :selected (and (foreign-regexp/dot-match-available-p)
                                        foreign-regexp/dot-match-a-newline-p)
                         :active   (foreign-regexp/dot-match-available-p)]
                        ["Extended Regular Expression" foreign-regexp/toggle-ext-regexp
                         :style    radio
                         :selected (and (foreign-regexp/ext-regexp-available-p)
                                        foreign-regexp/use-extended-regexp-p)
                         :active   (foreign-regexp/ext-regexp-available-p)]
                        ;;["--" 'ignore] ;; XXX: This makes `Eval Replacement' always active...
                        ["Eval Replacement" foreign-regexp/toggle-eval-replacement
                         :style    radio
                         :selected (and (foreign-regexp/eval-replacement-available-p)
                                        foreign-regexp/eval-replacement-p)
                         :active   (foreign-regexp/eval-replacement-available-p)])
                      "goto")

  ;; XXX: Should be removed?
  (easy-menu-add-item menu-bar-edit-menu
                      nil
                      ["--" 'ignore]
                      "goto")


  ;; Override defalut menu items.
  ;;
  (define-key menu-bar-search-menu [repeat-search-fwd]
    `(menu-item ,(purecopy "Repeat Forward") nonincremental-repeat-search-forward
                :enable (or (and (eq menu-bar-last-search-type 'string)
                                 search-ring)
                            (and (eq menu-bar-last-search-type 'regexp)
                                 regexp-search-ring)
                            ;; For foreign regexp search.
                            (and (eq menu-bar-last-search-type 'foreign-regexp)
                                 foreign-regexp/history
                                 (foreign-regexp/non-incremental/available-p)))
                :help ,(purecopy "Repeat last search forward")))
  (define-key menu-bar-search-menu [repeat-search-back]
    `(menu-item ,(purecopy "Repeat Backwards") nonincremental-repeat-search-backward
                :enable (or (and (eq menu-bar-last-search-type 'string)
                                 search-ring)
                            (and (eq menu-bar-last-search-type 'regexp)
                                 regexp-search-ring)
                            ;; For foreign regexp search.
                            (and (eq menu-bar-last-search-type 'foreign-regexp)
                                 foreign-regexp/history
                                 (foreign-regexp/non-incremental/available-p)))
                :help ,(purecopy "Repeat last search backwards"))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/menu/regexp-type-menu-gen &rest args) => MENU ITEMS
;; ----------------------------------------------------------------------------
(defun foreign-regexp/menu/regexp-type-menu-gen (&rest args)
  ""
  (mapcar (lambda (pair)
            (let ((kv-lst (cdr pair)))
              (vector
               ;; NAME
               (concat (cadr (memq :tag kv-lst)) "")
               ;; CALLBACK
               `(lambda (&rest args)
                  (interactive)
                  (foreign-regexp/regexp-type/set (quote ,(cadr (memq :name kv-lst))))) 
               :style 'radio
               :selected (eq (and (boundp 'foreign-regexp/regexp-type) ;; For compiler
                                  foreign-regexp/regexp-type)
                             (cadr (memq :name kv-lst)))
               :active   t
               :help "dummy")))
          `((nil :name nil :tag "None")
            ,@(let ((alst (copy-list
                           (and (boundp 'foreign-regexp/regexp-type/.type-alst) ;; For compiler
                                foreign-regexp/regexp-type/.type-alst))))
                (sort alst
                      (lambda (pair1 pair2)
                        (string< (format "%s" (car pair1))
                                 (format "%s" (car pair2)))))))))


;; ----------------------------------------------------------------------------
;;
;;  Main
;;
;; ----------------------------------------------------------------------------

(when foreign-regexp/menu/use-menu-p
  (foreign-regexp/menu/install))


;;; ===========================================================================
;;;
;;;  Key Bindings Definition.
;;;
;;; ===========================================================================

(defvar foreign-regexp/key-bindings/use-keybindings-p t
  "A flag if key bindings for foreign regexp will be installed or not.")

;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/key-bindings/install) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/key-bindings/install ()
  "Install key bindings for foreign regexp."
  
  ;; For Emacs < 23.
  (when (string< emacs-version "23")
    (defvar search-map (make-sparse-keymap)
      "Keymap for search related commands.")
    (define-key esc-map "s" search-map))
  
  (defvar reb-mode-search-map  (make-sparse-keymap)
    "Keymap for search related commands.")
  
  ;; keys for `foreign-regexp' commands.
  ;;
  (when (boundp 'search-map) ;; For compiler
    
    ;; Prefix keys.
    ;;
    (define-key isearch-mode-map    "\M-s"  search-map)
    (define-key reb-mode-map        "\M-s"  reb-mode-search-map)
    
    ;; Keys for `foreign-regexp' commands.
    ;;
    (define-key search-map          "\M-o"  'foreign-regexp/occur)
    (define-key search-map          "\M-%"  'foreign-regexp/query-replace)
    (define-key search-map          "\M-s"  'foreign-regexp/isearch-forward)
    (define-key search-map          "\M-r"  'foreign-regexp/isearch-backward)
    (define-key search-map          "\M-f"  'foreign-regexp/non-incremental/search-forward)
    (define-key search-map          "\M-F"  'foreign-regexp/non-incremental/search-backward)
    (define-key search-map          "\M-g"  'nonincremental-repeat-search-forward)
    (define-key search-map          "\M-G"  'nonincremental-repeat-search-backward)
    (define-key search-map          "\M-a"  'foreign-regexp/align)
    (define-key search-map          "\M-l"  're-builder)
    (define-key search-map          "\M-\\" 'foreign-regexp/quote-meta-in-region)
    
    (define-key reb-mode-search-map "\M-o"  'foreign-regexp/re-builder/occur-on-target-buffer)
    (define-key reb-mode-search-map "\M-%"  'foreign-regexp/re-builder/query-replace-on-target-buffer)
    (define-key reb-mode-search-map "\M-s"  'foreign-regexp/re-builder/isearch-forward-on-target-buffer)
    (define-key reb-mode-search-map "\M-r"  'foreign-regexp/re-builder/isearch-backward-on-target-buffer)
    (define-key reb-mode-search-map "\M-f"  'foreign-regexp/re-builder/non-incremental-search-forward-on-target-buffer)
    (define-key reb-mode-search-map "\M-F"  'foreign-regexp/re-builder/non-incremental-search-backward-on-target-buffer)
    (define-key reb-mode-search-map "\M-g"  'foreign-regexp/re-builder/non-incremental-search-forward-on-target-buffer)
    (define-key reb-mode-search-map "\M-G"  'foreign-regexp/re-builder/non-incremental-search-backward-on-target-buffer)
    (define-key reb-mode-search-map "\M-a"  'foreign-regexp/re-builder/align-on-target-buffer)
    (define-key reb-mode-search-map "\M-l"  're-builder)
    (define-key reb-mode-search-map "\M-\\" 'foreign-regexp/quote-meta-in-region)
    
    ;; Keys for changing `foreign-regexp' options.
    ;;
    (define-key search-map          "\M-i" 'foreign-regexp/toggle-case-fold)
    (define-key search-map          "\M-m" 'foreign-regexp/toggle-dot-match)
    (define-key search-map          "\M-x" 'foreign-regexp/toggle-ext-regexp)
    (define-key search-map          "\M-e" 'foreign-regexp/toggle-eval-replacement)
    
    (define-key reb-mode-search-map "\M-i" 'foreign-regexp/re-builder/toggle-case-fold-on-target-buffer)
    (define-key reb-mode-search-map "\M-m" 'foreign-regexp/re-builder/toggle-dot-match-on-target-buffer)
    (define-key reb-mode-search-map "\M-x" 'foreign-regexp/re-builder/toggle-ext-regexp-on-target-buffer)
    
    ;; XXX: This overrides a key for `next-matching-history-element'
    ;;      in `minibuffer-local-map', so please rebind it to a
    ;;      key you like.
    (define-key minibuffer-local-map "\M-s" search-map)))


;; ----------------------------------------------------------------------------
;;
;;  Main
;;
;; ----------------------------------------------------------------------------

(when foreign-regexp/key-bindings/use-keybindings-p
  (foreign-regexp/key-bindings/install))


;;; ===========================================================================
;;;
;;;  `query-replace' in foreign regexp with a help from external command.
;;;
;;; ===========================================================================

(defvar foreign-regexp/replace/external-command nil
  "Path of an external command which executes actual search/replace
operation.

Twelve arguments describe below will be passed to the command.

  1st: Path of a file which contains the text to which the
       search/replace operation will be applied.
       
       The text in this file is encoded in the value of
       `foreign-regexp/output-coding-system'.
       
  2nd: Path of a file to which the command should write the result
       of current search operation.
       
       The external command have to output a form like:
       
         (setq result 
               (
                ;; Search results by non warp-around search.
                (((1st-MATCH-START 1st-MATCH-END
                   1st-MATCH-CAPTURE-0-START 1st-MATCH-CAPTURE-0-END
                   1st-MATCH-CAPTURE-1-START 1st-MATCH-CAPTURE-1-END
                   ...)
                  \"REPLACEMENT-FOR-1st-MATCH\")
                 ((2nd-MATCH-START 2nd-MATCH-END
                   2nd-MATCH-CAPTURE-0-START 2nd-MATCH-CAPTURE-0-END
                   2nd-MATCH-CAPTURE-1-START 2nd-MATCH-CAPTURE-1-END
                   ...)
                  \"REPLACEMENT-FOR-2nd-MATCH\")
                 ...)
                ;; Search results by warp-around search.
                (...
                 ((Nth-MATCH-START Nth-MATCH-END
                   Nth-MATCH-CAPTURE-0-START Nth-MATCH-CAPTURE-0-END
                   Nth-MATCH-CAPTURE-1-START Nth-MATCH-CAPTURE-1-END
                   ...)
                  \"REPLACEMENT-FOR-Nth-MATCH\"))))
       
       to this file.
       
       Note that each start and end position in the form should be
       an offset from beginning of the text which has been searched.
       (This means each number should be started from 0, not from 1)
       
       The text in this file must be encoded in the value of
       `foreign-regexp/input-coding-system'.
       
  3rd: Path of a file in which the regexp we want to search is written.
       The command have a responsibility to search this regexp
       from the file specified by 1st argument, then write start and
       end positions of each match to the file specified by 2nd argument.
       
       The text in this file is encoded in the value of
       `foreign-regexp/output-coding-system'.
       
  4th: Path of a file in which the replacement expression is written.
       The command have a responsibility to interpolate variables
       in the expression on each match, or evaluate it when the 8th
       argument is not empty string or non-nil, then write them to
       the file specified by 2nd argument.
 
       The text in this file is encoded in the value of
       `foreign-regexp/output-coding-system'.
       
  5th: A dot matches newline flag.
       When the value of this flag is not empty string or non-nil,
       the character `.' should be matched to a newline character.
       
  6th: A case sensitive flag.
       When the value of this flag is not empty string or non-nil,
       the match operation should be done case-sensitively.
       
  7th: An extended regular expression flag.
       When the value of this flag is not empty string or non-nil,
       the current search regexp (see 3rd argument) should be
       interpreted as extended regular expression.
       
  8th: An eval replacement as expression flag.
       When the value of this flag is not empty string or non-nil,
       replacement expression given by 3rd argument will
       be evaluated as an expression when the foreign regexp
       supports this feature.
       
  9th: The number of search limit.
       The search/replace operation will be stopped when the count
       of the match is reached to this number.
       When the value of this argument is empty string or `NIL',
       the search/replacement operation will be applied unlimited
       
 10th: Current point. (start from zero)
       When the value of this argument is empty string or `NIL',
       the search/replacement operation will be stated from
       beginning of the text.

 11th: Start point of region that the search/replace operation
       should be applied. (start from zero)
       When this value is empty string or nil, the search will be
       applied to whole of the text.

 12th: End point of region that the search/replace operation
       should be applied. (start from zero)
       When this value is empty string or nil, the search will be
       applied to whole of the text.
")

(defvar foreign-regexp/replace/shell-script nil
  "A shell script which will be run as 
`foreign-regexp/replace/external-command'
when it has nil value.")


(defvar foreign-regexp/replace/defaults nil
  "Default values of FROM-STRING and TO-STRING for `foreign-regexp/query-replace'.

See also `query-replace-defaults'.")


;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/query-replace regexp replacement
;;                              &optional delimited start end) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/query-replace (regexp replacement &optional delimited start end)
  "Do `query-replace' with a help from external command.

See `isearch-forward-regexp' and `isearch-backward-regexp' for
more information."
  (interactive
   (and
    (foreign-regexp/replace/assert-available)
    (let ((common
           (let ((query-replace-from-history-variable 'foreign-regexp/history)
                 (query-replace-to-history-variable   'foreign-regexp/history)
                 (query-replace-defaults              foreign-regexp/replace/defaults))
             (foreign-regexp/read-from-minibuf/with-search-option-indicator
              (prog1 (query-replace-read-args
                      (concat "Query replace foreign regexp"
                              (if (and transient-mark-mode mark-active) " in region" ""))
                      t)
                (setq foreign-regexp/replace/defaults query-replace-defaults))))))
      (list (nth 0 common) (nth 1 common) (nth 2 common)
            ;; These are done separately here
            ;; so that command-history will record these expressions
            ;; rather than the values they had this time.
            (if (and transient-mark-mode mark-active)
                (region-beginning))
            (if (and transient-mark-mode mark-active)
                (region-end))))))
  (foreign-regexp/replace/assert-available)
  (foreign-regexp/replace/perform-replace
   regexp replacement t nil nil nil nil start end))
(put 'foreign-regexp/query-replace 'foreign-regexp/replace-command-p t)

;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/replace/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defun foreign-regexp/replace/available-p ()
  "Test if external command or shell script is defined or not."
  (or foreign-regexp/replace/external-command
      foreign-regexp/replace/shell-script))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/replace/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defun foreign-regexp/replace/assert-available ()
  "Raise error when no external command or shell script is defined."
  (or (foreign-regexp/replace/available-p)
      (error "[foreign-regexp] No external command or shell script is defined for replace.")))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/replace/search-by-external-command regexp replacement)
;;                                                                     => LIST
;; ----------------------------------------------------------------------------
(defun foreign-regexp/replace/search-by-external-command (regexp replacement min max &optional limit)
  "Scan current buffer with external command to detect matching
texts by REGEXP.

Overlays will be made on each matched text, and they will be
saved to the variable `foreign-regexp/replace/ovs-on-match/data'.

Variables in REPLACEMENT will be interpolated
on each match, and will be saved to the property
foreign-regexp/replace/replacement of each overlay.

Returns position of the neighborhood overlay of a pointer in
the list `foreign-regexp/replace/ovs-on-match/data'."
  (let* ((offset (point-min))
         (result (foreign-regexp/run-external-command
                  foreign-regexp/replace/external-command
                  foreign-regexp/replace/shell-script
                  (buffer-substring (point-min) (point-max))
                  regexp
                  replacement
                  (if foreign-regexp/dot-match-a-newline-p "DOT" "")
                  (if case-fold-search "" "CASE")
                  (if foreign-regexp/use-extended-regexp-p "EXT" "")
                  (if foreign-regexp/eval-replacement-p "EVAL" "")
                  limit
                  (- (point) offset)
                  (and min (- min offset))
                  (and max (- max offset)))))
    (foreign-regexp/replace/parse-search-result result offset)
    
    ;; Detect index of neighborhood overlay of a pointer.
    (position (car (member-if
                    #'(lambda (ov)
                        (<= (point) (overlay-start ov)))
                    (foreign-regexp/replace/ovs-on-match/get-all)))
              (foreign-regexp/replace/ovs-on-match/get-all))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/replace/parse-search-result result offset) => OVERLAYS
;; ----------------------------------------------------------------------------
(defun foreign-regexp/replace/parse-search-result (result offset)
  "Subroutine of `foreign-regexp/replace/search-by-external-command'."
  (foreign-regexp/replace/ovs-on-match/dispose)
  ;; RESULT is a list which has a structure like below:
  ;; (
  ;;  ;; Search results by non warp-around search.
  ;;  (((1st-MATCH-START 1st-MATCH-END
  ;;     1st-MATCH-CAPTURE-0-START 1st-MATCH-CAPTURE-0-END
  ;;     1st-MATCH-CAPTURE-1-START 1st-MATCH-CAPTURE-1-END
  ;;     ...)
  ;;    "REPLACEMENT-FOR-1st-MATCH")
  ;;   ((2nd-MATCH-START 2nd-MATCH-END
  ;;     2nd-MATCH-CAPTURE-0-START 2nd-MATCH-CAPTURE-0-END
  ;;     2nd-MATCH-CAPTURE-1-START 2nd-MATCH-CAPTURE-1-END
  ;;     ...)
  ;;    "REPLACEMENT-FOR-2nd-MATCH")
  ;;   ...)
  ;;  ;; Search results by warp-around search.
  ;;  (...
  ;;   ((Nth-MATCH-START Nth-MATCH-END
  ;;     Nth-MATCH-CAPTURE-0-START Nth-MATCH-CAPTURE-0-END
  ;;     Nth-MATCH-CAPTURE-1-START Nth-MATCH-CAPTURE-1-END
  ;;     ...)
  ;;    "REPLACEMENT-FOR-Nth-MATCH")))
  ;;
  ;; NOTE: Special variables in "REPLACEMENT"
  ;;       have to be expanded by external command.
  (save-excursion
    (let ((data    nil)
          (cur-buf (current-buffer)))
      ;;(message "[foreign-regexp] Parsing search results from external command...")
      (dolist (lst (reverse result))
        (dolist (item lst)
          (let* ((pos-lst     (nth 0 item))
                 (beg         (+ (nth 0 pos-lst) offset))
                 (end         (+ (nth 1 pos-lst) offset))
                 (replacement (nth 1 item)))
            (foreign-regexp/replace/ovs-on-match/add beg end cur-buf replacement))))
      ;;(message "[foreign-regexp] Parsing search results from external command...done")
      )))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/replace/perform-replace (from-string replacement
;;                                         query-flag ignore ignore
;;                                         &optional ignore map start end)
;;                                                                     => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/replace/perform-replace (from-string replacement
                                                           query-flag
                                                           ignore/regexp-flag
                                                           ignore/delimited-flag
                                                           &optional
                                                           ignore/repeat-count
                                                           map start end)
  "Replacement of `perform-replace' for foreign regexp.

Note that \"\\?\" in string is not supported like
original `perform-replace' does.

Also list in REPLACEMENT and REPEAT-COUNT are not supported."
  ;; Based on `perform-replace'.

  ;; XXX: The overlays `ovs-on-match', that looks like lazy-highlight,
  ;;      should be updated by `foreign-regexp/replace/search-by-external-command'
  ;;      whenever the function `replace-match' is called, but it is little
  ;;      bit annoying so we don't update them so much often here.
  (or map (setq map query-replace-map))
  (and query-flag minibuffer-auto-raise
       (raise-frame (window-frame (minibuffer-window))))
  (let* ((search-string from-string)
         (real-match-data nil)       ; The match data for the current match.
         (next-replacement nil)
         (keep-going t)
         (stack nil)
         (replace-count 0)
         (multi-buffer nil)
         (recenter-last-op nil)      ; Start cycling order with initial position.
         
         (min nil)
         ;; If non-nil, it is marker saying where in the buffer to stop.
         (max nil)
         ;; Data for the next match.  If a cons, it has the same format as
         ;; (match-data); otherwise it is t if a match is possible at point.
         (match-again t)
         (message
          (if query-flag
              (apply 'propertize
                     (substitute-command-keys
                      "Query replacing %s with %s: (\\<query-replace-map>\\[help] for mini buffer help) ")
                     minibuffer-prompt-properties)))
         (idx nil)
         (regexp-flag t)
         real-replacement)

    (cond
     ((stringp replacement)
      (setq real-replacement replacement
            replacement     nil))
     (t
      (error "[foreign-regexp] REPLACEMENT must be a string.")))

    ;; If region is active, in Transient Mark mode, operate on region.
    (when start
      (setq max (copy-marker (max start end)))
      (goto-char (setq min (min start end)))
      (deactivate-mark))

    ;; Do search by external command and detect index of
    ;; neighborhood overlay of a pointer.
    (setq idx (foreign-regexp/replace/search-by-external-command
               from-string
               real-replacement
               min max))

    (push-mark)
    (undo-boundary)
    (unwind-protect
        (flet ((update-real-match-data (idx)
                                       (setq real-match-data
                                             (let ((ov (foreign-regexp/replace/ovs-on-match/get-nth idx)))
                                               (when ov
                                                 (list (overlay-start ov)
                                                       (overlay-end   ov)))))
                                       (set-match-data real-match-data)
                                       real-match-data)
               (update-next-replacement (idx)
                                        (setq next-replacement
                                              (overlay-get (foreign-regexp/replace/ovs-on-match/get-nth idx)
                                                           'foreign-regexp/replace/replacement))))
          ;; Loop finding occurrences that perhaps should be replaced.
          (while (and idx
                      keep-going
                      (not (or (eobp) (and max (<= max (point)))))
                      (update-real-match-data idx)
                      (not (and max (< max (match-end 0)))))

            ;; Optionally ignore matches that have a read-only property.
            (unless (and query-replace-skip-read-only
                         (text-property-not-all
                          (nth 0 real-match-data) (nth 1 real-match-data)
                          'read-only nil))

              ;; Calculate the replacement string, if necessary.
              (set-match-data real-match-data)
              (update-next-replacement idx)
              
              (if (not query-flag)
                  (progn
                    (foreign-regexp/replace/highlight/dispose)
                    (replace-match next-replacement t t)
                    (setq replace-count (1+ replace-count)))
                (undo-boundary)
                (let (done replaced key def)
                  ;; Loop reading commands until one of them sets done,
                  ;; which means it has finished handling this
                  ;; occurrence.  Any command that sets `done' should
                  ;; leave behind proper match data for the stack.
                  ;; Commands not setting `done' need to adjust
                  ;; `real-match-data'.
                  (while (not done)
                    (set-match-data real-match-data)
                    (foreign-regexp/replace/highlight/put
                     (match-beginning 0) (match-end 0))
                    (goto-char (match-end 0))
                    ;; Bind message-log-max so we don't fill up the message log
                    ;; with a bunch of identical messages.
                    (let ((message-log-max nil)
                          (replacement-presentation next-replacement))
                      (message message
                               (query-replace-descr from-string)
                               (query-replace-descr replacement-presentation)))
                    (setq key (read-event))
                    ;; Necessary in case something happens during read-event
                    ;; that clobbers the match data.
                    (set-match-data real-match-data)
                    (setq key (vector key))
                    (setq def (lookup-key map key))
                    ;; Restore the match data while we process the command.
                    (cond ((eq def 'help)
                           (with-output-to-temp-buffer "*Help*"
                             (princ
                              (concat "Query replacing "
                                      (if regexp-flag "regexp " "")
                                      from-string " with "
                                      next-replacement ".\n\n"
                                      (substitute-command-keys
                                       query-replace-help)))
                             (with-current-buffer standard-output
                               (help-mode))))
                          ((eq def 'exit)
                           (setq keep-going nil)
                           (setq done t))
                          ((eq def 'exit-current)
                           (setq multi-buffer t keep-going nil done t))
                          ((eq def 'backup)
                           ;; XXX: The behavior is different from original
                           ;;      `perform-replace' because we don't update
                           ;;      `ovs-on-match' after `replace-match' operation
                           ;;      because of performance issue.
                           (if stack
                               (let ((elt (pop stack)))
                                 (goto-char (nth 0 elt))
                                 (setq idx (1- idx))
                                 (update-next-replacement idx)
                                 (update-real-match-data idx)
                                 (setq replaced (nth 1 elt)
                                       real-match-data
                                       (replace-match-data
                                        t real-match-data
                                        (nth 2 elt))))
                             (message "No previous match")
                             (ding 'no-terminate)
                             (sit-for 1)))
                          ((eq def 'act)
                           (or replaced
                               (replace-match next-replacement t t)
                               (setq replace-count (1+ replace-count)))
                           (setq done t replaced t))
                          ((eq def 'act-and-exit)
                           (or replaced
                               (replace-match next-replacement t t)
                               (setq replace-count (1+ replace-count)))
                           (setq keep-going nil)
                           (setq done t replaced t))
                          ((eq def 'act-and-show)
                           (when (not replaced)
                             (replace-match next-replacement t t)
                             (setq replace-count (1+ replace-count)
                                   real-match-data (replace-match-data
                                                    t real-match-data)
                                   replaced t)))
                          ((or (eq def 'automatic) (eq def 'automatic-all))
                           (when (not replaced)
                             (replace-match next-replacement t t)
                             (setq replace-count (1+ replace-count)))
                           (setq done t query-flag nil replaced t))
                          ((eq def 'skip)
                           (setq done t))
                          ((eq def 'recenter)
                           ;; `this-command' has the value `query-replace',
                           ;; so we need to bind it to `recenter-top-bottom'
                           ;; to allow it to detect a sequence of `C-l'.
                           (let ((this-command 'recenter-top-bottom)
                                 (last-command 'recenter-top-bottom))
                             (recenter-top-bottom)))

                          ;; Recursive-edit.
                          ((eq def 'edit)
                           (let ((opos (save-excursion
                                         (progn
                                           (goto-char (match-beginning 0))
                                           (point-marker)))))
                             (setq real-match-data (replace-match-data
                                                    nil real-match-data
                                                    real-match-data))
                             (goto-char (match-beginning 0))
                             (save-excursion
                               (save-window-excursion
                                 (recursive-edit)))
                             (goto-char opos)
                             (set-marker opos nil))
                           (setq idx (foreign-regexp/replace/search-by-external-command
                                      from-string
                                      next-replacement
                                      min max))
                           (update-next-replacement idx)
                           (update-real-match-data idx))
                          
                          ;; Edit replacement.
                          ((eq def 'edit-replacement)
                           (let ((opos (save-excursion
                                         (progn
                                           (goto-char (if replaced
                                                          (match-end 0) ;;After backup?
                                                        (match-beginning 0)))
                                           (point-marker)))))
                             (setq real-match-data (replace-match-data
                                                    nil real-match-data
                                                    real-match-data))
                             (setq real-replacement
                                   (read-string "Edit replacement string: "
                                                real-replacement))
                             (goto-char opos)
                             (set-marker opos nil))
                           (setq idx (foreign-regexp/replace/search-by-external-command
                                      from-string
                                      real-replacement
                                      min max))
                           (update-next-replacement idx)
                           (update-real-match-data idx)
                           
                           (if replaced
                               (setq idx (and idx (1- idx)))
                             (replace-match next-replacement t t)
                             (setq replace-count (1+ replace-count)))
                           (setq done t replaced t))

                          ;; Delete matched string then recursive-edit.
                          ((eq def 'delete-and-edit)
                           (let ((opos (save-excursion
                                         (progn
                                           (goto-char (match-end 0))
                                           (point-marker)))))
                             (set-marker-insertion-type opos t)
                             
                             (replace-match "" t t)
                             (setq real-match-data (replace-match-data
                                                    nil real-match-data))
                             (foreign-regexp/replace/ovs-on-match/dispose)
                             (foreign-regexp/replace/highlight/dispose)
                             
                             (save-excursion (recursive-edit))

                             (goto-char opos)
                             (set-marker opos nil))
                           (setq idx (foreign-regexp/replace/search-by-external-command
                                      from-string
                                      real-replacement
                                      min max))
                           (if (numberp idx)
                               (setq idx (1- idx)) ;;Do not forward current match.
                             (setq idx (foreign-regexp/replace/ovs-on-match/get-count))) ;;Done.
                           
                           (setq replaced t))

                          ;; Note: we do not need to treat `exit-prefix'
                          ;; specially here, since we reread
                          ;; any unrecognized character.
                          (t
                           (setq this-command 'mode-exited)
                           (setq keep-going nil)
                           (setq unread-command-events
                                 (append (listify-key-sequence key)
                                         unread-command-events))
                           (setq done t)))

                    (unless (eq def 'recenter)
                      ;; Reset recenter cycling order to initial position.
                      (setq recenter-last-op nil)))
                  ;; Record previous position for ^ when we move on.
                  ;; Change markers to numbers in the match data
                  ;; since lots of markers slow down editing.
                  (push (list (point) replaced
                              ;;  If the replacement has already happened, all we need is the
                              ;;  current match start and end.  We could get this with a trivial
                              ;;  match like
                              ;;  (save-excursion (goto-char (match-beginning 0))
                              ;;                  (search-forward (match-string 0))
                              ;;                  (match-data t))
                              ;;  if we really wanted to avoid manually constructing match data.
                              ;;  Adding current-buffer is necessary so that match-data calls can
                              ;;  return markers which are appropriate for editing.
                              (if replaced
                                  (list
                                   (match-beginning 0)
                                   (match-end 0)
                                   (current-buffer))
                                (match-data t)))
                        stack))))
            (setq idx (1+ idx))))
      (foreign-regexp/replace/ovs-on-match/dispose)
      (foreign-regexp/replace/highlight/dispose))
    (or unread-command-events
        (message "Replaced %d occurrence%s"
                 replace-count
                 (if (= replace-count 1) "" "s")))))


;;; ===========================================================================
;;;
;;;  An overlay on current match.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/replace/highlight/put beg end) => VOID
;; ----------------------------------------------------------------------------
(defvar foreign-regexp/replace/highlight-overlay nil)
(make-variable-buffer-local 'foreign-regexp/replace/highlight-overlay)
(defun foreign-regexp/replace/highlight/put (beg end)
  "Subroutine of `foreign-regexp/replace/perform-replace'.

Put overlay on current match."
  (if foreign-regexp/replace/highlight-overlay
      (move-overlay foreign-regexp/replace/highlight-overlay beg end)
    (let ((ov (make-overlay beg end (current-buffer) t)))
      (overlay-put ov 'priority 1001) ;higher than lazy overlays
      (when query-replace-highlight
        (overlay-put ov 'face 'query-replace))
      (setq foreign-regexp/replace/highlight-overlay ov))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/replace/highlight/dispose) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/replace/highlight/dispose ()
  "Subroutine of `foreign-regexp/replace/perform-replace'."
  (when foreign-regexp/replace/highlight-overlay
    (delete-overlay foreign-regexp/replace/highlight-overlay)
    (setq foreign-regexp/replace/highlight-overlay nil)))


;;; ===========================================================================
;;;
;;;  Overlays on all of matches.
;;;
;;; ===========================================================================

(defvar foreign-regexp/replace/ovs-on-match/data nil)
(make-variable-buffer-local 'foreign-regexp/replace/ovs-on-match/data)

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/replace/ovs-on-match/get-all) => LIST
;; ----------------------------------------------------------------------------
(defun foreign-regexp/replace/ovs-on-match/get-all ()
  "Get all of overlays put on each match."
  foreign-regexp/replace/ovs-on-match/data)

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/replace/ovs-on-match/add beg end buf replacement) => LIST
;; ----------------------------------------------------------------------------
(defun foreign-regexp/replace/ovs-on-match/add (beg end buf replacement)
  "Make overlay on match text and save it in
`foreign-regexp/replace/ovs-on-match/data'.

Each overlay has a replacement text as property
foreign-regexp/replace/replacement."
  (let ((ov (make-overlay beg end buf nil nil)))
    (when query-replace-lazy-highlight
      (overlay-put ov 'face lazy-highlight-face))
    (overlay-put ov 'foreign-regexp/replace/replacement replacement)
    (overlay-put ov 'priority 1000)
    (setq foreign-regexp/replace/ovs-on-match/data
          (nconc foreign-regexp/replace/ovs-on-match/data (cons ov nil)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/replace/ovs-on-match/dispose) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/replace/ovs-on-match/dispose ()
  "Delete overlays on matched strings created by external command
`foreign-regexp/replace'."
  (dolist (ov foreign-regexp/replace/ovs-on-match/data)
    (overlay-put ov 'foreign-regexp/replace/replacement nil)
    (overlay-put ov 'priority nil)
    (delete-overlay ov))
  (setq foreign-regexp/replace/ovs-on-match/data nil))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/replace/ovs-on-match/get-nth nth) => OVERLAY
;; ----------------------------------------------------------------------------
(defun foreign-regexp/replace/ovs-on-match/get-nth (nth)
  (nth nth foreign-regexp/replace/ovs-on-match/data))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/replace/ovs-on-match/get-count) => NUM
;; ----------------------------------------------------------------------------
(defun foreign-regexp/replace/ovs-on-match/get-count ()
  (length foreign-regexp/replace/ovs-on-match/data))


;;; ===========================================================================
;;;
;;;  `occur' by foreign regexp with a help from external command.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/occur regexp &optional nlines) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/occur (regexp &optional nlines)
  (interactive
   (and
    (foreign-regexp/occur/assert-available)
    (let ((regexp-history foreign-regexp/history))
      (foreign-regexp/read-from-minibuf/with-search-option-indicator
       (prog1
           (foreign-regexp/occur-read-primary-args)
         (setq foreign-regexp/history regexp-history))))))
  (foreign-regexp/occur/assert-available)
  (let ((orig-occur-engine-fn (symbol-function 'occur-engine)))
    (setf (symbol-function 'occur-engine)
          (symbol-function 'foreign-regexp/occur/occur-engine))
    (unwind-protect
        (foreign-regexp/search/with-regarding-string-as-foreign-regexp
            (regexp)
          (occur regexp nlines))
      (setf (symbol-function 'occur-engine)
            orig-occur-engine-fn))))


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/occur/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defalias 'foreign-regexp/occur/assert-available 'foreign-regexp/search/assert-available)

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/occur-read-primary-args) => LIST
;; ----------------------------------------------------------------------------
;; Based on `occur-read-primary-args'.
;; Just for prompt message.
(defun foreign-regexp/occur-read-primary-args ()
  (list (read-regexp "List lines matching foreign regexp"
                     (car regexp-history))
        (when current-prefix-arg
          (prefix-numeric-value current-prefix-arg))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/occur/occur-engine regexp buffers out-buf nlines
;;                                   case-fold-search title-face
;;                                   prefix-face match-face keep-props) => NUM
;; ----------------------------------------------------------------------------

(defun foreign-regexp/occur/occur-engine (regexp buffers out-buf nlines case-fold
                                                 title-face prefix-face match-face keep-props)
  "Alternate function of original `occur-engine'."
  ;; Based on `occur-engine', GNU Emacs 24.3.1.
  (with-current-buffer out-buf
    (let ((globalcount 0)
          (coding nil)
          (case-fold-search case-fold))
      ;; Map over all the buffers
      (dolist (buf buffers)
        (when (buffer-live-p buf)
          (let ((matches 0)     ;; count of matched lines
                (lines 1)       ;; line count
                (prev-after-lines nil)  ;; context lines of prev match
                (prev-lines nil)        ;; line number of prev match endpt
                (matchbeg 0)
        (matchend nil)
                (origpt nil)
                (begpt nil)
                (endpt nil)
                (marker nil)
                (curstring "")
                (ret nil)
                (inhibit-field-text-motion t)
                (headerpt (with-current-buffer out-buf (point))))
            (with-current-buffer buf
              (or coding
                  ;; Set CODING only if the current buffer locally
                  ;; binds buffer-file-coding-system.
                  (not (local-variable-p 'buffer-file-coding-system))
                  (setq coding buffer-file-coding-system))
              (save-excursion
                (goto-char (point-min)) ;; begin searching in the buffer
                (while (not (eobp))
                  (setq origpt (point))
                  (when (setq matchend (re-search-forward regexp nil t))
                    (setq matches (1+ matches)) ;; increment match count
                    (setq matchbeg (match-beginning 0))
                    ;; Get beginning of first match line and end of the last.
                    (save-excursion
                      (goto-char matchbeg)
                      (setq begpt (line-beginning-position))
                      (goto-char matchend)
                      (setq endpt (line-end-position)))
                    ;; Sum line numbers up to the first match line.
                    (save-excursion
                      (goto-char matchbeg)
                      (setq lines (+ lines (count-lines origpt (point))
                                     (if (bolp) 1 0)
                                     -1)))
                    (setq marker (make-marker))
                    (set-marker marker matchbeg)
                    (setq curstring (occur-engine-line begpt endpt keep-props))
                    ;; Highlight the matches
                    (goto-char begpt)
                    (while (re-search-forward regexp endpt t)
                      (setq matchend (point))
                      (add-text-properties
                       (- (match-beginning 0) begpt) (- (point) begpt)
                       (append
                        `(occur-match t)
                        (when match-face
                          ;; Use `face' rather than `font-lock-face' here
                          ;; so as to override faces copied from the buffer.
                          `(face ,match-face)))
                       curstring))
                    ;; Generate the string to insert for this match
                    (let* ((match-prefix
                            ;; Using 7 digits aligns tabs properly.
                            (apply #'propertize (format "%7d:" lines)
                                   (append
                                    (when prefix-face
                                      `(font-lock-face prefix-face))
                                    `(occur-prefix t mouse-face (highlight)
                                      ;; Allow insertion of text at
                                      ;; the end of the prefix (for
                                      ;; Occur Edit mode).
                                      front-sticky t rear-nonsticky t
                                      occur-target ,marker follow-link t
                                      help-echo "mouse-2: go to this occurrence"))))
                           (match-str
                            ;; We don't put `mouse-face' on the newline,
                            ;; because that loses.  And don't put it
                            ;; on context lines to reduce flicker.
                            (propertize curstring 'mouse-face (list 'highlight)
                                        'occur-target marker
                                        'follow-link t
                                        'help-echo
                                        "mouse-2: go to this occurrence"))
                           (out-line
                            (concat
                             match-prefix
                             ;; Add non-numeric prefix to all non-first lines
                             ;; of multi-line matches.
                             (replace-regexp-in-string
                              "\n"
                              "\n       :"
                              match-str)
                             ;; Add marker at eol, but no mouse props.
                             (propertize "\n" 'occur-target marker)))
                           (data
                            (if (= nlines 0)
                                ;; The simple display style
                                out-line
                              ;; The complex multi-line display style.
                              (setq ret (occur-context-lines
                                         out-line nlines keep-props begpt endpt
                                         lines prev-lines prev-after-lines))
                              ;; Set first elem of the returned list to `data',
                              ;; and the second elem to `prev-after-lines'.
                              (setq prev-after-lines (nth 1 ret))
                              (nth 0 ret))))
                      ;; Actually insert the match display data
                      (with-current-buffer out-buf
                        (insert data))))
                  (if matchend
                      (progn
                        ;; Sum line numbers between first and last match lines.
                        (setq lines (+ lines (count-lines begpt (point))
                                       (if (bolp) 1 0)
                                       -1)))
                    (goto-char (point-max)))
                  (setq prev-lines (1- lines)))
                ;; Flush remaining context after-lines.
                (when prev-after-lines
                  (with-current-buffer out-buf
                    (insert (apply #'concat (occur-engine-add-prefix
                                             prev-after-lines)))))))
            (when (not (zerop matches)) ;; is the count zero?
              (setq globalcount (+ globalcount matches))
              (with-current-buffer out-buf
                (goto-char headerpt)
                (let ((beg (point))
                      end)
                  (insert (propertize
                           (format "%d match%s%s in buffer: %s\n"
                                   matches (if (= matches 1) "" "es")
                                   ;; Don't display regexp for multi-buffer.
                                   (if (> (length buffers) 1)
                                       "" (format " for \"%s\""
                                                  (query-replace-descr regexp)))
                                   (buffer-name buf))
                           'read-only t))
                  (setq end (point))
                  (add-text-properties beg end
                                       (append
                                        (when title-face
                                          `(font-lock-face ,title-face))
                                        `(occur-title ,buf))))
                (goto-char (point-min)))))))
      ;; Display total match count and regexp for multi-buffer.
      (when (and (not (zerop globalcount)) (> (length buffers) 1))
        (goto-char (point-min))
        (let ((beg (point))
              end)
          (insert (format "%d match%s total for \"%s\":\n"
                          globalcount (if (= globalcount 1) "" "es")
                          (query-replace-descr regexp)))
          (setq end (point))
          (add-text-properties beg end (when title-face
                                         `(font-lock-face ,title-face))))
        (goto-char (point-min)))
      (if coding
          ;; CODING is buffer-file-coding-system of the first buffer
          ;; that locally binds it.  Let's use it also for the output
          ;; buffer.
          (set-buffer-file-coding-system coding))
      ;; Return the number of matches
      globalcount)))


;;; ===========================================================================
;;;
;;;  `search-forward' and `search-backward' by foreign regexp.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/search/forward regexp &optional bound noerror count limit)
;;                                                                    => POINT
;; ----------------------------------------------------------------------------
(defun foreign-regexp/search/forward (regexp &optional bound noerror count limit)
  "Search forward from point for REGEXP in foreign manner.
See also `re-search-forward'."
  (interactive
   (and
    (foreign-regexp/search/assert-available)
    (foreign-regexp/read-from-minibuf/with-search-option-indicator
     (list (read-from-minibuffer "Search for foreign regexp: "
                                 nil nil nil
                                 'foreign-regexp/history)))))
  (let* ((buf    (current-buffer))
         (data   (or (foreign-regexp/search/cache/get    buf regexp limit)
                     (foreign-regexp/search/cache/update buf regexp limit)))
         (pt     (point))
         (be-lst (car (member-if #'(lambda (be-lst)
                                     (<= pt (nth 0 be-lst)))
                                 data)))
         (beg    (and be-lst (nth 0 be-lst)))
         (end    (and be-lst (nth 1 be-lst))))
    (cond
     ((and be-lst
           (if bound (<= end bound) t))
      (set-match-data (copy-list be-lst))
      (goto-char end)
      (cond
       ((and count
             (< 0 (1- count)))
        (foreign-regexp/search/forward
         regexp bound noerror (1- count) limit))
       (t
        end)))
     ((eq noerror
          t)
      nil)
     (noerror
      (goto-char (or bound
                     (point-max)))
      nil)
     (t
      (signal 'search-failed
              regexp)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/search/backward regexp &optional bound noerror count limit)
;;                                                                    => POINT
;; ----------------------------------------------------------------------------
(defun foreign-regexp/search/backward (regexp &optional bound noerror count limit)
  "Search backward from point for REGEXP in foreign manner.
See also `re-search-backward'."
  (interactive
   (and
    (foreign-regexp/search/assert-available)
    (foreign-regexp/read-from-minibuf/with-search-option-indicator
     (list (read-from-minibuffer "Search for foreign regexp backward: "
                                 nil nil nil
                                 'foreign-regexp/history)))))
  (let* ((buf    (current-buffer))
         (data   (reverse (or (foreign-regexp/search/cache/get    buf regexp limit)
                              (foreign-regexp/search/cache/update buf regexp limit))))
         (pt     (point))
         (be-lst (car (member-if
                       #'(lambda (be-lst)
                           (<= (nth 1 be-lst) pt))
                       data)))
         (beg    (and be-lst (nth 0 be-lst)))
         (end    (and be-lst (nth 1 be-lst))))
    (cond
     ((and be-lst
           (if bound (<= bound beg) t))
      (set-match-data (copy-list be-lst))
      (goto-char beg)
      (cond
       ((and count
             (< 0 (1- count)))
        (foreign-regexp/search/backward
         regexp bound noerror (1- count) limit))
       (t
        beg)))
     ((eq noerror
          t)
      nil)
     (noerror
      (goto-char (or bound
                     (point-max)))
      nil)
     (t
      (signal 'search-failed
              regexp)))))


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/search/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defun foreign-regexp/search/available-p ()
  "Test if external command or shell script is defined or not."
  (foreign-regexp/replace/available-p))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/search/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defun foreign-regexp/search/assert-available ()
  "Raise error when no external command or shell script is defined."
  (or (foreign-regexp/search/available-p)
      (error "[foreign-regexp] No external command or shell script is defined for search.")))


;;; ===========================================================================
;;;
;;;  Cache data for `foreign-regexp/search/forward'
;;;                                       and `foreign-regexp/search/backward'.
;;;
;;; ===========================================================================

(defvar foreign-regexp/search/.cache-alst nil)


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/search/cache/pre-command-hook-fn) void
;; ----------------------------------------------------------------------------
(defun foreign-regexp/search/cache/pre-command-hook-fn ()
  "Clear cache before any command will run.

When `this-command' has property `foreign-regexp/search/ongoing-search-cmd-p',
cache won't be cleared."
  (condition-case c
      (when (not (and (symbolp this-command)
                      (get this-command
                           'foreign-regexp/search/ongoing-search-cmd-p)))
        (foreign-regexp/search/cache/clear-all))
    (error
     (message "[foreign-regexp] %s" (error-message-string c)))))

(put 'isearch-repeat-forward  'foreign-regexp/search/ongoing-search-cmd-p t)
(put 'isearch-repeat-backward 'foreign-regexp/search/ongoing-search-cmd-p t)

(add-hook 'pre-command-hook 'foreign-regexp/search/cache/pre-command-hook-fn)

;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/search/cache/get buf regexp &optional limit) => LIST
;; ----------------------------------------------------------------------------
(defun foreign-regexp/search/cache/get (buf regexp &optional limit)
  "Returns cache data, which is the result of a search
in BUF for REGEXP by external command."
  (with-current-buffer buf
    (let* ((cache-alst-for-buf (cdr (assq buf     foreign-regexp/search/.cache-alst)))
           (cache-alst-for-str (cdr (assq regexp  cache-alst-for-buf)))
           (cached-data        (cdr (assq 'data   cache-alst-for-str)))
           (cached-limit       (cdr (assq 'limit  cache-alst-for-str)))
           (cached-tick        (cdr (assq 'tick   cache-alst-for-str)))
           (cached-pt-min      (cdr (assq 'pt-min cache-alst-for-str)))
           (cached-pt-max      (cdr (assq 'pt-max cache-alst-for-str)))
           (cur-tick           (buffer-chars-modified-tick buf))
           (cur-pt-min         (point-min))
           (cur-pt-max         (point-max)))
      
      (when (and cache-alst-for-str
                 (or (not (equal cached-limit  limit))
                     (/=         cached-tick   cur-tick)
                     (/=         cached-pt-min cur-pt-min)
                     (/=         cached-pt-max cur-pt-max)))
        (assq-delete-all regexp cache-alst-for-buf)
        (setq cached-data nil))
      cached-data)))


;; ----------------------------------------------------------------------------
;;  (foreign-regexp/search/cache/clear buf regexp) => LIST
;; ----------------------------------------------------------------------------
(defun foreign-regexp/search/cache/clear (buf regexp)
  "Clear cache data, which is the result of a search
in BUF for REGEXP by external command."
  (when (assq regexp
              (cdr (assq buf foreign-regexp/search/.cache-alst)))
    (foreign-regexp/.debug foreign-regexp/search/debug-cache
                           "Clear: %s, \"%s\"\n" buf regexp)
    (assq-delete-all
     regexp
     (cdr (assq buf foreign-regexp/search/.cache-alst)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/search/cache/clear-all) => LIST
;; ----------------------------------------------------------------------------
(defun foreign-regexp/search/cache/clear-all ()
  "Clear all of cache data, which is the result of a search
by external command."
  (when foreign-regexp/search/.cache-alst
    (foreign-regexp/.debug foreign-regexp/search/debug-cache
                           "Clear: All\n")
    (setq foreign-regexp/search/.cache-alst nil)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/search/cache/update buf regexp) => LIST
;; ----------------------------------------------------------------------------
(defun foreign-regexp/search/cache/update (buf regexp &optional limit)
  "Update cache data, which is the result of a search
in BUF for REGEXP by external command."
  (foreign-regexp/.debug foreign-regexp/search/debug-cache
                         "Update: %s, \"%s\"\n" buf regexp)
  (condition-case c
      (with-current-buffer buf
        (let* ((pt-min (point-min))
               (pt-max (point-max))
               (offset (point-min))
               (result (foreign-regexp/run-external-command
                        foreign-regexp/replace/external-command
                        foreign-regexp/replace/shell-script
                        (buffer-substring pt-min pt-max)
                        regexp
                        ""
                        (if foreign-regexp/dot-match-a-newline-p "DOT" "")
                        (if case-fold-search "" "CASE")
                        (if foreign-regexp/use-extended-regexp-p "EXT" "")
                        nil
                        limit
                        (- (point) offset)
                        nil
                        nil))
               parsed-result)
          ;; Remove replacement from result.
          (dolist (lst (reverse result))
            (dolist (item lst)
              (setq parsed-result
                    (cons (mapcar #'(lambda (elm)
                                      ;; Translate index to count.
                                      (+ offset elm))
                                  (car item))
                          parsed-result))))
          (setq parsed-result (reverse parsed-result))
          
          ;; Remove old cache.
          (foreign-regexp/search/cache/clear buf regexp)

          ;; Save new cache.
          (or (assq buf foreign-regexp/search/.cache-alst)
              (push (list buf)
                    foreign-regexp/search/.cache-alst))
          (push (cons regexp
                      (list (cons 'data   parsed-result)
                            (cons 'limit  limit)
                            (cons 'tick   (buffer-chars-modified-tick buf))
                            (cons 'pt-min pt-min)
                            (cons 'pt-max pt-max)))
                (cdr (assq buf foreign-regexp/search/.cache-alst)))
          parsed-result))
    (error
     (unwind-protect
         ;; Remove old cache.
         (foreign-regexp/search/cache/clear buf regexp)
       (signal 'invalid-regexp
               (list (error-message-string c)))))))


  
;;; ===========================================================================
;;;
;;;  `isearch' for foreign regexp with a help from external command.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/isearch-forward &optional not-regexp no-recursive-edit)
;;                                                                     => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/isearch-forward (&optional not-regexp no-recursive-edit)
  "Do isearch with a help from external command.

See `isearch-forward-regexp' and `isearch-backward-regexp' for
more information."
  (interactive "P\np")
  (foreign-regexp/isearch/assert-available)
  
  ;; Setup `isearch-search-fun-function'.
  (when (not (boundp 'foreign-regexp/isearch/.orig-isearch-search-fun-function))
    (defvar foreign-regexp/isearch/.orig-isearch-search-fun-function
      isearch-search-fun-function))
  (setq isearch-search-fun-function #'foreign-regexp/isearch/isearch-search-fun-function)
  (add-hook 'isearch-mode-end-hook
            'foreign-regexp/isearch/.isearch-mode-end-hook-fn)
  
  ;; Just for prompt message.
  (foreign-regexp/ad-enable 'isearch-message-prefix 'after 'foreign-regexp/isearch/modify-prompt)
  (foreign-regexp/ad-activate 'isearch-message-prefix)
  
  (isearch-mode t (null not-regexp) nil (not no-recursive-edit)))


;; ----------------------------------------------------------------------------
;;  (foreign-regexp/isearch-backward &optional not-regexp no-recursive-edit)
;;                                                                     => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/isearch-backward (&optional not-regexp no-recursive-edit)
  "Do isearch with a help from external command.

See `isearch-forward-regexp' and `isearch-backward-regexp' for
more information."
  (interactive "P\np")
  (foreign-regexp/isearch/assert-available)
  
  ;; Setup `isearch-search-fun-function'.
  (when (not (boundp 'foreign-regexp/isearch/.orig-isearch-search-fun-function))
    (defvar foreign-regexp/isearch/.orig-isearch-search-fun-function
      isearch-search-fun-function))
  (setq isearch-search-fun-function #'foreign-regexp/isearch/isearch-search-fun-function)
  (add-hook 'isearch-mode-end-hook
            'foreign-regexp/isearch/.isearch-mode-end-hook-fn)
  
  ;; Just for prompt message.
  (foreign-regexp/ad-enable 'isearch-message-prefix 'after 'foreign-regexp/isearch/modify-prompt)
  (foreign-regexp/ad-activate 'isearch-message-prefix)
  
  (isearch-mode nil (null not-regexp) nil (not no-recursive-edit)))


;; ----------------------------------------------------------------------------
;;
;;  Advices
;;
;; ----------------------------------------------------------------------------

;; Just for prompt message.
(defadvice isearch-message-prefix (after foreign-regexp/isearch/modify-prompt
                                         (&optional c-q-hack ellipsis nonincremental))
  (when (string-match "\\b\\(\\([Rr]\\)egexp\\)\\b" ad-return-value)
    (setq ad-return-value
          (replace-match (propertize
                          (if (string= "R" (match-string 2 ad-return-value))
                              "Foreign regexp"
                            "foreign regexp")
                          'face 'minibuffer-prompt)
                         t t ad-return-value)))
  ;; Put search option indicator.
  (when (string-match "\\(: \\)$" ad-return-value)
    (setq ad-return-value
          (replace-match (propertize
                          (concat (foreign-regexp/search-option-indicator/make-indicator)
                                  ": ")
                          'face 'minibuffer-prompt)
                         t t ad-return-value))))

;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/isearch/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defalias 'foreign-regexp/isearch/available-p 'foreign-regexp/search/available-p)

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/isearch/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defalias 'foreign-regexp/isearch/assert-available 'foreign-regexp/search/assert-available)

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/isearch/search-option-changed-hook-fn) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/isearch/search-option-changed-hook-fn ()
  "Update display when search option is changed."
  (when isearch-mode
    (setq isearch-success t isearch-adjusted t)
    
    ;; Force run `isearch-lazy-highlight-new-loop'.
    (setq isearch-lazy-highlight-last-string nil)
    (isearch-update)
    
    ;; Suppress messages.
    (when (boundp 'no-message) ;; For compiler.
      (setq no-message t))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/isearch/setup-search-option-changed-hook) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/isearch/setup-search-option-changed-hook ()
  "Set call back function `foreign-regexp/isearch/search-option-changed-hook-fn'
to each search option changed hook."
  (add-hook 'foreign-regexp/case-fold-changed-hook
            'foreign-regexp/isearch/search-option-changed-hook-fn)

  (add-hook 'foreign-regexp/dot-match-changed-hook
            'foreign-regexp/isearch/search-option-changed-hook-fn)

  (add-hook 'foreign-regexp/ext-regexp-changed-hook
            'foreign-regexp/isearch/search-option-changed-hook-fn))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/isearch/.isearch-mode-end-hook-fn ) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/isearch/.isearch-mode-end-hook-fn ()
  "Clean up environment when isearch by foreign-regexp is finished."
  (when (not isearch-nonincremental)
    (when (boundp 'foreign-regexp/isearch/.orig-isearch-search-fun-function)
      (setq isearch-search-fun-function
            foreign-regexp/isearch/.orig-isearch-search-fun-function)
      (makunbound 'foreign-regexp/isearch/.orig-isearch-search-fun-function))
    
    ;; Just for prompt message.
    (foreign-regexp/ad-disable 'isearch-message-prefix 'after 'foreign-regexp/isearch/modify-prompt)
    (foreign-regexp/ad-activate 'isearch-message-prefix)
    
    (remove-hook 'isearch-mode-end-hook
                 'foreign-regexp/isearch/.isearch-mode-end-hook-fn)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/isearch/isearch-search-fun-function) => FUNCTION
;; ----------------------------------------------------------------------------
(defun foreign-regexp/isearch/isearch-search-fun-function ()
  "The value used as value of `isearch-search-fun' while
isearch by foreign-regexp is going on.

This function returns the search function
`foreign-regexp/search/forward' or `foreign-regexp/search/backward'
for isearch to use."
  (cond
   (isearch-forward
    'foreign-regexp/search/forward)
   (t
    'foreign-regexp/search/backward)))


;; ----------------------------------------------------------------------------
;;
;;  Main
;;
;; ----------------------------------------------------------------------------

(add-hook 'isearch-mode-hook
          'foreign-regexp/isearch/setup-search-option-changed-hook)


;;; ===========================================================================
;;;
;;;  `align' by foreign regexp with a help from external command.
;;;
;;; ===========================================================================
(require 'align)

(defvar foreign-regexp/align/wsp-regexp "([ \\t]+)"
  "A regexp corresponding to white spaces.")

(defvar foreign-regexp/align/narrow-before-foreign-regexp t
  "Non-nil to narrow to the region before foreign regexp is performed.
This is for better performance of foreign regexp.")


;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/align beg end regexp &optional group spacing repeat) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/align (beg end regexp &optional group spacing repeat)
  "Align the current region using a partial foreign regexp
read from the minibuffer.

The foreign regexp read from the minibuffer will be
supposed to be placed after whitespaces.

When called with prefix argument, align the current region
using a full foreign regexp read from the minibuffer.

Example) 

   < Use regexp of Perl in this example. >

   When texts in region is:

        (one 1)
        (ten 10)
        (hundred 100)
        (thousand 1000)

   Run command with prefix argument on the region with options:

        REGEXP: ([ \t]+)\d
                     |
                     +--- GROUP: 1
                          Alignment will be applied to each
                          lines by inserting whitespaces to
                          the place where the sub-expression
                          specified by GROUP is matched to.
        SPACING: 1
        REPEAT : y

   Result is:

        (one       1)
        (ten       10)
        (hundred   100)
        (thousand 1000)
                 |
                 +---- Aligned using spaces SPACING unit.

See also `align-regexp'."
  (interactive
   (append
    (list (region-beginning) (region-end))
    (if current-prefix-arg
        (list (foreign-regexp/read-from-minibuf/with-search-option-indicator
               (read-from-minibuffer "Complex align using foreign regexp: "
                                     foreign-regexp/align/wsp-regexp
                                     nil nil 'foreign-regexp/history))
              (string-to-number
               (read-string
                "Parenthesis group to modify (justify if negative): " "1"))
              (string-to-number
               (read-string "Amount of spacing (or column if negative): "
                            (number-to-string align-default-spacing)))
              (y-or-n-p "Repeat throughout line? "))
      (list (concat foreign-regexp/align/wsp-regexp
                    (foreign-regexp/read-from-minibuf/with-search-option-indicator
                     (read-from-minibuffer "Align foreign regexp: "
                                           nil
                                           nil nil 'foreign-regexp/history)))
            1 align-default-spacing nil))))
  (let ((rule
         (list (list nil (cons 'regexp regexp)
                     (cons 'group (abs group))
                     (if (< group 0)
                         (cons 'justify t)
                       (cons 'bogus nil))
                     (if (>= spacing 0)
                         (cons 'spacing spacing)
                       (cons 'column (abs spacing)))
                     (cons 'repeat repeat)))))
    
    (foreign-regexp/search/with-regarding-string-as-foreign-regexp
        (regexp)
      (align-region beg end 'entire rule nil nil))))


;; ----------------------------------------------------------------------------
;;
;;  Advices
;;
;; ----------------------------------------------------------------------------

(defadvice align-region (around foreign-regexp/align/align-region
                                (beg end separate rules exclude-rules
                                     &optional func))
  "Run `align-region' with regarding a regexp in the rule
which has an attribute `regexp-type' as foreign regexp.

When the value of an attribute `regexp-type' of a rule is

   T:
       The rule will be applied regardless of the current
       `regexp-type'.

   LIST OF SYMBOLS:
       The rule will be applied if current `regexp-type' is
       its member,

   SYMBOL:
       The rule will be applied if current `regexp-type' is
       `eq' to the SYMBOL.

   ALL OTHERS:
       The rule won't be applied."
  (let ((regexp-lst nil)
        (cooked-rules    (copy-list rules))
        (cooked-ex-rules (copy-list exclude-rules)))
    (dolist (cur-rules (list rules exclude-rules))
      (dolist (rule cur-rules)
        (when (assq 'foreign-regexp-type rule)
          (let ((regexp-type (cdr (assq 'regexp-type rule)))
                (regexp     (cdr (assq 'regexp     rule))))
            (cond
             ((and (stringp regexp)
                   
                   (or ;; `aien-type' is
                    ;; T
                    (eq regexp-type t) 
                    ;; LIST
                    (and (listp regexp-type)
                         (memq foreign-regexp/regexp-type regexp-type))
                    ;; SYMBOL
                    (eq regexp-type
                        foreign-regexp/regexp-type)))
              (push regexp regexp-lst))
             (t
              ;; Remove invalid rules.
              (setq cooked-rules    (delq rule cooked-rules))
              (setq cooked-ex-rules (delq rule cooked-ex-rules))))

            ;; Modify original arguments.
            (setq rules         cooked-rules)
            (setq exclude-rules cooked-ex-rules)))))

    (foreign-regexp/search/with-regarding-these-string-as-foreign-regexp
     (regexp-lst)
     (save-restriction
       (when (and foreign-regexp/align/narrow-before-foreign-regexp
                  beg end)
         ;; For better performance of foreign regexp.
         (narrow-to-region (min beg end) (max beg end)))
       ad-do-it))))
(ad-activate 'align-region)


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/align/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defalias 'foreign-regexp/align/available-p 'foreign-regexp/search/available-p)

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/align/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defalias 'foreign-regexp/align/assert-available 'foreign-regexp/search/assert-available)


;;; ===========================================================================
;;;
;;;  quote meta characters of foreign regexp by external command.
;;;
;;; ===========================================================================

(defvar foreign-regexp/quote-meta/external-command nil
  "Path of an external command to use to execute actual
quote-meta operation.

Two arguments describe below will be passed to the command.

 1st: Path of a file to which the command should write the result
      of quote-meta operation.

      The external command have to output a form like:

        (setq result \"quoted string\")

      to this file.

      The text in this file must be encoded in the value of
      `foreign-regexp/input-coding-system'.

 2nd: Path of a file in which the regexp, we want to quote meta
      characters in it, is written.

      The text in this file is encoded in the value of
      `foreign-regexp/output-coding-system'.")

(defvar foreign-regexp/quote-meta/shell-script nil
  "A shell script which will be run as
`foreign-regexp/quote-meta/external-command'
when it has nil value.")


;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/quote-meta-in-region beg end) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/quote-meta-in-region (beg end)
  "Quote meta characters in region in manner of external command."
  (interactive "r")
  (save-excursion
    (let ((quoted-str (foreign-regexp/quote-meta
                       (buffer-substring-no-properties beg end))))
      (delete-region beg end)
      (goto-char beg)
      (insert quoted-str))))

;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/quote-meta/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defun foreign-regexp/quote-meta/available-p ()
  "Test if external command or shell script is defined or not."
  (or foreign-regexp/quote-meta/external-command
      foreign-regexp/quote-meta/shell-script))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/quote-meta/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defun foreign-regexp/quote-meta/assert-available ()
  "Raise error when no external command or shell script is defined."
  (or (foreign-regexp/quote-meta/available-p)
      (error "[foreign-regexp] No external command or shell script is defined for quote-meta.")))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/quote-meta regexp) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/quote-meta (regexp)
  "Quote meta characters in a REGEXP in manner of external command."
  (interactive)
  (foreign-regexp/quote-meta/assert-available)
  
  (foreign-regexp/run-external-command
   foreign-regexp/quote-meta/external-command
   foreign-regexp/quote-meta/shell-script
   nil ;; Don't care about text in current buffer.
   regexp
   nil))


;;; ===========================================================================
;;;
;;;  `re-builder' in foreign regexp with a help from external command.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/exec-with-current-re re-var &rest body)
;;                                                     => RESULT FROM BODYFORM
;; ----------------------------------------------------------------------------
(defmacro foreign-regexp/re-builder/exec-with-current-re (re-var &rest body)
  "When the current buffer is *RE-Builder*, exit `re-builder'
and then run BODY with binding current RE to RE-VAR.

NOTE: RE-VAR will be defined as lexical variable by this macro."
  (declare (indent 1))
  `(progn
     (case reb-re-syntax
       ((foreign-regexp)
        (lexical-let ((,re-var (and (eq (get-buffer reb-buffer)
                                        (current-buffer))
                                    (with-current-buffer (get-buffer reb-buffer)
                                      (buffer-substring (point-min) (point-max))))))
          (when ,re-var
            (reb-quit)
            (kill-buffer (get-buffer reb-buffer))
            (set-buffer reb-target-buffer)
            ,@body)))
       (t
        (error "[foreign-regexp] RE-Builder syntax is not `foreign-regexp'.")))))


;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/query-replace-on-target-buffer) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/query-replace-on-target-buffer ()
  "Run `foreign-regexp/query-replace' with current RE on
`reb-target-buffer'."
  (interactive)
  (foreign-regexp/re-builder/assert-in-reb-buffer)
  (foreign-regexp/replace/assert-available)
  
  (foreign-regexp/re-builder/exec-with-current-re regexp
                                                  (when (match-beginning 0)
                                                    (goto-char (match-beginning 0)))
                                                  
                                                  (foreign-regexp/read-from-minibuf/with-initial-contents regexp
                                                                                                          (let ((this-command 'foreign-regexp/query-replace))
                                                                                                            (call-interactively 'foreign-regexp/query-replace)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/occur-on-target-buffer) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/occur-on-target-buffer ()
  "Run `foreign-regexp/occur' with current RE on
`reb-target-buffer'."
  (interactive)
  (foreign-regexp/re-builder/assert-in-reb-buffer)
  (foreign-regexp/occur/assert-available)
  
  (foreign-regexp/re-builder/exec-with-current-re regexp
                                                  (foreign-regexp/read-from-minibuf/with-initial-contents regexp
                                                                                                          (let ((this-command 'foreign-regexp/occur))
                                                                                                            (call-interactively 'foreign-regexp/occur)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/isearch-forward-on-target-buffer) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/isearch-forward-on-target-buffer ()
  "Run `foreign-regexp/isearch-forward' with current RE on
`reb-target-buffer'."
  (interactive)
  (foreign-regexp/re-builder/assert-in-reb-buffer)
  (foreign-regexp/isearch/assert-available)
  
  (foreign-regexp/re-builder/exec-with-current-re regexp
                                                  (add-hook 'isearch-mode-hook
                                                            (foreign-regexp/alambda ()
                                                                                    (remove-hook 'isearch-mode-hook
                                                                                                 #'self)
                                                                                    (isearch-push-state)
                                                                                    (setq isearch-string  regexp
                                                                                          isearch-message regexp)))
                                                  (foreign-regexp/isearch-forward)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/isearch-backward-on-target-buffer) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/isearch-backward-on-target-buffer ()
  "Run `foreign-regexp/isearch-backward' with current RE on
`reb-target-buffer'."
  (interactive)
  (foreign-regexp/re-builder/assert-in-reb-buffer)
  (foreign-regexp/isearch/assert-available)
  
  (foreign-regexp/re-builder/exec-with-current-re regexp
                                                  (add-hook 'isearch-mode-hook
                                                            (foreign-regexp/alambda ()
                                                                                    (remove-hook 'isearch-mode-hook
                                                                                                 #'self)
                                                                                    (isearch-push-state)
                                                                                    (setq isearch-string  regexp
                                                                                          isearch-message regexp)))
                                                  (foreign-regexp/isearch-backward)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/non-incremental-search-forward-on-target-buffer)
;;                                                                     => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/non-incremental-search-forward-on-target-buffer ()
  "Run `foreign-regexp/non-incremental/search-forward' with
current RE on `reb-target-buffer'."
  (interactive)
  (foreign-regexp/re-builder/assert-in-reb-buffer)
  (foreign-regexp/non-incremental/assert-available)
  
  (foreign-regexp/re-builder/exec-with-current-re regexp
                                                  (foreign-regexp/read-from-minibuf/with-initial-contents regexp
                                                                                                          (let ((this-command 'foreign-regexp/non-incremental/search-forward))
                                                                                                            (call-interactively 'foreign-regexp/non-incremental/search-forward)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/non-incremental-search-backward-on-target-buffer)
;;                                                                     => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/non-incremental-search-backward-on-target-buffer ()
  "Run `foreign-regexp/non-incremental/search-backward' with
current RE on `reb-target-buffer'."
  (interactive)
  (foreign-regexp/re-builder/assert-in-reb-buffer)
  (foreign-regexp/non-incremental/assert-available)
  
  (foreign-regexp/re-builder/exec-with-current-re regexp
                                                  (foreign-regexp/read-from-minibuf/with-initial-contents regexp
                                                                                                          (let ((this-command 'foreign-regexp/non-incremental/search-backward))
                                                                                                            (call-interactively 'foreign-regexp/non-incremental/search-backward)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/align-on-target-buffer) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/align-on-target-buffer ()
  "Run `foreign-regexp/align with
current RE on `reb-target-buffer'."
  (interactive)
  (foreign-regexp/re-builder/assert-in-reb-buffer)
  (foreign-regexp/align/assert-available)
  
  (foreign-regexp/re-builder/exec-with-current-re regexp
                                                  (foreign-regexp/read-from-minibuf/with-initial-contents regexp
                                                                                                          (let ((this-command 'foreign-regexp/align))
                                                                                                            (call-interactively 'foreign-regexp/align)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/toggle-case-fold-on-target-buffer
;;                                               &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/toggle-case-fold-on-target-buffer (&optional no-message)
  "Toggle `case-fold-search' on `reb-target-buffer'."
  (interactive)
  (foreign-regexp/re-builder/assert-in-reb-buffer)
  
  (with-current-buffer reb-target-buffer
    (foreign-regexp/toggle-case-fold no-message)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/toggle-dot-match-on-target-buffer
;;                                               &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/toggle-dot-match-on-target-buffer (&optional no-message)
  "Toggle `foreign-regexp/dot-match-a-newline-p' on `reb-target-buffer'."
  (interactive)
  (foreign-regexp/re-builder/assert-in-reb-buffer)
  
  (with-current-buffer reb-target-buffer
    (foreign-regexp/toggle-dot-match no-message)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/toggle-ext-regexp-on-target-buffer
;;                                               &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/toggle-ext-regexp-on-target-buffer (&optional no-message)
  "Toggle `foreign-regexp/use-extended-regexp-p' on `reb-target-buffer'."
  (interactive)
  (foreign-regexp/re-builder/assert-in-reb-buffer)
  
  (with-current-buffer reb-target-buffer
    (foreign-regexp/toggle-ext-regexp no-message)))


;; ----------------------------------------------------------------------------
;;
;;  Advices
;;
;; ----------------------------------------------------------------------------

(defadvice reb-next-match (around foreign-regexp/re-builder/next-match ())
  (case reb-re-syntax
    ((foreign-regexp)
     (foreign-regexp/re-builder/assert-available)
     (foreign-regexp/search/with-regarding-string-as-foreign-regexp
      ((reb-target-binding reb-regexp)
       ;; Match limit.
       (if (numberp reb-auto-match-limit)
           reb-auto-match-limit nil))
      ad-do-it))
    (t
     ad-do-it)))
(foreign-regexp/ad-activate 'reb-next-match)
(put 'reb-next-match  'foreign-regexp/search/ongoing-search-cmd-p t)

(defadvice reb-prev-match (around foreign-regexp/re-builder/prev-match ())
  (case reb-re-syntax
    ((foreign-regexp)
     (foreign-regexp/re-builder/assert-available)
     (foreign-regexp/search/with-regarding-string-as-foreign-regexp
      ((reb-target-binding reb-regexp)
       ;; Match limit.
       (if (numberp reb-auto-match-limit)
           reb-auto-match-limit nil))
      ad-do-it))
    (t
     ad-do-it)))
(foreign-regexp/ad-activate 'reb-prev-match)
(put 'reb-prev-match 'foreign-regexp/search/ongoing-search-cmd-p t)

(defadvice reb-copy (around foreign-regexp/re-builder/copy ())
  (case reb-re-syntax
    ((foreign-regexp)
     (foreign-regexp/re-builder/assert-available)
     (kill-new (buffer-substring-no-properties (point-min) (point-max))))
    (t ad-do-it)))
(foreign-regexp/ad-activate 'reb-copy)
(put 'reb-copy 'foreign-regexp/search/ongoing-search-cmd-p t)

(defadvice reb-change-syntax (around foreign-regexp/re-builder/change-syntax (&optional syntax))
  (interactive
   (list (intern
          (completing-read "Select syntax: "
                           (mapcar (lambda (el) (cons (symbol-name el) 1))
                                   (foreign-regexp/re-builder/get-syntax-lst))
                           nil t (symbol-name reb-re-syntax)))))
  (if (memq syntax '(read string lisp-re sregex rx foreign-regexp))
      (let ((buffer (get-buffer reb-buffer)))
        (setq reb-re-syntax syntax)
        (when buffer
          (with-current-buffer buffer
            (reb-initialize-buffer))))
    (error "Invalid syntax: %s"  syntax)))
(foreign-regexp/ad-activate 'reb-change-syntax)

(defadvice reb-update-modestring (around foreign-regexp/re-builder/update-mode-string ())
  "Put search option indicator on modeline."
  (case reb-re-syntax
    ((foreign-regexp)
     (setq reb-mode-string
           (concat
            (if reb-subexp-mode
                (format " (subexp %s)" (or reb-subexp-displayed "-"))
              "")
            " "
            (with-current-buffer reb-target-buffer
              (foreign-regexp/search-option-indicator/make-indicator))))
     (force-mode-line-update))
    (t
     ad-do-it)))
(foreign-regexp/ad-activate 'reb-update-modestring)

(defadvice reb-read-regexp (around foreign-regexp/re-builder/read-regexp ())
  (case reb-re-syntax
    ((foreign-regexp)
     (setq ad-return-value
           (buffer-substring-no-properties (point-min) (point-max))))
    (t ad-do-it)))
(foreign-regexp/ad-activate 'reb-read-regexp)

(defadvice reb-insert-regexp (around foreign-regexp/re-builder/insert-regexp ())
  (case reb-re-syntax
    ((foreign-regexp)
     (when reb-regexp
       (insert reb-regexp)))
    (t ad-do-it)))
(foreign-regexp/ad-activate 'reb-insert-regexp)

(defadvice reb-update-regexp (around foreign-regexp/re-builder/update-regexp ())
  (case reb-re-syntax
    ((foreign-regexp)
     (let ((regexp (reb-read-regexp)))
       (with-current-buffer reb-target-buffer
         ;; Do not reset `reb-regexp' so that `eq' can
         ;; determine the equivalence.
         (when (not (equal reb-regexp
                           regexp))
           (setq reb-regexp
                 regexp)
           (setq ad-return-value t)))))
    (t
     ad-do-it)))
(foreign-regexp/ad-activate 'reb-update-regexp)

(defadvice reb-count-subexps (around foreign-regexp/re-builder/count-subexps (re))
  (case reb-re-syntax
    ((foreign-regexp)
     (foreign-regexp/re-builder/assert-available)
     
     ;; Count number of subexp in cache data.
     ;;
     (let ((retval 0))
       (with-current-buffer reb-target-buffer
         (save-excursion
           (let ((buf    (current-buffer))
                 (regexp (reb-target-binding reb-regexp))
                 ;; Match limit.
                 (limit  (if (numberp reb-auto-match-limit)
                             reb-auto-match-limit nil)))
             (dolist (be-lst (or (foreign-regexp/search/cache/get    buf regexp limit)
                                 (foreign-regexp/search/cache/update buf regexp limit)))
               (setq retval (max retval (1- (/ (length be-lst) 2))))))
           (setq ad-return-value retval)))))
    (t ad-do-it)))
(foreign-regexp/ad-activate 'reb-count-subexps)

(defadvice reb-update-overlays (around foreign-regexp/re-builder/update-overlays (&optional subexp))
  (case reb-re-syntax
    ((foreign-regexp)
     (foreign-regexp/re-builder/assert-available)

     (foreign-regexp/search/with-regarding-string-as-foreign-regexp
      ((reb-target-binding reb-regexp)
       ;; Match limit.
       (if (numberp reb-auto-match-limit)
           reb-auto-match-limit nil))
      ad-do-it))
    (t
     ad-do-it)))
(foreign-regexp/ad-activate 'reb-update-overlays)


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/assert-in-reb-buffer) => VOID OR ERROR
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/assert-in-reb-buffer ()
  (when (not (reb-mode-buffer-p))
    (error "[foreign-regexp] Not in *RE-Builder* buffer.")))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defalias 'foreign-regexp/re-builder/available-p 'foreign-regexp/search/available-p)

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defalias 'foreign-regexp/re-builder/assert-available 'foreign-regexp/search/assert-available)

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/get-current-regexp) => STRING or NIL
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/get-current-regexp ()
  "Returns regular expression in buffer *RE-Builder*.

When the current buffer is not *RE-Builder*, returns nil."
  (let* ((reb-buf (get-buffer reb-buffer))
         (regexp (when (eq reb-buf (current-buffer))
                   (with-current-buffer reb-buf
                     (buffer-substring (point-min) (point-max))))))
    regexp))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/get-syntax-lst) => LIST
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/get-syntax-lst ()
  "Returns list of syntax defined in `reb-re-syntax'."
  ;; XXX: Ugly hack.
  ;;      This won't work if `reb-re-syntax' has
  ;;      structure other than default value.
  (let ((type (get 'reb-re-syntax 'custom-type)))
    (setq type (delete 'choice type))
    (mapcar #'(lambda (alt-type)
                (cond
                 ((symbolp alt-type)
                  alt-type)
                 ((listp alt-type)
                  (let (retval)
                    (while alt-type
                      (case (car alt-type)
                        ((const)
                         (setq alt-type (cdr alt-type)))
                        ((:tag)
                         (setq alt-type (cddr alt-type)))
                        (t
                         (setq retval (car alt-type))
                         (setq alt-type nil))))
                    retval))))
            type)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/reb-target-buffer-p buf) => BOOL
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/reb-target-buffer-p (buf)
  "Test if BUF is `reb-target-buffer' and if `reb-re-syntax' is
'foreign-regexp or not."
  (and (eq reb-re-syntax 'foreign-regexp)
       (get-buffer reb-buffer)
       reb-target-buffer
       (eq reb-target-buffer
           buf)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/search-option-changed-hook-fn) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/search-option-changed-hook-fn ()
  "Update mode string and update search status with external command.
Called when search option of `reb-target-buffer' is changed."
  (let ((cur-buf (current-buffer)))
    (when (foreign-regexp/re-builder/reb-target-buffer-p cur-buf)
      (with-current-buffer cur-buf
        (reb-update-modestring))
      (with-current-buffer (get-buffer reb-buffer)
        (reb-auto-update nil nil nil t)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/re-builder/setup-search-option-changed-hook) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/re-builder/setup-search-option-changed-hook ()
  "Set call back function `foreign-regexp/re-builder/search-option-changed-hook-fn'
to each search option changed hook."
  (when reb-target-buffer
    (with-current-buffer reb-target-buffer
      (add-hook 'foreign-regexp/case-fold-changed-hook
                'foreign-regexp/re-builder/search-option-changed-hook-fn)
      
      (add-hook 'foreign-regexp/dot-match-changed-hook
                'foreign-regexp/re-builder/search-option-changed-hook-fn)
      
      (add-hook 'foreign-regexp/ext-regexp-changed-hook
                'foreign-regexp/re-builder/search-option-changed-hook-fn))))

;; ----------------------------------------------------------------------------
;;
;;  Main
;;
;; ----------------------------------------------------------------------------

;; XXX: Ugly hack.
;; Put `foreign-regexp' to custom variable `reb-re-syntax'.
(when (not (memq 'foreign-regexp (foreign-regexp/re-builder/get-syntax-lst)))
  (put 'reb-re-syntax 'custom-type
       (nconc (get 'reb-re-syntax 'custom-type)
              '((const :tag "Foreign regexp syntax" foreign-regexp)))))

(add-hook 'reb-mode-hook
          'foreign-regexp/re-builder/setup-search-option-changed-hook)

(put 'reb-enter-subexp-mode 'foreign-regexp/search/ongoing-search-cmd-p t)
(put 'reb-quit-subexp-mode  'foreign-regexp/search/ongoing-search-cmd-p t)
(put 'reb-display-subexp    'foreign-regexp/search/ongoing-search-cmd-p t)


;;; ===========================================================================
;;;
;;;  Patches to `re-builder', restore cursor position on quitting.
;;;
;;; ===========================================================================

;; XXX: This section should be moved to outside of this library.

;; NOTE: `re-builder' moves cursor to beginning of buffer on quitting.
;;       This is very annoying when we run other commands, such as
;;       `foreign-regexp/query-replace' and `foreign-regexp/align', from
;;       *RE-Builder* buffer. So here we made a patch which corrects
;;       that behavior.

(defvar foreign-regexp/re-builder/targ-buf-state/.orig-pt)


;; ----------------------------------------------------------------------------
;;
;;  Advices
;;
;; ----------------------------------------------------------------------------

(defadvice re-builder (around foreign-regexp/re-builder/targ-buf-state/save ())
  "Save initial cursor position of the target buffer."
  (setq foreign-regexp/re-builder/targ-buf-state/.orig-pt (point))
  ad-do-it)
(ad-activate 're-builder)

(defadvice reb-change-target-buffer (around foreign-regexp/re-builder/targ-buf-state/update (buf))
  "Update initial cursor position of the target buffer."
  (when (window-live-p reb-target-window)
    (set-window-buffer reb-target-window buf))
  (with-current-buffer buf
    (setq foreign-regexp/re-builder/targ-buf-state/.orig-pt (point)))
  ad-do-it)
(ad-activate 'reb-change-target-buffer)

(defadvice reb-quit (around foreign-regexp/re-builder/targ-buf-state/restore ())
  "Set cursor position of target buffer to initial state."
  ad-do-it
  (when (and (buffer-live-p reb-target-buffer)
             foreign-regexp/re-builder/targ-buf-state/.orig-pt)
    (with-current-buffer reb-target-buffer
      (goto-char foreign-regexp/re-builder/targ-buf-state/.orig-pt)
      (setq foreign-regexp/re-builder/targ-buf-state/.orig-pt nil))))
(ad-activate 'reb-quit)

(defadvice reb-kill-buffer (around foreign-regexp/re-builder/targ-buf-state/restore ())
  "Set cursor position of target buffer to initial state."
  (when (reb-mode-buffer-p)
    (condition-case c (reb-quit) (error nil)))
  ad-do-it)
(ad-activate 'reb-kill-buffer)


;;; ===========================================================================
;;;
;;;  Non-incremental search for foreign regexp with a help from external command.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/non-incremental/search-forward regexp) => POINT
;; ----------------------------------------------------------------------------
;; From menu-bar.el
(defun foreign-regexp/non-incremental/search-forward (&optional regexp)
  "Read a regular expression and search for it nonincrementally."
  (interactive)
  (prog1
      (cond
       ((called-interactively-p 'interactive)
        (call-interactively 'foreign-regexp/search/forward))
       (t
        (foreign-regexp/search/forward regexp)))
    (setq menu-bar-last-search-type 'foreign-regexp)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/non-incremental/search-backward regexp) => POINT
;; ----------------------------------------------------------------------------
;; From menu-bar.el
(defun foreign-regexp/non-incremental/search-backward (&optional regexp)
  "Read a regular expression and search for it backward nonincrementally."
  (interactive)
  (prog1
      (cond
       ((called-interactively-p 'interactive)
        (call-interactively 'foreign-regexp/search/backward))
       (t
        (foreign-regexp/search/backward regexp)))
    (setq menu-bar-last-search-type 'foreign-regexp)))


;; ----------------------------------------------------------------------------
;;
;;  Advices
;;
;; ----------------------------------------------------------------------------

(when (require 'menu-bar nil t)
  (when (fboundp 'nonincremental-repeat-search-forward)
    (defadvice nonincremental-repeat-search-forward (around foreign-regexp/nonincremental-repeat-search-forward ())
      (cond
       ((and (eq menu-bar-last-search-type 'foreign-regexp)
             foreign-regexp/history)
        (setq ad-return-value
              (foreign-regexp/search/forward (car foreign-regexp/history))))
       (t
        ad-do-it)))
    (foreign-regexp/ad-activate 'nonincremental-repeat-search-forward)
    (put 'nonincremental-repeat-search-forward 'foreign-regexp/search/ongoing-search-cmd-p t))

  (when (fboundp 'nonincremental-repeat-search-backward)
    (defadvice nonincremental-repeat-search-backward (around foreign-regexp/nonincremental-repeat-search-backward ())
      (cond
       ((and (eq menu-bar-last-search-type 'foreign-regexp)
             foreign-regexp/history)
        (setq ad-return-value
              (foreign-regexp/search/backward (car foreign-regexp/history))))
       (t
        ad-do-it)))
    (foreign-regexp/ad-activate 'nonincremental-repeat-search-backward)
    (put 'nonincremental-repeat-search-backward 'foreign-regexp/search/ongoing-search-cmd-p t)))

;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/non-incremental/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defalias 'foreign-regexp/non-incremental/available-p 'foreign-regexp/search/available-p)

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/non-incremental/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defalias 'foreign-regexp/non-incremental/assert-available 'foreign-regexp/search/assert-available)


;;; ===========================================================================
;;;
;;;  The definition of transition between each command.
;;;
;;; ===========================================================================

(defvar foreign-regexp/transition/.running-cmd nil
  "Internal variable.")

(defvar foreign-regexp/transition/command-table
  '((:label re-builder
            :op-kind    re-builder-cmd
            :command    re-builder)
    (:label isearch-forward
            :op-kind    isearch-cmd
            :command    foreign-regexp/isearch-forward)
    (:label isearch-backward
            :op-kind    isearch-cmd
            :command    foreign-regexp/isearch-backward)
    (:label replace
            :op-kind    minibuf-cmd
            :command    foreign-regexp/query-replace
            ;; required by minibuf-cmd.
            :transition-allowed-in query-replace-read-from)
    (:label occur
            :op-kind    minibuf-cmd
            :command    foreign-regexp/occur
            ;; required by minibuf-cmd.
            :transition-allowed-in foreign-regexp/occur)
    (:label noinc-fwd
            :op-kind    minibuf-cmd
            :command    foreign-regexp/non-incremental/search-forward
            ;; required by minibuf-cmd.
            :transition-allowed-in foreign-regexp/non-incremental/search-forward)
    (:label noinc-bkwd
            :op-kind    minibuf-cmd
            :command    foreign-regexp/non-incremental/search-backward
            ;; required by minibuf-cmd.
            :transition-allowed-in foreign-regexp/non-incremental/search-backward)
    (:label align
            :op-kind    minibuf-cmd
            :command    foreign-regexp/align
            ;; required by minibuf-cmd.
            :transition-allowed-in foreign-regexp/align))
  "Not documented yet.")


;; ----------------------------------------------------------------------------
;;
;;  Private Macros
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/transition/.setup sym-table) => VOID
;; ----------------------------------------------------------------------------
(defmacro foreign-regexp/transition/.setup (sym-table)
  "Define transitions among each command."
  (let ((retval (list 'progn)))
    (dolist (rec (symbol-value sym-table))
      (let ((table                 (copy-list (symbol-value sym-table)))
            (label                 (cadr (memq :label   rec)))
            (op-kind               (cadr (memq :op-kind rec)))
            (command               (cadr (memq :command rec)))
            (transition-allowed-in (cadr (memq :transition-allowed-in rec))))
        
        (setq table (delq rec table))
        
        (dolist (targ-rec table)
          (let ((targ-label   (cadr (memq :label   targ-rec)))
                (targ-op-kind (cadr (memq :op-kind targ-rec)))
                (targ-command (cadr (memq :command targ-rec))))
            (case op-kind
              ((re-builder-cmd)
               ;; Nothing to do.
               ;;
               ;; foreign-regexp commands which makes transition from
               ;; `re-builder' are defined as `foreign-regexp/re-builder/run-*'.
               )
              ((isearch-cmd)
               (let ((ad-name-make-transition-to  (intern
                                                   (format
                                                    "foreign-regexp/transition/%s/make-transition-to-%s"
                                                    label targ-label)))
                     (fn-name-turn-on-transition  (intern
                                                   (format
                                                    "foreign-regexp/transition/%s/turn-on-make-transition-to-%s"
                                                    label targ-label)))
                     (fn-name-turn-off-transition (intern
                                                   (format
                                                    "foreign-regexp/transition/%s/turn-off-make-transition-to-%s"
                                                    label targ-label))))
                 (nconc
                  retval
                  `(
                    ;; Advise other foreign-regexp commands so that
                    ;; we can exit current isearch session and
                    ;; make transition to them.
                    (defadvice ,targ-command (around
                                              ,ad-name-make-transition-to
                                              (&rest args))
                      "Exit current isearch session and make a
transition to another foreign-regexp command."
                      ;; DEBUGGING
                      (foreign-regexp/.debug foreign-regexp/transition/debug-advices
                                             "TRANSITION: CALL: %s, %s"
                                             (quote ,targ-command)
                                             (quote ,ad-name-make-transition-to))
                      
                      (lexical-let ((regexp isearch-string))
                        (unwind-protect
                            (progn
                              ;; When `isearch-string' is empty,
                              ;; `isearch-exit' calls `isearch-edit-string'
                              ;; and isearch won't be quit. So we set
                              ;; dummy string to `isearch-string' if
                              ;; it is empty.
                              (when (= 0 (length isearch-string))
                                (setq isearch-string " "))
                              (isearch-exit))
                          ;; `isearch-exit' throws something in some case,
                          ;; so another foreign-regexp command should be called
                          ;; from within protected form.
                          ,@(case targ-op-kind
                              ((re-builder-cmd)
                               ;; NOTE: Do not run `re-builder' with timer,
                               ;;       or window won't be switched to
                               ;;       *RE-Builder* properly.
                               `((case reb-re-syntax
                                   ((foreign-regexp)
                                    (let ((this-command (quote ,targ-command)))
                                      (call-interactively (quote ,targ-command)))
                                    (with-current-buffer (get-buffer reb-buffer)
                                      (delete-region (point-min) (point-max))
                                      (insert regexp)))
                                   (t
                                    (error "[foreign-regexp] RE-Builder syntax is not `foreign-regexp'.")))))
                              ((minibuf-cmd)
                               `((run-with-idle-timer
                                  0 nil
                                  (lambda ()
                                    ;; Call another foreign-regexp command with setting
                                    ;; initial contents to `read-from-minibuffer'.
                                    (foreign-regexp/read-from-minibuf/with-initial-contents
                                     regexp
                                     (let ((this-command (quote ,targ-command)))
                                       (call-interactively (quote ,targ-command))))))))))))
                    ;; Should be turned on by `isearch-mode-hook'.
                    (foreign-regexp/ad-disable (quote ,targ-command) 'around (quote ,ad-name-make-transition-to))
                    (foreign-regexp/ad-activate (quote ,targ-command))
                    
                    ;; When `foreign-regexp/isearch' will be turned on,
                    ;; advise another foreign-regexp commands so that
                    ;; we can exit an isearch session and make a
                    ;; transition to them.
                    (defun ,fn-name-turn-on-transition ()
                      (foreign-regexp/ad-enable (quote ,targ-command) 'around (quote ,ad-name-make-transition-to))
                      (foreign-regexp/ad-activate (quote ,targ-command)))
                    (add-hook 'isearch-mode-hook (quote ,fn-name-turn-on-transition))
                    
                    ;; Disable advices of another foreign-regexp commands
                    ;; when `foreign-regexp/isearch' will be turned off.
                    (defun ,fn-name-turn-off-transition ()
                      (foreign-regexp/ad-disable (quote ,targ-command) 'around (quote ,ad-name-make-transition-to))
                      (foreign-regexp/ad-activate (quote ,targ-command)))
                    (add-hook 'isearch-mode-end-hook (quote ,fn-name-turn-off-transition))))))
              
              ((minibuf-cmd)
               (let ((ad-name-catch-transition-to (intern
                                                   (format
                                                    "foreign-regexp/transition/%s/catch-transition-to-%s"
                                                    label targ-label)))
                     (ad-name-throw-transition-to (intern
                                                   (format
                                                    "foreign-regexp/transition/%s/throw-transition-to-%s"
                                                    label targ-label)))
                     (ad-name-allow-transition    (intern
                                                   (format
                                                    "foreign-regexp/transition/%s/allow-transition-to-%s"
                                                    label targ-label)))
                     (g-transition-allowed-p      (gensym)))

                 ;; For two purposes below, we wrap each foreign-regexp
                 ;; command, which runs `read-from-minibuffer' to
                 ;; read input from minibuffer, with a wrapper command.
                 ;;
                 ;;   1. To set initial contents of `read-from-minibuffer',
                 ;;      we have to run advices of a command before
                 ;;      `interactive' form is run.
                 ;;
                 ;;      So we wrap a command with a wrapper
                 ;;      command and set advice to it.
                 ;;
                 ;;      Now we can run `interactive' form of a
                 ;;      command after advices has run, by calling
                 ;;      the command interactively from inside
                 ;;      of a wrapper command.
                 ;;
                 ;;   2. Remember running foreign-regexp command to
                 ;;      prevent duplicated calls while minibuffer
                 ;;      is active.
                 (eval `(progn
                          (defadvice ,command (around foreign-regexp/orig-fn (&rest args))
                            (interactive)
                            "A wrapper of original command to run advices before
interactive form is run.
And remember running command to prevent duplicate calls."
                            (if (eq this-command
                                    (quote ,command))
                                ;; Called interactively.
                                (unwind-protect
                                    (progn
                                      ;; Remember current command to
                                      ;; prevent duplicate calls.
                                      (setq foreign-regexp/transition/.running-cmd this-command)
                                      (setq ad-return-value
                                            (call-interactively (ad-get-orig-definition (quote ,command)))))
                                  (setq foreign-regexp/transition/.running-cmd nil))
                              ;; Called non-interactively.
                              (setq ad-return-value
                                    (apply (ad-get-orig-definition (quote ,command)) args))))
                          (foreign-regexp/ad-activate (quote ,command))))
                 (nconc
                  retval
                  `(
                    ;; Advice a command to allow making transition to
                    ;; another foreign-regexp command, during it is running.
                    (defadvice ,transition-allowed-in (around
                                                       ,ad-name-allow-transition
                                                       (&rest args))
                      "Allow making transition to another foreign-regexp command
while this function is running."
                      ;; DEBUGGING
                      (foreign-regexp/.debug foreign-regexp/transition/debug-advices
                                             "TRANSITION: CALL: %s, %s"
                                             (quote ,transition-allowed-in)
                                             (quote ,ad-name-allow-transition))
                      
                      (let ((,g-transition-allowed-p t))
                        ad-do-it))
                    (foreign-regexp/ad-activate (quote ,transition-allowed-in))
                    
                    ;; Advise to foreign-regexp commands, which reads
                    ;; input from minibuffer, to make transition to
                    ;; another foreign-regexp command when a tag
                    ;; is thrown.
                    (defadvice ,command (around
                                         ,ad-name-catch-transition-to
                                         (&rest args))
                      "Make a transition to another foreign-regexp command."
                      
                      ;; DEBUGGING
                      (foreign-regexp/.debug foreign-regexp/transition/debug-advices
                                             "TRANSITION: CALL: %s, %s"
                                             (quote ,command)
                                             (quote ,ad-name-catch-transition-to))
                      
                      ;; Prevent duplicate calls.
                      (when (eq this-command
                                foreign-regexp/transition/.running-cmd)
                        (error "[foreign-regexp] Command attempted to use minibuffer while in minibuffer"))
                      
                      (unwind-protect
                          (progn
                            (foreign-regexp/ad-enable (quote ,targ-command)
                                                      'around
                                                      (quote ,ad-name-throw-transition-to))
                            (foreign-regexp/ad-activate (quote ,targ-command))
                            
                            (foreign-regexp/catch-case var
                                                       ad-do-it
                                                       (,ad-name-throw-transition-to
                                                        ;; DEBUGGING
                                                        (foreign-regexp/.debug foreign-regexp/transition/debug-advices
                                                                               "TRANSITION: CATCHED BY: %s, %s\n"
                                                                               (quote ,command)
                                                                               (quote ,ad-name-catch-transition-to))
                                                        
                                                        (lexical-let ((regexp (cadr var))
                                                                      (orig-messasge-fn
                                                                       (symbol-function 'message)))
                                                          ,@(case targ-op-kind
                                                              ((re-builder-cmd)
                                                               ;; NOTE: Do not run `re-builder' with timer,
                                                               ;;       or window won't be switched to
                                                               ;;       *RE-Builder* properly.
                                                               `((case reb-re-syntax
                                                                   ((foreign-regexp)
                                                                    (let ((this-command (quote ,targ-command)))
                                                                      (call-interactively (quote ,targ-command)))
                                                                    (with-current-buffer (get-buffer reb-buffer)
                                                                      (delete-region (point-min) (point-max))
                                                                      (insert regexp)))
                                                                   (t
                                                                    (error "[foreign-regexp] RE-Builder syntax is not `foreign-regexp'.")))))
                                                              ((isearch-cmd)
                                                               `((run-with-idle-timer
                                                                  0 nil
                                                                  (lambda ()
                                                                    (add-hook 'isearch-mode-hook
                                                                              (foreign-regexp/alambda ()
                                                                                                      (remove-hook 'isearch-mode-hook
                                                                                                                   #'self)
                                                                                                      (isearch-push-state)
                                                                                                      (setq isearch-string  regexp
                                                                                                            isearch-message regexp)))
                                                                    (let ((this-command (quote ,targ-command)))
                                                                      (call-interactively (quote ,targ-command)))))))
                                                              ((minibuf-cmd)
                                                               `((run-with-idle-timer
                                                                  0 nil
                                                                  (lambda ()
                                                                    ;; Call another foreign-regexp command with setting
                                                                    ;; initial contents to `read-from-minibuffer'.
                                                                    (foreign-regexp/read-from-minibuf/with-initial-contents
                                                                     regexp
                                                                     (let ((this-command (quote ,targ-command)))
                                                                       (call-interactively (quote ,targ-command)))))))))
                                                          ;; FIXME: Turn off an annoying message
                                                          ;;        "Back to top level.".
                                                          (top-level)))))
                        (foreign-regexp/ad-disable (quote ,targ-command) 'around (quote ,ad-name-throw-transition-to))
                        (foreign-regexp/ad-activate (quote ,targ-command))))
                    (foreign-regexp/ad-activate (quote ,command))
                    
                    ;; Advise other foreign-regexp commands to throw a tag
                    ;; so that we can exit current foreign-regexp command and
                    ;; make transition to another one.
                    (defadvice ,targ-command (around ,ad-name-throw-transition-to (&rest args))
                      "Throw a tag so that we can exit current foreign-regexp command and
make a transition to another one.

Current contents of minibuffer will be thrown
as the value of a tag."
                      ;; DEBUGGING
                      (foreign-regexp/.debug foreign-regexp/transition/debug-advices
                                             "TRANSITION: CALL: %s, %s"
                                             (quote ,targ-command)
                                             (quote ,ad-name-throw-transition-to))

                      ;; MEMO-1: This advice will be enabled/disabled in
                      ;;         advice of each foreign-regexp command.
                      ;; MEMO-2: When the flag `,g-transition-allowed-p' is
                      ;;         nil, this advice behaves as if it was not there.
                      ;; MEMO-3: `,g-transition-allowed-p' is assigned a different
                      ;;         symbol for each foreign-regexp commands.
                      (cond
                       ((and (boundp (quote ,g-transition-allowed-p))
                             ,g-transition-allowed-p)
                        (let ((contents (foreign-regexp/read-minibuf-contents)))
                          (when contents
                            ;; DEBUGGING
                            (foreign-regexp/.debug foreign-regexp/transition/debug-advices
                                                   "TRANSITION: THROWN: %s ,%s"
                                                   (quote ,ad-name-throw-transition-to)
                                                   contents)
                            
                            (throw (quote ,ad-name-throw-transition-to)
                                   contents))))
                       (t
                        ad-do-it))))))))))))
    retval))


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/transition/setup) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/transition/setup ()
  "Setup transitions among foreign-regexp commands."
  (eval
   '(foreign-regexp/transition/.setup foreign-regexp/transition/command-table)))


;; ----------------------------------------------------------------------------
;;
;;  Main
;;
;; ----------------------------------------------------------------------------

(foreign-regexp/transition/setup)


;;; ===========================================================================
;;;
;;;  Variables and Functions for managing "Foreign-Regexp Types".
;;;
;;; ===========================================================================

(defcustom foreign-regexp/regexp-type nil
  "\"Type of the foreign regular expression\" that you want to use."
  :type    'foreign-regexp/regexp-type/custom-widget/regexp-type-selector
  :group   'foreign-regexp
  :set     '(lambda (sym val)
              (cond
               ((fboundp 'foreign-regexp/regexp-type/set)
                (foreign-regexp/regexp-type/set val))
               (t
                ;; When this file is being loaded,
                ;; `foreign-regexp/regexp-type/set' will be called
                ;; from `Main' section by timer.
                (setq foreign-regexp/regexp-type val)))))

(defvar foreign-regexp/regexp-type/.type-alst nil
  ;; FIXME: Write document.
  "Private variable.")


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/regexp-type/define &key name
;;                                       tag
;;                                       input-coding-system
;;                                       output-coding-system
;;                                       indicator-case-fold
;;                                       indicator-no-case-fold
;;                                       indicator-ext-regexp
;;                                       indicator-no-ext-regexp
;;                                       indicator-dot-match
;;                                       indicator-no-dot-match
;;                                       indicator-separator
;;                                       cmd-path-replace
;;                                       cmd-path-quote-meta
;;                                       script-replace
;;                                       script-quote-meta
;;                                       wsp-regexp-for-align) => VOID
;; ----------------------------------------------------------------------------
(defun* foreign-regexp/regexp-type/define (&key name
                                                tag
                                                input-coding-system
                                                output-coding-system
                                                indicator-case-fold
                                                indicator-no-case-fold
                                                indicator-ext-regexp
                                                indicator-no-ext-regexp
                                                indicator-dot-match
                                                indicator-no-dot-match
                                                indicator-eval-replacement
                                                indicator-no-eval-replacement
                                                indicator-separator
                                                script-replace
                                                script-quote-meta
                                                wsp-regexp-for-align
                                                cmd-path-replace
                                                cmd-path-quote-meta)
  ;; FIXME: Write document.
  "Define a Foreign Regexp Type.

Arguments are:

  NAME:
        Name of a Foreign Regexp. This must be a symbol.

  TAG:
        Name of a Foreign Regexp by human-friendly representation.
        TAG will be used as label string of menu item and custom
        widget. This must be a string.

  INPUT-CODING-SYSTEM:
        See `foreign-regexp/input-coding-system'.

  OUTPUT-CODING-SYSTEM:
        See `foreign-regexp/output-coding-system'.
      
  INDICATOR-CASE-FOLD:
        See `foreign-regexp/search-option-indicator/case-fold-str'.
      
  INDICATOR-NO-CASE-FOLD:
        See `foreign-regexp/search-option-indicator/no-case-fold-str'.
      
  INDICATOR-DOT-MATCH:
        See `foreign-regexp/search-option-indicator/dot-match-str'.
      
  INDICATOR-NO-DOT-MATCH:
        See `foreign-regexp/search-option-indicator/no-dot-match-str'.
      
  INDICATOR-EXT-REGEXP:
        See `foreign-regexp/search-option-indicator/ext-regexp-str'.
      
  INDICATOR-NO-EXT-REGEXP:
        See `foreign-regexp/search-option-indicator/no-ext-regexp-str'.
      
  INDICATOR-EVAL-REPLACEMENT:
        See `foreign-regexp/search-option-indicator/eval-replacement-str'.
  
  INDICATOR-NO-EVAL-REPLACEMENT:
        See `foreign-regexp/search-option-indicator/no-eval-replacement-str'.
  
  INDICATOR-SEPARATOR:
        See `foreign-regexp/search-option-indicator/separator-str'.
      
  CMD-PATH-REPLACE:
        See `foreign-regexp/replace/external-command'.

  CMD-PATH-QUOTE-META:
        See `foreign-regexp/quote-meta/external-command'.

  SCRIPT-REPLACE:
        See `foreign-regexp/replace/shell-script'.

  SCRIPT-QUOTE-META:
        See `foreign-regexp/quote-meta/shell-script'.

  WSP-REGEXP-FOR-ALIGN:
        See `foreign-regexp/align/wsp-regexp'."
  ;; Validation
  ;;
  (or name                          (error "[foreign-regexp] No `:name'!"))
  (or tag                           (error "[foreign-regexp] No `:tag'!"))
  (or input-coding-system           (error "[foreign-regexp] No `:input-coding-system'!"))
  (or output-coding-system          (error "[foreign-regexp] No `:output-coding-system'!"))
  (or indicator-case-fold           (error "[foreign-regexp] No `:indicator-case-fold'!"))
  (or indicator-no-case-fold        (error "[foreign-regexp] No `:indicator-no-case-fold'!"))
  (or indicator-ext-regexp          (error "[foreign-regexp] No `:indicator-ext-regexp'!"))
  (or indicator-no-ext-regexp       (error "[foreign-regexp] No `:indicator-no-ext-regexp'!"))
  (or indicator-dot-match           (error "[foreign-regexp] No `:indicator-dot-match'!"))
  (or indicator-no-dot-match        (error "[foreign-regexp] No `:indicator-no-dot-match'!"))
  (or indicator-eval-replacement    (error "[foreign-regexp] No `:indicator-eval-replacement'!"))
  (or indicator-no-eval-replacement (error "[foreign-regexp] No `:indicator-no-eval-replacement'!"))
  (or indicator-separator           (error "[foreign-regexp] No `:indicator-separator'!"))
  (or wsp-regexp-for-align          (error "[foreign-regexp] No `:wsp-regexp-for-align'!"))

  (or script-replace
      cmd-path-replace
      (error "[foreign-regexp] No `:script-replace' or `:cmd-path-replace'!"))
  (or script-quote-meta
      cmd-path-quote-meta
      (error "[foreign-regexp] No `:script-quote-meta' or `:cmd-path-quote-meta'!"))

  (foreign-regexp/regexp-type/forget name)
  (push (list name
              :name                          name
              :tag                           tag
              :input-coding-system           input-coding-system
              :output-coding-system          output-coding-system
              :indicator-case-fold           indicator-case-fold
              :indicator-no-case-fold        indicator-no-case-fold
              :indicator-ext-regexp          indicator-ext-regexp
              :indicator-no-ext-regexp       indicator-no-ext-regexp
              :indicator-dot-match           indicator-dot-match
              :indicator-no-dot-match        indicator-no-dot-match
              :indicator-eval-replacement    indicator-eval-replacement
              :indicator-no-eval-replacement indicator-no-eval-replacement
              :indicator-separator           indicator-separator
              :script-replace                script-replace
              :script-quote-meta             script-quote-meta
              :cmd-path-replace              cmd-path-replace
              :cmd-path-quote-meta           cmd-path-quote-meta
              :wsp-regexp-for-align          wsp-regexp-for-align)
        foreign-regexp/regexp-type/.type-alst)

  ;; When regex type is redefined, reload it.
  (when (eq foreign-regexp/regexp-type name)
    (foreign-regexp/regexp-type/set name))
  
  (foreign-regexp/regexp-type/custom-widget/regexp-type-selector/update))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/regexp-type/forget NAME) => ALIST
;; ----------------------------------------------------------------------------
(defun foreign-regexp/regexp-type/forget (name)
  "Remove an regexp type NAME from `foreign-regexp/regexp-type/.type-alst'."
  (setq foreign-regexp/regexp-type/.type-alst
        (remove-if '(lambda (pair) (eq name (car pair)))
                   foreign-regexp/regexp-type/.type-alst)))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/regexp-type/set NAME) => VOID
;; ----------------------------------------------------------------------------
(defvar foreign-regexp/regexp-type/.history nil
  "History list for a command `foreign-regexp/regexp-type/set'.")

(defun foreign-regexp/regexp-type/set (name)
  "Activate regexp type NAME."
  (interactive
   (list
    (intern
     (completing-read
      (format "Regexp type for foreign regexp (defalut %s): "
              foreign-regexp/regexp-type)
      (sort (mapcar (lambda (sym)
                      (format "%s" (car sym)))
                    foreign-regexp/regexp-type/.type-alst)
            #'string<)
      nil t nil
      'foreign-regexp/regexp-type/.history
      (format "%s" foreign-regexp/regexp-type) nil))))
  (let ((kv-lst (or (cdr (assq name foreign-regexp/regexp-type/.type-alst))
                    (cond
                     ((null name)
                      nil)
                     (t
                      (error "No such foreign-regexp definition `%s'." name))))))
    (setq foreign-regexp/input-coding-system                             (cadr (memq :input-coding-system           kv-lst)))
    (setq foreign-regexp/output-coding-system                            (cadr (memq :output-coding-system          kv-lst)))
    (setq foreign-regexp/search-option-indicator/case-fold-str           (cadr (memq :indicator-case-fold           kv-lst)))
    (setq foreign-regexp/search-option-indicator/no-case-fold-str        (cadr (memq :indicator-no-case-fold        kv-lst)))
    (setq foreign-regexp/search-option-indicator/ext-regexp-str          (cadr (memq :indicator-ext-regexp          kv-lst)))
    (setq foreign-regexp/search-option-indicator/no-ext-regexp-str       (cadr (memq :indicator-no-ext-regexp       kv-lst)))
    (setq foreign-regexp/search-option-indicator/dot-match-str           (cadr (memq :indicator-dot-match           kv-lst)))
    (setq foreign-regexp/search-option-indicator/no-dot-match-str        (cadr (memq :indicator-no-dot-match        kv-lst)))
    (setq foreign-regexp/search-option-indicator/eval-replacement-str    (cadr (memq :indicator-eval-replacement    kv-lst)))
    (setq foreign-regexp/search-option-indicator/no-eval-replacement-str (cadr (memq :indicator-no-eval-replacement kv-lst)))
    (setq foreign-regexp/search-option-indicator/separator-str           (cadr (memq :indicator-separator           kv-lst)))
    (setq foreign-regexp/replace/external-command                        (cadr (memq :cmd-path-replace              kv-lst)))
    (setq foreign-regexp/replace/shell-script                            (cadr (memq :script-replace                kv-lst)))
    (setq foreign-regexp/quote-meta/external-command                     (cadr (memq :cmd-path-quote-meta           kv-lst)))
    (setq foreign-regexp/quote-meta/shell-script                         (cadr (memq :script-quote-meta             kv-lst)))
    (setq foreign-regexp/align/wsp-regexp                                (cadr (memq :wsp-regexp-for-align          kv-lst)))

    (setq foreign-regexp/regexp-type name)
    (cond
     ((null name)
      (message "[foreign-regexp] Foreign-Regexp type is set to \"None\"."))
     (t
      (message "[foreign-regexp] Foreign-Regexp type is set to \"%s\"" name)))))

;; ----------------------------------------------------------------------------
;;  (foreign-regexp/regexp-type/custom-widget/regexp-type-selector/update NAME) => VOID
;; ----------------------------------------------------------------------------
(defun foreign-regexp/regexp-type/custom-widget/regexp-type-selector/update ()
  "Update a widget `foreign-regexp/regexp-type/custom-widget/regexp-type-selector' which
will be used to customize user option `foreign-regexp/regexp-type'."
  (define-widget 'foreign-regexp/regexp-type/custom-widget/regexp-type-selector 'lazy
    "A widget, which will be used to customize user option
`foreign-regexp/regexp-type'."
    :offset 4
    :tag    "Type"
    :type   `(choice
              (const :tag "None" nil)
              ,@(mapcar (lambda (pair)
                          (list 'const
                                :tag (cadr (memq :tag (cdr pair)))
                                (car pair)))
                        foreign-regexp/regexp-type/.type-alst))))


;; ----------------------------------------------------------------------------
;;
;;  Main
;;
;; ----------------------------------------------------------------------------
(run-with-idle-timer
 0 nil
 '(lambda ()
    (foreign-regexp/regexp-type/set foreign-regexp/regexp-type)))


;;; ===========================================================================
;;;
;;;  Predefined Shell Scripts.
;;;
;;; ===========================================================================

(defvar foreign-regexp/shell-script/foreign-regexp-quote-meta-aux.pl "#!/usr/bin/env perl
use strict;
use warnings;
use 5.008;

use Encode;
use utf8;

use English qw( -no_match_vars );
use FileHandle;

sub escape_perl_str_for_emacs {
    my $r_txt = shift;
    ${$r_txt} =~ s/\\\\/\\\\\\\\/og;
    ${$r_txt} =~ s/\"/\\\\\"/og;
}

sub main () {
    my $fn_out    = shift @ARGV or die \"No output file name!\";
    my $fn_pat    = shift @ARGV or die \"No pattern file name!\";
    my $code      = 'utf8';
    
    umask 0177;
    
    my($str_pat);
    use PerlIO::encoding;
    local $PerlIO::encoding::fallback = Encode::FB_CROAK(); # Die on invalid char.
    {
        local $INPUT_RECORD_SEPARATOR = undef;
        $str_pat = FileHandle->new($fn_pat, \"<:encoding($code)\")->getline;
        $str_pat = quotemeta($str_pat);
        escape_perl_str_for_emacs(\\$str_pat)
    }
    
    {
        my $fh_out = FileHandle->new($fn_out, \">:encoding($code)\");
        
        print $fh_out \"(setq result \\\"${str_pat}\\\")\\n\";
        print $fh_out \";;; EOF\\n\";
    }
    
    exit 0;
}

main();

# EOF

")

(defvar foreign-regexp/shell-script/foreign-regexp-quote-meta-aux.rb "#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort \"Ruby version is too old (1.9 or later is required).\" if RUBY_VERSION < \"1.9\"

def escape_ruby_str_for_emacs! (str)
  str.gsub!(/\\\\/) {'\\\\\\\\'}
  str.gsub!(/\"/ ) {'\\\\\"'}
end

def main ()
  fn_out, fn_pat = ARGV
  
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  retval = Regexp.escape(str_pat)
  escape_ruby_str_for_emacs!(retval)
  
  print '(setq result \"'
  print retval
  print '\")'

  exit 0
end

main

# EOF

")

(defvar foreign-regexp/shell-script/foreign-regexp-quote-meta-aux.js "#!/usr/bin/env node
// -*- coding: utf-8-unix -*-

const ENCODING = 'utf8';
const FS       = require('fs');
const UTIL     = require('util');

function write (path, str) {
    if (path == '/dev/stdout') UTIL.print(str)
    else FS.appendFileSync(path, str, ENCODING);
}

function escape_js_str_for_emacs (txt) {
    var retval = String(txt);
    retval = retval.replace(/\\\\/g, \"\\\\\\\\\");
    retval = retval.replace(/\"/g, \"\\\\\\\"\");
    return retval;
}

function quotemeta (txt) {
    return txt.replace(/[.|()[\\]{}+\\\\^$*?]/g, '\\\\$&');
}

function main (argv) {
    var fn_out   = argv[2];
    var fn_regx  = argv[3];
    
    var str_regx = FS.readFileSync(fn_regx, ENCODING);
    
    str_regx = quotemeta(str_regx);
    str_regx = escape_js_str_for_emacs(str_regx);
    
    write(fn_out, \"(setq result \\\"\");
    write(fn_out, str_regx);
    write(fn_out, \"\\\")\\n\");
}


try {
    main(process.argv);
} catch (e) {
    console.error(e.message); // Do not show stack.
    process.exit(1);
}

process.exit(0);

// EOF

")

(defvar foreign-regexp/shell-script/foreign-regexp-quote-meta-aux.py "#!/usr/bin/env python

import sys, re, codecs

def escape_python_str_for_emacs (txt):
    txt = re.sub(r'\\\\', r'\\\\\\\\', txt)
    txt = re.sub(r'\"', r'\\\\\"', txt)
    return txt

def main ():
    [prog, fn_out, fn_regx] = sys.argv
    
    str_regx  = codecs.open(fn_regx, 'r', 'utf_8').read()
    
    f = codecs.open(fn_out, 'w', 'utf8')
    
    str_regx = re.escape(str_regx);
    str_regx = escape_python_str_for_emacs(str_regx);
    
    f.write(\"(setq result \\\"\")
    f.write(str_regx)
    f.write(\"\\\")\\n;;; EOF\\n\")
    
    exit(0)

main()

# EOF

")

(defvar foreign-regexp/shell-script/foreign-regexp-replace-aux.pl "#!/usr/bin/env perl
# -*- coding: utf-8-unix -*-
use strict;
use warnings;
use 5.008;

use Encode;
use utf8;

package main;
use English qw( -no_match_vars );
use FileHandle;

sub interpolate_fn_gen {
    # Interpolate replacement string in environment
    # which has no lexical variable.
    #
    # Special-variables in the replacement string
    # will be interpolated.
    eval 'sub {\"'. escape_str_for_interpolate_fn_gen(${$_[0]}) .'\"}';
}

sub eval_fn_gen {
    # Eval replacement string in environment
    # which has no lexical variable.
    eval 'sub {'.${$_[0]}.'}';
}

sub escape_str_for_interpolate_fn_gen {
    my $txt = shift;
    $txt =~ s/\"/\\\\\"/og;
    $txt
}

sub escape_perl_str_for_emacs {
    my $r_txt = shift;
    ${$r_txt} =~ s/\\\\/\\\\\\\\/og;
    ${$r_txt} =~ s/\"/\\\\\"/og;
}

sub process_replace {
    my $r_str_body   = shift;
    my $r_str_regx   = shift;
    my $r_str_rpla   = shift;
    my $dot_p        = shift;
    my $case_p       = shift;
    my $ext_p        = shift;
    my $eval_p       = shift;
    my $limit        = shift;
    my $pos_start    = shift;
    my $rgn_beg      = shift;
    my $rgn_end      = shift;
    
    my $pos_wrap_end = undef;
    my $count        = 0;
    
    my $regx = eval (\"qr/\\${\\$r_str_regx}/mo\" .
                     ( $dot_p  ? \"s\" : \"\") .
                     (!$case_p ? \"i\" : \"\") .
                     ( $ext_p  ? \"x\" : \"\"));
    die $EVAL_ERROR if $EVAL_ERROR;
    
    my $interpolate_fn = ($eval_p
                          ? eval_fn_gen($r_str_rpla)
                          : interpolate_fn_gen($r_str_rpla));
    die \"Syntax error in replacement \\\"${$r_str_rpla}\\\":\\n${EVAL_ERROR}\" if $EVAL_ERROR;
    
    my $replace_fn = sub {
        my $rgn_beg = shift;
        my $rgn_end = shift;
        my $wrap_p  = shift;
        
        
        pos(${$r_str_body}) = $rgn_beg;
        
        while (((defined $limit) ? ($count < $limit) : 1) && (${$r_str_body} =~ m/${regx}/g)) {
            my $match_beg = $LAST_MATCH_START[0];
            my $match_end = $LAST_MATCH_END  [0];
            
            last if ($match_end > $rgn_end);
            last if ($wrap_p && (defined $pos_wrap_end) && ($pos_wrap_end <= $match_beg));
            $pos_wrap_end = $match_beg if ((not $wrap_p) && (not (defined $pos_wrap_end)));
            
            my $replacement = eval { $interpolate_fn->() };
            die \"Error while interpolating replacement \\\"${$r_str_rpla}\\\":\\n${EVAL_ERROR}\" if $EVAL_ERROR;
            
            escape_perl_str_for_emacs(\\$replacement);
            
            print \" ((\";
            print $match_beg, ' ';
            print $match_end, ' ';
            foreach my $i (1 .. $#LAST_MATCH_START) {
                print $LAST_MATCH_START[$i], ' ';
                print $LAST_MATCH_END  [$i], ' ';
            }
            print \" )\";
            print '\"', $replacement, '\"';
            print \" )\", \"\\n\";
            
            ++$count;
        }
    };
    
    $rgn_beg   = $rgn_beg || 0;
    $rgn_end   = $rgn_end || length(${$r_str_body});
    $pos_start = (($pos_start < $rgn_beg)
                  ? $rgn_beg
                  : (($pos_start > $rgn_end)
                     ? $rgn_end
                     : $pos_start));
    
    print \"(setq result '(\";
    print \" (\";
    $replace_fn->($pos_start, $rgn_end, 0);
    print \" )\";
    
    # Search wrap around.
    print \" (\";
    $replace_fn->($rgn_beg,
                  (defined $pos_wrap_end) ? $pos_wrap_end : $rgn_end,
                  1);
    print \" )\";
    print \"))\", \"\\n\";
    print \";;; EOF\", \"\\n\";
}

sub main () {
    my $fn_body   = shift @ARGV or die \"No input file name!\";
    my $fn_out    = shift @ARGV or die \"No output file name!\";
    my $fn_regx   = shift @ARGV or die \"No regexp file name!\";
    my $fn_rpla   = @ARGV ? shift @ARGV : die \"No replacement file!\";
    my $dot_p     = @ARGV ? shift(@ARGV) : die \"No dot matches new line flag.\";
    my $case_p    = @ARGV ? shift(@ARGV) : die \"No case sensitive flag.\";
    my $ext_p     = @ARGV ? shift(@ARGV) : die \"No extended regular expression flag.\";
    my $eval_p    = @ARGV ? shift(@ARGV) : die \"No eval replacement flag.\";
    my $limit     = @ARGV ? shift(@ARGV) : die \"No search limit.\";
    my $pos_start = @ARGV ? shift(@ARGV) : die \"No start position.\";
    my $rgn_beg   = shift @ARGV;
    my $rgn_end   = shift @ARGV;
    
    my $code      = 'utf8';
    
    my($str_body, $str_regx, $str_rpla);
    
    use PerlIO::encoding;
    local $PerlIO::encoding::fallback = Encode::FB_CROAK(); # Die on invalid char.
    {
        local $INPUT_RECORD_SEPARATOR = undef;
        $str_body = FileHandle->new($fn_body, \"<:encoding($code)\")->getline;
        $str_regx = FileHandle->new($fn_regx, \"<:encoding($code)\")->getline;
        $str_rpla = $fn_rpla ? FileHandle->new($fn_rpla, \"<:encoding($code)\")->getline : \"\";
    }
    
    umask 0177;
    *STDOUT = FileHandle->new($fn_out, \">:encoding($code)\");
    
    process_replace(\\$str_body, \\$str_regx, \\$str_rpla,
                    $dot_p, $case_p, $ext_p, $eval_p,
                    length($limit) ? $limit : undef, $pos_start, $rgn_beg, $rgn_end);
    
    exit 0;
}

main();

# EOF

")

(defvar foreign-regexp/shell-script/foreign-regexp-replace-aux.rb "#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort \"Ruby version is too old (1.9 or later is required).\" if RUBY_VERSION < \"1.9\"

def escape_str_for_interpolate_fn_gen (str)
  str.gsub(/\"/ ){'\\\\\"'}
end

def escape_ruby_str_for_emacs! (str)
  str.gsub!(/\\\\/) {'\\\\\\\\'}
  str.gsub!(/\"/ ) {'\\\\\"'}
end

def process_replace (__str_body__, __str_regx__, __str_rpla__,
                     __dot_p__, __case_p__, __ext_p__, __eval_p__,
                     __limit__, __pos_start__, __rgn_beg__, __rgn_end__)
  __pos_wrap_end__ = nil
  __count__        = 0
  
  __regx__ = Regexp.new(__str_regx__, ((__dot_p__  ? Regexp::MULTILINE : 0)  |
                                       (__case_p__ ? 0 : Regexp::IGNORECASE) |
                                       (__ext_p__  ? Regexp::EXTENDED  : 0)))
  __interpolate_fn__ = begin
                         (__eval_p__ ?
                          eval('Proc.new '+__str_rpla__) :
                          eval('Proc.new {\"'+escape_str_for_interpolate_fn_gen(__str_rpla__)+'\"}'))
                       rescue SyntaxError, ArgumentError
                         $stderr.print \"Syntax error in replacement \\\"#{__str_rpla__}\\\".\\n\"
                         $stderr.print $!.message
                         exit! 1
                       end
  
  __replace_fn__ = Proc.new { |__rgn_beg__, __rgn_end__, __wrap_p__|
    __pos__ = __rgn_beg__
    __last_0_width_pos__ = nil
    
    while ((__limit__ ? (__count__ < __limit__) : true)&& __str_body__.match(__regx__, __pos__)) do
      m = Regexp.last_match
      
      __match_beg__ = m.begin(0)
      __match_end__ = m.end(0)
      __0_width_p__ = (__match_beg__ == __match_end__)
      
      break if (__match_end__ > __rgn_end__)
      break if (__wrap_p__ && __pos_wrap_end__ && (__pos_wrap_end__ <= __match_beg__))
      __pos_wrap_end__ = __match_beg__ if ((not __wrap_p__) && (not __pos_wrap_end__))
      
      if (__0_width_p__ && __last_0_width_pos__ && (__match_beg__ == __last_0_width_pos__)) then
        # Do not enter into endless loop.
        __pos__ += 1
        break if (__pos__ > __rgn_end__)
        next
      elsif __0_width_p__ then
        __last_0_width_pos__ = __match_beg__
      else
        __last_0_width_pos__ = nil
      end
      
      __replacement__ = begin
                          __interpolate_fn__.call(m[0]).to_s
                        rescue Exception
                          $stderr.print \"Error while evaluating replacement \\\"#{__str_rpla__}\\\".\\n\"
                          $stderr.print $!.message, \"\\n\"
                          exit! 1
                        end
      
      escape_ruby_str_for_emacs!(__replacement__)
      
      print '(('
      m.length.times {|i|
        print m.begin(i), ' '
        print m.end(i),   ' '
      }
      print ')'
      print '\"', __replacement__, '\"'
      print ')'
      __count__ += 1
      __pos__   = __match_end__
    end
  }

  __rgn_beg__   = __rgn_beg__ || 0
  __rgn_end__   = __rgn_end__ || __str_body__.length
  __pos_start__ = ((__pos_start__ < __rgn_beg__) ?
                   __rgn_beg__ :
                   ((__pos_start__ > __rgn_end__) ?
                    __rgn_end__ :
                    __pos_start__))
  
  print \"(setq result '(\"
  print \"(\"
  __replace_fn__.call(__pos_start__, __rgn_end__, nil)
  print \")\"
  
  print \"(\"
  __replace_fn__.call(__rgn_beg__,
                      __pos_wrap_end__ ? __pos_wrap_end__ : __rgn_end__,
                      true)
  print \")\"
  print \"))\\n\"
  print \";;; EOF\\n\"
end

def main ()
  fn_body, fn_out, fn_regx, fn_rpla,
  dot_p, case_p, ext_p, eval_p,
  limit, pt_start, rgn_beg, rgn_end  = ARGV
  
  first_match_beg = nil

  str_body = open(fn_body, 'r:UTF-8') {|f| f.read}
  str_regx = open(fn_regx, 'r:UTF-8') {|f| f.read}
  str_rpla = open(fn_rpla, 'r:UTF-8') {|f| f.read}
  
  File.umask(0177)
  $stdout = open(fn_out, 'w:UTF-8')
  
  process_replace(str_body, str_regx, str_rpla, 
                  dot_p.empty?    ? nil : true,
                  case_p.empty?   ? nil : true,
                  ext_p.empty?    ? nil : true,
                  eval_p.empty?   ? nil : true,
                  limit.empty?    ? nil : limit.to_i,
                  pt_start.empty? ? nil : pt_start.to_i,
                  rgn_beg.empty?  ? nil : rgn_beg.to_i,
                  rgn_end.empty?  ? nil : rgn_end.to_i)
  
rescue Exception
  $stderr.print $!.message, \"\\n\"
  exit! 1
end

main()

# EOF

")

(defvar foreign-regexp/shell-script/foreign-regexp-replace-aux.js 
  (concat "#!" (cond
                ((eq window-system 'w32)
                 "node")
                (t
                 (or (foreign-regexp/which "node")
                     "node")))
          "
// -*- coding: utf-8-unix -*-

const ENCODING = 'utf8';
const FS       = require('fs');
const UTIL     = require('util');

// process.version

function write (path, str) {
    if (path == '/dev/stdout') UTIL.print(str)
    else FS.appendFileSync(path, str, ENCODING);
}

function eval_fn_gen (__expression__) {
    var indirect = eval; // For indirect eval call.
    return indirect(\"(\"+__expression__+\")\");
}

function quote_str_for_eval (str) {
    // str = str.replace(/\\\\/mg, '\\\\\\\\');
    str = str.replace(/\"/mg, '\\\\\"');
    return '\"'+str+'\"';
}

function escape_js_str_for_emacs (txt) {
    var retval = String(txt);
    retval = retval.replace(/\\\\/g, \"\\\\\\\\\");
    retval = retval.replace(/\"/g, \"\\\\\\\"\");
    return retval;
}


function number_p (c) {
    var code = c.charCodeAt(0);
    return (0x30 <= code) && (code <= 0x39);
}

function interpolate_fn_gen (str, num_capture) {
    var cur_pos = 0;
    var last_match_end = 0;
    
    var rpla_lst = [];
    var pos;
    
    while (-1 < (pos = str.indexOf('$', last_match_end))) {
        rpla_lst.push(quote_str_for_eval(str.substring(last_match_end, pos)));
        
        var peek = str.substr(pos + 1, 1);
        var expr;

        if (peek == '$') {
            rpla_lst.push(quote_str_for_eval('$'));
            pos += 2;
        } else if (peek == \"&\") {
            expr = 'RegExp.lastMatch';
            rpla_lst.push('('+expr+'?'+expr+':\"\")');
            pos += 2;
        } else if (peek == \"`\") {
            expr = 'RegExp.leftContext';
            rpla_lst.push('('+expr+'?'+expr+':\"\")');
            pos += 2;
        } else if (peek == \"'\") {
            expr = 'RegExp.rightContext';
            rpla_lst.push('('+expr+'?'+expr+':\"\")');
            pos += 2;
        } else if (number_p(peek)
                   && peek != '0'
                   && number_p(str.substr(pos + 2, 1))
                   && (parseInt(str.substr(pos + 1, 2)) <= num_capture)) {
            expr = '$'+str.substr(pos + 1, 2);
            rpla_lst.push('('+expr+'?'+expr+':\"\")');
            pos += 3;
        } else if (number_p(peek)
                   && peek != '0'
                   && (parseInt(peek) <= num_capture)) {
            expr = '$'+peek;
            rpla_lst.push('('+expr+'?'+expr+':\"\")');
            pos += 2;
        } else {
            rpla_lst.push(quote_str_for_eval('$'));
            pos += 1;
        }
        last_match_end = pos;
    }
    rpla_lst.push(quote_str_for_eval(str.substring(last_match_end, str.length)));
    
    var arg_lst = ['_ignore_'];
    for (var i = 1; i <= num_capture; ++i) { arg_lst.push('$'+i) };
    
    return eval('(function ('+arg_lst.join(',')+'){return '+rpla_lst.join('+')+'})');
}

function get_match_pos (regx, match, str_body) {
    var rt = [];
    if (match) {
        rt.push(regx.lastIndex - match[0].length);
        rt.push(regx.lastIndex);
        for (var i = 1, start; i < match.length; i++) {
            start = str_body.indexOf(match[i], rt[0]);
            rt.push(start);
            rt.push(start + match[i].length);
        }
    }
    return rt;
}


function process_replace (str_body, str_regx, str_rpla,
                          dot_p, case_p, ext_p, eval_p,
                          limit, pos_start, rgn_beg, rgn_end,
                          fn_out) {
    var match;
    var pos_wrap_end = null;
    var count        = 0;
    
    var regx = RegExp(str_regx,
                      \"gm\" +
                      (dot_p  ? \"\" : \"\")  + /* Not supported. Dot
                                               never matches a new
                                               line character. */
                      (case_p ? \"\" : \"i\") +
                      (ext_p  ? \"\" : \"\")); // Not supported

    var interpolate_fn = null;

    var replace_fn = function (rgn_beg, rgn_end, wrap_p) {
        var cur_pos = rgn_beg;
        last_0_width_pos = null;
        
        if (interpolate_fn == null) {
            regx.lastIndex = 0;
            if (match = regx.exec(str_body)) {
                num_capture = match.length - 1;
                
                try {
                    interpolate_fn = (eval_p
                                      ? eval_fn_gen(str_rpla)
                                      : interpolate_fn_gen(str_rpla, num_capture));
                } catch (e) {
                    throw {message: 'Syntax error in replacement \"'+str_rpla+'\":\\n'+e.message};
                }
            }
        }

    
        while (((limit == null) ? true : (count < limit))
               && ((regx.lastIndex = cur_pos) ? true : true)
               && (match = regx.exec(str_body))) {

            var lmi = get_match_pos(regx, match, str_body);

            var num_capture = match.length - 1;
            var offset = 0;
            
            var match_beg = lmi[offset + 0];
            var match_end = lmi[offset + 1];
            var _0_width_p = (match_beg == match_end);
            
            if (match_end > rgn_end) { break };
            if (wrap_p && (pos_wrap_end != null) && (pos_wrap_end <= match_beg)) { break };
            if ((! wrap_p) && (pos_wrap_end == null))  { pos_wrap_end = match_beg };
            
            if (_0_width_p && (last_0_width_pos != null) && (match_beg == last_0_width_pos))
            {
                // Do not enter into endless loop.
                cur_pos += 1;
                if (cur_pos > rgn_end)  { break };
                continue;
                
            }  else if (_0_width_p) {
                last_0_width_pos = match_beg;
                
            } else {
                last_0_width_pos = null;
            }

            try {
                replacement = interpolate_fn.apply(this, match.concat([match_beg, str_body]));
            } catch (e) {
                throw {message: 'Error while interpolating replacement \"'+str_rpla+'\":\\n'+e.message};
            }
            replacement = escape_js_str_for_emacs(replacement);
            
            write(fn_out, \"((\");
            
            for (var i = 0; i <= num_capture; ++i) {
                var idx = i*2 + offset;
                var sub_match_beg = lmi[idx];
                var sub_match_end = lmi[idx+1];
                
                if ((-1 < sub_match_beg) && (-1 < sub_match_end)) { // XXX: What this means?
                    write(fn_out, sub_match_beg + \" \" + sub_match_end + \" \")
                }
            }
            write(fn_out, \")\");
            write(fn_out, '\"'+replacement+'\"');
            write(fn_out, \")\");
            
            cur_pos = match_end;
            ++count;
        }
    }
     
    rgn_beg   = (rgn_beg == null) ? 0: rgn_beg;
    rgn_end   = (rgn_end == null) ? str_body.length : rgn_end;
    pos_start = ((pos_start < rgn_beg)
                 ? rgn_beg
                 : ((rgn_end < pos_start)
                    ? rgn_end
                    : pos_start));
    
    write(fn_out, \"(setq result '(\")
    write(fn_out, \"(\");
    replace_fn(pos_start, rgn_end, false);
    write(fn_out, \")\");

    write(fn_out, \"(\");
    replace_fn(rgn_beg, (pos_wrap_end == null) ? rgn_end : pos_wrap_end, true);
    write(fn_out, \")\");
    
    write(fn_out, \"))\\n\")
}

function main (argv) {
    var fn_body   = argv[2];
    var fn_out    = argv[3];
    var fn_regx   = argv[4];
    var fn_rpla   = argv[5];
    var dot_p     = argv[6].length? true : false;
    var case_p    = argv[7].length? true : false;
    var ext_p     = argv[8].length? true : false;
    var eval_p    = argv[9].length? true : false;
    var limit     = argv[10].length ? parseInt(argv[10]) : null;
    var pos_start = argv[11].length ? parseInt(argv[11]) : null;
    var rgn_beg   = argv[12].length ? parseInt(argv[12]) : null;
    var rgn_end   = argv[13].length ? parseInt(argv[13]) : null;
    
    var str_body = FS.readFileSync(fn_body,  ENCODING);
    var str_regx = FS.readFileSync(fn_regx, ENCODING);
    var str_rpla = FS.readFileSync(fn_rpla,  ENCODING);
    
    process_replace(str_body, str_regx, str_rpla, 
                    dot_p, case_p, ext_p, eval_p,
                    limit, pos_start, rgn_beg, rgn_end,
                    fn_out);
}


try {
    main(process.argv);
} catch (e) {
    console.error(e.message); // Do not show stack.
    process.exit(1);
}

process.exit(0);

// EOF

"))

(defvar foreign-regexp/shell-script/foreign-regexp-replace-aux.py "#!/usr/bin/env python

import sys, re, codecs


def interpolate_fn_gn (txt):
    env = {}
    txt = escape_str_for_interpolate_fn_gen(txt)
    
    m_lambda = re.match(r'^lambda', txt)
    m_def = re.search(r'^def\\s+([^\\s]+)\\s*\\(', txt, re.MULTILINE)
    
    if (m_lambda):
        txt = \"interpolate_fn = \" + txt
    elif (m_def):
        txt = (txt + \"\\ninterpolate_fn = \" + m_def.group(1) + \"\\n\")
    else:
        sys.stderr.write('Replacement is not type of lambda or def.')
        exit(1)
    
    try:
        exec(txt, {}, env)
    except Exception as e:
        sys.stderr.write(\"Error in replacement string: \")
        sys.stderr.write(str(e))
        exit(1)
    
    return env['interpolate_fn']


def escape_str_for_interpolate_fn_gen (txt):
  txt = re.sub(r'\"', r'\\\"', txt)
  return txt


def escape_python_str_for_emacs (txt):
    txt = re.sub(r'\\\\', r'\\\\\\\\', txt)
    txt = re.sub(r'\"', r'\\\\\"', txt)
    return txt


pos_wrap_end = None
count_match = 0

def do_replace (f, str_body, regx, str_rpla, interpolate_fn, limit, rgn_beg, rgn_end, wrap_p):
    global pos_wrap_end
    global count_match
    
    for match in regx.finditer(str_body, rgn_beg):
        if (limit != None) and (limit <= count_match):
            break
        
        if match.end() > rgn_end:
            break
        
        if wrap_p and (pos_wrap_end != None) and (pos_wrap_end <= match.start()):
            break
        
        if (not wrap_p) and (pos_wrap_end == None):
            pos_wrap_end = match.start()
        
        f.write(\"((\")
        f.write(\"%d %d \" % match.span())
        for i in range(len(match.groups())):
            if (match.groups())[i] != None: # Skip unmatched group.
                f.write(\"%d %d \" % match.span(i+1))
        f.write(\")\\\"\")
        
        try:
            rpla_expanded = (interpolate_fn and
                             [interpolate_fn(match)] or
                             [match.expand(str_rpla)])[0]
            if not (isinstance(rpla_expanded, unicode) or isinstance(rpla_expanded, str)):
                rpla_expanded = str(rpla_expanded)
            
            f.write(escape_python_str_for_emacs(rpla_expanded))
        except Exception as e:
            sys.stderr.write(\"Error while interpolating replacement: \")
            sys.stderr.write(str(e))
            exit(1)
        
        f.write(\"\\\")\")
        
        count_match += 1


def main ():
    [prog, fn_body, fn_out, fn_regx, fn_rpla,
     dot_p, case_p, ext_p, eval_p,
     limit, pos_start, rgn_beg, rgn_end] = sys.argv
    
    limit     = (len(limit)     and [int(limit)]     or [None])[0]
    pos_start = (len(pos_start) and [int(pos_start)] or [None])[0]
    rgn_beg   = (len(rgn_beg)   and [int(rgn_beg)]   or [None])[0]
    rgn_end   = (len(rgn_end)   and [int(rgn_end)]   or [None])[0]
    
    str_body = codecs.open(fn_body, 'r', 'utf_8').read()
    str_regx = codecs.open(fn_regx, 'r', 'utf_8').read()
    str_rpla = (fn_rpla and [codecs.open(fn_rpla, 'r', 'utf_8').read()] or [\"\"])[0]
    
    f = codecs.open(fn_out, 'w', 'utf8')
    
    
    try:
        regx = re.compile(str_regx, ((dot_p        and re.DOTALL     or 0) |
                                     ((not case_p) and re.IGNORECASE or 0) |
                                     (ext_p        and re.VERBOSE    or 0) |
                                     re.MULTILINE)) # XXX: Put re.UNICODE flag?
    except Exception as e:
        sys.stderr.write(\"Error while compiling regexp: \")
        sys.stderr.write(str(e))
        exit(1)
        
    interpolate_fn = (eval_p and [interpolate_fn_gn(str_rpla)] or [None])[0];
    
    rgn_beg   = rgn_beg or 0
    rgn_end   = rgn_end or len(str_body)
    pos_start = ((pos_start < rgn_beg) and
                 [rgn_beg] or
                 [((pos_start > rgn_end) and
                   [rgn_end] or
                   [pos_start])[0]])[0]
    
    f.write(\"(setq result '((\")
    
    do_replace(f, str_body, regx, str_rpla, interpolate_fn, limit, pos_start, rgn_end, False)
    
    f.write(\")(\")
    
    do_replace(f, str_body, regx, str_rpla, interpolate_fn, limit, rgn_beg,
               ((pos_wrap_end != None) and
                [pos_wrap_end] or
                [rgn_end])[0],
               True)
    
    f.write(\")))\\n;;; EOF\\n\")
    
    exit

main()

#EOF

")



;;; ===========================================================================
;;;
;;;  Define Foreign Regexp Types.
;;;
;;; ===========================================================================

(foreign-regexp/regexp-type/define
 :name 'perl
 :tag "Perl (v5.8 or later)"
 :input-coding-system           'utf-8-unix
 :output-coding-system          'utf-8-unix
 :script-replace                foreign-regexp/shell-script/foreign-regexp-replace-aux.pl
 :script-quote-meta             foreign-regexp/shell-script/foreign-regexp-quote-meta-aux.pl
 :wsp-regexp-for-align           "([ \\t]+)"
 :indicator-case-fold            "i"
 :indicator-no-case-fold         "-"
 :indicator-ext-regexp           "x"
 :indicator-no-ext-regexp        "-"
 :indicator-dot-match            "s"
 :indicator-no-dot-match         "-"
 :indicator-eval-replacement     "e"
 :indicator-no-eval-replacement  "-"
 :indicator-separator            "")

(foreign-regexp/regexp-type/define
 :name 'ruby
 :tag "Ruby (v1.9 or later)"
 :input-coding-system           'utf-8-unix
 :output-coding-system          'utf-8-unix
 :script-replace                foreign-regexp/shell-script/foreign-regexp-replace-aux.rb
 :script-quote-meta             foreign-regexp/shell-script/foreign-regexp-quote-meta-aux.rb
 :wsp-regexp-for-align           "([ \\t]+)"
 :indicator-case-fold            "i"
 :indicator-no-case-fold         "-"
 :indicator-ext-regexp           "x"
 :indicator-no-ext-regexp        "-"
 :indicator-dot-match            "m"
 :indicator-no-dot-match         "-"
 :indicator-eval-replacement     "e"
 :indicator-no-eval-replacement  "-"
 :indicator-separator            "")

(foreign-regexp/regexp-type/define
 :name 'javascript
 :tag "JavaScript (node.js)"
 :input-coding-system           'utf-8-unix
 :output-coding-system          'utf-8-unix
 :script-replace                foreign-regexp/shell-script/foreign-regexp-replace-aux.js
 :script-quote-meta             foreign-regexp/shell-script/foreign-regexp-quote-meta-aux.js
 :wsp-regexp-for-align           "([ \\t]+)"
 :indicator-case-fold            "i"
 :indicator-no-case-fold         "-"
 :indicator-ext-regexp           ""
 :indicator-no-ext-regexp        ""
 :indicator-dot-match            ""
 :indicator-no-dot-match         ""
 :indicator-eval-replacement     "e"
 :indicator-no-eval-replacement  "-"
 :indicator-separator            "")

(foreign-regexp/regexp-type/define
 :name 'python
 :tag "Python"
 :input-coding-system           'utf-8-unix
 :output-coding-system          'utf-8-unix
 :script-replace                foreign-regexp/shell-script/foreign-regexp-replace-aux.py
 :script-quote-meta             foreign-regexp/shell-script/foreign-regexp-quote-meta-aux.py
 :wsp-regexp-for-align           "([ \\t]+)"
 :indicator-case-fold            "I"
 :indicator-no-case-fold         "-"
 :indicator-ext-regexp           "X"
 :indicator-no-ext-regexp        "-"
 :indicator-dot-match            "S"
 :indicator-no-dot-match         "-"
 :indicator-eval-replacement     "e"
 :indicator-no-eval-replacement  "-"
 :indicator-separator            "")

;; Local variables:
;; byte-compile-warnings: (not cl-functions free-vars)
;; End:

;;; foreign-regexp.el ends here
