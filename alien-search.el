;; alien-search.el --- search and replace with alien regular expression.

;; Copyright (C) 2010 K-talo Miyazaki, all rights reserved.

;; Author: K-talo Miyazaki <Keitaro dot Miyazaki at gmail dot com>
;; Created: Sun Nov 28 23:50:45 2010 JST
;; Keywords: convenience emulations matching tools unix wp
;; Revision: $Id$
;; URL: 
;; GitHub: 

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
;; This library is just tested on Emacs 23.2.1 on Ubuntu 10.04
;; and Mac OS X 10.6.3, and won't be run with any version of XEmacs.

;;; Commentary:
;;
;; Overview
;; ========
;;
;;     *** CAUTION: THIS LIBRARY IS VERY EXPERIMENTAL!!! ***
;;
;; This library is an extension of `shell-command'.
;;
;; What this library does are:
;;
;;   1. Search for a pattern from text in current buffer with external
;;      program(*) in manner of their regular expression.
;;      
;;         (*) Ruby (v1.9 or later is required) scripts are
;;             predefined in this file.
;;
;;   2. Browse result of the search operation produced by external program
;;      (...and apply result of the replacement operations if required)
;;      through Emacs user interface like `occur', `isearch' and
;;      `query-replace'.
;;
;;
;; INSTALLING
;; ==========
;; To install this library, save this file to a directory in your
;; `load-path' (you can view the current `load-path' using "C-h v
;; load-path RET" within Emacs), then add the following line to your
;; .emacs startup file:
;;
;;    (require 'alien-search)
;;
;;
;; USING
;; =====
;;
;;  M-x alien-search/occur RET PATTERN RET
;;
;;    `occur' with regular expression in manner of external program.
;;
;;
;;  M-x alien-search/query-replace RET PATTERN RET REPLACEMENT RET
;;
;;    `query-replace' with regular expression in manner of external
;;    program.
;;
;;
;;  M-x alien-search/isearch-forward RET PATTERN
;;  M-x alien-search/isearch-backward RET PATTERN
;;
;;    `isearch' with regular expression in manner of external program.
;;
;;
;;  M-x alien-search/isearch-forward RET RET PATTERN RET
;;  M-x alien-search/isearch-backward RET RET PATTERN RET
;;
;;    Non-incremental search with regular expression in manner of
;;    external program.
;;
;;
;;  Note that notation of PATTERN and REPLACEMENT are different
;;  for each external program.
;;
;;
;;  If you want to escape meta characters in lisp string,
;;  use function `alien-search/quote-meta'.
;;
;;
;; USING WITH DEFAULT EXTERNAL PROGRAMS
;; ====================================
;; Default external programs, that are defined this file, brings
;; manner of Ruby to search/replace operations.
;;
;; These programs accepts regular expression of Ruby as PATTERN,
;; and interpolates special variables $&, $`, $', $+ and $1..$9
;; in REPLACEMENT as if it has been inside of double quotes
;; in a block of String#gsub method.
;;
;;   Example:
;;
;;     The command
;;
;;       M-x alien-search/query-replace RET (\d+)-(\d+) RET #$1...#$2 RET
;;
;;     replaces text in buffer
;;
;;       123-456
;;
;;     with text
;;
;;       123...456
;;   
;; 
;; Key map Examples
;; ================
;; (when (require 'alien-search nil t)
;;   (define-key esc-map [(f4)]  'alien-search/occur)
;;   ;; Override `query-replace'.
;;   (define-key esc-map [?\C-%] 'alien-search/query-replace)
;;   ;; Override `isearch-forward-regexp'.
;;   (define-key esc-map [?\C-s] 'alien-search/isearch-forward)
;;   ;; Override `isearch-backward-regexp'.
;;   (define-key esc-map [?\C-r] 'alien-search/isearch-backward))
;;
;;
;; FOR HACKERS
;; ===========
;; This library requires four external programs to execute
;; `alien-search/occur', `alien-search/query-replace',
;; `alien-search/isearch-*' and `alien-search/quote-meta'
;; with regular expressions that are alien to emacs.
;;
;; If you want to write these external programs by your choice
;; of language, see help documents of these variables:
;; 
;;   `alien-search/replace/external-command'
;;   `alien-search/occur/external-command'
;;   `alien-search/isearch/external-command'
;;   `alien-search/quote-meta/external-command'
;;
;; and set path to the programs to the variables when you wrote
;; them.
;;
;; See also default external programs written in Ruby, defined as:
;;
;;   `alien-search/replace/shell-script'
;;   `alien-search/occur/shell-script'
;;   `alien-search/isearch/shell-script'
;;   `alien-search/quote-meta/shell-script'
;;
;;
;; WISH LIST
;; =========
;; - Better handling of exceptions from external program,
;;   especially syntax error regarding to regular expression
;;   in isearch session.
;; - Better response?
;; - Write tests.
;; - validate-regexp by external program?

;;; Change Log:

;;; Code:

(provide 'alien-search)
(defconst alien-search/version "0.0")


(eval-when-compile (require 'cl))


;;; ===========================================================================
;;;
;;;  For Debugging.
;;;
;;; ===========================================================================

(eval-when-compile
  (defvar alien-search/search/debug-cache nil)
  (defvar alien-search/transition/debug-advices nil)
  )

;; ----------------------------------------------------------------------------
;;
;;  Macros
;;
;; ----------------------------------------------------------------------------

(defmacro alien-search/debug (class &rest args)
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


;;; ===========================================================================
;;;
;;;  Variables and functions common to each `alien-search' operation.
;;;
;;; ===========================================================================

(defcustom alien-search/tmp-dir "/tmp/"
  "Directory in which temporally files should be written."
  :type 'string
  :group 'alien-search)

(defcustom alien-search/tmp-file-prefix "alien-search-"
  "Prefix name of temporally files."
  :type 'string
  :group 'alien-search)

(defvar alien-search/input-coding-system  'utf-8-unix
  "Coding system to be used for decoding files which
contains texts passed from external commands to Emacs.")

(defvar alien-search/output-coding-system 'utf-8-unix
  "Coding system to be used for encoding files which
contains texts passed from Emacs to external commands.")

(defvar alien-search/history  nil
  "History list for some commands that runs alien-search.")

;; Search options.
;;
(defvar alien-search/dot-match-a-newline-p nil)
(make-variable-buffer-local 'alien-search/dot-match-a-newline-p)

(defvar alien-search/use-extended-regexp-p nil)
(make-variable-buffer-local 'alien-search/use-extended-regexp-p)


;; ----------------------------------------------------------------------------
;;
;;  Hooks
;;
;; ----------------------------------------------------------------------------
(defvar alien-search/case-fold-will-change-hook nil
  "Normal hook run before toggle `case-fold-search'.")

(defvar alien-search/ext-regexp-will-change-hook nil
  "Normal hook run before toggle `alien-search/use-extended-regexp-p'.")

(defvar alien-search/dot-match-will-change-hook nil
  "Normal hook run before toggle `alien-search/dot-match-a-newline-p'.")

(defvar alien-search/case-fold-changed-hook nil
  "Normal hook run after toggle `case-fold-search'.")

(defvar alien-search/ext-regexp-changed-hook nil
  "Normal hook run after toggle `alien-search/use-extended-regexp-p'.")

(defvar alien-search/dot-match-changed-hook nil
  "Normal hook run after toggle `alien-search/dot-match-a-newline-p'.")


;; ----------------------------------------------------------------------------
;;
;;  Macros
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/alambda parms &body body) => RESULT OF BODYFORM
;; ----------------------------------------------------------------------------
(defmacro* alien-search/alambda (parms &body body)
  "An utility macro.
This macro allows anonymous functions to refer themselves
through a symbol `self'."
  (declare (indent 1))
  `(labels ((self ,parms ,@body))
     #'self))

;; ----------------------------------------------------------------------------
;;  (alien-search/catch-case var bodyform &rest handlers) => RESULT OF BODYFORM
;;                                                           OR HANDLERS
;; ----------------------------------------------------------------------------
(defmacro alien-search/catch-case (var bodyform &rest handlers)
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
`alien-search/catch-case' and it executes the handler's
BODY... with VAR bound to (TAG-NAME . THROWNED-VALUE).
Then the value of the last BODY form is returned from the
`alien-search/catch-case' expression.

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
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/toggle-case-fold &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/toggle-case-fold (&optional no-message)
  "Toggle `case-fold-search'."
  (interactive)
  
  (run-hooks 'alien-search/case-fold-will-change-hook)
  (cond
   (isearch-mode
    ;; FIXME: Turn off an annoying message.
    (isearch-toggle-case-fold)
    (setq case-fold-search isearch-case-fold-search))
   (t
    (setq case-fold-search
          (not case-fold-search))))
  (run-hooks 'alien-search/case-fold-changed-hook)
  
  (when (not no-message)
    (minibuffer-message
     (format "[alien-search] case %ssensitive"
             (if case-fold-search "in" "")))))

;; ----------------------------------------------------------------------------
;;  (alien-search/toggle-dot-match &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/toggle-dot-match (&optional no-message)
  "Toggle `alien-search/dot-match-a-newline-p'."
  (interactive)
  (run-hooks 'alien-search/dot-match-will-change-hook)
  (setq alien-search/dot-match-a-newline-p
        (not alien-search/dot-match-a-newline-p))
  (run-hooks 'alien-search/dot-match-changed-hook)
  
  (when (not no-message)
    (minibuffer-message
     (format "[alien-search] . %s newline"
             (if alien-search/dot-match-a-newline-p "matches" "does not match")))))

;; ----------------------------------------------------------------------------
;;  (alien-search/toggle-ext-regexp &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/toggle-ext-regexp (&optional no-message)
  "Toggle `alien-search/use-extended-regexp-p'."
  (interactive)
  (run-hooks 'alien-search/ext-regexp-will-change-hook)
  (setq alien-search/use-extended-regexp-p
        (not alien-search/use-extended-regexp-p))
  (run-hooks 'alien-search/ext-regexp-changed-hook)
  
  (when (not no-message)
    (minibuffer-message
     (format "[alien-search] %sextended regex"
             (if alien-search/use-extended-regexp-p "" "no ")))))


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/run-external-command cmd-path shell-script body
;;                                     regexp replacement &rest other-args)
;;                                                                   => RESULT
;; ----------------------------------------------------------------------------
(defun alien-search/run-external-command (cmd-path shell-script
                                                   body
                                                   regexp
                                                   replacement
                                                   &rest other-args)
  "Run external command to execute operations regarding to search.

NOTES FOR DEVELOPERS: Variables in REPLACEMENT should be interpolated
                      on each match by external command."
  (let* ((base               (expand-file-name
                              alien-search/tmp-file-prefix
                              alien-search/tmp-dir))
         (fn-out-body        (make-temp-name base))
         (fn-out-regexp      (make-temp-name base))
         (fn-out-replacement (make-temp-name base))
         (fn-in-result       (make-temp-name base))
         (fn-cmd         (make-temp-name base))
         (cmd-basename       (if cmd-path
                                 (file-name-nondirectory cmd-path)
                               "shell-script"))
         (proc-output-buf    (get-buffer-create " *alien-search*"))
         (cur-buf            (current-buffer))
         (orig-file-modes    (default-file-modes))
         (coding-sys-out     alien-search/output-coding-system)
         (coding-sys-in      alien-search/input-coding-system)
         result)
    (unwind-protect
        (progn

          ;; Save information, which have to be passed to 
          ;; external command, to temporally files.
          (unwind-protect 
              (progn
                (set-default-file-modes #o0600)
                
                (when body
                  (with-temp-file fn-out-body
                    (set-buffer-file-coding-system coding-sys-out)
                    (insert body)))
                (with-temp-file fn-out-regexp
                  (set-buffer-file-coding-system coding-sys-out)
                  (insert regexp))
                (when replacement
                  (with-temp-file fn-out-replacement
                    (set-buffer-file-coding-system coding-sys-out)
                    (insert replacement))))
            (set-default-file-modes orig-file-modes))

          ;; Save shell-script to file when required.
          (when (not cmd-path)
            (with-temp-file fn-cmd
              (set-buffer-file-coding-system coding-sys-out)
              (insert shell-script))
            (set-file-modes fn-cmd #o0700)
            (setq cmd-path fn-cmd))
          
          ;;(message "[alien-search] Running...")
          
          ;; Do search by external command.
          (let ((status (apply #'call-process
                               `(,cmd-path
                                 nil ,(buffer-name proc-output-buf) nil
                                 ,@(if body (list fn-out-body) nil)
                                 ,fn-in-result
                                 ,fn-out-regexp
                                 ,@(if replacement (list fn-out-replacement) nil)
                                 ,@other-args))))
            (when (not (and (numberp status)
                            (zerop status)))
              (error "[alien-search] ERROR(%s): %s"
                     status
                     (with-current-buffer proc-output-buf
                       (buffer-substring (point-min) (point-max))))))
          
          ;;(message "[alien-search] Running...done")
          
          (with-current-buffer proc-output-buf
            (when (/= (point-min) (point-max))
              (message "[alien-search] messages from %s:\n%s"
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
;;  (alien-search/read-minibuf-contents) => (STRING . POSITION)
;; ----------------------------------------------------------------------------
(defun alien-search/read-minibuf-contents ()
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
;;  (alien-search/ad-enable function class name) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/ad-enable (&rest args)
  "`ad-enable-advice' runs `string-match' and breaks
current match-data as a side effect.
This is a side effect free version of `ad-enable-advice'."
  (let ((orig-match-data (match-data)))
    (apply 'ad-enable-advice args)
    (set-match-data orig-match-data)))

;; ----------------------------------------------------------------------------
;;  (alien-search/ad-disable function class name) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/ad-disable (&rest args)
  "`ad-enable-advice' runs `string-match' and breaks
current match-data as a side effect.
This is a side effect free version of `ad-disable-advice'."
  (let ((orig-match-data (match-data)))
    (apply 'ad-disable-advice args)
    (set-match-data orig-match-data)))

;; ----------------------------------------------------------------------------
;;  (alien-search/ad-activate function &optional compile) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/ad-activate (&rest args)
  "`ad-activate' runs `string-match' and breaks
current match-data as a side effect.
This is a side effect free version of `ad-activate'."
  (let ((orig-match-data (match-data)))
    (apply 'ad-activate args)
    (set-match-data orig-match-data)))


;;; ===========================================================================
;;;
;;;  A "Search Option" indicator.
;;;
;;; ===========================================================================

(defvar alien-search/search-option-indicator/case-fold-str ""
  "A string displayed when the search option
`case-fold-search' is on.")

(defvar alien-search/search-option-indicator/no-case-fold-str "Case"
  "A string displayed when the search option
`case-fold-search' is off.")

(defvar alien-search/search-option-indicator/dot-match-str ".=~\\n"
  "A string displayed when the search option
`alien-search/dot-match-a-newline-p' is on.")

(defvar alien-search/search-option-indicator/no-dot-match-str ""
  "A string displayed when the search option
`alien-search/dot-match-a-newline-p' is off.")

(defvar alien-search/search-option-indicator/ext-regexp-str "Ext"
  "A string displayed when the search option
`alien-search/use-extended-regexp-p' is on.")

(defvar alien-search/search-option-indicator/no-ext-regexp-str ""
  "A string displayed when the search option
`alien-search/use-extended-regexp-p' is off.")

(defvar alien-search/search-option-indicator/separator-str " "
  "A string displayed between search option strings.")


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/search-option-indicator/make-indicator) => STRING
;; ----------------------------------------------------------------------------
(defun alien-search/search-option-indicator/make-indicator ()
  "Make a string which shows current search option status."
  (let ((indicator (alien-search/search-option-indicator/make-indicator-aux)))
    (if (not (zerop (length indicator)))
        (concat "[" indicator "]")
      "")))

;; ----------------------------------------------------------------------------
;;  (alien-search/search-option-indicator/make-indicator-aux) => STRING
;; ----------------------------------------------------------------------------
(defun alien-search/search-option-indicator/make-indicator-aux ()
  "Auxiliary function of `alien-search/search-option-indicator/make-indicator'."
  (reduce '(lambda (str elm)
             (concat str
                     (cond
                      ((zerop (length str)) "")
                      ((zerop (length elm)) "")
                      (t alien-search/search-option-indicator/separator-str))
                     elm))
          (list ""
                (cond
                 ((and isearch-mode
                       isearch-case-fold-search)
                  alien-search/search-option-indicator/case-fold-str)
                 (isearch-mode
                  alien-search/search-option-indicator/no-case-fold-str)
                 (case-fold-search
                  alien-search/search-option-indicator/case-fold-str)
                 (t 
                  alien-search/search-option-indicator/no-case-fold-str))
                (if alien-search/dot-match-a-newline-p
                    alien-search/search-option-indicator/dot-match-str
                  alien-search/search-option-indicator/no-dot-match-str)
                (if alien-search/use-extended-regexp-p
                    alien-search/search-option-indicator/ext-regexp-str 
                  alien-search/search-option-indicator/no-ext-regexp-str))))


;;; ===========================================================================
;;;
;;;  Patches for `read-from-minibuffer'.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;
;;  Macros
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/read-from-minibuf/with-search-option-indicator &rest body)
;;                                                       => RESULT OF BODYFORM
;; ----------------------------------------------------------------------------
(defmacro alien-search/read-from-minibuf/with-search-option-indicator (&rest body)
  "Run body with search option indicator on prompt of `read-from-minibuffer'. "
  `(unwind-protect
       (progn
         (alien-search/ad-enable 'read-from-minibuffer 'around 'alien-search/read-from-minibuf/with-search-option-indicator)
         (alien-search/ad-activate 'read-from-minibuffer)
         ,@body)
     (alien-search/ad-disable 'read-from-minibuffer 'around 'alien-search/read-from-minibuf/with-search-option-indicator)
     (alien-search/ad-activate 'read-from-minibuffer)))

;; ----------------------------------------------------------------------------
;;  (alien-search/read-from-minibuf/with-initial-contents
;;                          initial-contents &rest body) => RESULT OF BODYFORM
;; ----------------------------------------------------------------------------
(defmacro alien-search/read-from-minibuf/with-initial-contents (initial-contents &rest body)
  "Run body with setting initial contents to `read-from-minibuffer'."
  (declare (indent 1))
  ;; Set initial contents for `read-from-minibuffer'.
  `(unwind-protect
       (progn
         (alien-search/ad-enable 'read-from-minibuffer 'before 'alien-search/read-from-minibuf/with-initial-contents)
         (alien-search/ad-activate 'read-from-minibuffer)
         (let ((alien-search/.initial-contents ,initial-contents))
           ,@body))
     (alien-search/ad-disable 'read-from-minibuffer 'before 'alien-search/read-from-minibuf/with-initial-contents)
     (alien-search/ad-activate 'read-from-minibuffer)))



;; ----------------------------------------------------------------------------
;;
;;  Advices
;;
;; ----------------------------------------------------------------------------

;; XXX: Should be defined by flet
;;      in `alien-search/read-from-minibuf/with-search-option-indicator'?
(defun alien-search/.signal-option-changed ()
  (let ((contents (alien-search/read-minibuf-contents)))
    (when contents
      (throw 'alien-search/.option-changed
             contents))))

(defadvice read-from-minibuffer (around alien-search/read-from-minibuf/with-search-option-indicator
                                        (prompt &optional initial-contents keymap read
                                                hist defalut-value inherit-input-method))
  "Run `read-from-minibuffer' with displaying search option indicator
on prompt string.

Search option indicator will be updated whenever options are changed via
commands `alien-search/toggle-case-fold',
`alien-search/toggle-dot-match' and
`alien-search/toggle-ext-regexp'.

This advice will be enabled by `alien-search/query-replace'
and `alien-search/occur'."
  ;; Preserve current alien-search/*-will-change-hooks.
  (let ((alien-search/case-fold-will-change-hook  alien-search/case-fold-will-change-hook)
        (alien-search/dot-match-will-change-hook  alien-search/dot-match-will-change-hook)
        (alien-search/ext-regexp-will-change-hook alien-search/ext-regexp-will-change-hook)
        (orig-prompt      (copy-sequence prompt))
        (initial-contents initial-contents))
    (unwind-protect
        (progn
          ;; Do not call this `read-from-minibuffer' recursively.
          (alien-search/ad-disable 'read-from-minibuffer 'around 'alien-search/read-from-minibuf/with-search-option-indicator)
          (alien-search/ad-activate 'read-from-minibuffer)
    
          ;; Do not toggle search options of *Minibuf-N* while reading
          ;; regexps, toggle re-options of CURRENT BUFFER instead.
          (lexical-let ((cur-buf (current-buffer)))
            (mapcar
             (lambda (lst)
               (lexical-let ((hook (nth 0 lst)) (toggle-fn (nth 1 lst)))
                 (add-hook hook
                           (alien-search/alambda ()
                             (with-current-buffer cur-buf
                               (progv (list hook) (list (symbol-value hook))
                                 ;; Do not call this hook recursively.
                                 (remove-hook hook #'self)
                                 ;; Run `alien-search/toggle-*' in current buffer.
                                 (funcall toggle-fn t)))
                             ;; Exit from function `alien-search/toggle-*'
                             ;; in *Minibuf-N*.
                             (alien-search/.signal-option-changed)))))
             '((alien-search/case-fold-will-change-hook  alien-search/toggle-case-fold)
               (alien-search/dot-match-will-change-hook  alien-search/toggle-dot-match)
               (alien-search/ext-regexp-will-change-hook alien-search/toggle-ext-regexp))))
    
          ;; Whenever search option is changed, restart `read-from-minibuffer' to
          ;; redisplay prompt.
          (while (setq initial-contents
                       (alien-search/catch-case data
                           (progn
                             ;; Put indicator on prompt.
                             (setq prompt
                                   (concat (substring orig-prompt
                                                      0
                                                      (string-match "\\( with\\)?: $" orig-prompt))
                                           (alien-search/search-option-indicator/make-indicator)
                                           (match-string 1 orig-prompt)
                                           ": "))
                             ;; Call read-from-minibuffer.
                             ad-do-it
                             ;; Break when nothing has been thrown.
                             nil)
                         (alien-search/.option-changed
                          ;; initial-contents is thrown with
                          ;; a tag `alien-search/.option-changed'.
                          (cdr data))))))
      (alien-search/ad-enable 'read-from-minibuffer 'around 'alien-search/read-from-minibuf/with-search-option-indicator)
      (alien-search/ad-activate 'read-from-minibuffer))))
(alien-search/ad-disable 'read-from-minibuffer
                         'around
                         'alien-search/read-from-minibuf/with-search-option-indicator)
(alien-search/ad-activate 'read-from-minibuffer)

(defadvice read-from-minibuffer (before alien-search/read-from-minibuf/with-initial-contents
                                        (prompt &optional initial-contents keymap read
                                                hist default-value inherit-input-method))
  "Set value of a variable `alien-search/.initial-contents'
to INITIAL-CONTENTS of `read-from-minibuffer'.

Value should be assigned to `alien-search/.initial-contents'
by caller function with `LET' form.

This advice will be enabled by `alien-search/re-builder/query-replace-on-target-buffer'
and `alien-search/re-builder/occur-on-target-buffer'."
  (alien-search/ad-disable 'read-from-minibuffer 'before 'alien-search/read-from-minibuf/with-initial-contents)
  (alien-search/ad-activate 'read-from-minibuffer)
  
  (when (and (boundp 'alien-search/.initial-contents)
             (stringp alien-search/.initial-contents))
    (setq initial-contents
          (cons alien-search/.initial-contents
                (1+ (length alien-search/.initial-contents)))))
  
  (setq alien-search/.initial-contents nil))
(alien-search/ad-disable 'read-from-minibuffer
                         'before
                         'alien-search/read-from-minibuf/with-initial-contents)
(alien-search/ad-activate 'read-from-minibuffer)


;;; ===========================================================================
;;;
;;;  Menu Definition.
;;;
;;; ===========================================================================

(defvar alien-search/menu/use-menu-p t
  "A flag if menu items for alien search will be installed or not.")


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/menu/install) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/menu/install ()
  "Install menu items for alien search."
  (easy-menu-add-item menu-bar-search-menu
                      nil
                      ["Alien Regexp Forward..." alien-search/non-incremental/search-forward
                       :active (alien-search/non-incremental/available-p)
                       :help "Search forward for an alien regular expression"]
                      "separator-repeat-search")
  (easy-menu-add-item menu-bar-search-menu
                      nil
                      ["Alien Regexp Backward..." alien-search/non-incremental/search-backward
                       :active (alien-search/non-incremental/available-p)
                       :help "Search backward for an alien regular expression"]
                      "separator-repeat-search")
                    
  (easy-menu-add-item menu-bar-i-search-menu
                      nil
                      ["Forward Alien Regexp..." alien-search/isearch-forward
                       :active (alien-search/isearch/available-p)
                       :help "Search forward for an alien regular expression as you type it"]
                      nil)

  (easy-menu-add-item menu-bar-i-search-menu
                      nil
                      ["Backward Alien Regexp..." alien-search/isearch-backward
                       :active (alien-search/isearch/available-p)
                       :help "Search backwards for an alien regular expression as you type it"]
                      "separator-repeat-search")

  (easy-menu-add-item menu-bar-replace-menu
                      nil
                      ["Replace Alien Regexp..." alien-search/query-replace
                       :active (alien-search/replace/available-p)
                       :help "Replace alien regular expression interactively, ask about each occurrence"]
                      "separator-replace-tags")

  (easy-menu-add-item menu-bar-edit-menu
                      nil
                      '("Alien Search Options"
                        ("Alien Type"
                         :filter (lambda (&rest args)
                                   (alien-search/menu/alien-type-menu-gen)))
                        ("--")
                        ["Case Insensitive" alien-search/toggle-case-fold
                         :style radio :selected (if isearch-mode
                                                    isearch-case-fold-search
                                                  case-fold-search)]
                        [". Matches a Newline" alien-search/toggle-dot-match
                         :style radio :selected alien-search/dot-match-a-newline-p]
                        ["Extended Regular Expression" alien-search/toggle-ext-regexp
                         :style radio :selected alien-search/use-extended-regexp-p])
                      "goto")

  ;; XXX: Should be removed?
  (easy-menu-add-item menu-bar-edit-menu
                      nil
                      '("---")
                      "goto")


  ;; Override defalut menu items.
  ;;
  (define-key menu-bar-search-menu [repeat-search-fwd]
    `(menu-item ,(purecopy "Repeat Forward") nonincremental-repeat-search-forward
                :enable (or (and (eq menu-bar-last-search-type 'string)
                                        search-ring)
                                   (and (eq menu-bar-last-search-type 'regexp)
                                        regexp-search-ring)
                                   ;; For alien regexp search.
                                   (and (eq menu-bar-last-search-type 'alien)
                                        alien-search/history
                                        (alien-search/non-incremental/available-p)))
                :help ,(purecopy "Repeat last search forward")))
  (define-key menu-bar-search-menu [repeat-search-back]
    `(menu-item ,(purecopy "Repeat Backwards") nonincremental-repeat-search-backward
                :enable (or (and (eq menu-bar-last-search-type 'string)
                                        search-ring)
                                   (and (eq menu-bar-last-search-type 'regexp)
                                        regexp-search-ring)
                                   ;; For alien regexp search.
                                   (and (eq menu-bar-last-search-type 'alien)
                                        alien-search/history
                                        (alien-search/non-incremental/available-p)))
                :help ,(purecopy "Repeat last search backwards"))))

;; ----------------------------------------------------------------------------
;;  (alien-search/menu/alien-type-menu-gen &rest args) => MENU ITEMS
;; ----------------------------------------------------------------------------
(defun alien-search/menu/alien-type-menu-gen (&rest args)
  ""
  (mapcar (lambda (pair)
            (let ((kv-lst (cdr pair)))
              (vector
               ;; NAME
               (concat (cadr (memq :tag kv-lst)) "")
               ;; CALLBACK
               `(lambda (&rest args)
                  (interactive)
                  (alien-search/alien-type/set (quote ,(cadr (memq :name kv-lst))))) 
               :style 'radio
               :selected (eq (and (boundp 'alien-search/alien-type) ;; For compiler
                                  alien-search/alien-type)
                             (cadr (memq :name kv-lst)))
               :active   t
               :help "dummy")))
          `((nil :name nil :tag "None")
            ,@(let ((alst (copy-list
                           (and (boundp 'alien-search/alien-type/.type-alst) ;; For compiler
                                alien-search/alien-type/.type-alst))))
                (sort alst
                      (lambda (pair1 pair2)
                        (string< (format "%s" (car pair1))
                                 (format "%s" (car pair2)))))))))


;; ----------------------------------------------------------------------------
;;
;;  Main
;;
;; ----------------------------------------------------------------------------

(when alien-search/menu/use-menu-p
  (alien-search/menu/install))


;;; ===========================================================================
;;;
;;;  `query-replace' in alien regexp with a help from external command.
;;;
;;; ===========================================================================

(defvar alien-search/replace/external-command nil
  "Path of an external command to use to execute actual search operation.

Seven arguments describe below will be passed to the command.

 1st: Path of a file which contains the text to be searched.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'.

 2nd: Path of a file to which the command should write the result
      of current search operation.

      The external command have to output a form like:

        (setq result
              '((1st-MATCH-START 1st-MATCH-END \"REPLACEMENT-FOR-1st-MATCH\")
                (2nd-MATCH-START 2nd-MATCH-END \"REPLACEMENT-FOR-2nd-MATCH\")
                ...))

      to this file.

      Note that each start and end position in the form should be
      an offset from beginning of the text which has been searched.
      (This means each number should be started from 0, not from 1)

      The text in this file must be encoded in the value of
      `alien-search/input-coding-system'.

 3rd: Path of a file in which the regexp we want to search is written.
      The command have a responsibility to search this regexp
      from the file specified by 1st argument, then write start and
      end positions of each match to the file specified by 2nd argument.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'.

 4th: Path of a file in which the replacement expression is written.
      The command have a responsibility to interpolate variables
      in the expression on each match, then write them to the file
      specified by 2nd argument.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'.

 5th: A dot matches newline flag.
      When the value of this flag is not empty string,
      . should be matched to a newline character.

 6th: A case sensitive flag.
      When the value of this flag is not empty string,
      the match operation should be done case-sensitive.

 7th: An extended regular expression flag.
      When the value of this flag is not empty string,
      the current search regexp (see 3rd arg) should be
      interpreted as extended regular expression.")

(defvar alien-search/replace/shell-script nil
  "A shell script which will be run as 
`alien-search/replace/external-command'
when it has nil value.")


(defvar alien-search/replace/defaults nil
  "Default values of FROM-STRING and TO-STRING for `alien-search/query-replace'.

See also `query-replace-defaults'.")


;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/query-replace regexp replacement
;;                              &optional delimited start end) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/query-replace (regexp replacement &optional delimited start end)
  "Do `query-replace' with a help from external command.

See `isearch-forward-regexp' and `isearch-backward-regexp' for
more information."
  (interactive
   (and
    (alien-search/replace/assert-available)
    (let ((common
           (let ((query-replace-from-history-variable 'alien-search/history)
                 (query-replace-to-history-variable   'alien-search/history)
                 (query-replace-defaults              alien-search/replace/defaults))
             (alien-search/read-from-minibuf/with-search-option-indicator
              (prog1 (query-replace-read-args
                      (concat "Query replace alien regexp"
                              (if (and transient-mark-mode mark-active) " in region" ""))
                      t)
                (setq alien-search/replace/defaults query-replace-defaults))))))
      (list (nth 0 common) (nth 1 common) (nth 2 common)
            ;; These are done separately here
            ;; so that command-history will record these expressions
            ;; rather than the values they had this time.
            (if (and transient-mark-mode mark-active)
                (region-beginning))
            (if (and transient-mark-mode mark-active)
                (region-end))))))
  (alien-search/replace/assert-available)
  (alien-search/replace/perform-replace
   regexp replacement t nil nil nil nil start end))


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/replace/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defun alien-search/replace/available-p ()
  "Test if external command or shell script is defined or not."
  (or alien-search/replace/external-command
      alien-search/replace/shell-script))

;; ----------------------------------------------------------------------------
;;  (alien-search/replace/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defun alien-search/replace/assert-available ()
  "Raise error when no external command or shell script is defined."
  (or (alien-search/replace/available-p)
      (error "[alien-search] No external command or shell script is defined for replace.")))

;; ----------------------------------------------------------------------------
;;  (alien-search/replace/search-by-external-command regexp replacement)
;;                                                                     => LIST
;; ----------------------------------------------------------------------------
(defun alien-search/replace/search-by-external-command (regexp replacement min max)
  "Scan current buffer with external command to detect matching
texts by REGEXP.

Overlays will be made on each matched text, and they will be
saved to the variable `alien-search/replace/ovs-on-match/data'.

Variables in REPLACEMENT will be interpolated
on each match, and will be saved to the property
alien-search/replace/replacement of each overlay.

Returns position of the neighborhood overlay of a pointer in
the list `alien-search/replace/ovs-on-match/data'."
  (let* ((offset (point-min))
         (result (alien-search/run-external-command
                  alien-search/replace/external-command
                  alien-search/replace/shell-script
                  (buffer-substring (point-min) (point-max))
                  regexp
                  replacement
                  (if alien-search/dot-match-a-newline-p "DOT" "")
                  (if case-fold-search "" "CASE")
                  (if alien-search/use-extended-regexp-p "EXT" ""))))
    (alien-search/replace/parse-search-result result offset min max)
    
    ;; Detect index of neighborhood overlay of a pointer.
    (position (car (member-if
                    #'(lambda (ov)
                        (<= (point) (overlay-start ov)))
                    (alien-search/replace/ovs-on-match/get-all)))
              (alien-search/replace/ovs-on-match/get-all))))

;; ----------------------------------------------------------------------------
;;  (alien-search/replace/parse-search-result result offset) => OVERLAYS
;; ----------------------------------------------------------------------------
(defun alien-search/replace/parse-search-result (result offset min max)
  "Subroutine of `alien-search/replace/search-by-external-command'."
  (alien-search/replace/ovs-on-match/dispose)
  ;; RESULT has structure like:
  ;;   ((MATCH_START MATCH_END "REPLACEMENT")
  ;;    ...)
  ;;
  ;; NOTE: Special variables in "REPLACEMENT"
  ;;       should be expanded by external command.
  (save-excursion
    (let ((data    nil)
          (cur-buf (current-buffer)))
      ;;(message "[alien-search] Parsing search results from external command...")
      (dolist (lst result)
        (let* ((beg         (+ (nth 0 lst) offset))
               (end         (+ (nth 1 lst) offset))
               (replacement (nth 2 lst)))
          (when (and (not (and min (< beg min)))
                     (not (and max (< max end))))
            (alien-search/replace/ovs-on-match/add beg end cur-buf replacement))))
      ;;(message "[alien-search] Parsing search results from external command...done")
      )))

;; ----------------------------------------------------------------------------
;;  (alien-search/replace/perform-replace (from-string replacement
;;                                         query-flag ignore ignore
;;                                         &optional ignore map start end)
;;                                                                     => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/replace/perform-replace (from-string replacement
                                                         query-flag
                                                         ignore/regexp-flag
                                                         ignore/delimited-flag
                                                         &optional
                                                         ignore/repeat-count
                                                         map start end)
  "Replacement of `perform-replace' for alien search.

Note that \"\\?\" in string is not supported like
original `perform-replace' does.

Also list in REPLACEMENT and REPEAT-COUNT are not supported."
  ;; Based on `perform-replace'.

  ;; XXX: The overlays `ovs-on-match', that looks like lazy-highlight,
  ;;      should be updated by `alien-search/replace/search-by-external-command'
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
         (recenter-last-op nil)	; Start cycling order with initial position.
         
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
      (error "[alien-search] REPLACEMENT must be a string.")))

    ;; If region is active, in Transient Mark mode, operate on region.
    (when start
      (setq max (copy-marker (max start end)))
      (goto-char (setq min (min start end)))
      (deactivate-mark))

    ;; Do search by external command and detect index of
    ;; neighborhood overlay of a pointer.
    (setq idx (alien-search/replace/search-by-external-command
               from-string
               real-replacement
               min max))

    (push-mark)
    (undo-boundary)
    (unwind-protect
        (flet ((update-real-match-data (idx)
                                       (setq real-match-data
                                             (let ((ov (alien-search/replace/ovs-on-match/get-nth idx)))
                                               (when ov
                                                 (list (overlay-start ov)
                                                       (overlay-end   ov)))))
                                       (set-match-data real-match-data)
                                       real-match-data)
               (update-next-replacement (idx)
                                        (setq next-replacement
                                              (overlay-get (alien-search/replace/ovs-on-match/get-nth idx)
                                                           'alien-search/replace/replacement))))
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
                    (alien-search/replace/highlight/dispose)
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
                    (alien-search/replace/highlight/put
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
                           (setq idx (alien-search/replace/search-by-external-command
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
                           (setq idx (alien-search/replace/search-by-external-command
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
                             (alien-search/replace/ovs-on-match/dispose)
                             (alien-search/replace/highlight/dispose)
                             
                             (save-excursion (recursive-edit))

                             (goto-char opos)
                             (set-marker opos nil))
                           (setq idx (alien-search/replace/search-by-external-command
                                      from-string
                                      real-replacement
                                      min max))
                           (if (numberp idx)
                               (setq idx (1- idx)) ;;Do not forward current match.
                             (setq idx (alien-search/replace/ovs-on-match/get-count))) ;;Done.
                           
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
                              ;;		     (search-forward (match-string 0))
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
      (alien-search/replace/ovs-on-match/dispose)
      (alien-search/replace/highlight/dispose))
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
;;  (alien-search/replace/highlight/put beg end) => VOID
;; ----------------------------------------------------------------------------
(defvar alien-search/replace/highlight-overlay nil)
(make-variable-buffer-local 'alien-search/replace/highlight-overlay)
(defun alien-search/replace/highlight/put (beg end)
  "Subroutine of `alien-search/replace/perform-replace'.

Put overlay on current match."
  (if alien-search/replace/highlight-overlay
      (move-overlay alien-search/replace/highlight-overlay beg end)
    (let ((ov (make-overlay beg end (current-buffer) t)))
      (overlay-put ov 'priority 1001) ;higher than lazy overlays
      (when query-replace-highlight
        (overlay-put ov 'face 'query-replace))
      (setq alien-search/replace/highlight-overlay ov))))

;; ----------------------------------------------------------------------------
;;  (alien-search/replace/highlight/dispose) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/replace/highlight/dispose ()
  "Subroutine of `alien-search/replace/perform-replace'."
  (when alien-search/replace/highlight-overlay
    (delete-overlay alien-search/replace/highlight-overlay)
    (setq alien-search/replace/highlight-overlay nil)))


;;; ===========================================================================
;;;
;;;  Overlays on all of matches.
;;;
;;; ===========================================================================

(defvar alien-search/replace/ovs-on-match/data nil)
(make-variable-buffer-local 'alien-search/replace/ovs-on-match/data)

;; ----------------------------------------------------------------------------
;;  (alien-search/replace/ovs-on-match/get-all) => LIST
;; ----------------------------------------------------------------------------
(defun alien-search/replace/ovs-on-match/get-all ()
  "Get all of overlays put on each match."
  alien-search/replace/ovs-on-match/data)

;; ----------------------------------------------------------------------------
;;  (alien-search/replace/ovs-on-match/add beg end buf replacement) => LIST
;; ----------------------------------------------------------------------------
(defun alien-search/replace/ovs-on-match/add (beg end buf replacement)
  "Make overlay on match text and save it in
`alien-search/replace/ovs-on-match/data'.

Each overlay has a replacement text as property
alien-search/replace/replacement."
  (let ((ov (make-overlay beg end buf nil nil)))
    (when query-replace-lazy-highlight
      (overlay-put ov 'face lazy-highlight-face))
    (overlay-put ov 'alien-search/replace/replacement replacement)
    (overlay-put ov 'priority 1000)
    (setq alien-search/replace/ovs-on-match/data
          (nconc alien-search/replace/ovs-on-match/data (cons ov nil)))))

;; ----------------------------------------------------------------------------
;;  (alien-search/replace/ovs-on-match/dispose) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/replace/ovs-on-match/dispose ()
  "Delete overlays on matched strings created by external command
`alien-search/replace'."
  (dolist (ov alien-search/replace/ovs-on-match/data)
    (overlay-put ov 'alien-search/replace/replacement nil)
    (overlay-put ov 'priority nil)
    (delete-overlay ov))
  (setq alien-search/replace/ovs-on-match/data nil))

;; ----------------------------------------------------------------------------
;;  (alien-search/replace/ovs-on-match/get-nth nth) => OVERLAY
;; ----------------------------------------------------------------------------
(defun alien-search/replace/ovs-on-match/get-nth (nth)
  (nth nth alien-search/replace/ovs-on-match/data))

;; ----------------------------------------------------------------------------
;;  (alien-search/replace/ovs-on-match/get-count) => NUM
;; ----------------------------------------------------------------------------
(defun alien-search/replace/ovs-on-match/get-count ()
  (length alien-search/replace/ovs-on-match/data))


;;; ===========================================================================
;;;
;;;  `occur' by alien regexp with a help from external command.
;;;
;;; ===========================================================================

;; XXX: `A dot matches newline flag' should be removed?
;;      (Is that flag nonsense thing? ...because `occur' is line
;;       oriented matching operation...)
(defvar alien-search/occur/external-command nil
  "Path of an external command to use to execute actual search
operation.

Six arguments describe below will be passed to the command.

 1st: Path of a file which contains the text to be searched.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'.

 2nd: Path of a file to which the command should write the result
      of current search operation.

      The external command have to output a form like:

        (setq result
              '(
                ;; Match positions in 1st line
                ((1st-MATCH-START 1st-MATCH-END)
                 (2nd-MATCH-START 2nd-MATCH-END)
                 ...)
                ;; When a line has no match, do not put anything.
                ...
                ;; Match positions in n-th line
                ((x-th-MATCH-START x-th-MATCH-END)
                 (y-th-MATCH-START y-th-MATCH-END)
                 ...)))

      to this file.

      Note that each start and end position in the form should be
      an offset from beginning of the text which has been searched.
      (This means each number should be started from 0, not from 1)

      The text in this file must be encoded in the value of
      `alien-search/input-coding-system'.

 3rd: Path of a file in which the regexp we want to search is written.
      The command have a responsibility to search this regexp
      from the file specified by 1st argument, then write start and
      end positions of each match to the file specified by 2nd argument.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'.

 4th: A dot matches newline flag.
      When the value of this flag is not empty string,
      . should be matched to a newline character.

 5th: A case sensitive flag.
      When the value of this flag is not empty string,
      the match operation should be done case-sensitive.

 6th: An extended regular expression flag.
      When the value of this flag is not empty string,
      the current search regexp (see 3rd arg) should be
      interpreted as extended regular expression.")

(defvar alien-search/occur/shell-script nil
  "A shell script which will be run as
`alien-search/occur/external-command'
when it has nil value.")


;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/occur regexp &optional nlines) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/occur (regexp &optional nlines)
  (interactive
   (and
    (alien-search/occur/assert-available)
    (let ((regexp-history alien-search/history))
      (alien-search/read-from-minibuf/with-search-option-indicator
       (prog1
           (alien-search/occur-read-primary-args)
         (setq alien-search/history regexp-history))))))
  (alien-search/occur/assert-available)
  (let ((orig-occur-engine-fn (symbol-function 'occur-engine)))
    (setf (symbol-function 'occur-engine)
          (symbol-function 'alien-search/occur/occur-engine))
    (unwind-protect
        (occur regexp nlines)
      (setf (symbol-function 'occur-engine)
            orig-occur-engine-fn))))


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/occur/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defun alien-search/occur/available-p ()
  "Test if external command or shell script is defined or not."
  (or alien-search/occur/external-command
      alien-search/occur/shell-script))

;; ----------------------------------------------------------------------------
;;  (alien-search/occur/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defun alien-search/occur/assert-available ()
  "Raise error when no external command or shell script is defined."
  (or (alien-search/occur/available-p)
      (error "[alien-search] No external command or shell script is defined for occur.")))

;; ----------------------------------------------------------------------------
;;  (alien-search/occur-read-primary-args) => LIST
;; ----------------------------------------------------------------------------
;; Based on `occur-read-primary-args'.
;; Just for prompt message.
(defun alien-search/occur-read-primary-args ()
  (list (read-regexp "List lines matching alien regexp"
                     (car regexp-history))
        (when current-prefix-arg
          (prefix-numeric-value current-prefix-arg))))

;; ----------------------------------------------------------------------------
;;  (alien-search/occur/occur-engine regexp buffers out-buf nlines
;;                                   case-fold-search title-face
;;                                   prefix-face match-face keep-props) => NUM
;; ----------------------------------------------------------------------------
(defun alien-search/occur/occur-engine (regexp buffers out-buf nlines
                                               case-fold-search title-face
                                               prefix-face match-face keep-props)
  "Alternate function of original `occur-engine'."
  ;; Based on `occur-engine'.
  (with-current-buffer out-buf
    (let ((globalcount 0)
          (coding nil))
      ;; Map over all the buffers
      (dolist (buf buffers)
        (when (buffer-live-p buf)
          (let ((matches 0)	;; count of matched lines
                (curstring "")
                (inhibit-field-text-motion t)
                (headerpt (with-current-buffer out-buf (point)))
                (*search-result-alst* nil)
                (matches-in-line nil)
                result)
            (with-current-buffer buf
              (setq result (alien-search/run-external-command
                            alien-search/occur/external-command
                            alien-search/occur/shell-script
                            (buffer-substring (point-min) (point-max))
                            regexp
                            nil
                            (if alien-search/dot-match-a-newline-p "DOT" "")
                            (if case-fold-search "" "CASE")
                            (if alien-search/use-extended-regexp-p "EXT" "")))
              (or coding
                  ;; Set CODING only if the current buffer locally
                  ;; binds buffer-file-coding-system.
                  (not (local-variable-p 'buffer-file-coding-system))
                  (setq coding buffer-file-coding-system))
              (save-excursion
                (goto-char (point-min)) ;; begin searching in the buffer
                (while (setq matches-in-line (prog1 (car result)
                                               (setq result (cdr result))))
                  (let* ((pt-min   (point-min))
                         (matchbeg (+ pt-min (caar matches-in-line))) ;; [Count + Offset => Count]
                         (lines    (progn (goto-char matchbeg)
                                          (line-number-at-pos)))
                         (marker   (point-marker))
                         (begpt    (progn (beginning-of-line)
                                          (point)))
                         (endpt    (progn (end-of-line)
                                          (point)))
                         match-pair)
                    (setq matches (1+ matches)) ;; increment match count
                    
                    (if (and keep-props
                             (if (boundp 'jit-lock-mode) jit-lock-mode)
                             (text-property-not-all begpt endpt 'fontified t))
                        (if (fboundp 'jit-lock-fontify-now)
                            (jit-lock-fontify-now begpt endpt)))
                    (if (and keep-props (not (eq occur-excluded-properties t)))
                        (progn
                          (setq curstring (buffer-substring begpt endpt))
                          (remove-list-of-text-properties
                           0 (length curstring) occur-excluded-properties curstring))
                      (setq curstring (buffer-substring-no-properties begpt endpt)))
                    ;; Highlight the matches
                    (while (setq match-pair (prog1 (car matches-in-line)
                                              (setq matches-in-line
                                                    (cdr matches-in-line))))
                      (add-text-properties
                       (- (+ pt-min (nth 0 match-pair)) begpt) ;; [Count + Offset => Count]
                       (- (+ pt-min (nth 1 match-pair)) begpt) ;; [Count + Offset => Count]
                       (append
                        `(occur-match t)
                        (when match-face
                          ;; Use `face' rather than `font-lock-face' here
                          ;; so as to override faces copied from the buffer.
                          `(face ,match-face)))
                       curstring))
                    ;; Generate the string to insert for this match
                    (let* ((out-line
                            (concat
                             ;; Using 7 digits aligns tabs properly.
                             (apply #'propertize (format "%7d:" lines)
                                    (append
                                     (when prefix-face
                                       `(font-lock-face prefix-face))
                                     `(occur-prefix t mouse-face (highlight)
                                                    occur-target ,marker follow-link t
                                                    help-echo "mouse-2: go to this occurrence")))
                             ;; We don't put `mouse-face' on the newline,
                             ;; because that loses.  And don't put it
                             ;; on context lines to reduce flicker.
                             (propertize curstring 'mouse-face (list 'highlight)
                                         'occur-target marker
                                         'follow-link t
                                         'help-echo
                                         "mouse-2: go to this occurrence")
                             ;; Add marker at eol, but no mouse props.
                             (propertize "\n" 'occur-target marker)))
                           (data
                            (if (= nlines 0)
                                ;; The simple display style
                                out-line
                              ;; The complex multi-line display style.
                              (occur-context-lines out-line nlines keep-props)
                              )))
                      ;; Actually insert the match display data
                      (with-current-buffer out-buf
                        (let ((beg (point))
                              (end (progn (insert data) (point))))
                          (unless (= nlines 0)
                            (insert "-------\n")))))))))
            (when (not (zerop matches)) ;; is the count zero?
              (setq globalcount (+ globalcount matches))
              (with-current-buffer out-buf
                (goto-char headerpt)
                (let ((beg (point))
                      end)
                  (insert (format "%d match%s for \"%s\" in buffer: %s\n"
                                  matches (if (= matches 1) "" "es")
                                  regexp (buffer-name buf)))
                  (setq end (point))
                  (add-text-properties beg end
                                       (append
                                        (when title-face
                                          `(font-lock-face ,title-face))
                                        `(occur-title ,buf))))
                (goto-char (point-min)))))))
      (if coding
          ;; CODING is buffer-file-coding-system of the first buffer
          ;; that locally binds it.  Let's use it also for the output
          ;; buffer.
          (set-buffer-file-coding-system coding))
      ;; Return the number of matches
      globalcount)))


;;; ===========================================================================
;;;
;;;  `search-forward' and `search-backward' for alien regexp
;;;                                         with a help from external command.
;;;
;;; ===========================================================================

(defvar alien-search/search/external-command nil
  "Path of an external command to use to execute actual search operation.

Seven arguments describe below will be passed to the command.

 1st: Path of a file which contains the text to be searched.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'.

 2nd: Path of a file to which the command should write the result
      of current search operation.

      The external command have to output a form like:

        (setq result
              '((1st-MATCH-START 1st-MATCH-END
                 SUB-MATCH-1-IN-1st-MATCH-START SUB-MATCH-1-IN-1st-MATCH-END
                 SUB-MATCH-2-IN-1st-MATCH-START SUB-MATCH-2-IN-1st-MATCH-END
                 ...)
                (2nd-MATCH-START 2nd-MATCH-END)
                 SUB-MATCH-1-IN-2nd-MATCH-START SUB-MATCH-1-IN-2nd-MATCH-END
                 SUB-MATCH-2-IN-2nd-MATCH-START SUB-MATCH-2-IN-2nd-MATCH-END
                 ...)
                ...)

      to this file.

      Note that each start and end position in the form should be
      an offset from beginning of the text which has been searched.
      (This means each number should be started from 0, not from 1)

      The text in this file must be encoded in the value of
      `alien-search/input-coding-system'.

 3rd: Path of a file in which the regexp we want to search is written.
      The command have a responsibility to search this regexp
      from the file specified by 1st argument, then write start and
      end positions of each match to the file specified by 2nd argument.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'.

 4th: A dot matches newline flag.
      When the value of this flag is not empty string,
      . should be matched to a newline character.

 5th: A case sensitive flag.
      When the value of this flag is not empty string,
      the match operation should be done case-sensitive.

 6th: An extended regular expression flag.
      When the value of this flag is not empty string,
      the current search regexp (see 3rd arg) should be
      interpreted as extended regular expression.

 7th: Positive integer when we want limit the matches, or empty
      string when we don't want limit the matches.")

(defvar alien-search/search/shell-script nil
  "A shell script which will be run as
`alien-search/search/external-command'
when it has nil value.")


;; ----------------------------------------------------------------------------
;;
;;  Macros
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/search/with-regarding-string-as-alien-regexp
;;                  (string &optional limit) &rest body) => RESULT OF BODYFORM
;; ----------------------------------------------------------------------------
(defmacro alien-search/search/with-regarding-string-as-alien-regexp (args &rest body)
  "Run BODY with applying `alien-search/search/forward' and
`alien-search/search/backward' to STRING in substitution for
`re-search-forward' and `re-search-backward'.

Note that the equivalence of the STRING are tested in `eq'.

When LIMIT is a number, match will be limited to the LIMIT.
When LIMIT is NIL, match won't be limited.

\(FN (STRING &OPTIONAL LIMIT) &REST BODY)"
  (declare (indent 1))
  (let ((g-orig-re-fwd-fn  (gensym))
        (g-orig-re-bkwd-fn (gensym))
        (g-regexp          (gensym))
        (g-limit           (gensym))
        (g-args            (gensym)))
    `(let ((,g-orig-re-fwd-fn  (symbol-function 're-search-forward))
           (,g-orig-re-bkwd-fn (symbol-function 're-search-backward)))
       (unwind-protect
           (lexical-let ((,g-regexp ,(nth 0 args))
                         (,g-limit  ,(nth 1 args)))
             (setf (symbol-function 're-search-forward)
                   (lambda (regexp &optional bound noerror count)
                     (cond
                      ((eq regexp
                           ,g-regexp)
                       (funcall 'alien-search/search/forward
                                regexp bound noerror count ,g-limit))
                      (t
                       (funcall ,g-orig-re-fwd-fn 
                                regexp bound noerror count)))))
             (setf (symbol-function 're-search-backward)
                   (lambda (regexp &optional bound noerror count)
                     (cond
                      ((eq regexp
                           ,g-regexp)
                       (funcall 'alien-search/search/backward
                                regexp bound noerror count ,g-limit))
                      (t
                       (funcall ,g-orig-re-bkwd-fn
                                regexp bound noerror count)))))
             ,@body)
         (setf (symbol-function 're-search-forward)  ,g-orig-re-fwd-fn)
         (setf (symbol-function 're-search-backward) ,g-orig-re-bkwd-fn)))))

;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/search/forward regexp &optional bound noerror count limit)
;;                                                                    => POINT
;; ----------------------------------------------------------------------------
(defun alien-search/search/forward (regexp &optional bound noerror count limit)
  "Search forward from point for REGEXP in alien manner.
See also `re-search-forward'."
  (interactive
   (and
    (alien-search/search/assert-available)
    (alien-search/read-from-minibuf/with-search-option-indicator
     (list (read-from-minibuffer "Search for alien regexp: "
                                 nil nil nil
                                 'alien-search/history)))))
  (let* ((buf    (current-buffer))
         (data   (or (alien-search/search/cache/get    buf regexp limit)
                     (alien-search/search/cache/update buf regexp limit)))
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
        (alien-search/search/forward
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
;;  (alien-search/search/backward regexp &optional bound noerror count limit)
;;                                                                    => POINT
;; ----------------------------------------------------------------------------
(defun alien-search/search/backward (regexp &optional bound noerror count limit)
  "Search backward from point for REGEXP in alien manner.
See also `re-search-backward'."
  (interactive
   (and
    (alien-search/search/assert-available)
    (alien-search/read-from-minibuf/with-search-option-indicator
     (list (read-from-minibuffer "Search for alien regexp backward: "
                                 nil nil nil
                                 'alien-search/history)))))
  (let* ((buf    (current-buffer))
         (data   (reverse (or (alien-search/search/cache/get    buf regexp limit)
                              (alien-search/search/cache/update buf regexp limit))))
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
        (alien-search/search/backward
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
;;  (alien-search/search/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defun alien-search/search/available-p ()
  "Test if external command or shell script is defined or not."
  (or alien-search/search/external-command
      alien-search/search/shell-script))

;; ----------------------------------------------------------------------------
;;  (alien-search/search/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defun alien-search/search/assert-available ()
  "Raise error when no external command or shell script is defined."
  (or (alien-search/search/available-p)
      (error "[alien-search] No external command or shell script is defined for search.")))


;;; ===========================================================================
;;;
;;;  Cache data for `alien-search/search/forward'
;;;                                       and `alien-search/search/backward'.
;;;
;;; ===========================================================================

(defvar alien-search/search/.cache-alst nil)


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/search/cache/pre-command-hook-fn) void
;; ----------------------------------------------------------------------------
(defun alien-search/search/cache/pre-command-hook-fn ()
  "Clear cache before any command will run.

When `this-command' has property `alien-search/search/ongoing-search-cmd-p',
cache won't be cleared."
  (condition-case c
      (when (not (and (symbolp this-command)
                      (get this-command
                           'alien-search/search/ongoing-search-cmd)))
        (alien-search/search/cache/clear-all))
    (error
     (message "[alien-search] %s" c))))

(put 'isearch-repeat-forward  'alien-search/search/ongoing-search-cmd t)
(put 'isearch-repeat-backward 'alien-search/search/ongoing-search-cmd t)

(add-hook 'pre-command-hook 'alien-search/search/cache/pre-command-hook-fn)

;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/search/cache/get buf regexp &optional limit) => LIST
;; ----------------------------------------------------------------------------
(defun alien-search/search/cache/get (buf regexp &optional limit)
  "Returns cache data, which is the result of a search
in BUF for REGEXP by external command."
  (with-current-buffer buf
    (let* ((cache-alst-for-buf (cdr (assq buf     alien-search/search/.cache-alst)))
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
;;  (alien-search/search/cache/clear buf regexp) => LIST
;; ----------------------------------------------------------------------------
(defun alien-search/search/cache/clear (buf regexp)
  "Clear cache data, which is the result of a search
in BUF for REGEXP by external command."
  (when (assq regexp
              (cdr (assq buf alien-search/search/.cache-alst)))
    (alien-search/debug alien-search/search/debug-cache
                        "Clear: %s, \"%s\"\n" buf regexp)
    (assq-delete-all
     regexp
     (cdr (assq buf alien-search/search/.cache-alst)))))

;; ----------------------------------------------------------------------------
;;  (alien-search/search/cache/clear-all) => LIST
;; ----------------------------------------------------------------------------
(defun alien-search/search/cache/clear-all ()
  "Clear all of cache data, which is the result of a search
by external command."
  (when alien-search/search/.cache-alst
    (alien-search/debug alien-search/search/debug-cache
                        "Clear: All\n")
    (setq alien-search/search/.cache-alst nil)))

;; ----------------------------------------------------------------------------
;;  (alien-search/search/cache/update buf regexp) => LIST
;; ----------------------------------------------------------------------------
(defun alien-search/search/cache/update (buf regexp &optional limit)
  "Update cache data, which is the result of a search
in BUF for REGEXP by external command."
  (alien-search/debug alien-search/search/debug-cache
                      "Update: %s, \"%s\"\n" buf regexp)
  (condition-case c
      (with-current-buffer buf
        (let ((pt-min      (point-min))
              (pt-max      (point-max))
              (case-fold-p (if isearch-mode
                               isearch-case-fold-search
                             case-fold-search))
              result)
          (setq result
                (alien-search/run-external-command
                 alien-search/search/external-command
                 alien-search/search/shell-script
                 (buffer-substring pt-min pt-max)
                 regexp
                 nil
                 (if alien-search/dot-match-a-newline-p "DOT"  "")
                 (if (not case-fold-p)                  "CASE" "") 
                 (if alien-search/use-extended-regexp-p "EXT"  "")
                 (cond
                  ((null     limit) "")
                  ((integerp limit) (format "%s" limit))
                  (t
                   (error "[alien-search] `%s' is not type of number or null."
                          limit)))))

          (dolist (be-lst result)
            (dotimes (i (length be-lst))
              (setf (nth i be-lst)
                    (+ pt-min (nth i be-lst))))) ;; [Count + Offset => Count]

          ;; Remove old cache.
          (alien-search/search/cache/clear buf regexp)

          ;; Save new cache.
          (or (assq buf alien-search/search/.cache-alst)
              (push (list buf)
                    alien-search/search/.cache-alst))
          (push (cons regexp
                      (list (cons 'data   result)
                            (cons 'limit  limit)
                            (cons 'tick   (buffer-chars-modified-tick buf))
                            (cons 'pt-min pt-min)
                            (cons 'pt-max pt-max)))
                (cdr (assq buf alien-search/search/.cache-alst)))
          result))
    (error
     (unwind-protect
         ;; Remove old cache.
         (alien-search/search/cache/clear buf regexp)
       (signal 'invalid-regexp
               (list (concat "[alien-search] "
                             (error-message-string c))))))))

  
;;; ===========================================================================
;;;
;;;  `isearch' for alien regexp with a help from external command.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/isearch-forward &optional not-regexp no-recursive-edit)
;;                                                                     => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/isearch-forward (&optional not-regexp no-recursive-edit)
  "Do isearch with a help from external command.

See `isearch-forward-regexp' and `isearch-backward-regexp' for
more information."
  (interactive "P\np")
  (alien-search/isearch/assert-available)
  
  ;; Setup `isearch-search-fun-function'.
  (when (not (boundp 'alien-search/isearch/orig-isearch-search-fun-function))
    (setq alien-search/isearch/orig-isearch-search-fun-function
          isearch-search-fun-function))
  (setq isearch-search-fun-function #'alien-search/isearch/isearch-search-fun-function)
  (add-hook 'isearch-mode-end-hook
            'alien-search/isearch/.isearch-mode-end-hook-fn)
  
  ;; Just for prompt message.
  (alien-search/ad-enable 'isearch-message-prefix 'after 'alien-search/isearch/modify-prompt)
  (alien-search/ad-activate 'isearch-message-prefix)
  
  (isearch-mode t (null not-regexp) nil (not no-recursive-edit)))


;; ----------------------------------------------------------------------------
;;  (alien-search/isearch-backward &optional not-regexp no-recursive-edit)
;;                                                                     => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/isearch-backward (&optional not-regexp no-recursive-edit)
  "Do isearch with a help from external command.

See `isearch-forward-regexp' and `isearch-backward-regexp' for
more information."
  (interactive "P\np")
  (alien-search/isearch/assert-available)
  
  ;; Setup `isearch-search-fun-function'.
  (when (not (boundp 'alien-search/isearch/orig-isearch-search-fun-function))
    (setq alien-search/isearch/orig-isearch-search-fun-function
          isearch-search-fun-function))
  (setq isearch-search-fun-function #'alien-search/isearch/isearch-search-fun-function)
  (add-hook 'isearch-mode-end-hook
            'alien-search/isearch/.isearch-mode-end-hook-fn)
  
  ;; Just for prompt message.
  (alien-search/ad-enable 'isearch-message-prefix 'after 'alien-search/isearch/modify-prompt)
  (alien-search/ad-activate 'isearch-message-prefix)
  
  (isearch-mode nil (null not-regexp) nil (not no-recursive-edit)))


;; ----------------------------------------------------------------------------
;;
;;  Advices
;;
;; ----------------------------------------------------------------------------

;; Just for prompt message.
(defadvice isearch-message-prefix (after alien-search/isearch/modify-prompt
                                         (&optional c-q-hack ellipsis nonincremental))
  (when (string-match "\\b\\(\\([Rr]\\)egexp\\)\\b" ad-return-value)
    (setq ad-return-value
          (replace-match (propertize
                          (if (string= "R" (match-string 2 ad-return-value))
                              "Alien regexp"
                            "alien regexp")
                          'face 'minibuffer-prompt)
                         t t ad-return-value)))
  ;; Put search option indicator.
  (when (string-match "\\(: \\)$" ad-return-value)
    (setq ad-return-value
          (replace-match (propertize
                          (concat (alien-search/search-option-indicator/make-indicator)
                                  ": ")
                          'face 'minibuffer-prompt)
                         t t ad-return-value))))

;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/isearch/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defalias 'alien-search/isearch/available-p 'alien-search/search/available-p)

;; ----------------------------------------------------------------------------
;;  (alien-search/isearch/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defalias 'alien-search/isearch/assert-available 'alien-search/search/assert-available)

;; ----------------------------------------------------------------------------
;;  (alien-search/isearch/search-option-changed-hook-fn) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/isearch/search-option-changed-hook-fn ()
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
;;  (alien-search/isearch/setup-search-option-changed-hook) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/isearch/setup-search-option-changed-hook ()
  "Set call back function `alien-search/isearch/search-option-changed-hook-fn'
to each search option changed hook."
  (add-hook 'alien-search/case-fold-changed-hook
            'alien-search/isearch/search-option-changed-hook-fn)

  (add-hook 'alien-search/dot-match-changed-hook
            'alien-search/isearch/search-option-changed-hook-fn)

  (add-hook 'alien-search/ext-regexp-changed-hook
            'alien-search/isearch/search-option-changed-hook-fn))

;; ----------------------------------------------------------------------------
;;  (alien-search/isearch/.isearch-mode-end-hook-fn ) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/isearch/.isearch-mode-end-hook-fn ()
  "Clean up environment when isearch by alien-search is finished."
  (when (not isearch-nonincremental)
    (when (boundp 'alien-search/isearch/orig-isearch-search-fun-function)
      (setq isearch-search-fun-function
            alien-search/isearch/orig-isearch-search-fun-function)
      (makunbound 'alien-search/isearch/orig-isearch-search-fun-function))
    
    ;; Just for prompt message.
    (alien-search/ad-disable 'isearch-message-prefix 'after 'alien-search/isearch/modify-prompt)
    (alien-search/ad-activate 'isearch-message-prefix)
    
    (remove-hook 'isearch-mode-end-hook
                 'alien-search/isearch/.isearch-mode-end-hook-fn)))

;; ----------------------------------------------------------------------------
;;  (alien-search/isearch/isearch-search-fun-function) => FUNCTION
;; ----------------------------------------------------------------------------
(defun alien-search/isearch/isearch-search-fun-function ()
  "The value used as value of `isearch-search-fun' while
isearch by alien-search is going on.

This function returns the search function
`alien-search/search/forward' or `alien-search/search/backward'
for isearch to use."
  (cond
   (isearch-forward
    'alien-search/search/forward)
   (t
    'alien-search/search/backward)))


;; ----------------------------------------------------------------------------
;;
;;  Main
;;
;; ----------------------------------------------------------------------------

(add-hook 'isearch-mode-hook
          'alien-search/isearch/setup-search-option-changed-hook)


;;; ===========================================================================
;;;
;;;  quote meta characters of alien regexp by external command.
;;;
;;; ===========================================================================

(defvar alien-search/quote-meta/external-command nil
  "Path of an external command to use to execute actual
quote-meta operation.

Two arguments describe below will be passed to the command.

 1st: Path of a file to which the command should write the result
      of quote-meta operation.

      The external command have to output a form like:

        (setq result \"quoted string\")

      to this file.

      The text in this file must be encoded in the value of
      `alien-search/input-coding-system'.

 2nd: Path of a file in which the regexp, we want to quote meta
      characters in it, is written.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'.")

(defvar alien-search/quote-meta/shell-script nil
  "A shell script which will be run as
`alien-search/quote-meta/external-command'
when it has nil value.")


;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/quote-meta-in-region beg end) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/quote-meta-in-region (beg end)
  "Quote meta characters in region in manner of external command."
  (interactive "r")
  (save-excursion
    (let ((quoted-str (alien-search/quote-meta
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
;;  (alien-search/quote-meta/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defun alien-search/quote-meta/available-p ()
  "Test if external command or shell script is defined or not."
  (or alien-search/quote-meta/external-command
      alien-search/quote-meta/shell-script))

;; ----------------------------------------------------------------------------
;;  (alien-search/quote-meta/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defun alien-search/quote-meta/assert-available ()
  "Raise error when no external command or shell script is defined."
  (or (alien-search/quote-meta/available-p)
      (error "[alien-search] No external command or shell script is defined for quote-meta.")))

;; ----------------------------------------------------------------------------
;;  (alien-search/quote-meta regexp) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/quote-meta (regexp)
  "Quote meta characters in a REGEXP in manner of external command."
  (interactive)
  (alien-search/quote-meta/assert-available)
  
  (alien-search/run-external-command
   alien-search/quote-meta/external-command
   alien-search/quote-meta/shell-script
   nil ;; Don't care about text in current buffer.
   regexp
   nil))


;;; ===========================================================================
;;;
;;;  `re-builder' in alien regexp with a help from external command.
;;;
;;; ===========================================================================
(require 're-builder)

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/exec-with-current-re re-var &rest body)
;;                                                     => RESULT FROM BODYFORM
;; ----------------------------------------------------------------------------
(defmacro alien-search/re-builder/exec-with-current-re (re-var &rest body)
  "When the current buffer is *RE-Builder*, exit `re-builder'
and then run BODY with binding current RE to RE-VAR.

NOTE: RE-VAR will be defined as lexical variable by this macro."
  (declare (indent 1))
  `(progn
     (case reb-re-syntax
       ((alien)
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
        (error "[alien-search] RE-Builder syntax is not `alien'.")))))


;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/query-replace-on-target-buffer) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/query-replace-on-target-buffer ()
  "Run `alien-search/query-replace' with current RE on
`reb-target-buffer'."
  (interactive)
  (alien-search/re-builder/assert-in-reb-buffer)
  (alien-search/replace/assert-available)
  
  (alien-search/re-builder/exec-with-current-re regexp
    (when (match-beginning 0)
      (goto-char (match-beginning 0)))
    
    (alien-search/read-from-minibuf/with-initial-contents regexp
      (let ((this-command 'alien-search/query-replace))
        (call-interactively 'alien-search/query-replace)))))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/occur-on-target-buffer) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/occur-on-target-buffer ()
  "Run `alien-search/occur' with current RE on
`reb-target-buffer'."
  (interactive)
  (alien-search/re-builder/assert-in-reb-buffer)
  (alien-search/occur/assert-available)
  
  (alien-search/re-builder/exec-with-current-re regexp
    (alien-search/read-from-minibuf/with-initial-contents regexp
      (let ((this-command 'alien-search/occur))
        (call-interactively 'alien-search/occur)))))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/isearch-forward-on-target-buffer) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/isearch-forward-on-target-buffer ()
  "Run `alien-search/isearch-forward' with current RE on
`reb-target-buffer'."
  (interactive)
  (alien-search/re-builder/assert-in-reb-buffer)
  (alien-search/isearch/assert-available)
  
  (alien-search/re-builder/exec-with-current-re regexp
    (add-hook 'isearch-mode-hook
              (alien-search/alambda ()
                (remove-hook 'isearch-mode-hook
                             #'self)
                (isearch-push-state)
                (setq isearch-string  regexp
                      isearch-message regexp)))
    (alien-search/isearch-forward)))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/isearch-backward-on-target-buffer) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/isearch-backward-on-target-buffer ()
  "Run `alien-search/isearch-backward' with current RE on
`reb-target-buffer'."
  (interactive)
  (alien-search/re-builder/assert-in-reb-buffer)
  (alien-search/isearch/assert-available)
  
  (alien-search/re-builder/exec-with-current-re regexp
    (add-hook 'isearch-mode-hook
              (alien-search/alambda ()
                (remove-hook 'isearch-mode-hook
                             #'self)
                (isearch-push-state)
                (setq isearch-string  regexp
                      isearch-message regexp)))
    (alien-search/isearch-backward)))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/non-incremental-search-forward-on-target-buffer)
;;                                                                     => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/non-incremental-search-forward-on-target-buffer ()
  "Run `alien-search/non-incremental/search-forward' with
current RE on `reb-target-buffer'."
  (interactive)
  (alien-search/re-builder/assert-in-reb-buffer)
  (alien-search/non-incremental/assert-available)
  
  (alien-search/re-builder/exec-with-current-re regexp
    (alien-search/non-incremental/search-forward regexp)))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/non-incremental-search-backward-on-target-buffer)
;;                                                                     => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/non-incremental-search-backward-on-target-buffer ()
  "Run `alien-search/non-incremental/search-backward' with
current RE on `reb-target-buffer'."
  (interactive)
  (alien-search/re-builder/assert-in-reb-buffer)
  (alien-search/non-incremental/assert-available)
  
  (alien-search/re-builder/exec-with-current-re regexp
    (alien-search/non-incremental/search-forward regexp)))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/toggle-case-fold-on-target-buffer
;;                                               &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/toggle-case-fold-on-target-buffer (&optional no-message)
  "Toggle `case-fold-search' on `reb-target-buffer'."
  (interactive)
  (alien-search/re-builder/assert-in-reb-buffer)
  
  (with-current-buffer reb-target-buffer
    (alien-search/toggle-case-fold no-message)))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/toggle-dot-match-on-target-buffer
;;                                               &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/toggle-dot-match-on-target-buffer (&optional no-message)
  "Toggle `alien-search/dot-match-a-newline-p' on `reb-target-buffer'."
  (interactive)
  (alien-search/re-builder/assert-in-reb-buffer)
  
  (with-current-buffer reb-target-buffer
    (alien-search/toggle-dot-match no-message)))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/toggle-ext-regexp-on-target-buffer
;;                                               &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/toggle-ext-regexp-on-target-buffer (&optional no-message)
  "Toggle `alien-search/use-extended-regexp-p' on `reb-target-buffer'."
  (interactive)
  (alien-search/re-builder/assert-in-reb-buffer)
  
  (with-current-buffer reb-target-buffer
    (alien-search/toggle-ext-regexp no-message)))


;; ----------------------------------------------------------------------------
;;
;;  Advices
;;
;; ----------------------------------------------------------------------------

(defadvice reb-next-match (around alien-search/re-builder/ext-match ())
  (case reb-re-syntax
    ((alien)
     (alien-search/re-builder/assert-available)
     (alien-search/search/with-regarding-string-as-alien-regexp
         ((reb-target-binding reb-regexp)
          ;; Match limit.
          (if (numberp reb-auto-match-limit)
              reb-auto-match-limit nil))
       ad-do-it))
    (t
     ad-do-it)))
(alien-search/ad-activate 'reb-next-match)
(put 'reb-next-match  'alien-search/search/ongoing-search-cmd t)

(defadvice reb-prev-match (around alien-search/re-builder/prev-match ())
  (case reb-re-syntax
    ((alien)
     (alien-search/re-builder/assert-available)
     (alien-search/search/with-regarding-string-as-alien-regexp
         ((reb-target-binding reb-regexp)
          ;; Match limit.
          (if (numberp reb-auto-match-limit)
              reb-auto-match-limit nil))
       ad-do-it))
    (t
     ad-do-it)))
(alien-search/ad-activate 'reb-prev-match)
(put 'reb-prev-match 'alien-search/search/ongoing-search-cmd t)

(defadvice reb-copy (around alien-search/re-builder/copy ())
  (case reb-re-syntax
   ((alien)
    (alien-search/re-builder/assert-available)
    (kill-new (buffer-substring-no-properties (point-min) (point-max))))
   (t ad-do-it)))
(alien-search/ad-activate 'reb-copy)
(put 'reb-copy 'alien-search/search/ongoing-search-cmd t)

(defadvice reb-change-syntax (around alien-search/re-builder/change-syntax (&optional syntax))
  (interactive
   (list (intern
          (completing-read "Select syntax: "
                           (mapcar (lambda (el) (cons (symbol-name el) 1))
                                   (alien-search/re-builder/get-syntax-lst))
                           nil t (symbol-name reb-re-syntax)))))
  (if (memq syntax '(read string lisp-re sregex rx alien))
      (let ((buffer (get-buffer reb-buffer)))
	(setq reb-re-syntax syntax)
	(when buffer
          (with-current-buffer buffer
            (reb-initialize-buffer))))
    (error "Invalid syntax: %s"  syntax)))
(alien-search/ad-activate 'reb-change-syntax)

(defadvice reb-update-modestring (around alien-search/re-builder/update-mode-string ())
  "Put search option indicator on modeline."
  (case reb-re-syntax
   ((alien)
    (setq reb-mode-string
          (concat
           (if reb-subexp-mode
               (format " (subexp %s)" (or reb-subexp-displayed "-"))
             "")
           " "
           (with-current-buffer reb-target-buffer
             (alien-search/search-option-indicator/make-indicator))))
    (force-mode-line-update))
   (t
    ad-do-it)))
(alien-search/ad-activate 'reb-update-modestring)

(defadvice reb-read-regexp (around alien-search/re-builder/read-regexp ())
  (case reb-re-syntax
   ((alien)
    (setq ad-return-value
          (buffer-substring-no-properties (point-min) (point-max))))
   (t ad-do-it)))
(alien-search/ad-activate 'reb-read-regexp)

(defadvice reb-insert-regexp (around alien-search/re-builder/insert-regexp ())
  (case reb-re-syntax
   ((alien)
    (when reb-regexp
      (insert reb-regexp)))
   (t ad-do-it)))
(alien-search/ad-activate 'reb-insert-regexp)

(defadvice reb-update-regexp (around alien-search/re-builder/update-regexp ())
  (case reb-re-syntax
   ((alien)
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
(alien-search/ad-activate 'reb-update-regexp)

(defadvice reb-count-subexps (around alien-search/re-builder/count-subexps (re))
  (case reb-re-syntax
   ((alien)
    (alien-search/re-builder/assert-available)
    
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
            (dolist (be-lst (or (alien-search/search/cache/get    buf regexp limit)
                                (alien-search/search/cache/update buf regexp limit)))
              (setq retval (max retval (1- (/ (length be-lst) 2))))))
          (setq ad-return-value retval)))))
   (t ad-do-it)))
(alien-search/ad-activate 'reb-count-subexps)

(defadvice reb-update-overlays (around alien-search/re-builder/update-overlays (&optional subexp))
  (case reb-re-syntax
    ((alien)
     (alien-search/re-builder/assert-available)

     (alien-search/search/with-regarding-string-as-alien-regexp
         ((reb-target-binding reb-regexp)
          ;; Match limit.
          (if (numberp reb-auto-match-limit)
              reb-auto-match-limit nil))
       ad-do-it))
    (t
     ad-do-it)))
(alien-search/ad-activate 'reb-update-overlays)

       
;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/assert-in-reb-buffer) => VOID OR ERROR
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/assert-in-reb-buffer ()
  (when (not (reb-mode-buffer-p))
    (error "[alien-search] Not in *RE-Builder* buffer.")))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defalias 'alien-search/re-builder/available-p 'alien-search/search/available-p)

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defalias 'alien-search/re-builder/assert-available 'alien-search/search/assert-available)

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/get-current-regexp) => STRING or NIL
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/get-current-regexp ()
  "Returns regular expression in buffer *RE-Builder*.

When the current buffer is not *RE-Builder*, returns nil."
  (let* ((reb-buf (get-buffer reb-buffer))
         (regexp (when (eq reb-buf (current-buffer))
                   (with-current-buffer reb-buf
                     (buffer-substring (point-min) (point-max))))))
    regexp))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/get-syntax-lst) => LIST
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/get-syntax-lst ()
  "Returns list of syntax defined in `reb-re-syntax'."
  ;; XXX: Ugly hack.
  ;;      This won't work if `reb-re-syntax' has
  ;;      structure other than default value.
  (let ((type (get 'reb-re-syntax 'custom-type)))
    (setq type (delete 'choice type))
    (mapcar '(lambda (alt-type)
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
;;  (alien-search/re-builder/reb-target-buffer-p buf) => BOOL
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/reb-target-buffer-p (buf)
  "Test if BUF is `reb-target-buffer' and if `reb-re-syntax' is
'alien or not."
  (and (eq reb-re-syntax 'alien)
       (get-buffer reb-buffer)
       reb-target-buffer
       (eq reb-target-buffer
           buf)))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/search-option-changed-hook-fn) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/search-option-changed-hook-fn ()
  "Update mode string and update search status with external command.
Called when search option of `reb-target-buffer' is changed."
  (let ((cur-buf (current-buffer)))
    (when (alien-search/re-builder/reb-target-buffer-p cur-buf)
      (with-current-buffer cur-buf
        (reb-update-modestring))
      (with-current-buffer (get-buffer reb-buffer)
        (reb-auto-update nil nil nil t)))))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/setup-search-option-changed-hook) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/setup-search-option-changed-hook ()
  "Set call back function `alien-search/re-builder/search-option-changed-hook-fn'
to each search option changed hook."
  (when reb-target-buffer
    (with-current-buffer reb-target-buffer
      (add-hook 'alien-search/case-fold-changed-hook
                'alien-search/re-builder/search-option-changed-hook-fn)
      
      (add-hook 'alien-search/dot-match-changed-hook
                'alien-search/re-builder/search-option-changed-hook-fn)
      
      (add-hook 'alien-search/ext-regexp-changed-hook
                'alien-search/re-builder/search-option-changed-hook-fn))))
    
;; ----------------------------------------------------------------------------
;;
;;  Main
;;
;; ----------------------------------------------------------------------------

;; XXX: Ugly hack.
;; Put `alien' to custom variable `reb-re-syntax'.
(when (not (memq 'alien (alien-search/re-builder/get-syntax-lst)))
  (put 'reb-re-syntax 'custom-type
       (nconc (get 'reb-re-syntax 'custom-type)
              '((const :tag "Alien syntax" alien)))))

(add-hook 'reb-mode-hook
          'alien-search/re-builder/setup-search-option-changed-hook)

(put 'reb-enter-subexp-mode 'alien-search/search/ongoing-search-cmd t)
(put 'reb-quit-subexp-mode  'alien-search/search/ongoing-search-cmd t)
(put 'reb-display-subexp    'alien-search/search/ongoing-search-cmd t)


;;; ===========================================================================
;;;
;;;  Non-incremental search for alien regexp with a help from external command.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/non-incremental/search-forward regexp) => POINT
;; ----------------------------------------------------------------------------
;; From menu-bar.el
(defun alien-search/non-incremental/search-forward (&optional regexp)
  "Read a regular expression and search for it nonincrementally."
  (interactive)
  (prog1
      (cond
       ((called-interactively-p)
        (call-interactively 'alien-search/search/forward))
       (t
        (alien-search/search/forward regexp)))
    (setq menu-bar-last-search-type 'alien)))

;; ----------------------------------------------------------------------------
;;  (alien-search/non-incremental/search-backward regexp) => POINT
;; ----------------------------------------------------------------------------
;; From menu-bar.el
(defun alien-search/non-incremental/search-backward (&optional regexp)
  "Read a regular expression and search for it backward nonincrementally."
  (interactive)
  (prog1
      (cond
       ((called-interactively-p)
        (call-interactively 'alien-search/search/backward))
       (t
        (alien-search/search/backward regexp)))
    (setq menu-bar-last-search-type 'alien)))


;; ----------------------------------------------------------------------------
;;
;;  Advices
;;
;; ----------------------------------------------------------------------------

(when (require 'menu-bar nil t)
  (when (fboundp 'nonincremental-repeat-search-forward)
    (defadvice nonincremental-repeat-search-forward (around alien-search/nonincremental-repeat-search-forward ())
      (cond
       ((and (eq menu-bar-last-search-type 'alien)
             alien-search/history)
        (setq ad-return-value
              (alien-search/search/forward (car alien-search/history))))
       (t
        ad-do-it)))
    (alien-search/ad-activate 'nonincremental-repeat-search-forward)
    (put 'nonincremental-repeat-search-forward 'alien-search/search/ongoing-search-cmd t))

  (when (fboundp 'nonincremental-repeat-search-backward)
    (defadvice nonincremental-repeat-search-backward (around alien-search/nonincremental-repeat-search-backward ())
      (cond
       ((and (eq menu-bar-last-search-type 'alien)
             alien-search/history)
        (setq ad-return-value
              (alien-search/search/backward (car alien-search/history))))
       (t
        ad-do-it)))
    (alien-search/ad-activate 'nonincremental-repeat-search-backward)
    (put 'nonincremental-repeat-search-backward 'alien-search/search/ongoing-search-cmd t)))

;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/non-incremental/available-p) => BOOL
;; ----------------------------------------------------------------------------
(defalias 'alien-search/non-incremental/available-p 'alien-search/search/available-p)

;; ----------------------------------------------------------------------------
;;  (alien-search/non-incremental/assert-available) => VOID or ERROR
;; ----------------------------------------------------------------------------
(defalias 'alien-search/non-incremental/assert-available 'alien-search/search/assert-available)


;;; ===========================================================================
;;;
;;;  The definition of transition between each command.
;;;
;;; ===========================================================================

(defvar alien-search/transition/.running-cmd nil
  "Internal variable.")

(defvar alien-search/transition/command-table
  '((:label re-builder
            :op-kind    re-builder-cmd
            :command    re-builder)
    (:label isearch-forward
            :op-kind    isearch-cmd
            :command    alien-search/isearch-forward)
    (:label isearch-backward
            :op-kind    isearch-cmd
            :command    alien-search/isearch-backward)
    (:label replace
            :op-kind    minibuf-cmd
            :command    alien-search/query-replace
            ;; required by minibuf-cmd.
            :transition-allowed-in query-replace-read-from)
    (:label occur
            :op-kind    minibuf-cmd
            :command    alien-search/occur
            ;; required by minibuf-cmd.
            :transition-allowed-in alien-search/occur)
    (:label noinc-fwd
            :op-kind    minibuf-cmd
            :command    alien-search/non-incremental/search-forward
            ;; required by minibuf-cmd.
            :transition-allowed-in alien-search/non-incremental/search-forward)
    (:label noinc-bkwd
            :op-kind    minibuf-cmd
            :command    alien-search/non-incremental/search-backward
            ;; required by minibuf-cmd.
            :transition-allowed-in alien-search/non-incremental/search-backward))
    "Not documented yet.")


;; ----------------------------------------------------------------------------
;;
;;  Macros
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/transition/.setup sym-table) => VOID
;; ----------------------------------------------------------------------------
(defmacro alien-search/transition/.setup (sym-table)
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
               ;; Alien-search commands which makes transition from
               ;; `re-builder' are defined as `alien-search/re-builder/run-*'.
               )
              ((isearch-cmd)
               (let ((ad-name-make-transition-to  (intern
                                                   (format
                                                    "alien-search/transition/%s/make-transition-to-%s"
                                                    label targ-label)))
                     (fn-name-turn-on-transition  (intern
                                                   (format
                                                    "alien-search/transition/%s/turn-on-make-transition-to-%s"
                                                    label targ-label)))
                     (fn-name-turn-off-transition (intern
                                                   (format
                                                    "alien-search/transition/%s/turn-off-make-transition-to-%s"
                                                    label targ-label))))
                 (nconc
                  retval
                  `(
                    ;; Advise other alien-search commands so that
                    ;; we can exit current isearch session and
                    ;; make transition to them.
                    (defadvice ,targ-command (around
                                              ,ad-name-make-transition-to
                                              (&rest args))
                      "Exit current isearch session and make a
transition to another alien-search command."
                      ;; DEBUGGING
                      (alien-search/debug alien-search/transition/debug-advices
                                          "TRANSITION: CALL: %s, %s"
                                          (quote ,targ-command)
                                          (quote ,ad-name-make-transition-to))
                      
                      (lexical-let ((regexp isearch-string))
                        (unwind-protect
                            (isearch-exit)
                          ;; `isearch-exit' throws something in some case,
                          ;; so another alien-search command should be called
                          ;; from within protected form.
                          ,@(case targ-op-kind
                              ((re-builder-cmd)
                               ;; NOTE: Do not run `re-builder' with timer,
                               ;;       or window won't be switched to
                               ;;       *RE-Builder* properly.
                               `((case reb-re-syntax
                                   ((alien)
                                    (let ((this-command (quote ,targ-command)))
                                      (call-interactively (quote ,targ-command)))
                                    (with-current-buffer (get-buffer reb-buffer)
                                      (delete-region (point-min) (point-max))
                                      (insert regexp)))
                                   (t
                                    (error "[alien-search] RE-Builder syntax is not `alien'.")))))
                              ((minibuf-cmd)
                               `((run-with-idle-timer
                                  0 nil
                                  (lambda ()
                                    ;; Call another alien-search command with setting
                                    ;; initial contents to `read-from-minibuffer'.
                                    (alien-search/read-from-minibuf/with-initial-contents
                                        regexp
                                      (let ((this-command (quote ,targ-command)))
                                        (call-interactively (quote ,targ-command))))))))))))
                    ;; Should be turned on by `isearch-mode-hook'.
                    (alien-search/ad-disable (quote ,targ-command) 'around (quote ,ad-name-make-transition-to))
                    (alien-search/ad-activate (quote ,targ-command))
                    
                    ;; When `alien-search/isearch' will be turned on,
                    ;; advise another alien-search commands so that
                    ;; we can exit an isearch session and make a
                    ;; transition to them.
                    (defun ,fn-name-turn-on-transition ()
                      (alien-search/ad-enable (quote ,targ-command) 'around (quote ,ad-name-make-transition-to))
                      (alien-search/ad-activate (quote ,targ-command)))
                    (add-hook 'isearch-mode-hook (quote ,fn-name-turn-on-transition))
                    
                    ;; Disable advices of another alien-search commands
                    ;; when `alien-search/isearch' will be turned off.
                    (defun ,fn-name-turn-off-transition ()
                      (alien-search/ad-disable (quote ,targ-command) 'around (quote ,ad-name-make-transition-to))
                      (alien-search/ad-activate (quote ,targ-command)))
                    (add-hook 'isearch-mode-end-hook (quote ,fn-name-turn-off-transition))))))
                      
              ((minibuf-cmd)
               (let ((orig-command                (intern
                                                   (format
                                                    "ad-Orig-%s"
                                                    command)))
                     (ad-name-catch-transition-to (intern
                                                   (format
                                                    "alien-search/transition/%s/catch-transition-to-%s"
                                                    label targ-label)))
                     (ad-name-throw-transition-to (intern
                                                   (format
                                                    "alien-search/transition/%s/throw-transition-to-%s"
                                                    label targ-label)))
                     (ad-name-allow-transition    (intern
                                                   (format
                                                    "alien-search/transition/%s/allow-transition-to-%s"
                                                    label targ-label)))
                     (g-transition-allowed-p      (gensym)))

                 ;; For two purposes below, we wrap each alien-search
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
                 ;;   2. Remember running alien-search command to
                 ;;      prevent duplicated calls while minibuffer
                 ;;      is active.
                 (eval `(progn
                          (defadvice ,command (around alien-search/orig-fn (&rest args))
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
                                      (setq alien-search/transition/.running-cmd this-command)
                                      
                                      (setq ad-return-value
                                            (call-interactively (quote ,orig-command))))
                                  (setq alien-search/transition/.running-cmd nil))
                              ;; Called non-interactively.
                              (setq ad-return-value
                                    (apply (quote ,orig-command) args))))
                          (alien-search/ad-activate (quote ,command))))
                 (nconc
                  retval
                  `(
                    ;; Advice a command to allow making transition to
                    ;; another alien-search command, during it is running.
                    (defadvice ,transition-allowed-in (around
                                                       ,ad-name-allow-transition
                                                       (&rest args))
                      "Allow making transition to another alien-search command
while this function is running."
                      ;; DEBUGGING
                      (alien-search/debug alien-search/transition/debug-advices
                                          "TRANSITION: CALL: %s, %s"
                                          (quote ,transition-allowed-in)
                                          (quote ,ad-name-allow-transition))
                      
                      (let ((,g-transition-allowed-p t))
                        ad-do-it))
                    (alien-search/ad-activate (quote ,transition-allowed-in))
                    
                    ;; Advise to alien-search commands, which reads
                    ;; input from minibuffer, to make transition to
                    ;; another alien-search command when a tag
                    ;; is thrown.
                    (defadvice ,command (around
                                         ,ad-name-catch-transition-to
                                         (&rest args))
                      "Make a transition to another alien-search command."
                      
                      ;; DEBUGGING
                      (alien-search/debug alien-search/transition/debug-advices
                                          "TRANSITION: CALL: %s, %s"
                                          (quote ,command)
                                          (quote ,ad-name-catch-transition-to))
                      
                      ;; Prevent duplicate calls.
                      (when (eq this-command
                                alien-search/transition/.running-cmd)
                        (error "[alien-search] Command attempted to use minibuffer while in minibuffer"))
                      
                      (unwind-protect
                          (progn
                            (alien-search/ad-enable (quote ,targ-command)
                                                    'around
                                                    (quote ,ad-name-throw-transition-to))
                            (alien-search/ad-activate (quote ,targ-command))
                            
                            (alien-search/catch-case var
                                ad-do-it
                              (,ad-name-throw-transition-to
                               ;; DEBUGGING
                               (alien-search/debug alien-search/transition/debug-advices
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
                                          ((alien)
                                           (let ((this-command (quote ,targ-command)))
                                             (call-interactively (quote ,targ-command)))
                                           (with-current-buffer (get-buffer reb-buffer)
                                             (delete-region (point-min) (point-max))
                                             (insert regexp)))
                                          (t
                                           (error "[alien-search] RE-Builder syntax is not `alien'.")))))
                                     ((isearch-cmd)
                                      `((run-with-idle-timer
                                         0 nil
                                         (lambda ()
                                           (add-hook 'isearch-mode-hook
                                                     (alien-search/alambda ()
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
                                           ;; Call another alien-search command with setting
                                           ;; initial contents to `read-from-minibuffer'.
                                           (alien-search/read-from-minibuf/with-initial-contents
                                               regexp
                                             (let ((this-command (quote ,targ-command)))
                                               (call-interactively (quote ,targ-command)))))))))
                                 ;; FIXME: Turn off an annoying message
                                 ;;        "Back to top level.".
                                 (top-level)))))
                        (alien-search/ad-disable (quote ,targ-command) 'around (quote ,ad-name-throw-transition-to))
                        (alien-search/ad-activate (quote ,targ-command))))
                    (alien-search/ad-activate (quote ,command))
                    
                    ;; Advise other alien-search commands to throw a tag
                    ;; so that we can exit current alien-search command and
                    ;; make transition to another one.
                    (defadvice ,targ-command (around ,ad-name-throw-transition-to (&rest args))
                      "Throw a tag so that we can exit current alien-search command and
make a transition to another one.

Current contents of minibuffer will be thrown
as the value of a tag."
                      ;; DEBUGGING
                      (alien-search/debug alien-search/transition/debug-advices
                                          "TRANSITION: CALL: %s, %s"
                                          (quote ,targ-command)
                                          (quote ,ad-name-throw-transition-to))

                      ;; MEMO-1: This advice will be enabled/disabled in
                      ;;         advice of each alien-search command.
                      ;; MEMO-2: When the flag `,g-transition-allowed-p' is
                      ;;         nil, this advice behaves as if it was not there.
                      ;; MEMO-3: `,g-transition-allowed-p' is assigned a different
                      ;;         symbol for each alien-search commands.
                      (cond
                       ((and (boundp (quote ,g-transition-allowed-p))
                             ,g-transition-allowed-p)
                        (let ((contents (alien-search/read-minibuf-contents)))
                          (when contents
                            ;; DEBUGGING
                            (alien-search/debug alien-search/transition/debug-advices
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
;;  (alien-search/transition/setup) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/transition/setup ()
  "Setup transitions among alien-search commands."
  (eval
   '(alien-search/transition/.setup alien-search/transition/command-table)))


;; ----------------------------------------------------------------------------
;;
;;  Main
;;
;; ----------------------------------------------------------------------------

(alien-search/transition/setup)


;;; ===========================================================================
;;;
;;;  Variables and Functions for managing "Alien Types".
;;;
;;; ===========================================================================

(defcustom alien-search/alien-type nil
  "\"Type of the alien regular expression\" that you want to use for alien search."
  :type    'alien-search/alien-type/custom-widget/alien-type-selector
  :group   'alien-search
  :set     '(lambda (sym val)
              (cond
               ((fboundp 'alien-search/alien-type/set)
                (alien-search/alien-type/set val))
               (t
                ;; When this file is being loaded,
                ;; `alien-search/alien-type/set' will be called
                ;; from `Main' section by timer.
                (setq alien-search/alien-type val)))))

(defvar alien-search/alien-type/.type-alst nil
  ;; FIXME: Write document.
  "Private variable.")


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/alien-type/define &key name
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
;;                                       cmd-path-search
;;                                       cmd-path-replace
;;                                       cmd-path-occur
;;                                       cmd-path-quote-meta
;;                                       script-search
;;                                       script-replace
;;                                       script-occur
;;                                       script-quote-meta) => VOID
;; ----------------------------------------------------------------------------
(defun* alien-search/alien-type/define (&key name
                                             tag
                                             input-coding-system
                                             output-coding-system
                                             indicator-case-fold
                                             indicator-no-case-fold
                                             indicator-ext-regexp
                                             indicator-no-ext-regexp
                                             indicator-dot-match
                                             indicator-no-dot-match
                                             indicator-separator
                                             script-search
                                             script-replace
                                             script-occur
                                             script-quote-meta
                                             cmd-path-search
                                             cmd-path-replace
                                             cmd-path-occur
                                             cmd-path-quote-meta)
  ;; FIXME: Write document.
  "Define an Alien Type.

Arguments are:

  NAME:
        Name of an Aline Type. This must be a symbol.

  TAG:
        Name of an Alien Type by human-friendly representation.
        TAG will be used as label string of menu item and custom
        widget. This must be a string.

  INPUT-CODING-SYSTEM:
        See `alien-search/input-coding-system'.

  OUTPUT-CODING-SYSTEM:
        See `alien-search/output-coding-system'.
      
  INDICATOR-CASE-FOLD:
        See `alien-search/search-option-indicator/case-fold-str'.
      
  INDICATOR-NO-CASE-FOLD:
        See `alien-search/search-option-indicator/no-case-fold-str'.
      
  INDICATOR-DOT-MATCH:
        See `alien-search/search-option-indicator/dot-match-str'.
      
  INDICATOR-NO-DOT-MATCH:
        See `alien-search/search-option-indicator/no-dot-match-str'.
      
  INDICATOR-EXT-REGEXP:
        See `alien-search/search-option-indicator/ext-regexp-str'.
      
  INDICATOR-NO-EXT-REGEX:
        See `alien-search/search-option-indicator/no-ext-regexp-str'.
      
  INDICATOR-SEPARATOR:
        See `alien-search/search-option-indicator/separator-str'.
      
  CMD-PATH-SEARCH:
        See `alien-search/search/external-command'.

  CMD-PATH-REPLACE:
        See `alien-search/replace/external-command'.

  CMD-PATH-OCCUR:
        See `alien-search/occur/external-command'.

  CMD-PATH-QUOTE-META:
        See `alien-search/quote-meta/external-command'.

  SCRIPT-SEARCH:
        See `alien-search/search/shell-script'.
      
  SCRIPT-REPLACE:
        See `alien-search/replace/shell-script'.

  SCRIPT-OCCUR:
        See `alien-search/occur/shell-script'.
      
  SCRIPT-QUOTE-META:
        See `alien-search/quote-meta/shell-script'."
  ;; Validation
  ;;
  (or name                    (error "[alien-search] No `:name'!"))
  (or tag                     (error "[alien-search] No `:tag'!"))
  (or input-coding-system     (error "[alien-search] No `:input-coding-system'!"))
  (or output-coding-system    (error "[alien-search] No `:output-coding-system'!"))
  (or indicator-case-fold     (error "[alien-search] No `:indicator-case-fold'!"))
  (or indicator-no-case-fold  (error "[alien-search] No `:indicator-no-case-fold'!"))
  (or indicator-ext-regexp    (error "[alien-search] No `:indicator-ext-regexp'!"))
  (or indicator-no-ext-regexp (error "[alien-search] No `:indicator-no-ext-regexp'!"))
  (or indicator-dot-match     (error "[alien-search] No `:indicator-dot-match'!"))
  (or indicator-no-dot-match  (error "[alien-search] No `:indicator-no-dot-match'!"))
  (or indicator-separator     (error "[alien-search] No `:indicator-separator'!"))

  (or script-search
      cmd-path-search
      (error "[alien-search] No `:cmd-path-search' or `:cmd-path-search'!"))
  (or script-replace
      cmd-path-replace
      (error "[alien-search] No `:script-replace' or `:cmd-path-replace'!"))
  (or script-occur
      cmd-path-occur
      (error "[alien-search] No `:script-occur' or `:cmd-path-occur'!"))
  (or script-quote-meta
      cmd-path-quote-meta
      (error "[alien-search] No `:script-quote-meta' or `:cmd-path-quote-meta'!"))

  (alien-search/alien-type/forget name)
  (push (list name
              :name                     name
              :tag                      tag
              :input-coding-system      input-coding-system
              :output-coding-system     output-coding-system
              :indicator-case-fold      indicator-case-fold
              :indicator-no-case-fold   indicator-no-case-fold
              :indicator-ext-regexp     indicator-ext-regexp
              :indicator-no-ext-regexp  indicator-no-ext-regexp
              :indicator-dot-match      indicator-dot-match
              :indicator-no-dot-match   indicator-no-dot-match
              :indicator-separator      indicator-separator
              :script-search            script-search
              :script-replace           script-replace
              :script-occur             script-occur
              :script-quote-meta        script-quote-meta
              :cmd-path-search          cmd-path-search
              :cmd-path-replace         cmd-path-replace
              :cmd-path-occur           cmd-path-occur
              :cmd-path-quote-meta      cmd-path-quote-meta)
        alien-search/alien-type/.type-alst)

  (alien-search/alien-type/custom-widget/alien-type-selector/update))

;; ----------------------------------------------------------------------------
;;  (alien-search/alien-type/forget NAME) => ALIST
;; ----------------------------------------------------------------------------
(defun alien-search/alien-type/forget (name)
  "Remove an alien type NAME from `alien-search/alien-type/.type-alst'."
  (setq alien-search/alien-type/.type-alst
        (remove-if '(lambda (pair) (eq name (car pair)))
                   alien-search/alien-type/.type-alst)))

;; ----------------------------------------------------------------------------
;;  (alien-search/alien-type/set NAME) => VOID
;; ----------------------------------------------------------------------------
(defvar alien-search/alien-type/.history nil
  "History list for a command `alien-search/alien-type/set'.")

(defun alien-search/alien-type/set (name)
  "Activate alien type NAME."
  (interactive
   (list
    (intern
     (completing-read
      (format "Alien regexp type for alien search (defalut %s): "
              alien-search/alien-type)
      (sort (mapcar (lambda (sym)
                      (format "%s" (car sym)))
                    alien-search/alien-type/.type-alst)
            #'string<)
      nil t nil
      'alien-search/alien-type/.history
      (format "%s" alien-search/alien-type) nil))))
  (let ((kv-lst (or (cdr (assq name alien-search/alien-type/.type-alst))
                    (cond
                     ((null name)
                      nil)
                     (t
                      (error "No such alien definition `%s'." name))))))
    (setq alien-search/input-coding-system                       (cadr (memq :input-coding-system    kv-lst)))
    (setq alien-search/output-coding-system                      (cadr (memq :output-coding-system   kv-lst)))
    (setq alien-search/search-option-indicator/case-fold-str     (cadr (memq :indicator-case-fold    kv-lst)))
    (setq alien-search/search-option-indicator/no-case-fold-str  (cadr (memq :indicator-no-case-fold kv-lst)))
    (setq alien-search/search-option-indicator/ext-regexp-str    (cadr (memq :indicator-ext-regexp   kv-lst)))
    (setq alien-search/search-option-indicator/no-ext-regexp-str (cadr (memq :indicator-no-ext-regexp kv-lst)))
    (setq alien-search/search-option-indicator/dot-match-str     (cadr (memq :indicator-dot-match    kv-lst)))
    (setq alien-search/search-option-indicator/no-dot-match-str  (cadr (memq :indicator-no-dot-match kv-lst)))
    (setq alien-search/search-option-indicator/separator-str     (cadr (memq :indicator-separator    kv-lst)))
    (setq alien-search/search/external-command                   (cadr (memq :cmd-path-search        kv-lst)))
    (setq alien-search/search/shell-script                       (cadr (memq :script-search          kv-lst)))
    (setq alien-search/replace/external-command                  (cadr (memq :cmd-path-replace       kv-lst)))
    (setq alien-search/replace/shell-script                      (cadr (memq :script-replace         kv-lst)))
    (setq alien-search/occur/external-command                    (cadr (memq :cmd-path-occur         kv-lst)))
    (setq alien-search/occur/shell-script                        (cadr (memq :script-occur           kv-lst)))
    (setq alien-search/quote-meta/external-command               (cadr (memq :cmd-path-quote-meta    kv-lst)))
    (setq alien-search/quote-meta/shell-script                   (cadr (memq :script-quote-meta      kv-lst)))

    (setq alien-search/alien-type name)
    (cond
     ((null name)
      (message "[alien-search] Alien type is set to \"None\"."))
     (t
      (message "[alien-search] Alien type is set to \"%s\"" name)))))

;; ----------------------------------------------------------------------------
;;  (alien-search/alien-type/custom-widget/alien-type-selector/update NAME) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/alien-type/custom-widget/alien-type-selector/update ()
  "Update a widget `alien-search/alien-type/custom-widget/alien-type-selector' which
will be used to customize user option `alien-search/alien-type'."
  (define-widget 'alien-search/alien-type/custom-widget/alien-type-selector 'lazy
    "A widget, which will be used to customize user option
`alien-search/alien-type'."
    :offset 4
    :tag    "Type"
    :type   `(choice
              (const :tag "None" nil)
              ,@(mapcar (lambda (pair)
                          (list 'const
                                :tag (cadr (memq :tag (cdr pair)))
                                (car pair)))
                        alien-search/alien-type/.type-alst))))


;; ----------------------------------------------------------------------------
;;
;;  Main
;;
;; ----------------------------------------------------------------------------
(run-with-idle-timer
 0 nil
 '(lambda ()
    (alien-search/alien-type/set alien-search/alien-type)))

;;; alien-search.el ends here
