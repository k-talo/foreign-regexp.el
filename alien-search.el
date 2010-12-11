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
;;   `alien-search/replace/external-program'
;;   `alien-search/occur/external-program'
;;   `alien-search/isearch/external-program'
;;   `alien-search/quote-meta/external-program'
;;
;; and set path to the programs to the variables when you wrote
;; them.
;;
;; See also default external programs written in Ruby, defined as:
;;
;;   `alien-search/replace/default-shell-script'
;;   `alien-search/occur/default-shell-script'
;;   `alien-search/isearch/default-shell-script'
;;   `alien-search/quote-meta/default-shell-script'
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
;;;  Common variable and functions to each `alien-search' operation.
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

(defcustom alien-search/input-coding-system  'utf-8-unix
  "Coding system to be used for decoding files which
contains texts passed from external programs to Emacs."
  :type  'coding-system
  :group 'alien-search)

(defcustom alien-search/output-coding-system 'utf-8-unix
  "Coding system to be used for encoding files which
contains texts passed from Emacs to external programs."
  :type  'coding-system
  :group 'alien-search)

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
;;  (alien-search/with-search-option-indicator-on-minibuf-prompt &rest body)
;;                                                           => RESULT OF BODY
;; ----------------------------------------------------------------------------
(defmacro* alien-search/alambda (parms &body body)
  (declare (indent 1))
  `(labels ((self ,parms ,@body))
     #'self))
(defmacro alien-search/with-search-option-indicator-on-minibuf-prompt (&rest body)
  "Run `read-from-minibuffer' with displaying search option indicator
on prompt string.

Search option indicator will be updated whenever options are changed via
commands `alien-search/toggle-case-fold',
`alien-search/toggle-dot-match' and
`alien-search/toggle-ext-regexp'."
  (let* ((g-orig-read-from-minibuffer-fn (gensym))
         (hook-togglefn-lst '((alien-search/case-fold-will-change-hook
                               alien-search/toggle-case-fold)
                              (alien-search/ext-regexp-will-change-hook
                               alien-search/toggle-ext-regexp)
                              (alien-search/dot-match-will-change-hook
                               alien-search/toggle-dot-match))))
    `(lexical-let ((,g-orig-read-from-minibuffer-fn (symbol-function 'read-from-minibuffer)))
       (flet ((signal-option-changed
               ()
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
                     (throw 'alien-search/util/.option-changed
                            (cons (buffer-substring bol-pt eol-pt)
                                  offset)))))))
         (unwind-protect
             (progn
               (setf (symbol-function 'read-from-minibuffer)
                     (lambda (prompt &optional initial-contents keymap read
                                     hist defalut-value inherit-input-method)
                       ;; Preserve current alien-search/*-will-change-hooks.
                       (let (,@(mapcar (lambda (lst)
                                         (let ((will-hook (car lst)))
                                           `(,will-hook ,will-hook)))
                                       hook-togglefn-lst)
                             (initial-contents initial-contents)
                             hook-fn-alst
                             retval)
                         ;; Do not call this `read-from-minibuffer' recursively.
                         (setf (symbol-function 'read-from-minibuffer)
                               ,g-orig-read-from-minibuffer-fn)

                         (lexical-let ((cur-buf (current-buffer)))
                           ;; Do not toggle search options of *Minibuf-N* while reading
                           ;; regexps, toggle re-options of CURRENT BUFFER instead.
                           ,@(mapcar (lambda (lst)
                                       (let ((will-hook (car lst))
                                             (toggle-fn (cadr lst)))
                                         `(add-hook
                                           (quote ,will-hook)
                                           (alien-search/alambda ()
                                             (with-current-buffer cur-buf
                                               (let ((,will-hook ,will-hook)) ;Preserve
                                                 ;; Do not call this hook recursively.
                                                 (remove-hook (quote ,will-hook)
                                                              #'self)
                                                 (,toggle-fn t)))
                                             (signal-option-changed)))))
                                     hook-togglefn-lst)
                           ;; Whenever search option is changed,
                           ;; restart `read-from-minibuffer' to
                           ;; redisplay prompt.
                           (while (setq initial-contents
                                        (catch 'alien-search/util/.option-changed
                                          (setq retval
                                                (funcall
                                                 ,g-orig-read-from-minibuffer-fn
                                                 ;; Put lighter before ": " of prompt.
                                                 (concat (substring prompt
                                                                    0
                                                                    (string-match ": $" prompt))
                                                         (alien-search/search-option-indicator/make-indicator)
                                                         ": ")
                                                 initial-contents
                                                 keymap
                                                 read
                                                 hist
                                                 defalut-value
                                                 inherit-input-method))
                                          nil)))
                           retval))))
               ,@body)
           (setf (symbol-function 'read-from-minibuffer)
                 ,g-orig-read-from-minibuffer-fn))))))

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
  (setq case-fold-search
        (not case-fold-search))
  (when isearch-mode
    (setq isearch-case-fold-search case-fold-search))
  (run-hooks 'alien-search/case-fold-changed-hook)
  
  (when (not no-message)
    (minibuffer-message "[alien-search] case %ssensitive"
                        (if case-fold-search "in" ""))))

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
    (minibuffer-message "[alien-search] . %s newline"
                        (if alien-search/dot-match-a-newline-p "matches" "does not match"))))

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
    (minibuffer-message "[alien-search] %sextended regex"
                        (if alien-search/use-extended-regexp-p "" "no "))))

;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/run-external-program prog-path default-shell-script body
;;                                     pattern replacement &rest other-args)
;;                                                                   => RESULT
;; ----------------------------------------------------------------------------
(defun alien-search/run-external-program (prog-path default-shell-script
                                                    body
                                                    pattern
                                                    replacement
                                                    &rest other-args)
  "Run external program to execute operations regarding to search.

NOTES FOR DEVELOPERS: Variables in REPLACEMENT should be interpolated
                      on each match by external program."
  (let* ((base               (expand-file-name
                              alien-search/tmp-file-prefix
                              alien-search/tmp-dir))
         (fn-out-body        (make-temp-name base))
         (fn-out-pattern     (make-temp-name base))
         (fn-out-replacement (make-temp-name base))
         (fn-in-result       (make-temp-name base))
         (fn-program         (make-temp-name base))
         (prog-basename      (if prog-path
                                 (file-name-nondirectory prog-path)
                               "default-shell-script"))
         (proc-output-buf    (get-buffer-create " *alien-search*"))
         (cur-buf            (current-buffer))
         (orig-file-modes    (default-file-modes))
         (coding-sys-out     alien-search/output-coding-system)
         (coding-sys-in      alien-search/input-coding-system)
         result)
    (unwind-protect
        (progn

          ;; Save information, which have to be passed to 
          ;; external program, to temporally files.
          (unwind-protect 
              (progn
                (set-default-file-modes #o0600)
                
                (when body
                  (with-temp-file fn-out-body
                    (set-buffer-file-coding-system coding-sys-out)
                    (insert body)))
                (with-temp-file fn-out-pattern
                  (set-buffer-file-coding-system coding-sys-out)
                  (insert pattern))
                (when replacement
                  (with-temp-file fn-out-replacement
                    (set-buffer-file-coding-system coding-sys-out)
                    (insert replacement))))
            (set-default-file-modes orig-file-modes))

          ;; Save default-shell-script to file when required.
          (when (not prog-path)
            (with-temp-file fn-program
              (set-buffer-file-coding-system coding-sys-out)
              (insert default-shell-script))
            (set-file-modes fn-program #o0700)
            (setq prog-path fn-program))
          
          ;;(message "[alien-search] Running...")
          
          ;; Do search by external program.
          (let ((status (apply #'call-process
                               `(,prog-path
                                 nil ,(buffer-name proc-output-buf) nil
                                 ,@(if body (list fn-out-body) nil)
                                 ,fn-in-result
                                 ,fn-out-pattern
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
                       prog-basename
                       (buffer-substring (point-min) (point-max)))))
          
          ;; Parse result from external program.
          (let ((coding-system-for-read coding-sys-in))
            ;; Loaded data will be stored to the local variable `result'.
            (load (expand-file-name fn-in-result) nil t t))
          result)
      
      ;; Cleanup.
      (and (file-exists-p fn-out-pattern    ) (delete-file fn-out-pattern    ))
      (and (file-exists-p fn-out-replacement) (delete-file fn-out-replacement))
      (and (file-exists-p fn-out-body       ) (delete-file fn-out-body       ))
      (and (file-exists-p fn-in-result      ) (delete-file fn-in-result      ))
      (and (file-exists-p fn-program        ) (delete-file fn-program        ))
      (kill-buffer proc-output-buf))))


;;; ===========================================================================
;;;
;;;  Search option indicator
;;;
;;; ===========================================================================

(defcustom alien-search/search-option-indicator/case-fold-str ""
  "A string displayed when the search option
`case-fold-search' is on."
  :type 'string
  :group 'alien-search)

(defcustom alien-search/search-option-indicator/no-case-fold-str "Case"
  "A string displayed when the search option
`case-fold-search' is off."
  :type 'string
  :group 'alien-search)

(defcustom alien-search/search-option-indicator/dot-match-str ".=~\\n"
  "A string displayed when the search option
`alien-search/dot-match-a-newline-p' is on."
  :type 'string
  :group 'alien-search)

(defcustom alien-search/search-option-indicator/no-dot-match-str ""
  "A string displayed when the search option
`alien-search/dot-match-a-newline-p' is off."
  :type 'string
  :group 'alien-search)

(defcustom alien-search/search-option-indicator/ext-regexp-str "Ext"
  "A string displayed when the search option
`alien-search/use-extended-regexp-p' is on."
  :type 'string
  :group 'alien-search)

(defcustom alien-search/search-option-indicator/no-ext-regex-str ""
  "A string displayed when the search option
`alien-search/use-extended-regexp-p' is off."
  :type 'string
  :group 'alien-search)

(defcustom alien-search/search-option-indicator/separator-str " "
  "A string displayed between search option strings."
  :type 'string
  :group 'alien-search)


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
                  alien-search/search-option-indicator/no-ext-regex-str))))


;;; ===========================================================================
;;;
;;;  Menus
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
                       :help "Search forward for an alien regular expression"]
                      "separator-repeat-search")
  (easy-menu-add-item menu-bar-search-menu
                      nil
                      ["Alien Regexp Backward..." alien-search/non-incremental/search-backward
                       :help "Search backward for an alien regular expression"]
                      "separator-repeat-search")
                    
  (easy-menu-add-item menu-bar-i-search-menu
                      nil
                      ["Forward Alien Regexp..." alien-search/isearch-forward
                       :help "Search forward for an alien regular expression as you type it"]
                      nil)

  (easy-menu-add-item menu-bar-i-search-menu
                      nil
                      ["Backward Alien Regexp..." alien-search/isearch-backward
                       :help "Search backwards for an alien regular expression as you type it"]
                      "separator-repeat-search")

  (easy-menu-add-item menu-bar-replace-menu
                      nil
                      ["Replace Alien Regexp..." alien-search/query-replace
                       :help "Replace alien regular expression interactively, ask about each occurrence"]
                      "separator-replace-tags")

  (easy-menu-add-item menu-bar-edit-menu
                      nil
                      '("Alien Search Options"
                        ["Case Insensitive" alien-search/toggle-case-fold
                         :style radio :selected (not case-fold-search)]
                        ["Make . Match a Newline" alien-search/toggle-dot-match
                         :style radio :selected alien-search/dot-match-a-newline-p]
                        ["Use Extended Regular Expression" alien-search/toggle-ext-regexp
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
                                        alien-search/history))
                :help ,(purecopy "Repeat last search forward")))
  (define-key menu-bar-search-menu [repeat-search-back]
    `(menu-item ,(purecopy "Repeat Backwards") nonincremental-repeat-search-backward
                :enable (or (and (eq menu-bar-last-search-type 'string)
                                        search-ring)
                                   (and (eq menu-bar-last-search-type 'regexp)
                                        regexp-search-ring)
                                   ;; For alien regexp search.
                                   (and (eq menu-bar-last-search-type 'alien)
                                        alien-search/history))
                :help ,(purecopy "Repeat last search backwards"))))


;; ----------------------------------------------------------------------------
;;
;;  Main
;;
;; ----------------------------------------------------------------------------

(when alien-search/menu/use-menu-p
  (alien-search/menu/install))


;;; ===========================================================================
;;;
;;;  `query-replace' with a help from external program.
;;;
;;; ===========================================================================

(defcustom alien-search/replace/external-program nil
  "Path of an external program to use to execute actual search operation.

Seven arguments describe below will be passed to the program.

 1st: Path of a file which contains the text to be searched.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'.

 2nd: Path of a file to which the program should write the result
      of current search operation.

      The external program have to output a form like:

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

 3rd: Path of a file in which the pattern we want to search is written.
      The program have a responsibility to search this pattern
      from the file specified by 1st argument, then write start and
      end positions of each match to the file specified by 2nd argument.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'.

 4th: Path of a file in which the replacement expression is written.
      The program have a responsibility to interpolate variables
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
      the current search pattern(:see 3rd arg) should be
      interpreted as extended regular expression."
  :type 'string
  :group 'alien-search)

(defcustom alien-search/replace/default-shell-script
  "#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort \"Ruby version is too old (1.9 or later is required).\" if RUBY_VERSION < \"1.9\"

def escape_str_for_eval! (str)
  str.gsub!(/\"/ ){'\\\\\"'}
end

def escape_ruby_str_for_emacs! (str)
  str.gsub!(/\\\\/) {'\\\\\\\\'}
  str.gsub!(/\"/ ) {'\\\\\"'}
end

def main ()
  fn_in, fn_out, fn_pat, fn_rpl, dot_p, case_p, ext_p = ARGV
  
  str_in  = open(fn_in,  'r:UTF-8') {|f| f.read}
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}
  str_rpl = open(fn_rpl, 'r:UTF-8') {|f| f.read}
  
  pat = Regexp.new(str_pat, ((dot_p.empty?  ? 0 : Regexp::MULTILINE)  |
                             (case_p.empty? ? Regexp::IGNORECASE : 0) |
                             (ext_p.empty?  ? 0 : Regexp::EXTENDED)))
  
  escape_str_for_eval!(str_rpl)
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  print \"(setq result '(\"
  
  str_in.scan( pat ) do |m|
    replacement = eval '\"' + str_rpl + '\"'
    escape_ruby_str_for_emacs!(replacement)
    
    print '('
    print Regexp.last_match.begin(0), ' '
    print Regexp.last_match.end(0),   ' '
    print '\"', replacement, '\"'
    print ')'
  end
  
  print \"))\\n\"
  print \";;; EOF\\n\"
  
  exit 0
  
rescue RegexpError
  $stderr.print $!.message
  exit 1
end

main

# EOF
"
  "A shell script which will be run as 
`alien-search/replace/external-program'
when it has nil value."
  :type  'string
  :group 'alien-search)


(defvar alien-search/replace/defaults nil
  "Default values of FROM-STRING and TO-STRING for `alien-search/query-replace'.

See also `query-replace-defaults'.")


;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/query-replace pattern replacement
;;                              &optional delimited start end) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/query-replace (pattern replacement &optional delimited start end)
  "Do `query-replace' with a help from external program.

See `isearch-forward-regexp' and `isearch-backward-regexp' for
more information."
  (interactive
   (let ((common
          (let ((query-replace-from-history-variable 'alien-search/history)
                (query-replace-to-history-variable   'alien-search/history)
                (query-replace-defaults              alien-search/replace/defaults))
            (prog1 (alien-search/with-search-option-indicator-on-minibuf-prompt
                    (query-replace-read-args
                     (concat "Query replace alien regexp"
                             (if (and transient-mark-mode mark-active) " in region" ""))
                     t))
              (setq alien-search/replace/defaults query-replace-defaults)))))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           ;; These are done separately here
           ;; so that command-history will record these expressions
           ;; rather than the values they had this time.
           (if (and transient-mark-mode mark-active)
               (region-beginning))
           (if (and transient-mark-mode mark-active)
               (region-end)))))
  (alien-search/replace/perform-replace
   pattern replacement t nil nil nil nil start end))


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/replace/search-by-external-program pattern replacement)
;;                                                                     => LIST
;; ----------------------------------------------------------------------------
(defun alien-search/replace/search-by-external-program (pattern replacement min max)
  "Scan current buffer with external program to detect matching
texts by PATTERN.

Overlays will be made on each matched text, and they will be
saved to the variable `alien-search/replace/ovs-on-match/data'.

Variables in REPLACEMENT will be interpolated
on each match, and will be saved to the property
alien-search/replace/replacement of each overlay.

Returns position of the neighborhood overlay of a pointer in
the list `alien-search/replace/ovs-on-match/data'."
  (let* ((offset (point-min))
         (result (alien-search/run-external-program
                  alien-search/replace/external-program
                  alien-search/replace/default-shell-script
                  (buffer-substring (point-min) (point-max))
                  pattern
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
  "Subroutine of `alien-search/replace/search-by-external-program'."
  (alien-search/replace/ovs-on-match/dispose)
  ;; RESULT has structure like:
  ;;   ((MATCH_START MATCH_END "REPLACEMENT")
  ;;    ...)
  ;;
  ;; NOTE: Special variables in "REPLACEMENT"
  ;;       should be expanded by external program.
  (save-excursion
    (let ((data    nil)
          (cur-buf (current-buffer)))
      ;;(message "[alien-search] Parsing search results from external program...")
      (dolist (lst result)
        (let* ((beg         (+ (nth 0 lst) offset))
               (end         (+ (nth 1 lst) offset))
               (replacement (nth 2 lst)))
          (when (and (not (and min (< beg min)))
                     (not (and max (< max end))))
            (alien-search/replace/ovs-on-match/add beg end cur-buf replacement))))
      ;;(message "[alien-search] Parsing search results from external program...done")
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
  ;;      should be updated by `alien-search/replace/search-by-external-program'
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

    ;; Do search by external program and detect index of
    ;; neighborhood overlay of a pointer.
    (setq idx (alien-search/replace/search-by-external-program
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
                           (setq idx (alien-search/replace/search-by-external-program
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
                           (setq idx (alien-search/replace/search-by-external-program
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
                           (setq idx (alien-search/replace/search-by-external-program
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
;;;  Overlay on current match.
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
;;;  Overlays on all of matches by external program.
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
  "Delete overlays on matched strings created by external program
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
;;;  `occur' with a help from external program.
;;;
;;; ===========================================================================

;; XXX: `A dot matches newline flag' should be removed?
;;      (Is that flag nonsense thing? ...because `occur' is line
;;       oriented matching operation...)
(defcustom alien-search/occur/external-program nil
  "Path of an external program to use to execute actual search
operation.

Six arguments describe below will be passed to the program.

 1st: Path of a file which contains the text to be searched.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'.

 2nd: Path of a file to which the program should write the result
      of current search operation.

      The external program have to output a form like:

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

 3rd: Path of a file in which the pattern we want to search is written.
      The program have a responsibility to search this pattern
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
      the current search pattern(:see 3rd arg) should be
      interpreted as extended regular expression."
  :type  'string
  :group 'alien-search)

(defcustom alien-search/occur/default-shell-script
  "#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort \"Ruby version is too old (1.9 or later is required).\" if RUBY_VERSION < \"1.9\"

def main ()
  fn_in, fn_out, fn_pat, dot_p, case_p, ext_p = ARGV
  
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}
  offset = 0
  
  pat = Regexp.new(str_pat, ((dot_p.empty?  ? 0 : Regexp::MULTILINE)  |
                             (case_p.empty? ? Regexp::IGNORECASE : 0) |
                             (ext_p.empty?  ? 0 : Regexp::EXTENDED)))
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  print \"(setq result '(\"
  
  open(fn_in, 'r:UTF-8') do |file_in|
    while line = file_in.gets do
      matched = 0
      len = line.length
      line.chomp!
      
      line.scan( pat ) do
        print '(' if matched == 0
        print '('
        print offset + Regexp.last_match.begin(0), ' '
        print offset + Regexp.last_match.end(0)
        print ')'
        matched += 1
      end
      print ')' if matched != 0
      
      offset += len
    end
  end
  
  print \"))\\n\"
  print \";;; EOF\\n\"
  
  exit 0
  
rescue RegexpError
  $stderr.print $!.message
  exit 1
end

main

# EOF
"
  "A shell script which will be run as
`alien-search/occur/external-program'
when it has nil value."
  :type  'string
  :group 'alien-search)


;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/occur regexp &optional nlines) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/occur (regexp &optional nlines)
  (interactive (let ((regexp-history alien-search/history))
                 (prog1
                     (alien-search/with-search-option-indicator-on-minibuf-prompt
                      (alien-search/occur-read-primary-args))
                   (setq alien-search/history regexp-history))))
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
              (setq result (alien-search/run-external-program
                            alien-search/occur/external-program
                            alien-search/occur/default-shell-script
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
;;;  `isearch' with a help from external program.
;;;
;;; ===========================================================================

(defcustom alien-search/isearch/external-program nil
  "Path of an external program to use to execute actual search operation.

Six arguments describe below will be passed to the program.

 1st: Path of a file which contains the text to be searched.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'.

 2nd: Path of a file to which the program should write the result
      of current search operation.

      The external program have to output a form like:

        (setq result
              '((1st-MATCH-START 1st-MATCH-END)
                (2nd-MATCH-START 2nd-MATCH-END)
                 ...))

      to this file.

      Note that each start and end position in the form should be
      an offset from beginning of the text which has been searched.
      (This means each number should be started from 0, not from 1)

      The text in this file must be encoded in the value of
      `alien-search/input-coding-system'.

 3rd: Path of a file in which the pattern we want to search is written.
      The program have a responsibility to search this pattern
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
      the current search pattern(:see 3rd arg) should be
      interpreted as extended regular expression."
  :type  'string
  :group 'alien-search)

(defcustom alien-search/isearch/default-shell-script
  "#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort \"Ruby version is too old (1.9 or later is required).\" if RUBY_VERSION < \"1.9\"

def main ()
  fn_in, fn_out, fn_pat, dot_p, case_p, ext_p = ARGV
  
  str_in  = open(fn_in,  'r:UTF-8') {|f| f.read}
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}

  pat = Regexp.new(str_pat, ((dot_p.empty?  ? 0 : Regexp::MULTILINE)  |
                             (case_p.empty? ? Regexp::IGNORECASE : 0) |
                             (ext_p.empty?  ? 0 : Regexp::EXTENDED)))
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  print \"(setq result '(\"
  
  str_in.scan( pat ) do
    print '('
    print Regexp.last_match.begin(0), ' '
    print Regexp.last_match.end(0)
    print ')'
  end
  
  print \"))\\n\"
  print \";;; EOF\\n\"
  
  exit 0

rescue RegexpError
  $stderr.print $!.message
  exit 1
end

main

# EOF
"
  "A shell script which will be run as
`alien-search/isearch/external-program'
when it has nil value."
  :type  'string
  :group 'alien-search)


(defvar alien-search/isearch/.cached-data nil
  "Private variable.")
(defvar alien-search/isearch/.last-regexp nil
  "Private variable.")


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
  "Do isearch with a help from external program.

See `isearch-forward-regexp' and `isearch-backward-regexp' for
more information."
  (interactive "P\np")
  (setq alien-search/isearch/.cached-data nil)
  (setq alien-search/isearch/.last-regexp nil)
  
  ;; Setup `isearch-search-fun-function'.
  (when (not (boundp 'alien-search/isearch/orig-isearch-search-fun-function))
    (setq alien-search/isearch/orig-isearch-search-fun-function
          isearch-search-fun-function))
  (setq isearch-search-fun-function #'alien-search/isearch/isearch-search-fun-function)
  (add-hook 'isearch-mode-end-hook
            'alien-search/isearch/.isearch-mode-end-hook-fn)
  
  ;; Just for prompt message.
  (ad-enable-advice 'isearch-message-prefix 'after 'alien-search/isearch/modify-prompt)
  (ad-activate 'isearch-message-prefix)
  
  (isearch-mode t (null not-regexp) nil (not no-recursive-edit)))

(defun alien-search/isearch-backward (&optional not-regexp no-recursive-edit)
  "Do isearch with a help from external program.

See `isearch-forward-regexp' and `isearch-backward-regexp' for
more information."
  (interactive "P\np")
  (setq alien-search/isearch/.cached-data nil)
  (setq alien-search/isearch/.last-regexp nil)
  
  ;; Setup `isearch-search-fun-function'.
  (when (not (boundp 'alien-search/isearch/orig-isearch-search-fun-function))
    (setq alien-search/isearch/orig-isearch-search-fun-function
          isearch-search-fun-function))
  (setq isearch-search-fun-function #'alien-search/isearch/isearch-search-fun-function)
  (add-hook 'isearch-mode-end-hook
            'alien-search/isearch/.isearch-mode-end-hook-fn)
  
  ;; Just for prompt message.
  (ad-enable-advice 'isearch-message-prefix 'after 'alien-search/isearch/modify-prompt)
  (ad-activate 'isearch-message-prefix)
  
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
;;  (alien-search/isearch/search-option-changed-hook-fn) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/isearch/search-option-changed-hook-fn ()
  "Update display when search option is changed."
  (when isearch-mode
    (setq isearch-success t isearch-adjusted t)
    (setq alien-search/isearch/.cached-data nil)
    
    ;; Force run `isearch-lazy-highlight-new-loop'.
    (setq isearch-lazy-highlight-last-string nil)
    (isearch-update)
    
    ;; Suppress messages.
    (setq no-message t)))

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
    (ad-disable-advice 'isearch-message-prefix 'after 'alien-search/isearch/modify-prompt)
    (ad-activate 'isearch-message-prefix)
    
    (remove-hook 'isearch-mode-end-hook
                 'alien-search/isearch/.isearch-mode-end-hook-fn)))

;; ----------------------------------------------------------------------------
;;  (alien-search/isearch/isearch-search-fun-function) => FUNCTION
;; ----------------------------------------------------------------------------
(defun alien-search/isearch/isearch-search-fun-function ()
  "The value used as value of `isearch-search-fun' while
isearch by alien-search is going on.

This function returns the search function
`alien-search/isearch/search-fun' for isearch to use."
  #'alien-search/isearch/search-fun)

;; ----------------------------------------------------------------------------
;;  (alien-search/isearch/search-fun regexp &optional bound noerror count)
;;                                                                    => POINT
;; ----------------------------------------------------------------------------
(defun alien-search/isearch/search-fun (regexp &optional bound noerror count)
  "Search for the first occurrence of REGEXP in alien manner.
If found, move point to the end of the occurrence,
update the match data, and return point.

This function will be used as alternate function of `re-search-forward'
and `re-search-backward' while isearch by alien-search is on."
  (when (or (not (equal alien-search/isearch/.last-regexp
                        regexp))
            (not alien-search/isearch/.cached-data))
    (condition-case c
        (let ((pt-min (point-min))
              (pt-max (point-max)))
          (setq alien-search/isearch/.cached-data
                (alien-search/run-external-program
                 alien-search/isearch/external-program
                 alien-search/isearch/default-shell-script
                 (buffer-substring pt-min pt-max)
                 regexp
                 nil
                 (if alien-search/dot-match-a-newline-p "DOT" "")
                 (if isearch-case-fold-search "" "CASE")
                 (if alien-search/use-extended-regexp-p "EXT" "")))
          (dolist (be-lst alien-search/isearch/.cached-data)
            (setf (nth 0 be-lst)
                  (+ pt-min (nth 0 be-lst))) ;; [Count + Offset => Count]
            (setf (nth 1 be-lst)
                  (+ pt-min (nth 1 be-lst))))) ;; [Count + Offset => Count]
      (error
       (signal 'invalid-regexp
               (cdr c))))
    (setq alien-search/isearch/.last-regexp
          regexp))
  (let ((forward-p isearch-forward)
        (pt (point)))
    (if forward-p
        ;; Search forward
        (let* ((data alien-search/isearch/.cached-data)
               (be-lst (car (member-if
                             #'(lambda (be-lst)
                                 (<= pt (nth 0 be-lst)))
                             data)))
               (beg (and be-lst (nth 0 be-lst)))
               (end (and be-lst (nth 1 be-lst))))
          (when (and be-lst
                     (if bound
                         (<= end bound)
                       t))
            (set-match-data (list beg end))
            (goto-char end)
            end))
      ;; Search backward
      (let* ((data (reverse alien-search/isearch/.cached-data))
             (be-lst (car (member-if
                           #'(lambda (be-lst)
                               (<= (nth 1 be-lst) pt))
                           data)))
             (beg (and be-lst (nth 0 be-lst)))
             (end (and be-lst (nth 1 be-lst))))
        (when (and be-lst
                   (if bound
                       (<= bound beg)
                     t))
          (set-match-data (list beg end))
          (goto-char beg)
          beg)))))

;; ----------------------------------------------------------------------------
;;
;;  Main
;;
;; ----------------------------------------------------------------------------

(add-hook 'isearch-mode-hook
          'alien-search/isearch/setup-search-option-changed-hook)


;;; ===========================================================================
;;;
;;;  quote meta characters by external program.
;;;
;;; ===========================================================================

(defcustom alien-search/quote-meta/external-program nil
  "Path of an external program to use to execute actual
quote-meta operation.

Two arguments describe below will be passed to the program.

 1st: Path of a file to which the program should write the result
      of quote-meta operation.

      The external program have to output a form like:

        (setq result \"quoted string\")

      to this file.

      The text in this file must be encoded in the value of
      `alien-search/input-coding-system'.

 2nd: Path of a file in which the pattern, we want to quote meta
      characters in it, is written.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'."
  :type 'string
  :group 'alien-search)

(defcustom alien-search/quote-meta/default-shell-script
  "#!/usr/bin/env ruby
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
"
  "A shell script which will be run as
`alien-search/quote-meta/external-program'
when it has nil value."
  :type 'string
  :group 'alien-search)


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/quote-meta pattern) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/quote-meta (pattern)
  "Quote meta characters in a PATTERN in manner of external program."
  (interactive)
  (alien-search/run-external-program
   alien-search/quote-meta/external-program
   alien-search/quote-meta/default-shell-script
   nil ;; Don't care about text in current buffer.
   pattern
   nil))


;;; ===========================================================================
;;;
;;;  `re-builder' with a help from external program.
;;;
;;; ===========================================================================
(require 're-builder)

(defcustom alien-search/re-builder/external-program nil
  "Path of an external program to use to execute actual search operation.

Seven arguments describe below will be passed to the program.

 1st: Path of a file which contains the text to be searched.

      The text in this file is encoded in the value of
      `alien-search/output-coding-system'.

 2nd: Path of a file to which the program should write the result
      of current search operation.

      The external program have to output a form like:

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

 3rd: Path of a file in which the pattern we want to search is written.
      The program have a responsibility to search this pattern
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
      the current search pattern(:see 3rd arg) should be
      interpreted as extended regular expression.

 7th: Positive integer when we want limit the matches, or empty
      string when we don't want limit the matches."
  :type  'string
  :group 'alien-search)

(defcustom alien-search/re-builder/default-shell-script
  "#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort \"Ruby version is too old (1.9 or later is required).\" if RUBY_VERSION < \"1.9\"

def main ()
  fn_in, fn_out, fn_pat, dot_p, case_p, ext_p, limit = ARGV
  
  str_in  = open(fn_in,  'r:UTF-8') {|f| f.read}
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}
  
  pat = Regexp.new(str_pat, ((dot_p.empty?  ? 0 : Regexp::MULTILINE)  |
                             (case_p.empty? ? Regexp::IGNORECASE : 0) |
                             (ext_p.empty?  ? 0 : Regexp::EXTENDED)))
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  print \"(setq result '(\"
  
  limit = (Integer limit rescue nil)
  count = 0
  
  str_in.scan( pat ) do
    break unless (!limit || ((count += 1) <= limit))
    
    print '('
    Regexp.last_match.length.times {|i|
      print Regexp.last_match.begin(i), ' '
      print Regexp.last_match.end(i),   ' '
    }
    print ')'
  end
  
  print \"))\\n\"
  print \";;; EOF\\n\"
  
  exit 0

rescue RegexpError
  $stderr.print $!.message
  exit 1
end

main

# EOF
"
  "A shell script which will be run as
`alien-search/re-builder/external-program'
when it has nil value."
  :type  'string
  :group 'alien-search)

(defvar alien-search/re-builder/.cached-data nil
  "Private variable.")
(defvar alien-search/re-builder/.last-regexp nil
  "Private variable.")


;; ----------------------------------------------------------------------------
;;
;;  Macros
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/with-overriding-re-search-fn ((&keys re-search-forward-fn
;;                                                     re-search-backward-fn
;;                                                     when-regexp-eq)
;;                                              &rest body) => RESULT FROM BODY
;; ----------------------------------------------------------------------------
(defmacro alien-search/with-overriding-re-search-fn (args &rest body)
  "Run BODY with alternating `re-search-forward' and
`re-search-backward' with functions RE-SEARCH-FORWARD-FN
and RE-SEARCH-BACKWARD-FN when the REGEXP, the first
argument of `re-search-forward' and `re-search-backward',
is `eq' to the string WHEN-REGEXP-EQ."
  (declare (indent defun))
  (let ((g-orig-re-search-forward-fn  (gensym))
        (g-orig-re-search-backward-fn (gensym))
        (g-re-search-forward-fn       (gensym))
        (g-re-search-backward-fn      (gensym))
        (g-when-regexp-eq             (gensym))
        (g-args                       (gensym)))
    `(let ((,g-orig-re-search-forward-fn  (symbol-function 're-search-forward))
           (,g-orig-re-search-backward-fn (symbol-function 're-search-backward))
           (,g-re-search-forward-fn  ,(or (cadr (memq :re-search-forward-fn args))
                                          (error "No `:re-search-forward-fn'!")))
           (,g-re-search-backward-fn ,(or (cadr (memq :re-search-backward-fn args))
                                          (error "No `:re-search-backward-fn'!")))
           (,g-when-regexp-eq        ,(or (cadr (memq :when-regexp-eq args))
                                          (error "No `:when-regexp-eq'!"))))
       (unwind-protect
           (progn
             (setf (symbol-function 're-search-forward)
                   '(lambda (&rest ,g-args)
                      (cond
                       ((eq (car ,g-args)
                            ,g-when-regexp-eq)
                        (apply ,g-re-search-forward-fn ,g-args))
                       (t
                        (apply ,g-orig-re-search-forward-fn ,g-args)))))
             (setf (symbol-function 're-search-backward)
                   '(lambda (&rest ,g-args)
                      (cond
                       ((eq (car ,g-args)
                            ,g-when-regexp-eq)
                        (apply ,g-re-search-backward-fn ,g-args))
                       (t
                        (apply ,g-orig-re-search-backward-fn ,g-args)))))
             ,@body)
         (setf (symbol-function 're-search-forward)  ,g-orig-re-search-forward-fn)
         (setf (symbol-function 're-search-backward) ,g-orig-re-search-backward-fn)))))


;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/toggle-case-fold-on-target-buffer
;;                                               &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/toggle-case-fold-on-target-buffer (&optional no-message)
  "Toggle `case-fold-search' on `reb-target-buffer'."
  (interactive)
  (cond
   (reb-target-buffer
    (with-current-buffer reb-target-buffer
      (alien-search/toggle-case-fold no-message)))
   (t
    (error "[alien-search] No `reb-target-buffer'."))))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/toggle-dot-match-on-target-buffer
;;                                               &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/toggle-dot-match-on-target-buffer (&optional no-message)
  "Toggle `alien-search/dot-match-a-newline-p' on `reb-target-buffer'."
  (interactive)
  (cond
   (reb-target-buffer
    (with-current-buffer reb-target-buffer
      (alien-search/toggle-dot-match no-message)))
   (t
    (error "[alien-search] No `reb-target-buffer'."))))

;; ----------------------------------------------------------------------------
;;  (alien-search/re-builder/toggle-ext-regexp-on-target-buffer
;;                                               &optional no-message) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/toggle-ext-regexp-on-target-buffer (&optional no-message)
  "Toggle `alien-search/use-extended-regexp-p' on `reb-target-buffer'."
  (interactive)
  (cond
   (reb-target-buffer
    (with-current-buffer reb-target-buffer
      (alien-search/toggle-ext-regexp no-message)))
   (t
    (error "[alien-search] No `reb-target-buffer'."))))

;; ----------------------------------------------------------------------------
;;
;;  Advices
;;
;; ----------------------------------------------------------------------------

(defadvice re-builder (before alien-search/re-builder/re-builder ())
  (setq alien-search/re-builder/.cached-data nil))
(ad-activate 're-builder)

(defadvice reb-next-match (around alien-search/re-builder/ext-match ())
  (case reb-re-syntax
    ((alien)
     (alien-search/with-overriding-re-search-fn (:re-search-forward-fn
                                                 'alien-search/re-builder/search-fun
                                                 :re-search-backward-fn
                                                 'alien-search/re-builder/search-fun
                                                 :when-regexp-eq
                                                 (with-current-buffer reb-target-buffer
                                                   reb-regexp))
       (let ((alien-search/re-builder/forward-p t))
         ad-do-it)))
    (t
     ad-do-it)))
(ad-activate 'reb-next-match)

(defadvice reb-prev-match (around alien-search/re-builder/prev-match ())
  (case reb-re-syntax
    ((alien)
     (alien-search/with-overriding-re-search-fn (:re-search-forward-fn
                                                 'alien-search/re-builder/search-fun
                                                 :re-search-backward-fn
                                                 'alien-search/re-builder/search-fun
                                                 :when-regexp-eq
                                                 (with-current-buffer reb-target-buffer
                                                   reb-regexp))
       (let ((alien-search/re-builder/forward-p nil))
         ad-do-it)))
    (t
     ad-do-it)))
(ad-activate 'reb-prev-match)

(defadvice reb-copy (around alien-search/re-builder/copy ())
  (case reb-re-syntax
   ((alien)
    (kill-new (buffer-substring-no-properties (point-min) (point-max))))
   (t ad-do-it)))
(ad-activate 'reb-copy)

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
(ad-activate 'reb-change-syntax)

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
(ad-activate 'reb-update-modestring)

(defadvice reb-read-regexp (around alien-search/re-builder/read-regexp ())
  (case reb-re-syntax
   ((alien)
    (setq ad-return-value
          (buffer-substring-no-properties (point-min) (point-max))))
   (t ad-do-it)))
(ad-activate 'reb-read-regexp)

(defadvice reb-insert-regexp (around alien-search/re-builder/insert-regexp ())
  (case reb-re-syntax
   ((alien)
    (when reb-regexp
      (insert reb-regexp)))
   (t ad-do-it)))
(ad-activate 'reb-insert-regexp)

(defadvice reb-count-subexps (around alien-search/re-builder/count-subexps (re))
  (case reb-re-syntax
   ((alien)
    (let ((retval 0))
      ;; Update `alien-search/re-builder/.cached-data'.
      (alien-search/re-builder/search-fun (reb-target-binding reb-regexp) (point-max))
      
      (dolist (be-lst alien-search/re-builder/.cached-data)
        (setq retval (max retval (1- (/ (length be-lst) 2)))))
      (setq ad-return-value retval)))
   (t ad-do-it)))
(ad-activate 'reb-count-subexps)

(defadvice reb-update-overlays (around alien-search/re-builder/update-overlays (&optional subexp))
  (case reb-re-syntax
    ((alien)
     (setq alien-search/re-builder/.cached-data nil)
     (alien-search/with-overriding-re-search-fn (:re-search-forward-fn
                                                 'alien-search/re-builder/search-fun
                                                 :re-search-backward-fn
                                                 'alien-search/re-builder/search-fun
                                                 :when-regexp-eq
                                                 (with-current-buffer reb-target-buffer
                                                   reb-regexp))
       ad-do-it))
    (t
     ad-do-it)))
(ad-activate 'reb-update-overlays)

       
;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

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
;;  (alien-search/re-builder/search-fun regexp &optional bound noerror count)
;;                                                                    => POINT
;; ----------------------------------------------------------------------------
(defun alien-search/re-builder/search-fun (regexp &optional bound noerror count)
  "Search for the first occurrence of REGEXP in alien manner.
If found, move point to the end of the occurrence,
update the match data, and return point.

This function will be used as alternate function of `re-search-forward'
and `re-search-backward' by `re-builder'."
  (when (or (not (equal alien-search/re-builder/.last-regexp
                        regexp))
            (not alien-search/re-builder/.cached-data))
    (setq alien-search/re-builder/.cached-data
          ;; Do not run external program when
          ;; regexp is empty.
          (if (and (stringp regexp)
                   (not (equal regexp "")))
              (condition-case c
                  (with-current-buffer reb-target-buffer
                    (alien-search/run-external-program
                     alien-search/re-builder/external-program
                     alien-search/re-builder/default-shell-script
                     (buffer-substring (point-min) (point-max))
                     regexp
                     nil
                     (if alien-search/dot-match-a-newline-p "DOT" "")
                     (if case-fold-search "" "CASE")
                     (if alien-search/use-extended-regexp-p "EXT" "")
                     (if (numberp reb-auto-match-limit)
                         (format "%s" reb-auto-match-limit)
                       "")))
                (error
                 ;; It seems that re-builder throws away error messages,
                 ;; so we leave error messages on *Messages* buffer.
                 (message "%s" c)
                 (error c)))
            nil))
    (setq alien-search/re-builder/.last-regexp
          regexp))
  (let ((forward-p (if (boundp 'alien-search/re-builder/forward-p)
                       alien-search/re-builder/forward-p
                     t))
        (pt (with-current-buffer reb-target-buffer
              (point))))
    (if forward-p
        ;; Search forward
        (let* ((data alien-search/re-builder/.cached-data)
               (be-lst (car (member-if
                             #'(lambda (be-lst)
                                 (<= pt (1+ (nth 0 be-lst)))) ;;1+ = [Offset => Count]
                             data)))
               (beg (and be-lst (1+ (nth 0 be-lst)))) ;;1+ = [Offset => Count]
               (end (and be-lst (1+ (nth 1 be-lst))))) ;;1+ = [Offset => Count]
          (when (and be-lst
                     (if bound
                         (<= end bound)
                       t))
            (set-match-data `(,beg
                              ,end
                              ;;1+ = [Offset => Count]
                              ,@(mapcar #'1+ (cddr be-lst))))
            (goto-char end)
            end))
      ;; Search backward
      (let* ((data (reverse alien-search/re-builder/.cached-data))
             (be-lst (car (member-if
                           #'(lambda (be-lst)
                               (<= (1+ (nth 1 be-lst)) pt)) ;;1+ = [Offset => Count]
                           data)))
             (beg (and be-lst (1+ (nth 0 be-lst)))) ;;1+ = [Offset => Count]
             (end (and be-lst (1+ (nth 1 be-lst))))) ;;1+ = [Offset => Count]
        (when (and be-lst
                   (if bound
                       (<= bound beg)
                     t))
          (set-match-data `(,beg
                            ,end
                            ;;1+ = [Offset => Count]
                            ,@(mapcar #'1+ (cddr be-lst))))
          (goto-char beg)
          beg)))))

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
  "Update mode string and update search status with external program.
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


;;; ===========================================================================
;;;
;;;  Non-incremental search with a help from external program.
;;;
;;; ===========================================================================

(defvar alien-search/non-incremental/.last-regexp nil
  "Private variable.")

(defvar alien-search/non-incremental/.cached-data nil
  "Private variable.")


;; ----------------------------------------------------------------------------
;;
;;  Commands
;;
;; ----------------------------------------------------------------------------

;; From menu-bar.el
(defun alien-search/non-incremental/search-forward (string)
  "Read a regular expression and search for it nonincrementally."
  (interactive (list (read-from-minibuffer "Search for alien regexp: "
                                           nil nil nil
                                           'alien-search/history)))
  (let ((alien-search/isearch/.last-regexp alien-search/non-incremental/.last-regexp)
        (alien-search/isearch/.cached-data alien-search/non-incremental/.cached-data)
        (isearch-forward t))
    (alien-search/isearch/search-fun string)
    (setq menu-bar-last-search-type 'alien)
    
    (setq alien-search/non-incremental/.last-regexp alien-search/isearch/.last-regexp)
    (setq alien-search/isearch/.last-regexp alien-search/isearch/.cached-data)
    (add-hook 'pre-command-hook 'alien-search/non-incremental/.clear-cache)))
(put 'alien-search/non-incremental/search-forward  'alien-search/nonincremental-command-p t)

;; From menu-bar.el
(defun alien-search/non-incremental/search-backward (string)
  "Read a regular expression and search for it backward nonincrementally."
  (interactive (list (read-from-minibuffer "Search for alien regexp: "
                                           nil nil nil
                                           'alien-search/history)))
  (let ((alien-search/isearch/.last-regexp alien-search/non-incremental/.last-regexp)
        (alien-search/isearch/.cached-data alien-search/non-incremental/.cached-data)
        (isearch-forward nil))
    (alien-search/isearch/search-fun string)
    (setq menu-bar-last-search-type 'alien)
    
    (setq alien-search/non-incremental/.last-regexp alien-search/isearch/.last-regexp)
    (setq alien-search/isearch/.last-regexp alien-search/isearch/.cached-data)
    (add-hook 'pre-command-hook 'alien-search/non-incremental/.clear-cache)))
(put 'alien-search/non-incremental/search-backward 'alien-search/nonincremental-command-p t)


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
        (setq ad-return-valur
              (alien-search/non-incremental/search-forward (car alien-search/history))))
       (t
        ad-do-it)))
    (ad-activate 'nonincremental-repeat-search-forward)
    (put 'nonincremental-repeat-search-forward 'alien-search/nonincremental-command-p t))

  (when (fboundp 'nonincremental-repeat-search-backward)
    (defadvice nonincremental-repeat-search-backward (around alien-search/nonincremental-repeat-search-backward ())
      (cond
       ((and (eq menu-bar-last-search-type 'alien)
             alien-search/history)
        (setq ad-return-valur
              (alien-search/non-incremental/search-backward (car alien-search/history))))
       (t
        ad-do-it)))
    (ad-activate 'nonincremental-repeat-search-backward)
    (put 'nonincremental-repeat-search-backward 'alien-search/nonincremental-command-p t)))


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/non-incremental/.clear-cache) => VOID
;; ----------------------------------------------------------------------------
(defun alien-search/non-incremental/.clear-cache ()
  "Private function."
  (when (not (get this-command 'alien-search/nonincremental-command-p))
    (setq alien-search/non-incremental/.cached-data nil)
    (remove-hook 'pre-command-hook 'alien-search/non-incremental/.clear-cache)))

;;; alien-search.el ends here
