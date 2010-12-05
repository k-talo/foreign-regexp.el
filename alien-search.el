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
;; - Toggle case (in)?sensitive search?
;;   or support `case-fold-search'?
;; - Set extra options to external program?
;; - Better handling of exceptions from external program,
;;   especially syntax error regarding to regular expression
;;   in isearch session.
;; - Better response?
;; - Write tests.
;; - validate-regexp by external program?
;; - Care about `reb-auto-match-limit' in re-builder?

;;; Change Log:

;;; Code:

(provide 'alien-search)
(defconst alien-search/version "0.0")


(eval-when-compile (require 'cl))


;;; ===========================================================================
;;;
;;;  Common variable and functions to alien-search operation.
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


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;;  (alien-search/run-external-program prog-path default-shell-script body
;;                                     pattern &optional replacement) => RESULT
;; ----------------------------------------------------------------------------
(defun alien-search/run-external-program (prog-path default-shell-script
                                                    body
                                                    pattern
                                                    &optional
                                                    replacement)
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
                                 ))))
            (when (not (and (numberp status)
                            (zerop status)))
              (error "[alien-search] %s exited with status \"%s\":\n%s"
                     prog-basename
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
;;;  `query-replace' with a help from external program.
;;;
;;; ===========================================================================

(defcustom alien-search/replace/external-program nil
  "Path of an external program to use to execute actual search operation.

Four arguments describe below will be passed to the program.

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
      `alien-search/output-coding-system'."
  :type 'string
  :group 'alien-search)

(defcustom alien-search/replace/default-shell-script
  "#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort \"Ruby version is too old (1.9 or later is required).\" if RUBY_VERSION < \"1.9\"

def escape_str_to_eval! (str)
  str.gsub!(/\"/ ){'\\\\\"'}
end

def escape_ruby_str_for_emacs! (str)
  str.gsub!(/\\\\/) {'\\\\\\\\'}
  str.gsub!(/\"/ ) {'\\\\\"'}
end

def main ()
  fn_in, fn_out, fn_pat, fn_rpl = ARGV
  
  str_in  = open(fn_in,  'r:UTF-8') {|f| f.read}
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}
  str_rpl = open(fn_rpl, 'r:UTF-8') {|f| f.read}
  
  escape_str_to_eval!(str_rpl)
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  print \"(setq result '(\"
  
  str_in.scan( Regexp.new(str_pat) ) do |m|
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
            (prog1 (query-replace-read-args
                    (concat "Query replace alien regexp"
                            (if (and transient-mark-mode mark-active) " in region" ""))
                    t)
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
                  replacement)))
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
;;                                     query-flag ignore ignore
;;                                     &optional ignore map start end) => VOID
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

(defcustom alien-search/occur/external-program nil
  "Path of an external program to use to execute actual search
operation.

Three arguments describe below will be passed to the program.

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
      `alien-search/output-coding-system'."
  :type  'string
  :group 'alien-search)

(defcustom alien-search/occur/default-shell-script
  "#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort \"Ruby version is too old (1.9 or later is required).\" if RUBY_VERSION < \"1.9\"

def main ()
  fn_in, fn_out, fn_pat = ARGV
  
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}
  offset = 0
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  print \"(setq result '(\"
  
  open(fn_in, 'r:UTF-8') do |file_in|
    while line = file_in.gets do
      matched = 0
      len = line.length
      line.chomp!
      
      line.scan( Regexp.new(str_pat) ) do
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
                     (alien-search/occur-read-primary-args)
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
                            regexp))
              (or coding
                  ;; Set CODING only if the current buffer locally
                  ;; binds buffer-file-coding-system.
                  (not (local-variable-p 'buffer-file-coding-system))
                  (setq coding buffer-file-coding-system))
              (save-excursion
                (goto-char (point-min)) ;; begin searching in the buffer
                (while (setq matches-in-line (prog1 (car result)
                                               (setq result (cdr result))))
                  (let* ((matchbeg (1+ (caar matches-in-line))) ;;1+ = [Offset => Count]
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
                       (- (1+ (nth 0 match-pair)) begpt) ;;1+ = [Offset => Count]
                       (- (1+ (nth 1 match-pair)) begpt) ;;1+ = [Offset => Count]
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

Three arguments describe below will be passed to the program.

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
      `alien-search/output-coding-system'."
  :type  'string
  :group 'alien-search)

(defcustom alien-search/isearch/default-shell-script
  "#!/usr/bin/env ruby
# -*- coding: utf-8-unix -*-

abort \"Ruby version is too old (1.9 or later is required).\" if RUBY_VERSION < \"1.9\"

def main ()
  fn_in, fn_out, fn_pat = ARGV
  
  str_in  = open(fn_in,  'r:UTF-8') {|f| f.read}
  str_pat = open(fn_pat, 'r:UTF-8') {|f| f.read}
  
  $stdout = open(fn_out, 'w:UTF-8')
  
  print \"(setq result '(\"
  
  str_in.scan( Regexp.new(str_pat) ) do
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
;;  (alien-search/isearch-forward &optional not-regexp no-recursive-edit) => VOID
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
                         t t ad-return-value))))


;; ----------------------------------------------------------------------------
;;
;;  Functions
;;
;; ----------------------------------------------------------------------------

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
    (setq alien-search/isearch/.cached-data
          (alien-search/run-external-program
           alien-search/isearch/external-program
           alien-search/isearch/default-shell-script
           (buffer-substring (point-min) (point-max))
           regexp))
    (setq alien-search/isearch/.last-regexp
          regexp))
  (let ((forward-p isearch-forward)
        (pt (point)))
    (if forward-p
        ;; Search forward
        (let* ((data alien-search/isearch/.cached-data)
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
            (set-match-data (list beg end))
            (goto-char end)
            end))
      ;; Search backward
      (let* ((data (reverse alien-search/isearch/.cached-data))
             (be-lst (car (member-if
                           #'(lambda (be-lst)
                               (<= (1+ (nth 1 be-lst)) pt)) ;;1+ = [Offset => Count]
                           data)))
             (beg (and be-lst (1+ (nth 0 be-lst)))) ;;1+ = [Offset => Count]
             (end (and be-lst (1+ (nth 1 be-lst)))))
        (when (and be-lst
                   (if bound
                       (<= bound beg)
                     t))
          (set-match-data (list beg end))
          (goto-char beg)
          beg)))))


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
   pattern))


;;; ===========================================================================
;;;
;;;  `re-builder' with a help from external program.
;;;
;;; ===========================================================================
(require 're-builder)

(defcustom alien-search/re-builder/external-program "~/bin/alien-search-re-builder-aux.rb"
  "Path of an external program to use to execute actual search operation.

Three arguments describe below will be passed to the program.

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
      `alien-search/output-coding-system'."
  :type  'string
  :group 'alien-search)

(defcustom alien-search/re-builder/default-shell-script
  ""
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
;;  (alien-search/with-overriding-re-search-fn (&keys re-search-forward-fn
;;                                                    re-search-backward-fn
;;                                                    when-regexp-eq)
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
              (alien-search/run-external-program
               alien-search/re-builder/external-program
               alien-search/re-builder/default-shell-script
               (with-current-buffer reb-target-buffer
                 (buffer-substring (point-min) (point-max)))
               regexp)
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
          
;;; alien-search.el ends here
