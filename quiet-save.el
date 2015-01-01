;;; quiet-save.el --- Auto-save buffers quietly      -*- lexical-binding: t; -*-

;; Copyright (C) 2007 Kentaro Kuribayashi
;; Copyright (C) 2014 papaeye

;; Author: Kentaro Kuribayashi <kentarok@gmail.com>
;;         papaeye <papaeye@gmail.com>
;; Keywords: convenience, files
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.5"))
;; Note: auto-save-buffers-enhanced.el borrows main ideas and some
;;       codes written by Satoru Takabayashi and enhances original
;;       one. Thanks a lot!!!
;;       See http://0xcc.net/misc/auto-save/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a fork of auto-save-buffers-enhanced.
;; See https://github.com/kentaro/auto-save-buffers-enhanced
;;
;; I forked it for four main reasons:
;;
;; 1. To shorten the name.
;; 2. To learn Emacs Lisp from such a small code base.
;; 3. To remove obsolete features like SVK support.
;; 4. To remove unrelated features like saving *scratch*.
;;
;; "quiet" means you don't have to type C-x C-s periodically and you
;; don't see "Wrote *filename*" in the echo area.
;;
;; quiet-save borrows many codes from the following site/software:
;;
;; - http://homepage3.nifty.com/oatu/emacs/misc.html
;; - https://github.com/cask/shut-up
;; - recentf.el
;;
;; To use quiet-save, just add the following code into your .emacs:
;;
;;     (require 'quiet-save)
;;     (quiet-save-mode)
;;
;; If you want quiet-save to work only with the files under the
;; directories checked out from Git or Mercurial, add the following
;; code into your .emacs:
;;
;;     (setq quiet-save-vc-root-backends '(git hg))
;;     (setq quiet-save-keep '(quiet-save-vc-root))

;;; Code:

(require 'cl-lib)

(defgroup quiet-save nil
  "Auto-saving buffers quietly."
  :group 'files)

(defcustom quiet-save-exclude nil
  "List of regexps and predicates for filenames excluded from the auto-saving
buffers.  When a filename matches any of the regexps or satisfies any of the
predicates in `find-file-hook' it is excluded from the auto-saving buffers.
A predicate is a function that is passed a filename to check and that
must return non-nil to exclude it."
  :type '(repeat (choice regexp function))
  :group 'quiet-save)

(defcustom quiet-save-keep nil
  "List of regexps and predicates for filenames kept in the auto-saving
buffers.  Regexps and predicates are tried in the specified order
in `find-file-hook'.
When nil all filenames are kept in the auto-saving buffers.
When a filename matches any of the regexps or satisfies any of the
predicates it is kept in the auto-saving buffers.
A predicate is a function that is passed a filename to check and that
must return non-nil to keep it."
  :type '(repeat (choice regexp function))
  :group 'quiet-save)

(defcustom quiet-save-last-exclude nil
  "List of regexps and predicates for filenames excluded from the auto-saving
buffers.  When a filename matches any of the regexps or satisfies any of the
predicates just before saving it is excluded from the auto-saving buffers.
A predicate is a function that is passed a filename to check and that
must return non-nil to exclude it."
  :type '(repeat (choice regexp function))
  :group 'quiet-save)

(defcustom quiet-save-last-keep nil
  "List of regexps and predicates for filenames kept in the auto-saving
buffers.  Regexps and predicates are tried in the specified order
just before saving.
When nil all filenames are kept in the auto-saving buffers.
When a filename matches any of the regexps or satisfies any of the
predicates it is kept in the auto-saving buffers.
A predicate is a function that is passed a filename to check and that
must return non-nil to keep it."
  :type '(repeat (choice regexp function))
  :group 'quiet-save)

(defcustom quiet-save-case-fold-search nil
  "Non-nil if quiet-save searches and matches should ignore case."
  :type 'boolean
  :group 'quiet-save)

(defcustom quiet-save-delay 0.5
  "Seconds of idle time after auto-saving buffers."
  :type 'number
  :group 'quiet-save)

(defcustom quiet-save-vc-root-backends '(git)
  "List of version control backends for which `quiet-save-vc-root'
will be used."
  :type '(repeat symbol)
  :group 'quiet-save)

(defvar quiet-save-vc-backend-alist
  '((git . ".git") (hg . ".hg")))

(defun quiet-save-include-p (filename checks)
  (let ((case-fold-search quiet-save-case-fold-search)
        (keepit t))
    (while (and checks keepit)
      (setq keepit (not (ignore-errors
			  (if (stringp (car checks))
			      (string-match (car checks) filename)
			    (funcall (car checks) filename))))
	    checks (cdr checks)))
    keepit))

(defun quiet-save-keep-p (filename checks)
  (let* ((case-fold-search quiet-save-case-fold-search)
	 (keepit (null checks)))
    (while (and checks (not keepit))
      (setq keepit (condition-case nil
		       (if (stringp (car checks))
			   (string-match (car checks) filename)
			 (funcall (car checks) filename))
		     (error nil))
	    checks (cdr checks)))
    keepit))

(defun quiet-save-vc-root (filename)
  (let ((backend-roots
	 (mapcar (lambda (backend)
		   (cdr (assq backend quiet-save-vc-backend-alist)))
		 quiet-save-vc-root-backends)))
    (locate-dominating-file filename
			    (lambda (dir)
			      (let ((roots backend-roots)
				    found)
				(while (and roots (not found))
				  (if (file-exists-p
				       (expand-file-name (car roots) dir))
				      (setq found dir))
				  (setq roots (cdr roots)))
				found)))))

(defvar quiet-save-buffer-list nil)

(eval-and-compile
  (fset 'quiet-save-write-region-original (symbol-function 'write-region)))

(defun quiet-save-write-region (start end filename &optional append visit
				      lockname mustbenew)
  (unless (or (stringp visit) (not visit))
    (setq visit 'no-message))
  (quiet-save-write-region-original start end filename append visit
				    lockname mustbenew))

(defun quiet-save-buffers ()
  (cl-letf (((symbol-function 'write-region) #'quiet-save-write-region))
    (save-current-buffer
      (dolist (buffer quiet-save-buffer-list)
	(set-buffer buffer)
	(when (and buffer-file-name
		   (buffer-modified-p)
		   (not buffer-read-only)
		   (quiet-save-include-p buffer-file-name
					 quiet-save-last-exclude)
		   (quiet-save-keep-p buffer-file-name
				      quiet-save-last-keep)
		   (not (buffer-base-buffer))
		   (file-writable-p buffer-file-name))
	  (basic-save-buffer)
	  (set-visited-file-modtime)
	  (set-buffer-modified-p nil))))))

(defvar quiet-save-idle-timer nil)

;;;###autoload
(defun quiet-save-turn-on ()
  (when (and (quiet-save-include-p buffer-file-name quiet-save-exclude)
	     (quiet-save-keep-p buffer-file-name quiet-save-keep))
    (add-to-list 'quiet-save-buffer-list (current-buffer))
    (add-hook 'kill-buffer-hook 'quiet-save-turn-off nil t)))

;;;###autoload
(defun quiet-save-turn-off ()
  (setq quiet-save-buffer-list
	(delq (current-buffer) quiet-save-buffer-list))
  (remove-hook 'kill-buffer-hook 'quiet-save-turn-off t))

(defun quiet-save-adjust-local-modes (on-or-off)
  (save-current-buffer
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (funcall on-or-off))))

;;;###autoload
(define-minor-mode quiet-save-mode nil
  :global t
  :group 'quiet-save
  (if quiet-save-idle-timer
      (setq quiet-save-idle-timer
	    (cancel-timer quiet-save-idle-timer)))
  (cond
   (quiet-save-mode
    (setq quiet-save-idle-timer
	  (run-with-idle-timer quiet-save-delay t 'quiet-save-buffers))
    (add-hook 'find-file-hook 'quiet-save-turn-on)
    (quiet-save-adjust-local-modes #'quiet-save-turn-on))
   (t
    (remove-hook 'find-file-hook 'quiet-save-turn-on)
    (quiet-save-adjust-local-modes #'quiet-save-turn-off))))

(provide 'quiet-save)
;;; quiet-save.el ends here
