;;; quiet-save.el --- Auto-save buffers quietly      -*- lexical-binding: t; -*-

;; Copyright (C) 2007 Kentaro Kuribayashi

;; Author: Kentaro Kuribayashi <kentarok@gmail.com>
;; Maintainer: papaeye <papaeye@gmail.com>
;; Keywords: convenience, files
;; Version: 0.1.0
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

(eval-when-compile
  (require 'cl))

(defgroup quiet-save nil
  "Auto-saving buffers quietly."
  :group 'auto-save)

(defcustom quiet-save-exclude nil
  "List of regexps and predicates for filenames excluded from the auto-saving
buffers."
  :type '(repeat (choice regexp function))
  :group 'quiet-save)

(defcustom quiet-save-keep nil
  "List of regexps and predicates for filenames kept in the auto-saving
 buffers."
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

(defun quiet-save-include-p (filename)
  (let ((case-fold-search quiet-save-case-fold-search)
	(checks quiet-save-exclude)
        (keepit t))
    (while (and checks keepit)
      (setq keepit (not (ignore-errors
			  (if (stringp (car checks))
			      (string-match (car checks) filename)
			    (funcall (car checks) filename))))
	    checks (cdr checks)))
    keepit))

(defun quiet-save-keep-p (filename)
  (let* ((case-fold-search quiet-save-case-fold-search)
	 (checks quiet-save-keep)
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
			    (if (= (length backend-roots) 1)
				(car backend-roots)
			      (lambda (dir)
				(let ((roots backend-roots)
				      found)
				  (while (and roots (not found))
				    (if (file-exists-p
					 (expand-file-name (car roots) dir))
					(setq found dir))
				    (setq roots (cdr roots)))
				  found))))))

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
      (dolist (buffer (buffer-list))
	(set-buffer buffer)
	(when (and buffer-file-name
		   (buffer-modified-p)
		   (not buffer-read-only)
		   (quiet-save-include-p buffer-file-name)
		   (quiet-save-keep-p buffer-file-name)
		   (not (buffer-base-buffer))
		   (file-writable-p buffer-file-name))
	  (basic-save-buffer)
	  (set-visited-file-modtime)
	  (set-buffer-modified-p nil))))))

(defvar quiet-save-idle-timer nil)

;;;###autoload
(define-minor-mode quiet-save-mode nil
  :global t
  :group 'quiet-save
  (if quiet-save-idle-timer
      (setq quiet-save-idle-timer
	    (cancel-timer quiet-save-idle-timer)))
  (if quiet-save-mode
      (setq quiet-save-idle-timer
	    (run-with-idle-timer quiet-save-delay t 'quiet-save-buffers))))

(provide 'quiet-save)
;;; quiet-save.el ends here
