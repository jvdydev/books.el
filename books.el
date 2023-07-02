;;; books.el --- Digital Book Management for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 jvdydev

;; Author: jvdydev
;; Maintainer: Judy
;; Created: 2023
;; Version: 0.6
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/jvdydev/books.el
;; Keywords: convenience tools

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'subr-x)

;;; Constants
(defconst books--default-narrow-list
  '("Title" "Author" "File Name" "Page Count")
  "List used to narrow results parsed from running exiftool.")

;;; Customization
(defgroup books nil
  "Books.el: Digital Book Management for Emacs"
  :prefix "books-")

(defcustom books-book-directory "~/Books/"
  "Directory to search for book files in."
  :group 'books
  :type 'string)

(defcustom books-book-file-extensions '("pdf")
  "List of extensions to allow for files to be shown."
  :group 'books
  :type 'list)

(defcustom books-viewer #'find-file
  "Set the viewer to open the book file in.
Can either be an Emacs Lisp function (like find-file) or a string naming an executable."
  :group 'books
  :type 'function)

;;; Caching layer
(defvar books--cache nil
  "Books cache.")

(defun books--build-cache ()
  (setq books--cache
        (mapcar (lambda (f) (books--item-to-row (books--file-metadata f)))
                (books--list-book-files))))

;;; Helper functions for processing
(defun books--split-string (string &optional separator omit-nulls trim limit)
  "Like `split-string', but with optional LIMIT of the maximum list length.
If used, any splits beyond are re-joined using SEPARATOR.

This behaves similarly to `str:split' for Common Lisp."
  (let ((split (split-string string separator omit-nulls trim)))
    (if (and limit (> (length split) limit))
        (let ((actual-limit (- limit 1)))
          (append (butlast split (- (length split) actual-limit))
                  (list (string-join (nthcdr actual-limit split) separator))))
      split)))

(defun books--list-book-files ()
  "List all files in BOOKS-BOOK-DIRECTORY that end in a file extension from BOOKS-BOOK-FILE-EXTENSIONS."
  (flatten-list
   (mapcar
    (lambda (file-type)
      (directory-files books-book-directory nil (concat ".*." file-type "$")))
    books-book-file-extensions)))

;;; Exiftool
(defun books--run-exiftool (filename)
  "Run exiftool on FILENAME in BOOKS-BOOK-DIRECTORY."
  (shell-command-to-string
   (concat "exiftool " books-book-directory filename)))

(defun books--items-to-plist (items)
  "Convert list of lists to plist."
  (flatten-list
   (mapcar
    (lambda (e)
      (append
       (list
        (intern-soft (concat ":" (string-replace " " "-" (downcase (car e))))))
       (cdr e)))
    items)))

(defun books--parse-exiftool-output (output)
  "Parse exiftool output for a single file.
Return a plist containing the data parsed and narrowed."
  (books--items-to-plist
   (cl-remove-if-not
    (lambda (e)
      (member (car e) '("Title" "Author" "File Name" "Page Count")))
    (mapcar
     (lambda (s)
       (books--split-string s ":" t "[ \t\n\r]+" 2))
     (split-string output "\n" t)))))

(defun books--file-metadata (filepath)
  "Run and parse file metadata for FILEPATH."
  (books--parse-exiftool-output
   (books--run-exiftool filepath)))

;;; Frontend
;;;; Actions
(defun books-open-book-at-point ()
  "Open book at point."
  (interactive)
  (let ((filepath (concat books-book-directory
                          (aref (tabulated-list-get-entry) 2))))
    (if (symbolp books-viewer)
        (funcall books-viewer filepath)
      (start-process "*book-process*" "*book-runner*"
                     books-viewer filepath))))

;;;; Modemap
;; FIX ME Doesn't apply
(defvar books-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'books-open-book-at-point)
    map))

;;;; books-mode
(defun books--item-to-row (item)
  "Return a list (nil [...]) of data in item."
  (list nil (vector (or (plist-get item :author) "Unknown Author")
                    (or (plist-get item :title) "Unknown Title")
                    (or (plist-get item :file-name) "Invalid file")
                    (or (plist-get item :page-count) "Unknown"))))

(define-derived-mode books-mode tabulated-list-mode "books"
  "Books Mode"
  (let ((columns [("Author" 25) ("Title" 50) ("File" 50) ("Pages" 15)])
        (rows books--cache))
    ;; Set up buffer
    (buffer-disable-undo)
    (kill-all-local-variables)
    (setq truncate-lines t)

    (setq major-mode 'books-mode)
    (use-local-map books-mode-map)

    ;; Load tabulated mode
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))


;;;; Runners
(defun books ()
  "Open a new buffer with books-mode."
  (interactive)
  (unless books--cache
    (message "Building book cache ...")
    (books--build-cache))
  (switch-to-buffer "*books*")
  (books-mode)
  (message "Opening books"))

(provide 'books)
;;; books.el ends here
