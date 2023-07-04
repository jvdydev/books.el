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
(defconst books--column-names
  '("Title" "Author" "File Name" "Page Count")
  "List of columns parsed from running exiftool.")

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

(defcustom books-sort-column "Title"
  "Column used for sorting entries. See `books--column-names' for options."
  :group 'books
  :type 'string
  :options books--column-names)

(defcustom books-cache-file (expand-file-name "books.cache" user-emacs-directory)
  "File to cache book entries for speedy buffer loading."
  :group 'books
  :type 'string)

;;; Caching layer
(defvar books--cache nil
  "Books cache.")

(defun books--write-cache-to-file ()
  "Write the current `books--cache' state to file.
This overwrites any cached data in the file."
  (with-temp-file books-cache-file
    (insert "(setq books--cache '(\n")
    (mapc (lambda (i) (insert (format "%S\n" i))) books--cache)
    (insert "))")))

(defun books--load-cache-from-file ()
  "Load cached books from file.
Will not change books--cache if the file is missing."
  (when (file-exists-p books-cache-file)
    (with-temp-buffer "*books-cache-loader*"
                      (insert-file-contents books-cache-file)
                      (eval-buffer))))

(defun books--sort-book (book other)
  "Sort books based on `books-sort-column'.
To be used as predicate for `sort'."
  (let ((prop (cond
               ((string-equal books-sort-column "Title")
                :title)
               ((string-equal books-sort-column "Author")
                :author)
               ((string-equal books-sort-column "File Name")
                :file-name)
               ((string-equal books-sort-column "Page Count")
                :page-count)
               (t
                (user-error (format "Unknown sorting column: %s" books-sort-column))))))
    (funcall #'string<
             (plist-get book prop)
             (plist-get other prop))))

(defun books--build-cache (&optional force)
  "Load and/or rebuild the books cache.
If FORCE is non-nil, rebuild the entire book cache without using existing information."
  (when force
    (setq books--cache nil)
    (delete-file books-cache-file))

  (message "Building books cache ...")
  (books--load-cache-from-file)
  (let* ((cached-books
          (mapcar (lambda (b) (plist-get b :file-name)) books--cache))
         (uncached-books
          (cl-remove-if (lambda (b) (member b cached-books)) (books--list-book-files))))
    (if uncached-books
        (message "Caching new books: %s" uncached-books)
      (message "Cache up to date"))
    (setq books--cache (sort
                        (append books--cache
                                (mapcar (lambda (f) (books--file-metadata f))
                                        uncached-books))
                        #'books--sort-book))
    (books--write-cache-to-file))
  (message "Books cache built"))

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
   (mapcar (lambda (e)
             (append
              (list (intern-soft
                     (concat ":" (string-replace " " "-" (downcase (car e))))))
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
        (rows (mapcar #'books--item-to-row books--cache)))
    ;; Set up buffer
    (switch-to-buffer "*books*")
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
  (books--build-cache)
  (switch-to-buffer "*books*")
  (books-mode)
  (message "Opening books"))

(provide 'books)
;;; books.el ends here
