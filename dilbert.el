;;; dilbert.el --- View Dilbert comics.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Daniils Petrovs

;; Author: Daniils Petrovs <thedanpetrov@gmail.com>
;; URL: https://example.com/package-name.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: something

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows flanges to be easily frobnicated.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + foo
;; + bar

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'package-name)

;;;; Usage

;; Run one of these commands:

;; `package-name-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `package-name' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

;;; Code:

;;;; Requirements

(require 'url)
(require 'image)
(require 'enlive)
(require 'dash)
(require 'browse-url)

(define-derived-mode dilbert-mode special-mode "dilbert"
  "Major mode for viewing Dilbert (https://dilbert.com) comics."
  :group 'dilbert)

;;;; Customization

(defgroup dilbert nil
  "Settings for `dilbert'."
  :group 'multimedia
  :link '(url-link "https://example.com/package-name.el"))

(defcustom dilbert-cache-dir (let ((dir (concat user-emacs-directory "dilbert/")))
							   (make-directory dir :parents)
							   dir)
  "Directory for caching comics and other files."
  :group 'dilbert
  :type 'directory)

;;;; Variables

(defvar dilbert-var nil
  "A variable.")

;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps
;; that have many bindings.

(defvar dilbert-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "dilbert map"))
        (maps (list
               ;; Mappings go here, e.g.:
               "RET" #'dilbert-RET-command
               [remap search-forward] #'dilbert-search-forward
               )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map))
;;;; Commands

;;;###autoload
(defun dilbert-get-latest ()
)

;;;; Functions

;;;; Private

(defun dilbert-fetch-homepage ()
  "Fetch the Dilbert homepage containing the latest comic strip."
  (enlive-fetch dilbert-home-url))

(defun dilbert-get-latest-comic-url ()
  "Get the latest Dilbert comic image URL."
  (->>
   (enlive-query-all (dilbert-fetch-homepage) [img.img-comic])
   (first)
   (second)
   (assoc 'src)
   (cdr)))

(defun dilbert-download (img-url)
  "Download the comic strip image at IMG-URL. If exists, just return img path."
  (let* ((img-hash (-> img-url (split-string "/") (last) (first)))
		(file-name (format "%s%s" dilbert-cache-dir img-hash)))
	(if (file-exists-p file-name)
		file-name
	  (url-copy-file img-url file-name))
	file-name))

;;;; Footer

(provide 'dilbert)

;;; dilbert.el ends here
