;;; ox-neruda.el --- Neruda Gem specific helpers for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2019 Free Software Foundation, Inc.

;; Author: Étienne Deparis <etienne at depar dot is>
;; Keywords: org, export

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements specific helpers function, needed by the Ruby
;; Gem Neruda, which offers an easy way to publish a static website
;; using Org files as sources.

;;; Code:

(require 'org)
(require 'ox-html)

(org-export-define-derived-backend 'neruda 'html
  :translate-alist
  '((inner-template . org-neruda-inner-template)))

;;; Function Declarations

(defvar neruda/current-work-dir nil
  "Location of the current neruda website base directory.")

(defvar neruda/org-temp-dir nil
  "Location of the local org-mode temporary directory (where to place
org timestamps and id locations).")

(defun neruda/org-i18n-export (link description format)
  "Export a i18n link"
  (let* ((splitted-link (split-string link "|"))
         (path (car splitted-link))
         (desc (or description path))
         (lang (cadr splitted-link)))
    (pcase format
      (`html (if lang
                 (format "<a href=\"%s\" hreflang=\"%s\">%s</a>"
                         path lang desc)
               (format "<a href=\"%s\">%s</a>" path desc)))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (_ path))))

(defun neruda/org-i18n-follow (link)
  "Visit a i18n link"
  (browse-url (car (split-string link "|"))))

(defun org-neruda-inner-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((basickeys '(:description :keywords :creator :title :date
                      :author :email :language))
         (data-file
           ;; publishing are made from the src directory
           (format "%s/tmp/chunks/%s.data"
             neruda/current-work-dir
             (substring (plist-get info :output-file)
               (+ 1 (length (concat neruda/current-work-dir "/public_html")))))))
    (let ((data-dir (file-name-directory data-file)))
      (unless (file-exists-p data-dir)) (make-directory data-dir t))
    (with-current-buffer (find-file data-file)
      (insert "---\n")
      (dolist (key basickeys)
        (let ((value (org-export-data (plist-get info key) info))
               (keyname (substring (symbol-name key) 1)))
          (if (eq key :description)
            (insert (format "%s: |\n%s\n" keyname
                      (replace-regexp-in-string "^" "  " value)))
            (insert (format "%s: \"%s\"\n" keyname
                      (replace-regexp-in-string "\"" "\\\\\"" value))))))
      (save-buffer)))
  ;; Don't forget to run the parent method
  (org-html-inner-template contents info))


;;; End-user functions

(defun org-neruda-publish-to-html (plist filename pub-dir)
  "Publish an org file for Neruda static website generator.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let* ((target-html-file ;; (org-publish-org-to 'neruda filename ".html" plist pub-dir))
           (org-html-publish-to-html plist filename pub-dir))
          (relative-target-html-file (substring target-html-file
                                       (+ 1 (length neruda/current-work-dir))))
          ;; Always be verbose, this is the parent task to hide these messages
          (command (format "rake -v 'site:customize_output[%s]'"
                     relative-target-html-file)))
    (message (replace-regexp-in-string "\n$" "" (shell-command-to-string command)))
    target-html-file))

(defun neruda/init-export-variables (work-dir)
  "Initialize some variables needed for customized export.

WORK-DIR is used to initialize `neruda/current-work-dir', which is then
used to to load website specific dependencies."
  (setq neruda/current-work-dir work-dir
        neruda/org-temp-dir (expand-file-name "tmp" neruda/current-work-dir)
        org-publish-timestamp-directory (expand-file-name "timestamps/" neruda/org-temp-dir)
        org-id-locations-file (expand-file-name "id-locations.el" neruda/org-temp-dir)
        make-backup-files nil
        enable-local-variables :all
        org-confirm-babel-evaluate nil
        org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-htmlize-output-type 'css
        org-html-text-markup-alist '((bold . "<strong>%s</strong>")
                                     (code . "<code>%s</code>")
                                     (italic . "<em>%s</em>")
                                     (strike-through . "<del>%s</del>")
                                     (underline . "<span class=\"underline\">%s</span>")
                                      (verbatim . "<code>%s</code>")))
  (org-link-set-parameters "i18n"
    :export #'neruda/org-i18n-export
    :follow #'neruda/org-i18n-follow))


(provide 'ox-neruda)

;;; ox-neruda.el ends here
