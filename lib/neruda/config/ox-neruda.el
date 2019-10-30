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


;;; End-user functions

(defun neruda/publish-to-html-and-customize-output (plist filename pub-dir)
  "Wrap the `org-html-publish-to-html' function and customize its output.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let* ((html-file (org-html-publish-to-html plist filename pub-dir))
         (relative-html-file (substring html-file (+ 1 (length neruda/current-work-dir))))
         (command (concat "rake 'site:customize_output[" relative-html-file "]'")))
    (message (replace-regexp-in-string "\n$" "" (shell-command-to-string command)))
    html-file))

(defun neruda/init-export-variables (work-dir org-version)
  "Initialize some variables needed for customized export.

WORK-DIR is used to initialize `neruda/current-work-dir', which is then
used to to load website specific dependencies.

ORG-VERSION is used to load the right local version of org-mode."
  (setq neruda/current-work-dir work-dir
        neruda/org-temp-dir (expand-file-name "tmp" neruda/current-work-dir))
  ;; Load modern version of htmlize.el
  (load-file (expand-file-name "htmlize.el" neruda/current-work-dir))
  ;; Load org mode
  (add-to-list 'load-path (expand-file-name
                            (concat "org-" org-version "/lisp")
                            neruda/current-work-dir))
  (require 'org)
  (require 'ox-html)
  (org-link-set-parameters "i18n"
    :export #'neruda/org-i18n-export
    :follow #'neruda/org-i18n-follow)
  (setq org-publish-timestamp-directory (expand-file-name "timestamps/" neruda/org-temp-dir)
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
                                      (verbatim . "<code>%s</code>"))))


(provide 'ox-neruda)

;;; ox-neruda.el ends here
