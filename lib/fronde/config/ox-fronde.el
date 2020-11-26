;;; ox-fronde.el --- Fronde Gem specific helpers for Org Export Engine -*- lexical-binding: t; -*-

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
;; Gem Fronde, which offers an easy way to publish a static website
;; using Org files as sources.

;;; Code:

(require 'org)
(require 'ox-html)

;;; Function Declarations

(defvar fronde/version ""
  "Version of the current fronde installation")

(defvar fronde/current-work-dir nil
  "Location of the current fronde website base directory.")

(defvar fronde/org-temp-dir nil
  "Location of the local Org temporary directory (where to place
org timestamps and id locations).")

(defun fronde/org-html-format-spec (upstream info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  (let ((output (funcall upstream info)))
    (push `(?A . ,(format "<span class=\"author\">%s</span>"
                    (org-export-data (plist-get info :author) info)))
      output)
    (push `(?k . ,(org-export-data (plist-get info :keywords) info)) output)
    (push `(?K . ,(format "<ul class=\"keywords-list\">%s</ul>"
                    (mapconcat
	                    (lambda (k) (format "<li class=\"keyword\">%s</li>" k))
	                    (split-string (or (plist-get info :keywords) "")  ",+ *")
	                    "\n")))
      output)
    (push `(?l . ,(org-export-data (plist-get info :language) info)) output)
    (push `(?n . ,(format "Fronde %s" fronde/version)) output)
    (push `(?N . ,(format "<a href=\"https://git.umaneti.net/fronde/about/\">Fronde</a> %s" fronde/version)) output)
    (push `(?x . ,(org-export-data (plist-get info :description) info)) output)
    (push `(?X . ,(format "<p>%s</p>"
                    (org-export-data (plist-get info :description) info)))
      output)))

(defun fronde/org-i18n-export (link description format)
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
      (_ (format "%s (%s)" desc path)))))

(defun fronde/org-i18n-follow (link)
  "Visit a i18n link"
  (browse-url (car (split-string link "|"))))

(org-link-set-parameters "i18n"
  :export #'fronde/org-i18n-export
  :follow #'fronde/org-i18n-follow)


;;; Set configuration options

(setq fronde/org-temp-dir (expand-file-name "var/tmp" fronde/current-work-dir)
      org-publish-timestamp-directory (expand-file-name "timestamps/" fronde/org-temp-dir)
      org-id-locations-file (expand-file-name "id-locations.el" fronde/org-temp-dir)
      make-backup-files nil
      enable-local-variables :all
      org-confirm-babel-evaluate nil
      org-export-with-broken-links t
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-htmlize-output-type 'css
      org-html-text-markup-alist '((bold . "<strong>%s</strong>")
                                    (code . "<code>%s</code>")
                                    (italic . "<em>%s</em>")
                                    (strike-through . "<del>%s</del>")
                                    (underline . "<span class=\"underline\">%s</span>")
                                    (verbatim . "<code>%s</code>")))
(advice-add 'org-html-format-spec :around #'fronde/org-html-format-spec)

(provide 'ox-fronde)

;;; ox-fronde.el ends here
