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
(require 'ox-gmi)

;;; Function Declarations

(defvar fronde-version ""
  "Version of the current fronde installation.")

(defvar fronde-current-work-dir nil
  "Location of the current fronde website base directory.")

(defvar fronde-domain ""
  "Target domain with scheme of the current fronde installation.")

(defvar fronde-org-temp-dir nil
  "Location of the local Org temporary directory.
This is where to place org timestamps and id locations.")

(defvar fronde--keywords-slugs-alist nil
  "A list associating each keywords to its related slug.

Can be hydrated with `fronde--build-keywords-list'.")

(defun fronde--build-keywords-list ()
  (let ((keywords-file (format "%s/keywords" fronde-org-temp-dir))
        keywords-slugs)
    (when (file-readable-p keywords-file)
      (let ((content (with-temp-buffer
                       (insert-file-contents keywords-file)
                       (string-trim-right (buffer-string)))))
        (mapcar
         (lambda (line)
           (push (split-string line "\x1f") keywords-slugs))
         (split-string content "\x1e"))))
    keywords-slugs))

(defun fronde--format-rich-keywords (info function)
  "Extract keywords from INFO and apply FUNCTION on them.
FUNCTION is expected to format each keyword for a rich display for the
current export backend.  FUNCTION must receive 3 arguments: the current
KEYWORD, its related SLUG and the current project BASE-URI."
  (let ((base-uri (plist-get info :fronde-base-uri)))
    (mapcar
     (lambda (k)
       (let ((slug (cadr (assoc k fronde--keywords-slugs-alist))))
         (funcall function k slug base-uri)))
     (split-string
      (org-export-data (plist-get info :keywords) info)
      ",+ *"))))

(defun fronde--org-html-format-spec (upstream info)
  "Advise UPSTREAM to return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  (let ((output (funcall upstream info)))
    (push `(?A . ,(format "<span class=\"author\">%s</span>"
                    (org-export-data (plist-get info :author) info)))
          output)
    (push `(?k . ,(org-export-data (plist-get info :keywords) info)) output)
    (push `(?K . ,(format "<ul class=\"keywords-list\">\n%s</ul>"
                    (apply #'concat
                      (fronde--format-rich-keywords
                        info
	                    (lambda (k slug base-uri)
                          (format "<li class=\"keyword\"><a href=\"%stags/%s.html\">%s</a></li>\n"
                            base-uri slug k))))))
          output)
    (push `(?l . ,(org-export-data (plist-get info :language) info)) output)
    (push `(?n . ,(format "Fronde %s" fronde-version)) output)
    (push `(?N . ,(format "<a href=\"https://etienne.depar.is/fronde/\">Fronde</a> %s" fronde-version)) output)
    (push `(?x . ,(org-export-data (plist-get info :description) info)) output)
    (push `(?X . ,(format "<p>%s</p>"
                    (org-export-data (plist-get info :description) info)))
          output)))

(defun fronde--org-gmi-format-spec (upstream info)
  "Advise UPSTREAM to return format specification for gemini postamble.
INFO is a plist used as a communication channel."
  (let ((output (funcall upstream info)))
    (push `(?K . ,(org-gmi--build-links-list
                   (fronde--format-rich-keywords
                    info
                    (lambda (k slug base-uri)
                      (list (format "%stags/%s.gmi" base-uri slug)
                            (format "🏷️ %s" k))))))
          output)
    (push `(?n . ,(format "Fronde %s" fronde-version)) output)))

(defun fronde--org-i18n-export (link description backend)
  "Export the given i18n LINK with its DESCRIPTION for the current BACKEND."
  (let* ((splitted-link (split-string link "::"))
         (path (car splitted-link))
         (desc (or description path))
         (lang (cadr splitted-link)))
    (pcase backend
      (`html (if lang
                 (format "<a href=\"%s\" hreflang=\"%s\">%s</a>"
                         path lang desc)
               (format "<a href=\"%s\">%s</a>" path desc)))
      (_ nil))))

(defun fronde--org-i18n-follow (link)
  "Visit the given i18n LINK."
  (browse-url (car (split-string link "::"))))

(org-link-set-parameters "i18n"
  :export #'fronde--org-i18n-export
  :follow #'fronde--org-i18n-follow)


;;; Set configuration options

(setq fronde-org-temp-dir (expand-file-name "var/tmp" fronde-current-work-dir)
      fronde--keywords-slugs-alist (fronde--build-keywords-list)
      org-publish-timestamp-directory (expand-file-name "timestamps/" fronde-org-temp-dir)
      org-id-locations-file (expand-file-name "id-locations.el" fronde-org-temp-dir)
      make-backup-files nil
      enable-dir-local-variables nil
      enable-local-variables t ;; enforce default
      org-confirm-babel-evaluate t ;; enforce default
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
(advice-add 'org-html-format-spec :around #'fronde--org-html-format-spec)
(advice-add 'org-gmi--format-spec :around #'fronde--org-gmi-format-spec)

(provide 'ox-fronde)

;;; ox-fronde.el ends here
