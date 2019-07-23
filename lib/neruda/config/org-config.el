;; Needed for nice htmlize
(package-initialize)
;; Load org mode
(add-to-list 'load-path "__WORK_DIR__/org-__ORG_VER__/lisp")
(require 'org)

(org-link-set-parameters "i18n"
                         :export #'org-i18n-export
                         :follow #'org-i18n-follow)

(defun org-i18n-export (link description format)
  "Export a i18n link"
  (let* ((splitted-link (split-string link "|"))
         (path (car splitted-link))
         (desc (or description path))
         (lang (car (cdr splitted-link))))
    (pcase format
      (`html (if lang
                 (format "<a href=\"%s\" hreflang=\"%s\">%s</a>"
                         path lang desc)
               (format "<a href=\"%s\">%s</a>" path desc)))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (_ path))))

(defun org-i18n-follow (link)
  "Visit a i18n link"
  (browse-url (car (split-string link "|"))))

(defun pablo-publish-to-html-and-customize-output (plist filename pub-dir)
  "Wrap the `org-html-publish-to-html' function and customize its output.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let* ((html-file (org-html-publish-to-html plist filename pub-dir))
         (workdir "__WORK_DIR__/")
         (relative-html-file (substring html-file (length workdir)))
         (command (concat "rake 'site:customize_output[" relative-html-file "]'")))
    (message (replace-regexp-in-string "\n$" "" (shell-command-to-string command)))
    html-file))

(setq make-backup-files nil
      enable-local-variables :all
      org-publish-timestamp-directory "__WORK_DIR__/tmp/"
      org-id-locations-file "__WORK_DIR__/tmp/org-id-locations"
      org-confirm-babel-evaluate nil
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-metadata-timestamp-format "__LONG_DATE_FMT__"
      org-html-text-markup-alist '((bold . "<strong>%s</strong>")
                                   (code . "<code>%s</code>")
                                   (italic . "<em>%s</em>")
                                   (strike-through . "<del>%s</del>")
                                   (underline . "<span class=\"underline\">%s</span>")
                                   (verbatim . "<code>%s</code>"))
      org-publish-project-alist
      `(__ALL_PROJECTS__
        __THEME_CONFIG__
        ("website" :components (__ALL_PROJECTS_NAMES__ "theme"))))
