;; Needed for nice htmlize
(package-initialize)
;; Load org mode
(add-to-list 'load-path "__TEST_DIR__/org-9.2.4/lisp")
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
         (workdir "__TEST_DIR__/")
         (relative-html-file (substring html-file (length workdir)))
         (command (concat "rake 'site:customize_output[" relative-html-file "]'")))
    (message (replace-regexp-in-string "\n$" "" (shell-command-to-string command)))
    html-file))

(setq make-backup-files nil
      enable-local-variables :all
      org-publish-timestamp-directory "__TEST_DIR__/tmp/"
      org-id-locations-file "__TEST_DIR__/tmp/org-id-locations"
      org-confirm-babel-evaluate nil
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-metadata-timestamp-format "%a %d %B %Y à %H:%M"
      org-html-text-markup-alist '((bold . "<strong>%s</strong>")
                                   (code . "<code>%s</code>")
                                   (italic . "<em>%s</em>")
                                   (strike-through . "<del>%s</del>")
                                   (underline . "<span class=\"underline\">%s</span>")
                                   (verbatim . "<code>%s</code>"))
      org-publish-project-alist
      `(("org"
         :base-directory "__TEST_DIR__/src"
         :base-extension "org"
         :recursive t
         :exclude "tata\.org"
         :publishing-directory "__TEST_DIR__/public_html"
         :publishing-function pablo-publish-to-html-and-customize-output
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\" href=\"style.css\"/>")
        ("org-assets"
         :base-directory "__TEST_DIR__/src"
         :base-extension "jpg\\|gif\\|png\\|svg\\|pdf"
         :recursive t
         :publishing-directory "__TEST_DIR__/public_html"
         :publishing-function org-publish-attachment)
        ("test"
         :base-directory "__TEST_DIR__/titi/test"
         :base-extension "org"
         :recursive nil
         :exclude "ugly\.org"
         :publishing-directory "__TEST_DIR__/public_html/test"
         :publishing-function pablo-publish-to-html-and-customize-output
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\" href=\"style.css\"/>")
        ("test-assets"
         :base-directory "__TEST_DIR__/titi/test"
         :base-extension "jpg\\|gif\\|png\\|svg\\|pdf"
         :recursive nil
         :publishing-directory "__TEST_DIR__/public_html/test"
         :publishing-function org-publish-attachment)
        ("tata"
         :base-directory "__TEST_DIR__/titi/tutu/tata"
         :base-extension "org"
         :recursive t
         :publishing-directory "__TEST_DIR__/public_html/tata"
         :publishing-function pablo-publish-to-html-and-customize-output
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\" href=\"style.css\"/>")
        ("tata-assets"
         :base-directory "__TEST_DIR__/titi/tutu/tata"
         :base-extension "jpg\\|gif\\|png\\|svg\\|pdf"
         :recursive t
         :publishing-directory "__TEST_DIR__/public_html/tata"
         :publishing-function org-publish-attachment)
        ("theme"
         :base-directory "__TEST_DIR__/themes/"
         :base-extension "jpg\\|gif\\|png\\|js\\|css\\|otf\\|ttf\\|woff2?"
         :recursive t
         :publishing-directory "__TEST_DIR__/public_html/assets"
         :publishing-function org-publish-attachment)
        ("website" :components ("org" "org-assets" "test" "test-assets" "tata" "tata-assets" "theme"))))
