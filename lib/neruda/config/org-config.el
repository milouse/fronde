;; Load neruda lib
(add-to-list 'load-path (expand-file-name "org-__ORG_VER__/lisp" "__WORK_DIR__"))
;; Load modern version of htmlize.el
(load-file (expand-file-name "htmlize.el" "__WORK_DIR__"))
(load-file "__NERUDA_DIR__/ox-neruda.el")
(neruda/init-export-variables "__WORK_DIR__")

(setq user-mail-address "__AUTHOR_EMAIL__"
      user-full-name "__AUTHOR_NAME__"
      org-html-metadata-timestamp-format "__LONG_DATE_FMT__"
      org-publish-project-alist
      `(__ALL_PROJECTS__
        __THEME_CONFIG__
        ("website" :components (__ALL_PROJECTS_NAMES__ "theme"))))
