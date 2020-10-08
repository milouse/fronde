;; Add custom org mode to load path
(add-to-list 'load-path (expand-file-name "org-__ORG_VER__/lisp" "__WORK_DIR__"))
;; Load modern version of htmlize.el
(load-file (expand-file-name "htmlize.el" "__WORK_DIR__"))

;; Current project options
(setq neruda/version "__VERSION__"
      neruda/current-work-dir "__WORK_DIR__"
      user-mail-address "__AUTHOR_EMAIL__"
      user-full-name "__AUTHOR_NAME__"
      org-html-metadata-timestamp-format "__LONG_DATE_FMT__"
      org-publish-project-alist
      `(__ALL_PROJECTS__
        __THEME_CONFIG__
        ("website" :components (__ALL_PROJECTS_NAMES__))))

;; Load neruda lib
(load-file (expand-file-name "ox-neruda.el" "__NERUDA_DIR__"))
