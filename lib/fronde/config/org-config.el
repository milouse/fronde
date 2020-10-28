;; Add org-mode to load path
(add-to-list 'load-path (expand-file-name "org-__ORG_VER__/lisp" "__WORK_DIR__"))
;; Load last version of htmlize.el
(load-file (expand-file-name "htmlize.el" "__WORK_DIR__"))

;; Current project options
(setq fronde/version "__VERSION__"
      fronde/current-work-dir "__WORK_DIR__"
      user-mail-address "__AUTHOR_EMAIL__"
      user-full-name "__AUTHOR_NAME__"
      org-html-metadata-timestamp-format "__LONG_DATE_FMT__"
      org-publish-project-alist
      `(__ALL_PROJECTS____THEME_CONFIG__
        ("website" :components (__ALL_PROJECTS_NAMES__))))

;; Load fronde lib
(load-file (expand-file-name "ox-fronde.el" "__FRONDE_DIR__"))
