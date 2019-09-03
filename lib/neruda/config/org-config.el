;; Load neruda lib
(load-file "__NERUDA_DIR__/ox-neruda.el")
(neruda/init-export-variables "__WORK_DIR__" "__ORG_VER__")

(setq org-html-metadata-timestamp-format "__LONG_DATE_FMT__"
      org-publish-project-alist
      `(__ALL_PROJECTS__
        __THEME_CONFIG__
        ("website" :components (__ALL_PROJECTS_NAMES__ "theme"))))
