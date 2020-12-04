;; Add org-mode to load path
(add-to-list 'load-path (expand-file-name "org-__ORG_VERSION__/lisp" "__TEST_DIR__/lib"))
;; Load last version of htmlize.el
(load-file (expand-file-name "htmlize.el" "__TEST_DIR__/lib"))

;; Current project options
(setq fronde/version "__VERSION__"
      fronde/current-work-dir "__TEST_DIR__"
      user-mail-address ""
      user-full-name "Tata"
      org-html-metadata-timestamp-format "%A %-d of %B, %Y at %R"
      org-publish-project-alist
      `(("org"
         :base-extension "org"
         :publishing-function org-html-publish-to-html
         :base-directory "__TEST_DIR__/src"
         :publishing-directory "__TEST_DIR__/public_html"
         :recursive nil
         :section-numbers nil
         :with-toc nil
         :html-postamble "<footer>Published by Fronde.</footer>"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\"
      href=\"/assets/toto/css/style.css\">
<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\"
      href=\"/assets/toto/css/htmlize.css\">"
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :exclude "tata\.org")
        ("org-assets"
         :base-extension "jpg\\|gif\\|png\\|svg\\|pdf"
         :publishing-function org-publish-attachment
         :base-directory "__TEST_DIR__/src"
         :publishing-directory "__TEST_DIR__/public_html"
         :recursive nil)
        ("news"
         :base-extension "org"
         :publishing-function org-html-publish-to-html
         :base-directory "__TEST_DIR__/src/news"
         :publishing-directory "__TEST_DIR__/public_html/news"
         :recursive t
         :section-numbers nil
         :with-toc nil
         :html-postamble "<footer>Published by Fronde.</footer>"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\"
      href=\"/assets/toto/css/style.css\">
<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\"
      href=\"/assets/toto/css/htmlize.css\">
<link rel=\"alternate\" type=\"application/atom+xml\" title=\"Atom 1.0\"
      href=\"/feeds/index.xml\" />"
         :html-head-include-default-style nil
         :html-head-include-scripts nil)
        ("news-assets"
         :base-extension "jpg\\|gif\\|png\\|svg\\|pdf"
         :publishing-function org-publish-attachment
         :base-directory "__TEST_DIR__/src/news"
         :publishing-directory "__TEST_DIR__/public_html/news"
         :recursive t)
        ("test"
         :base-extension "org"
         :publishing-function org-html-publish-to-html
         :base-directory "__TEST_DIR__/titi/test"
         :publishing-directory "__TEST_DIR__/public_html/test"
         :recursive nil
         :section-numbers nil
         :with-toc nil
         :html-postamble "<footer>Published by Fronde.</footer>"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\"
      href=\"/assets/toto/css/style.css\">
<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\"
      href=\"/assets/toto/css/htmlize.css\">"
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :exclude "ugly\.org")
        ("test-assets"
         :base-extension "jpg\\|gif\\|png\\|svg\\|pdf"
         :publishing-function org-publish-attachment
         :base-directory "__TEST_DIR__/titi/test"
         :publishing-directory "__TEST_DIR__/public_html/test"
         :recursive nil)
        ("tata"
         :base-extension "org"
         :publishing-function org-html-publish-to-html
         :base-directory "__TEST_DIR__/titi/tutu/tata"
         :publishing-directory "__TEST_DIR__/public_html/tata"
         :recursive t
         :section-numbers nil
         :with-toc nil
         :html-postamble "<footer>Published by Fronde.</footer>"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\"
      href=\"/assets/toto/css/style.css\">
<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\"
      href=\"/assets/toto/css/htmlize.css\">"
         :html-head-include-default-style nil
         :html-head-include-scripts nil)
        ("tata-assets"
         :base-extension "jpg\\|gif\\|png\\|svg\\|pdf"
         :publishing-function org-publish-attachment
         :base-directory "__TEST_DIR__/titi/tutu/tata"
         :publishing-directory "__TEST_DIR__/public_html/tata"
         :recursive t)
        ("theme-toto"
         :base-directory "__TEST_DIR__/themes/toto"
         :base-extension "jpg\\|gif\\|png\\|js\\|css\\|otf\\|ttf\\|woff2?"
         :recursive t
         :publishing-directory "__TEST_DIR__/public_html/assets/toto"
         :publishing-function org-publish-attachment)
        ("website" :components ("org" "org-assets" "news" "news-assets" "test" "test-assets" "tata" "tata-assets" "theme-toto"))))

;; Load fronde lib
(load-file (expand-file-name "ox-gmi.el" "__TEST_DIR__/lib"))
(load-file (expand-file-name "ox-fronde.el" "__BASE_DIR__/lib/fronde/config"))
