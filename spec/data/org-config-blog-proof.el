;; Load neruda lib
(load-file "__BASE_DIR__/lib/neruda/config/ox-neruda.el")
(neruda/init-export-variables "__TEST_DIR__" "__ORG_VERSION__")

(setq org-html-metadata-timestamp-format "%A %-d of %B, %Y at %H:%M"
      org-publish-project-alist
      `(("org"
         :base-directory "__TEST_DIR__/src"
         :base-extension "org"
         :recursive t
         :exclude "tata\.org"
         :publishing-directory "__TEST_DIR__/public_html"
         :publishing-function neruda/publish-to-html-and-customize-output
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\" href=\"style.css\"/>"
         :html-postamble "<footer>Published by Neruda.</footer>"
         :html-head-include-default-style nil
         :html-head-include-scripts nil)
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
         :publishing-function neruda/publish-to-html-and-customize-output
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\" href=\"style.css\"/>"
         :html-postamble "<footer>Published by Neruda.</footer>"
         :html-head-include-default-style nil
         :html-head-include-scripts nil)
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
         :publishing-function neruda/publish-to-html-and-customize-output
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\" href=\"style.css\"/>"
         :html-postamble "<footer>Published by Neruda.</footer>"
         :html-head-include-default-style nil
         :html-head-include-scripts nil)
        ("tata-assets"
         :base-directory "__TEST_DIR__/titi/tutu/tata"
         :base-extension "jpg\\|gif\\|png\\|svg\\|pdf"
         :recursive t
         :publishing-directory "__TEST_DIR__/public_html/tata"
         :publishing-function org-publish-attachment)
        ("theme"
         :base-directory "__TEST_DIR__/themes/toto"
         :base-extension "jpg\\|gif\\|png\\|js\\|css\\|otf\\|ttf\\|woff2?"
         :recursive t
         :publishing-directory "__TEST_DIR__/public_html/assets"
         :publishing-function org-publish-attachment)
        ("website" :components ("org" "org-assets" "test" "test-assets" "tata" "tata-assets" "theme"))))
