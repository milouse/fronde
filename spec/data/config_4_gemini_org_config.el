;; Add org-mode to load path
(add-to-list 'load-path (expand-file-name "org-{{ org_version }}/lisp" "{{ test_dir }}/lib"))
;; Load last version of htmlize.el
(load-file (expand-file-name "htmlize.el" "{{ test_dir }}/lib"))

;; Current project options
(setq fronde-version "{{ version }}"
      fronde-current-work-dir "{{ test_dir }}"
      fronde-domain "https://test.com"
      user-mail-address ""
      user-full-name "Tata"
      org-html-metadata-timestamp-format "%A %-d of %B, %Y at %R"
      org-gmi-timestamp-format "%A %-d of %B, %Y at %R"
      org-publish-project-alist
      `(("src"
         :base-directory "{{ test_dir }}/src"
         :base-extension "org"
         :fronde-base-uri "https://test.com/src/"
         :gemini-head ""
         :gemini-postamble "📅 Last modification on %C
📝 Written by %a with %c, and published with %n"
         :publishing-directory "{{ test_dir }}/public_gmi/src"
         :publishing-function org-gmi-publish-to-gemini
         :recursive t
         :section-numbers nil
         :with-toc nil)
        ("src-assets"
         :base-directory "{{ test_dir }}/src"
         :base-extension "gif\\|jpg\\|png\\|svg\\|pdf"
         :publishing-directory "{{ test_dir }}/public_gmi/src"
         :publishing-function org-publish-attachment
         :recursive t)
        ("news"
         :base-directory "{{ test_dir }}/news"
         :base-extension "org"
         :fronde-base-uri "https://test.com/news/"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\"
      href=\"https://test.com/assets/my-theme/css/style.css\">
<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\"
      href=\"https://test.com/assets/my-theme/css/htmlize.css\">
<link rel=\"alternate\" type=\"application/atom+xml\" title=\"news\"
      href=\"https://test.com/news/feeds/index.xml\" />"
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-postamble "<p><span class=\"author\">Written by %a</span>
with %c, and published with %N</p>
<p class=\"date\">Last modification on %C</p>
<p class=\"validation\">%v</p>"
         :publishing-directory "{{ test_dir }}/public_html/news"
         :publishing-function org-html-publish-to-html
         :recursive t
         :section-numbers nil
         :with-toc nil)
        ("news-assets"
         :base-directory "{{ test_dir }}/news"
         :base-extension "gif\\|jpg\\|png\\|svg\\|pdf"
         :publishing-directory "{{ test_dir }}/public_html/news"
         :publishing-function org-publish-attachment
         :recursive t)
        ("other"
         :base-directory "{{ test_dir }}/other"
         :base-extension "org"
         :fronde-base-uri "https://test.com/other/"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\"
      href=\"https://test.com/assets/my-theme/css/style.css\">
<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\"
      href=\"https://test.com/assets/my-theme/css/htmlize.css\">"
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-postamble "<p><span class=\"author\">Written by %a</span>
with %c, and published with %N</p>
<p class=\"date\">Last modification on %C</p>
<p class=\"validation\">%v</p>"
         :publishing-directory "{{ test_dir }}/public_html/other"
         :publishing-function org-html-publish-to-html
         :recursive t
         :section-numbers nil
         :with-toc nil)
        ("other-assets"
         :base-directory "{{ test_dir }}/other"
         :base-extension "gif\\|jpg\\|png\\|svg\\|pdf"
         :publishing-directory "{{ test_dir }}/public_html/other"
         :publishing-function org-publish-attachment
         :recursive t)
        ("theme-my-theme"
         :base-directory "{{ test_dir }}/themes/my-theme"
         :base-extension "css\\|js\\|gif\\|jpg\\|png\\|svg\\|otf\\|ttf\\|woff2?"
         :publishing-directory "{{ test_dir }}/public_html/assets/my-theme"
         :publishing-function org-publish-attachment
         :recursive t)
        ("website" :components ("src" "src-assets" "news" "news-assets" "other" "other-assets" "theme-my-theme"))))

;; Load fronde lib
(load-file (expand-file-name "ox-gmi.el" "{{ test_dir }}/lib"))
(load-file (expand-file-name "ox-fronde.el" "{{ base_dir }}/lib/fronde/config/data"))
