;; Add org-mode to load path
(add-to-list 'load-path (expand-file-name "org-{{ org_version }}/lisp" "{{ test_dir }}/lib"))
;; Load last version of htmlize.el
(load-file (expand-file-name "htmlize.el" "{{ test_dir }}/lib"))

;; Current project options
(setq fronde/version "{{ version }}"
      fronde/current-work-dir "{{ test_dir }}"
      user-mail-address ""
      user-full-name "Tata"
      org-html-metadata-timestamp-format "%A %-d of %B, %Y at %R"
      org-gmi-timestamp-format "%A %-d of %B, %Y at %R"
      org-publish-project-alist
      `(("src"
         :base-directory "{{ test_dir }}/src"
         :base-extension "org"
         :gemini-head ""
         :gemini-postamble "📅 Last modification on %C
📝 Written by %a with %c, and published with %n"
         :publishing-directory "{{ test_dir }}/public_gmi/src"
         :publishing-function org-gmi-publish-to-gemini
         :recursive nil
         :section-numbers nil
         :with-toc nil)
        ("src-assets"
         :base-directory "{{ test_dir }}/src"
         :base-extension "gif\\|jpg\\|png\\|svg\\|pdf"
         :publishing-directory "{{ test_dir }}/public_gmi/src"
         :publishing-function org-publish-attachment
         :recursive nil)
        ("news"
         :base-directory "{{ test_dir }}/src/news"
         :base-extension "org"
         :html-head "<link rel=\"alternate\" type=\"application/atom+xml\" title=\"Atom 1.0\"
      href=\"/feeds/index.xml\" />"
         :html-head-include-default-style t
         :html-head-include-scripts t
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
         :base-directory "{{ test_dir }}/src/news"
         :base-extension "gif\\|jpg\\|png\\|svg\\|pdf"
         :publishing-directory "{{ test_dir }}/public_html/news"
         :publishing-function org-publish-attachment
         :recursive t)
        ("tags"
         :base-directory "{{ test_dir }}/tags"
         :base-extension "org"
         :html-head ""
         :html-head-include-default-style t
         :html-head-include-scripts t
         :html-postamble "<p><span class=\"author\">Written by %a</span>
with %c, and published with %N</p>
<p class=\"date\">Last modification on %C</p>
<p class=\"validation\">%v</p>"
         :publishing-directory "{{ test_dir }}/public_html/tags"
         :publishing-function org-html-publish-to-html
         :recursive nil
         :section-numbers nil
         :with-toc nil)
        ("tags-assets"
         :base-directory "{{ test_dir }}/tags"
         :base-extension "gif\\|jpg\\|png\\|svg\\|pdf"
         :publishing-directory "{{ test_dir }}/public_html/tags"
         :publishing-function org-publish-attachment
         :recursive nil)
        ("website" :components ("src" "src-assets" "news" "news-assets" "tags" "tags-assets"))))

;; Load fronde lib
(load-file (expand-file-name "ox-gmi.el" "{{ test_dir }}/lib"))
(load-file (expand-file-name "ox-fronde.el" "{{ base_dir }}/lib/fronde/config/data"))
