;; Add org-mode to load path
(add-to-list 'load-path (expand-file-name "org-{{ org_version }}/lisp" "{{ work_dir }}/lib"))
;; Load last version of htmlize.el
(load-file (expand-file-name "htmlize.el" "{{ work_dir }}/lib"))

;; Current project options
(setq fronde-version "{{ version }}"
      fronde-current-work-dir "{{ work_dir }}"
      fronde-domain "{{ domain }}"
      user-mail-address "{{ author.email }}"
      user-full-name "{{ author.name }}"
      org-html-metadata-timestamp-format "{{ long_date_fmt }}"
      org-gmi-timestamp-format "{{ long_date_fmt }}"
      org-publish-project-alist
      `({% for project in all_projects -%}
        ("{{ project.name }}"
         {%- for attribute in project.attributes %}
         :{{ attribute[0] }} {{ attribute[1] | cast_lisp_value: attribute[0] }}
         {%- endfor %})
        {% endfor -%}
        ("website" :components ("{{ all_projects | map: 'name' | join: '" "' | remove: '" "tags' }}"))))

;; Load fronde lib
(load-file (expand-file-name "ox-gmi.el" "{{ work_dir }}/lib"))
(load-file (expand-file-name "ox-fronde.el" "{{ fronde_data_dir }}"))
