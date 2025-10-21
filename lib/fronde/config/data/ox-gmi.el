;;; ox-gmi.el --- Gemini Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Étienne Pflieger

;; Author: Étienne Pflieger <etienne@pflieger.bzh>
;; Created: 29 November 2020
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: wp
;; Homepage: https://git.umaneti.net/ox-gmi.el/

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Gemini back-end for Org exporter, based on
;; `markdown' back-end.  It also heavily depends on the `ascii' back-end.

;;; Installation:

;; Obviously, this package depends on `org-mode', which must be loaded first.

;; You first need to download this file.  We recommend you to put it
;; somewhere under your `user-emacs-directory' folder (often ~/.emacs.d).

;; cd ~/.emacs.d
;; curl -O https://git.umaneti.net/fronde/plain/lib/fronde/config/data/ox-gmi.el

;; Then, you may want to compile it. You can do so with the Emacs command
;; M-x byte-compile-file RET ~/.emacs.d/ox-gmi.el

;; When it's done, you can add the following line in your GNU Emacs config
;; file:
;; (load-file "~/.emacs.d/ox-gmi.elc")

;; And everything should be fine.


;;; Code:

(require 'ox-ascii)
(require 'ox-publish)

(declare-function gemini-mode "gemini")


;;; Define Back-End

(org-export-define-derived-backend 'gmi 'ascii
  :menu-entry
  '(?g "Export to Gemini"
       ((?G "To temporary buffer"
            (lambda (a s v b) (org-gmi-export-as-gemini a s v)))
        (?g "To file"
            (lambda (a s v b) (org-gmi-export-to-gemini a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-gmi-export-to-gemini t s v)
                (org-open-file (org-gmi-export-to-gemini nil s v)))))))
  :translate-alist '((center-block . org-gmi-center)
                     (example-block . org-gmi-preformatted-block)
                     (export-block . org-gmi-export-block)
                     (fixed-width . org-gmi-preformatted-block)
                     (footnote-reference . org-gmi-footnote-reference)
                     (headline . org-gmi-headline)
                     (inner-template . org-gmi-inner-template)
                     (item . org-gmi-item)
                     (keyword . org-gmi-keyword)
                     (line-break . org-gmi-line-break)
                     (link . org-gmi-link)
                     (paragraph . org-gmi-paragraph)
                     (quote-block . org-gmi-quote-block)
                     (section . org-gmi-section)
                     (src-block . org-gmi-preformatted-block)
                     (template . org-gmi-template))
  :options-alist '((:creator "CREATOR" nil org-gmi-creator-string)
                   (:description "DESCRIPTION" nil nil parse)
                   (:keywords "KEYWORDS" nil nil parse)
                   (:subtitle "SUBTITLE" nil nil parse)
                   (:gemini-postamble
                    nil "gemini-postamble" org-gmi-postamble)
                   (:gemini-timestamp-format
                    nil nil org-gmi-timestamp-format)
                   (:gemini-postamble-format
                    nil nil org-gmi-postamble-format)))



;;; Inner Variables

(defvar org-gmi--links-in-section '()
  "AList storing all links in current section.")



;;; User Configuration Variables

(defgroup org-export-gmi nil
  "Options for exporting Org mode files to Gemini."
  :tag "Org Export Gemini"
  :group 'org-export)

(defcustom org-gmi-postamble 't
  "Non-nil means insert a postamble in Gemini export.

When set to `auto', check against the
`org-export-with-author/email/creator/date' variables to set the
content of the postamble.  When set to a string, use this string
as the postamble.  When t, insert a string as defined by the
formatting string in `org-gmi-postamble-format'.

When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

Setting :gemini-postamble in publishing projects will take
precedence over this variable."
  :group 'org-export-gmi
  :type '(choice (const :tag "No postamble" nil)
                 (const :tag "Auto postamble" auto)
                 (const :tag "Default formatting string" t)
                 (string :tag "Custom formatting string")
                 (function :tag "Function (must return a string)")))

(defcustom org-gmi-postamble-format
  '(("en" "📝 %a with %c\n📅 %d")
    ("fr" "📝 %a avec %c\n📅 %d"))
  "Alist of languages and format strings for the Gemini postamble.

The first element of each list is the language code, as used for
the LANGUAGE keyword.  See `org-export-default-language'.

The second element of each list is a format string to format the
postamble itself.  This format string can contain these elements:

  %t stands for the title.
  %s stands for the subtitle.
  %a stands for the author's name.
  %e stands for the author's email.
  %d stands for the date.
  %k stands for the keywords list.
  %l stands for the language code name.
  %x stands for the description.
  %c will be replaced by `org-gmi-creator-string'.
  %T will be replaced by the export time.
  %C will be replaced by the last modification time.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-gmi
  :type '(repeat
          (list (string :tag "Language")
                (string :tag "Format string"))))

(defcustom org-gmi-creator-string
  (format "GNU/Emacs %s (Org mode %s)"
          emacs-version
          (if (fboundp 'org-version) (org-version) "unknown version"))
  "Information about the creator of the Gemini document.
This option can also be set on with the CREATOR keyword."
  :group 'org-export-gmi
  :type '(string :tag "Creator string"))

(defcustom org-gmi-timestamp-format "%Y-%m-%d %a %H:%M"
  "Format used for timestamps in postamble.
See `format-time-string' for more information on its components."
  :group 'org-export-gmi
  :type 'string)



;;; Inner Functions

(defun org-gmi--build-headline (level title &optional tags)
  "Generate a headline TITLE for the given LEVEL.
TAGS are the tags set on the section."
  (let ((level-mark (make-string level ?#)))
    (concat level-mark " " title tags "\n\n")))

(defun org-gmi--build-links-list (links)
  "Return a string describing a list of links.
LINKS is an alist like `org-gmi--links-in-section'"
  (let (links-list)
    (string-trim-right
     (apply #'concat
            (reverse
             (dolist (link-data links links-list)
               (push (apply #'org-gmi--format-link link-data)
                     links-list)))))))

(defun org-gmi--build-toc (info &optional depth)
  "Return a table of contents.
INFO is a plist used as a communication channel.
Optional argument DEPTH, when non-nil, is an integer specifying the
depth of the table."
  (mapconcat
   (lambda (headline)
     (let* ((prefix
             (if (not (org-export-numbered-headline-p headline info)) ""
               (concat
                (mapconcat 'number-to-string
                           (org-export-get-headline-number headline info)
                           ".")
                ". ")))
            (title (org-export-data-with-backend
                    (org-export-get-alt-title headline info)
                    (org-export-toc-entry-backend 'gmi)
                    info))
            (tags (and (plist-get info :with-tags)
                       (not (eq 'not-in-toc (plist-get info :with-tags)))
                       (org-make-tag-string
                        (org-export-get-tags headline info)))))
       (concat prefix title tags)))
   (org-export-collect-headlines info depth) "\n"))

(defun org-gmi--build-postamble (info)
  "Return document postamble as a string, or nil.
INFO is a plist used as a communication channel."
  (let ((section (plist-get info :gemini-postamble))
        (spec (org-gmi--format-spec info)))
    (when section
      (let ((section-contents
             (if (functionp section) (funcall section info)
               (cond
                ((stringp section) (format-spec section spec))
                ((eq section 'auto)
                 (let ((date (cdr (assq ?d spec)))
                       (author (cdr (assq ?a spec)))
                       (email (cdr (assq ?e spec)))
                       (creator (cdr (assq ?c spec))))
                   (concat
                    (and (plist-get info :with-date)
                         (org-string-nw-p date)
                         (format "📅 %s\n" date))
                    (and (plist-get info :with-author)
                         (org-string-nw-p author)
                         (format "📝 %s\n" author))
                    (and (plist-get info :with-email)
                         (org-string-nw-p email)
                         (format "💌 %s\n" email))
                    (and (plist-get info :time-stamp-file)
                         (format
                          "🕓 %s\n"
                          (format-time-string
                           (plist-get info :gemini-timestamp-format))))
                    (and (plist-get info :with-creator)
                         (org-string-nw-p creator)
                         (format "⚙ %s\n" creator)))))
                (t
                 (let ((formats (plist-get info :gemini-postamble-format))
                       (language (plist-get info :language)))
                   (format-spec
                    (cadr (or (assoc-string language formats t)
                              (assoc-string "en" formats t)))
                    spec)))))))
        (when (org-string-nw-p section-contents)
          (format "\n\n--\n%s\n"
                  (org-element-normalize-string section-contents)))))))

(defun org-gmi--format-spec (info)
  "Return format specification for postamble.
INFO is a plist used as a communication channel."
  (let ((timestamp-format (plist-get info :gemini-timestamp-format)))
    `((?t . ,(org-export-data (plist-get info :title) info))
      (?s . ,(org-export-data (plist-get info :subtitle) info))
      (?d . ,(org-export-data (org-export-get-date info timestamp-format) info))
      (?T . ,(format-time-string timestamp-format))
      (?a . ,(org-export-data (plist-get info :author) info))
      (?e . ,(org-export-data (plist-get info :email) info))
      (?c . ,(plist-get info :creator))
      (?C . ,(let ((file (plist-get info :input-file)))
               (format-time-string
                timestamp-format
                (and file (file-attribute-modification-time
                           (file-attributes file))))))
      (?k . ,(org-export-data (plist-get info :keywords) info))
      (?l . ,(org-export-data (plist-get info :language) info))
      (?x . ,(org-export-data (plist-get info :description) info)))))

(defun org-gmi--format-paragraph (paragraph &optional prefix)
  "Transcode PARAGRAPH into Gemini format.
If PREFIX is non-nil, add it at the beginning of each lines."
  (replace-regexp-in-string
   "^\\s-?" (or prefix "")
   (org-trim
    (replace-regexp-in-string "\r?\n\\([^\r\n]\\)" " \\1" paragraph))))

(defun org-gmi--format-link (dest label &optional reference)
  "Return a formatted link pointing to DEST with the given LABEL.

When REFERENCE is given, it is added between bracket to generate single link
entry in a links list."
  (if reference
      (format "=> %s [%d] %s\n" dest reference label)
    (format "=> %s %s\n" dest label)))

(defun org-gmi--link-alone-on-line-p (link)
  "Return t if the given LINK occupies its whole line."
  (let* ((link-start (org-element-property :begin link))
         (link-end (org-element-property :end link))
         (full-link (buffer-substring-no-properties link-start link-end))
         (raw-link (org-element-property :raw-link link)))
    (save-excursion
      (org-goto-line (org-current-line link-start))
      (let ((line-content (org-trim (thing-at-point 'line t))))
        (or (string= raw-link line-content)
            (string= full-link line-content))))))

(defun org-gmi--extract-picture-label (link info)
  "Extract a worthy label for the given LINK, when HREF points to a picture."
  (let* ((paragraph (org-element-parent link))
         (alt (plist-get
               (org-export-read-attribute :attr_gmi paragraph)
               :alt))
         (caption (org-export-data (org-export-get-caption paragraph) info)))
    (if (org-string-nw-p caption)
        caption
      alt)))

(defun org-gmi--number-to-utf8-exponent (number)
  "Convert a NUMBER to its utf8 exponent display."
  (let ((exponents '((?1 . "¹")
                     (?2 . "²")
                     (?3 . "³")
                     (?4 . "⁴")
                     (?5 . "⁵")
                     (?6 . "⁶")
                     (?7 . "⁷")
                     (?8 . "⁸")
                     (?9 . "⁹")
                     (?0 . "⁰"))))
    (mapconcat
     (lambda (digit) (cdr (assoc digit exponents)))
     (number-to-string number)
     "")))


;;; Transcode Functions

(defun org-gmi-preformatted-block (block _contents info)
  "Transcode BLOCK element into Gemini format.
INFO is a plist used as a communication channel."
  (let ((language (org-export-data (org-element-property :language block) info))
        (caption (org-export-data (org-export-get-caption block) info)))
    (setq caption (if (org-string-nw-p caption)
                      (format "%s - %s" language caption)
                    language))
    (format "```%s\n%s```\n"
            caption
            (org-remove-indentation
             (org-export-format-code-default block info)))))

(defun org-gmi-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to Gemini."
  (if (member (org-element-property :type export-block) '("GEMINI" "GMI"))
      (org-remove-indentation (org-element-property :value export-block))))

(defun org-gmi-center (_center contents info)
  "Transcode a CENTER block from Org to Gemini.
CONTENTS is the block value.  INFO is a plist holding contextual
information."
  (format "```\n%s```"
          (org-ascii--fill-string contents 80 info 'center)))

(defun org-gmi-entity (_entity contents _info)
  "Transcode an ENTITY object from Org to Gemini.
CONTENTS is the entity itself."
  contents)

(defun org-gmi-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to Gemini.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-gmi--number-to-utf8-exponent
   (org-export-get-footnote-number footnote-reference info)))

(defun org-gmi-headline (headline contents info)
  "Transcode HEADLINE element into Gemini format.
CONTENTS is the headline value.  INFO is a plist used as
a communication channel."
  (let* ((level (1+ (org-export-get-relative-level headline info)))
         (title (org-export-data (org-element-property :title headline) info))
         (todo (and (plist-get info :with-todo-keywords)
                    (let ((todo (org-element-property :todo-keyword
                                                      headline)))
                      (and todo (concat (org-export-data todo info) " ")))))
         (tags (and (plist-get info :with-tags)
                    (let ((tag-list (org-export-get-tags headline info)))
                      (and tag-list
                           (concat "     " (org-make-tag-string tag-list))))))
         (priority
          (and (plist-get info :with-priority)
               (let ((char (org-element-property :priority headline)))
                 (and char (format "[#%c] " char)))))
         ;; Headline text without tags.
         (heading (concat todo priority title)))
    (if
        ;; Cannot create a headline.  Fall-back to a list.
        (or (org-export-low-level-p headline info)
            (> level 3)) ;; Gemtext only allow 3 levels of title
        (let ((bullet
               (if (not (org-export-numbered-headline-p headline info)) "*"
                 (concat (number-to-string
                          (car (last (org-export-get-headline-number
                                      headline info))))
                         "."))))
          (concat bullet (make-string (- 4 (length bullet)) ?\s)
                  heading tags "\n\n" contents))
      ;; Else
      (concat (org-gmi--build-headline level heading tags) contents))))

(defun org-gmi-inner-template (contents info)
  "Return body of document after converting it to Gemini syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Document contents.
   contents
   ;; Footnotes section.
   (let ((definitions (org-export-collect-footnote-definitions info)))
     (when definitions
       (concat
        "\n\n"
        (org-gmi--build-headline
         2 (org-ascii--translate "Footnotes" info))
        (mapconcat
         (lambda (ref)
           (let ((id (car ref))
                 (def (nth 2 ref)))
             (format "%s %s" (org-gmi--number-to-utf8-exponent id)
                     (replace-regexp-in-string
                      "\r?\n" " "
                      (org-trim (org-export-data def info))))))
         definitions "\n\n"))))))

(defun org-gmi-template (contents info)
  "Return body of document after converting it to Gemini syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Document title.
   (org-gmi--build-headline
    1 (org-export-data (plist-get info :title) info))
   ;; TOC
   (let* ((depth (plist-get info :with-toc))
          (toc-contents (when depth
                          (org-gmi--build-toc
                           info (and (wholenump depth) depth)))))
     (when (org-string-nw-p toc-contents)
       (concat
        (org-gmi--build-headline 2 (org-ascii--translate "Table of Contents" info))
        toc-contents
        "\n\n\n")))
   ;; Document contents.
   contents
   ;; Postamble
   (org-gmi--build-postamble info)))

(defun org-gmi-item (item contents info)
  "Transcode ITEM element into Gemini format.
CONTENTS is the item value.  INFO is a plist used as a
communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
         (struct (org-element-property :structure item))
         (bullet (if (not (eq type 'ordered)) "*"
                   (concat (number-to-string
                            (car (last (org-list-get-item-number
                                        (org-element-property :begin item)
                                        struct
                                        (org-list-prevs-alist struct)
                                        (org-list-parents-alist struct)))))
                           "."))))
    (concat bullet
            " "
            (pcase (org-element-property :checkbox item)
              (`on "[X] ")
              (`trans "[-] ")
              (`off "[ ] "))
            (let ((tag (org-element-property :tag item)))
              (and tag (format "%s: " (org-export-data tag info))))
            (and contents (org-trim contents)))))

(defun org-gmi-keyword (keyword _contents _info)
  "Transcode a KEYWORD element into Gemini format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (when (member (org-element-property :key keyword) '("GEMINI" "GMI"))
    (org-element-property :value keyword)))

(defun org-gmi-line-break (_line-break _contents _info)
  "Transcode LINE-BREAK object into Gemini format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  " ")

(defun org-gmi-link (link desc info)
  "Transcode a LINK object from Org to Gemini.
DESC is the description part of the link, or the empty string."
  (let ((link-type (org-element-property :type link))
        (link-path (org-element-property :path link))
        href lang)
    ;; Cleanup path
    (setq href
          (if (member link-type '("file" "fuzzy"))
              (if (string= ".org" (downcase (file-name-extension link-path ".")))
                  ;; Replace local org file to gmi files during publication
                  (format "%s.gmi" (file-name-sans-extension link-path))
                link-path)
            (org-element-property :raw-link link)))
    (when (string= (substring href 0 5) "i18n:")
      (pcase-let ((`(,path ,raw-lang) (split-string (substring href 5) "::")))
        (setq href path lang raw-lang)))
    (let* ((scheme (car (split-string href ":" t)))
           ;; Avoid cut lines in link labels
           (raw-label (replace-regexp-in-string "\r?\n" " " (or desc href)))
           (label (if (and (not (member href (list desc scheme))) ;; relative link
                           (not (member (downcase scheme)
                                        '("gemini" "file"))))
                      (format "%s (%s)" raw-label (upcase scheme))
                    raw-label)))
      (when lang (setq label (format "%s [%s]" label lang)))
      ;; Handle pictures
      (let* ((dest-ext (file-name-extension href))
             (found-label
              (when (and (not desc)
                         (member (or (and dest-ext (downcase dest-ext)) "")
                                 '("jpeg" "jpg" "png" "gif" "svg" "webp" "avif")))
                (org-gmi--extract-picture-label link info))))
        (when found-label (setq label found-label)))
      ;; Do we need to add the link at the end of the section or should it be
      ;; directly printed in its own line?
      (if (org-gmi--link-alone-on-line-p link)
          (org-gmi--format-link href label)
        ;; As links are specific for a section, which should not be that long (?),
        ;; we will always use the first label encountered for a link as
        ;; reference.
        (let ((link-data (assoc href org-gmi--links-in-section)))
          (unless link-data
            (setq link-data (list href label
                                  ;; Default next-reference
                                  (1+ (length org-gmi--links-in-section))))
            (add-to-list 'org-gmi--links-in-section link-data t))
          (format "%s[%d]" raw-label (caddr link-data)))))))

(defun org-gmi-paragraph (_paragraph contents _info)
  "Transcode PARAGRAPH element into Gemini format.
CONTENTS is the paragraph value."
  (org-gmi--format-paragraph contents))

(defun org-gmi-quote-block (_quote-block contents _info)
  "Transcode QUOTE-BLOCK element into Gemini format.
CONTENTS is the quote-block value."
  (org-gmi--format-paragraph contents "> "))

(defun org-gmi-section (_section contents _info)
  "Transcode SECTION into Gemini format.
CONTENTS is the section value."
  (let ((output
         (format "%s\n%s"
          contents
          (org-gmi--build-links-list org-gmi--links-in-section))))
    (setq org-gmi--links-in-section '()) ;; Reset link list
    output))


;;; Interactive function

;;;###autoload
(defun org-gmi-export-as-gemini (&optional async subtreep visible-only)
  "Export current buffer to a Gemini buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Gemini Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer
    'gmi "*Org Gemini Export*"
    async subtreep visible-only nil nil
    (lambda ()
      (if (featurep 'gemini-mode)
          (gemini-mode)
        (text-mode)))))

;;;###autoload
(defun org-gmi-export-to-gemini (&optional async subtreep visible-only)
  "Export current buffer to a Gemini file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".gmi" subtreep)))
    (org-export-to-file 'gmi outfile async subtreep visible-only)))


;;;###autoload
(defun org-gmi-publish-to-gemini (plist filename pub-dir)
  "Publish an org file to Gemini.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'gmi filename ".gmi" plist pub-dir))

(provide 'ox-gmi)

;;; ox-gmi.el ends here
