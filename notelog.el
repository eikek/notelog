;;; nlog.el -- gather org notes to publish

(require 'org)
(require 'dash)
(require 'cl) ;; loop, lexical-let

(defvar notelog-remove-marker-tag t
  "Remove the tag from entries that is used to mark it as being
  published.")

(defvar notelog-date-property-name "DATE"
  "The property name specifying the date of the post in a
  subtree. If a property with this name exists and its value is a
  date, it is used to as creation date for the post. Otherwise
  the first timestamp found below the headline is used.")

(defvar notelog-filename-property "NLOG_FILENAME"
  "The property to use to generate the file name. If no such
  property is defined, it is generated from the title.")

(defvar notelog-apply-default-note-mods t
  "By default each note is processed such its title is the
  headline of the subtree and all other subtrees are promoted
  accordingly. Set this to nil to skip this.")

(defun notelog--next-visible-heading (&optional n)
  "Wrapper around `org-next-visible-heading' but return t if
moving was successful. Otherwise return nil."
  (let ((pos (point)))
    (org-next-visible-heading (or n 1))
    (and (/= (point) pos)
         (org-at-heading-p))))

(defun notelog--extract-headlines (markertag)
  "Searches current buffer for headlines marked with
 MARKERTAG. Create a buffer for each subtree and return all in a
 list."
  (unless (eq major-mode 'org-mode)
    (error "Buffer not in org mode."))
  (let ((bufcounter 0)
        (result))
    (show-all)
    (goto-char (point-min))
    (while (notelog--next-visible-heading)
      (when (member markertag (org-get-tags))
        (save-excursion
          (org-copy-subtree)
          ;;;;(org-narrow-to-subtree)
          ;;;;(copy-region-as-kill (point-min) (point-max))
          (with-current-buffer (get-buffer-create
                                (format "*notelog-subtree-%d*"
                                        (incf bufcounter)))
            (org-mode)
            (yank)
            (push (current-buffer) result)))))
    result))

(defun notelog--title->filename (title)
  "Create a string usable as filename from TITLE."
  (downcase
   (concat (replace-regexp-in-string "[^[:alpha:]0-9]" "_" title)
           ".org")))

(defun notelog--make-filename (&rest parts)
  "Calls `expand-file-name' for each string in PARTS."
  (-reduce (lambda (a b) (expand-file-name b a)) parts))

(defun notelog--fix-heading-levels! ()
  "Assumes that current buffer is a note buffer. Promotes all
  direct subtrees to level 1."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((heading-points))
     (while (let ((beg (point)))
              (org-forward-heading-same-level 1)
              (and (org-at-heading-p) (/= beg (point))))
       (push (point) heading-points))
     (dolist (beg heading-points)
       (goto-char beg)
       (-when-let (cur-level (org-current-level))
         (loop repeat (/ (- cur-level 1) (org-level-increment))
               do (org-promote-subtree)))))))

(defun notelog--heading-to-title! (meta)
  "Assumes that current buffer is a note buffer. Removes the
  first headline (level 1) and instead adds it as a #+TITLE
  property at the top of the file"
  (goto-char (point-min))
  (when (or (org-at-heading-p) (notelog--next-visible-heading))
    (forward-line 0)
    (delete-region (point) (point-at-eol))
    (goto-char (point-min))
    (insert "#+title: " (plist-get meta :title) "\n")))
;;; todo: remove drawers

(defun notelog--goto-first-heading ()
  (goto-char (point-min))
  (unless (or (org-at-heading-p) (notelog--next-visible-heading))
    (error "No heading found for this note.")))

(defun notelog--process-note (meta &optional modify-fn)
  "Applies the default modifications according to
`notelog-apply-default-note-mods' and uses META to call MODIFY-FN,
if specified."
  (notelog--goto-first-heading)
  (when notelog-apply-default-note-mods
    (notelog--heading-to-title! meta)
    (notelog--fix-heading-levels!))
  (when modify-fn
    (funcall modify-fn meta)))

(defun notelog--find-date-in-text ()
  "Find the first date/timestamp in current subtree. Assumes to
be in the note buffer."
  (org-with-wide-buffer
   (notelog--goto-first-heading)
   (while (and (not (org-at-timestamp-p))
               (search-forward-regexp "[[<]" nil t)))
   (when (org-at-timestamp-p)
     (let ((beg (point)))
       (when (search-forward-regexp "[]>]" nil t)
         (replace-regexp-in-string
          "[^0-9\\-]" ""
          (buffer-substring-no-properties beg (point))))))))

(defun notelog--string-trim (regexp str)
  (replace-regexp-in-string (concat regexp "$") ""
                            (replace-regexp-in-string (concat "^" regexp) "" str)))

(defun notelog--digits-only-date-p (str)
  (string-match-p "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$" str))

(defun notelog--date-string-p (str)
  (string-match-p "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$" str))

(defun notelog--normalize-date-str (str &optional noerror)
  (let ((trimmed (notelog--string-trim "[^0-9]" str)))
    (cond ((notelog--date-string-p trimmed) trimmed)
          ((notelog--digits-only-date-p trimmed)
           (format "%s-%s-%s"
                   (substring trimmed 0 4)
                   (substring trimmed 4 6)
                   (substring trimmed 6 8)))
          (t (if noerror nil (error "Unknown date string: %s" str))))))

(defun notelog--deduce-date-from-filename (file)
  "Tries to find a date in the given filename."
  (when file
    (loop for cand in (split-string file "[^0-9\\-]" t)
          for datestr = (notelog--normalize-date-str cand t)
          if datestr return datestr)))

(defun notelog--find-date (file)
  "Find a date string in current buffer."
  (or (car (org-property-values notelog-date-property-name))
      (notelog--find-date-in-text)
      (notelog--deduce-date-from-filename file)))

(defun notelog--make-note-data (file markertag)
  "Return data of current note buffer."
  (let ((parts (org-heading-components))
        (dateprop (org-property-values notelog-date-property-name)))
    (list :tags (if notelog-remove-marker-tag
                    (remove markertag (org-get-tags))
                  (org-get-tags))
          :date (notelog--find-date file)
          :nlog_filename (car (org-property-values notelog-filename-property))
          ;;;:extract "..." ;;; todo first 20 or something words from content
          :title (nth 4 parts))))

(defun notelog--make-note-filename (meta out &optional subfolder-fn)
  "Return a filename in directory OUT that is deduced from the
  buffer's content. The SUBFOLDER-FN is used to determine an
  optional subfolder below OUT. All missing directories are
  created, if necessary. The file name component is either
  created from the title property in META or the property value
  of `notelog-filename-property' is used."
  (let* ((title (plist-get meta :title))
         (fname (or (plist-get meta :nlog_filename)
                    (notelog--title->filename title)))
         (subf  (if subfolder-fn (or (funcall subfolder-fn meta) "") ""))
         (path (apply 'notelog--make-filename (append (cons out (-list subf))
                                                      (list fname)))))
    (let ((dir (file-name-directory path)))
      (unless (file-exists-p dir)
        (mkdir dir t)))
    path))


(defun notelog--lastmod (file)
  (nth 5 (file-attributes file)))

(defun notelog--file-newer-p (file1 file2)
  "Return t if file1 is newer than file2 according to
last-modification time."
  (if (and (file-exists-p file1)
           (file-exists-p file2))
      (org-time< (notelog--lastmod file2)
                 (notelog--lastmod file1))
    (not (file-exists-p file2))))

(defun notelog--compare-meta (meta1 meta2)
  (let ((date1 (or (plist-get meta1 :date)
                   (format-time-string "%Y-%m-%d" (plist-get meta1 :lastmod))))
        (date2 (or (plist-get meta2 :date)
                   (format-time-string "%Y-%m-%d" (plist-get meta2 :lastmod)))))
    (not (org-time< date1 date2))))

(defun notelog--create-index-org (metas index-template out)
  "Create the index.org file containing links to all note files
in META list. INDEX-TEMPLATE can be a file, it is used as a
template and the line '# --links' is replaced by the list of
note-links. If INDEX-TEMPLATE is a string not pointing to a file
it is used as the title of the generated file."
  (with-temp-buffer
    (if (and index-template (file-exists-p index-template))
        (progn
          (insert-file-contents index-template)
          (goto-char (point-min))
          (when (search-forward-regexp "^#\s*\\-\\-links" nil t)
            (forward-line 0)
            (delete-region (point) (point-at-eol))))
      (insert "#+title: " (or index-template "Index") "\n\n"))
    (org-mode)
    (setq metas (sort metas #'notelog--compare-meta))
    (dolist (meta metas)
      (let* ((target (plist-get meta :filename))
             (tags (plist-get meta :tags))
             (link (concat "file:" (substring target (1+ (length out))))))
        (insert "- ")
        (org-insert-link nil link (plist-get meta :title))
        (when tags
          (insert " ")
          (dolist (tag tags)
            (insert "/" tag "/ ")))
        (insert "\n")))
    (write-file (notelog--make-filename out "index.org"))))

(defun notelog--subdir-level (meta subfolder-fn)
  "Return the number of directories TARGET is below OUT."
  (if (not subfolder-fn) 0
    (let ((subfolder (funcall subfolder-fn meta)))
      (if (listp subfolder)
          (length subfolder)
        1))))

(defun notelog-create-notes (files markertag out &optional subfolder-fn modify-fn index-template)
  "Create a set of note files from the given list of FILES.

Each file in FILES is scanned for subtrees that are tagged with
MARKERTAG. A separate file is created out of each such subtree
and stored below OUT according to SUBFOLDER-FN.

SUBFOLDER-FN accepts one argument -- a meta data plist -- and
returns a list of names (or a single name) that are used as
subdirectories below OUT. OUT is the target directory.

MODIFY-FN is called in each note buffer before saving. It accepts
a meta data plist as its sole argument and may change the current
buffer. It can return a new meta plist (e.g. to change the title
property).

By default the notes are modified such that the highest headline
is converted into a #+title option. This can be skipped by
setting `notelog-apply-default-note-mods' to nil.

Finally a index.org files is created in OUT and the
INDEX-TEMPLATE file is used if available. If INDEX-TEMPLATE is
specified but not a file, it is used as the #+title for the
generated index.org file."
  (let ((metas))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (goto-char (point-min))
        (dolist (buf (notelog--extract-headlines markertag))
          (with-current-buffer buf
            (notelog--goto-first-heading)
            (let* ((meta0 (append (list :source file
                                        :lastmode (notelog--lastmod file)
                                        :out out)
                                  (notelog--make-note-data file markertag)))
                   (meta1 (plist-put meta0
                                     :subdir-level
                                     (notelog--subdir-level meta0 subfolder-fn)))
                   (meta2 (notelog--process-note meta1 modify-fn))
                   (meta (or meta2 meta1))
                   (target (notelog--make-note-filename meta out subfolder-fn)))
              (setq meta (append (list :filename target) meta))
              (when (notelog--file-newer-p file target)
                (write-file target))
              (push meta metas)))
          (kill-buffer buf))))
    (notelog--create-index-org metas index-template out)
    metas))

(defun notelog-subfolder-per-tag (tags)
  "Create a function to be used as 'subfolder-fn' with
`notelog-create-notes'. The argument is a list of tag names. The
function checks the tag list of the note and returns the tag name
if it is a member of the given TAGS list."
  (lexical-let ((ftags tags))
    (lambda (meta)
      (let ((taglist (plist-get meta :tags)))
        (loop for name in taglist
              if (member name ftags)
              return name)))))


(defun notelog--list-files (dir &optional filter)
  (delete nil
          (mapcar (lambda (entry)
                    (let ((fname (notelog--make-filename dir entry)))
                      (if filter
                          (and (funcall filter fname) fname)
                        fname)))
                  (directory-files dir))))

(defun notelog--expand-files (files-and-dirs)
  "Expand a list of files and directories in a list of org
files. Each directory is included non-recursively."
  (cl-flet ((org-file-filter (file)
                          (and (file-regular-p file)
                               (string= "org" (file-name-extension file)))))
    (mapcan (lambda (f)
              (if (file-directory-p f)
                  (notelog--list-files f #'org-file-filter)
                (and (org-file-filter f) (list f))))
            files-and-dirs)))


;;;;;

(defvar notelog-default-output-directory nil
  "The output directory.")

(defvar notelog-default-modify-fns nil
  "A list of functions that are called for each note buffer. It
  must accept one argument, the meta data plist of the current
  note.")

(defvar notelog-default-subfolder-fn nil
  "The subfolder-fn to use. You may set this to the result of
  `notelog-subfolder-per-tag' which accepts a list of tags and
  returns a function that will return a tag name if the current
  note is tagged with one of the tags given to
  `notelog-subfolder-per-tag'.")

(defvar notelog-default-marker-tag "pub"
  "The marker tag to filter notes.")

(defvar notelog-default-input-files nil
  "A list of org files and directories that is searched for notes
  to publish. If a directory is in the list, all org files in it
  are considered.")

(defvar notelog-default-index-template "My Notes"
  "A file to a org file that is used as a template when creating
  the index.org file. It should contain a line '# --lines' which
  is replaced by a list of links to the notes. If this value is
  not a file, it is used as the title for the generated file.")

(defun notelog-make-modify-fn (fns)
  "Create one modify function that calls each function in FNS
  feeding the output of the previous into the next (if
  non-nil)."
  (lexical-let ((funs fns))
    (lambda (meta)
      (reduce (lambda (ret fn)
                (or (funcall fn ret) ret))
              funs
              :initial-value meta))))

(defun notelog-generate-default-notes ()
  "Creates a log of notes gathered from
`notelog-default-input-files' which is written to
`notelog-default-output-directory'. Use standard org-publish to
publish the results in `notelog-default-output-directory'."
  (interactive)
  (unless notelog-default-output-directory
    (user-error "A output directory must be set. Configure notelog-default-output-directory."))
  (unless notelog-default-marker-tag
    (user-error "A marker tag must be set. Configure notelog-default-marker-tag."))
  (unless notelog-default-input-files
    (user-error "Input files must be set. Configure notelog-default-input-files."))
  (notelog-create-notes (notelog--expand-files notelog-default-input-files)
                        notelog-default-marker-tag
                        notelog-default-output-directory
                        notelog-default-subfolder-fn
                        (notelog-make-modify-fn notelog-default-modify-fns)
                        notelog-default-index-template))

(provide 'notelog)
