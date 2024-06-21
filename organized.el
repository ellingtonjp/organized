(defvar organized--heading-regexp "^\\(\*+\\)\\([[:space:]]+\\)\\(.*\\)")
(defvar organized--structure-begin-regexp "^[[:space:]]*#\\+begin_")
(defvar organized--structure-end-regexp "^[[:space:]]*#\\+end_")
(defvar organized--drawer-regexp "^[[:space:]]*:.*:[[:space:]]*$")
(defvar organized--drawer-end-regexp "^[[:blank:]]*:END:[[:blank:]]*$")
(defvar organized--scheduled-regexp "^[[:blank:]]*SCHEDULED:.*$")
(defvar organized--deadline-regexp "^[[:blank:]]*DEADLINE:.*$")

;; note: intentionally not supporting * as a list item!
(defvar organized--list-item-regexp "^[[:space:]]*\\(- \\|\\+ \\|[0-9]+\\. \\|[0-9]+) \\|[a-zA-Z]+\\. \\|[a-zA-Z]+) \\)")
(defvar organized--list-item-empty-regexp (concat organized--list-item-regexp "[[:space:]]*$"))

(defvar organized--downcase-ignore '(":.*:"))
(defvar organized--downcase '(
			      "a"
			      "an"
			      "the"
			      "and"
			      "but"
			      "for"
			      "nor"
			      "or"
			      "so"
			      "yet"
			      "as"
			      "at"
			      "by"
			      "for"
			      "in"
			      "of"
			      "off"
			      "on"
			      "per"
			      "to"
			      "up"
			      "via"
			      "vs"
			      ))

(defgroup organized nil
  "Customization options for org file formatting."
  :prefix "organized-"
  :group 'org)

(defcustom organized-heading-padding-above 2
  "Number of blank lines above org headings"
  :type 'integer
  :group 'organized)

(defcustom organized-heading-padding-above-no-content 1
  "Number of blank lines between headings with no content"
  :type 'integer
  :group 'organized)


(defcustom organized-heading-padding-below 1
  "Number of blank lines below org headings"
  :type 'integer
  :group 'organized)

(defcustom organized-heading-padding-between-drawer 0
  "Blank lines between heading and drawer, if it exists"
  :type 'integer
  :group 'organized)

(defcustom organized-heading-padding-between-scheduled 0
  "Blank lines between heading and scheduled, if it exists"
  :type 'integer
  :group 'organized)

(defcustom organized-heading-padding-between-deadline 0
  "Blank lines between heading and deadline, if it exists"
  :type 'integer
  :group 'organized)

(defcustom organized-heading-padding-below-drawer 1
  "Blank lines after a drawer"
  :type 'integer
  :group 'organized)

(defcustom organized-heading-padding-below-scheduled 1
  "Blank lines after scheduled"
  :type 'integer
  :group 'organized)

(defcustom organized-heading-padding-below-deadline 1
  "Blank lines after deadline"
  :type 'integer
  :group 'organized)

(defcustom organized-heading-padding-between-scheduled-deadline 0
  "Blank lines between schedule and deadline"
  :type 'integer
  :group 'organized)

(defcustom organized-title-case-todo-headings nil
  "Whether to use title case for TODO headings"
  :type 'boolean
  :group 'organized)

(defcustom organized-title-case-heading-tags nil
  "Whether to title case tags"
  :type 'boolean
  :group 'organized)

(defcustom organized-organizers
  '(organized-remove-leading-whitespace
    organized-pad-headings
    organized-title-case-headings
    organized-remove-empty-list-items
    whitespace-cleanup)
  "List of organizers to run"
  :type 'list
  :group 'organized)

(defun organized--acronym-p (word)
  (let ((case-fold-search nil))
    (string-match "^[A-Z][A-Z]" word)))

(defun organized--first-word (string)
  (when (string-match "\\b\\w+\\b" string)
    (match-string 0 string)))

(defun organized--heading-content (line)
  (when (string-match organized--heading-regexp line)
    (substring-no-properties (match-string 2 line))))

(defun organized--todo-keywords ()
  (let ((keyword-sequences org-todo-keywords)
      keywords)
    (dolist (sequence keyword-sequences)
      (dolist (word (cdr sequence))
	(when (string-match "^\\w+" word)
	  (push (match-string 0 word) keywords))))
    keywords))

(defun organized--title-case-line ()
  (save-excursion
    (save-restriction
      (when (and (not organized-title-case-heading-tags)
               (re-search-forward org-tag-group-re nil t))
        (narrow-to-region (pos-bol) (match-beginning 0)))
      (beginning-of-line)
      (let ((first-word t))
        (while (not (eolp))
	  (forward-word)
	  (let ((word (thing-at-point 'word)))
	    (when word
	      (cond
               ((string-match-p (car organized--downcase-ignore) word) nil)
	       ((organized--acronym-p word) nil)
	       (first-word (capitalize-word -1))
	       ((and (member (downcase word) organized--downcase))
	        (downcase-word -1))
	       (t (capitalize-word -1))))
	    (setq first-word nil)))))))

(defun organized--next-heading ()
  (re-search-forward organized--heading-regexp nil t))

(defun organized--previous-non-blank-line ()
  (beginning-of-line)	    ; fixes issue when point is already at EOL
  (re-search-backward "^.+$" nil t))

(defun organized--blank-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[[:space:]]*$")))

(defun organized--heading-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at organized--heading-regexp)))

(defun organized--heading-todo-line-p ()
  (save-excursion
    (beginning-of-line)
    (let ((heading (organized--heading-content
		    (thing-at-point 'line))))
      (and heading
	   (member
	    (organized--first-word heading)
	    (organized--todo-keywords))
	   t))))

(defun organized--drawer-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at organized--drawer-regexp)))

(defun organized--drawer-end-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at organized--drawer-end-regexp)))

(defun organized--scheduled-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at organized--scheduled-regexp)))

(defun organized--deadline-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at organized--deadline-regexp)))

(defun organized--drawer-beginning-p ()
  (save-excursion
    (beginning-of-line)
    (and (looking-at organized--drawer-regexp)
	 (not (looking-at organized--drawer-end-regexp)))))

(defun organized--next-end-of-drawer ()
  (re-search-forward organized--drawer-end-regexp nil t))

(defun organized--next-scheduled ()
  (re-search-forward organized--scheduled-regexp nil t))

(defun organized--next-deadline ()
  (re-search-forward organized--deadline-regexp nil t))

;; only finds valid drawers (ie with beginning and end)
(defun organized--has-immediate-drawer-p ()
  (save-excursion
    (forward-line 1)
    ;; forward until we find a non-blank line or end of buffer
    (while (and (organized--blank-line-p)
		(eq (forward-line 1) 0)))

    ;; true if drawer with beginning+end, nil otherwise
    (if (organized--drawer-beginning-p)
	(let ((found-header-or-drawer-end nil))
	  (while (not found-header-or-drawer-end)
	    (setq forward-result (forward-line 1))
	    (cond
	     ((eq forward-result 1) (setq found-header-or-drawer-end t))
	     ((organized--heading-line-p) (setq found-header-or-drawer-end t))
	     ((organized--drawer-end-p) (setq found-header-or-drawer-end t))))
	  (organized--drawer-end-p))
      nil)))

(defun organized--has-immediate-scheduled-p ()
  (save-excursion
    (forward-line 1)
    ;; forward until we find a non-blank line or end of buffer
    (while (and (organized--blank-line-p)
		(eq (forward-line 1) 0)))

    ;; true if on scheduled line, nil otherwise
    (if (organized--scheduled-p)
	t
	(let ((found-scheduled nil))
	  (while (not found-scheduled)
	    (cond
	     ((eq forward-result 1) (setq found-scheduled t))
	     ((organized--scheduled-p) (setq found-scheduled t)))
	    (setq forward-result (forward-line 1)))
	  (organized--scheduled-p)))))

(defun organized--has-immediate-deadline-p ()
  (save-excursion
    (forward-line 1)
    ;; forward until we find a non-blank line or end of buffer
    (while (and (organized--blank-line-p)
		(eq (forward-line 1) 0)))

    ;; true if on scheduled line, nil otherwise
    (if (organized--deadline-p)
	t
	(let ((found-deadline nil))
	  (while (not found-deadline)
	    (cond
	     ((eq forward-result 1) (setq found-deadline t))
	     ((organized--deadline-p) (setq found-deadline t)))
	    (setq forward-result (forward-line 1)))
	  (organized--deadline-p)))))

(defun organized--has-immediate-drawer-p ()
  (save-excursion
    (forward-line 1)
    ;; forward until we find a non-blank line or end of buffer
    (while (and (organized--blank-line-p)
		(eq (forward-line 1) 0)))

    ;; true if drawer with beginning+end, nil otherwise
    (if (organized--drawer-beginning-p)
	(let ((found-header-or-drawer-end nil))
	  (while (not found-header-or-drawer-end)
	    (setq forward-result (forward-line 1))
	    (cond
	     ((eq forward-result 1) (setq found-header-or-drawer-end t))
	     ((organized--heading-line-p) (setq found-header-or-drawer-end t))
	     ((organized--drawer-end-p) (setq found-header-or-drawer-end t))))
	  (organized--drawer-end-p))
      nil)))

(defun organized--list-item-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at organized--list-item-regexp)))

(defun organized--structure-begin-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at organized--structure-begin-regexp)))

(defun organized--structure-end-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at organized--structure-end-regexp)))

(defun organized--empty-list-item-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at organized--list-item-empty-regexp)))

(defun organized--count-blank-lines-above ()
  (save-excursion
    (let ((n 0))
      (while (and
	      (equal (forward-line -1) 0) ; stop if start of buffer
	      (organized--blank-line-p))
	(setq n (1+ n)))
      n)))

(defun organized--count-blank-lines-below ()
  (save-excursion
    (let ((n 0))
      (while (and
	      (equal (forward-line 1) 0) ; stop if end of buffer
	      (organized--blank-line-p)
	      (not (eobp)))
	(setq n (1+ n)))
      n)))

(defun organized--ensure-blank-lines-above (n)
  (setq col (current-column))
  (beginning-of-line)

  ;; we could calculate the required lines and pass that number
  ;; to `delete-backward-char`, but it wouldn't properly handle
  ;; lines with spaces/tabs/other whitespace. Instead, just loop
  ;; til its all gone.
  (while (> (organized--count-blank-lines-above) n)
    (delete-backward-char 1))

  ;; we don't have the same whitespace problem here, but lets
  ;; loop for consistency.
  (while (< (organized--count-blank-lines-above) n)
    (insert "\n"))

  (move-to-column col))

(defun organized--ensure-blank-lines-below (n)
  (while (> (organized--count-blank-lines-below) n)
    (save-excursion
      (if (equal 0 (forward-line))  ; be sure we moved before deleting
	  (kill-whole-line))))
  (while (< (organized--count-blank-lines-below) n)
    (save-excursion
      (forward-line 1)
      (insert "\n"))))

(defun organized--pad-heading ()
  (let (no-content padding-above)
    (save-excursion
      ;; go to previous non-blank and see if it's a header to determine if there's content
      (beginning-of-line)
      (when (and (organized--previous-non-blank-line)
		 (organized--heading-line-p))
	(setq no-content t)))

    (setq padding-above
       (if no-content
	   organized-heading-padding-above-no-content
	 organized-heading-padding-above))

    (setq padding-below organized-heading-padding-below)
    (cond ((organized--has-immediate-drawer-p)
	   (save-excursion
	     (setq padding-below organized-heading-padding-between-drawer)
	     (organized--next-end-of-drawer)
	     (organized--ensure-blank-lines-below organized-heading-padding-below-drawer)))
	  ((organized--has-immediate-scheduled-p)
	   (save-excursion
	     (setq padding-below organized-heading-padding-between-scheduled)
	     (organized--next-scheduled)
	     (if (organized--has-immediate-deadline-p)
		 (organized--ensure-blank-lines-below organized-heading-padding-between-scheduled-deadline)
		 (organized--ensure-blank-lines-below organized-heading-padding-below-scheduled))))
	  ((organized--has-immediate-deadline-p)
	   (save-excursion
	     (setq padding-below organized-heading-padding-between-deadline)
	     (organized--next-deadline)
	     (if (organized--has-immediate-scheduled-p)
		 (organized--ensure-blank-lines-below organized-heading-padding-between-scheduled-deadline)
		 (organized--ensure-blank-lines-below organized-heading-padding-below-deadline)))))
    (organized--ensure-blank-lines-above padding-above)
    (organized--ensure-blank-lines-below padding-below)))

(defun organized--clean-heading-str (heading-str)
  (replace-regexp-in-string organized--heading-regexp " " heading-str nil nil 1))

(defun organized--clean-heading ()
  (let* ((line-start (line-beginning-position))
	 (line-end (line-end-position))
	 (line-content (buffer-substring-no-properties line-start line-end)))
    (let ((new-heading (organized--clean-heading-str line-content)))
      (delete-region line-start line-end)
      (goto-char line-start)
      (insert new-heading))))

(defun organized--with-subword-disabled (fn &rest args)
  "Temporarily disable `subword-mode`, run FN with ARGS, then restore `subword-mode` to its previous state."
  (let ((subword-mode-enabled (bound-and-true-p subword-mode)))
    (when subword-mode-enabled
      (subword-mode -1))
    (unwind-protect
        (apply fn args)
      (when subword-mode-enabled
        (subword-mode 1)))))

;;;###autoload
(defun organized-pad-headings ()
  (save-excursion
    (goto-char (point-min))
    (while (organized--next-heading)
      (organized--pad-heading))))

;;;###autoload
(defun organized-remove-leading-whitespace ()
  "Removes leading whitespace from all lines except:
- lines between #+begin_* and #+end_
- list items
- lines directly following list items, ie no whitespace"
  (save-excursion
    (goto-char (point-min))
    (let ((in-structure nil)
	(in-list nil))
      (while (not (eobp))
	(cond
	 ((organized--structure-begin-line-p) (setq in-structure t))
	 ((organized--structure-end-line-p) (setq in-structure nil)))
	(cond
	 ((organized--list-item-line-p) (setq in-list t))
	 ((organized--blank-line-p) (setq in-list nil)))

	(unless (or in-structure in-list)
	  (beginning-of-line)
	  (delete-horizontal-space))
	(forward-line 1)))))

;;;###autoload
(defun organized-remove-empty-list-items ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (organized--empty-list-item-line-p)
	  (kill-whole-line)
	(forward-line 1)))))

;;;###autoload
(defun organized-title-case-headings ()
  (let ((subword-enabled-p (bound-and-true-p subword-mode)))
    (save-excursion
      (message "test")
      (subword-mode -1)
      (goto-char (point-min))
      (while (organized--next-heading)
	(if (or (not (organized--heading-todo-line-p))
	       (and (organized--heading-todo-line-p)
		  organized-title-case-todo-headings))
	    (organized--title-case-line)))
      (subword-mode subword-enabled-p))))

;;;###autoload
(defun organized-organize ()
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (dolist (organizer organized-organizers)
	(funcall organizer)))))

;;;###autoload
(define-minor-mode organized-mode
  "Minor mode for autoformatting org files"
  :lighter " OrgPretty"
  :global nil
  (if organized-mode
      (add-hook 'before-save-hook #'organized-organize nil t)
    (remove-hook 'before-save-hook #'organized-organize t)))

(provide 'organized)
