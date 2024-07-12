(defvar organized--heading-regexp "^\\(\*+\\)\\([[:space:]]+\\)\\(.*\\)")
(defvar organized--structure-begin-regexp "^[[:space:]]*#\\+begin_")
(defvar organized--structure-end-regexp "^[[:space:]]*#\\+end_")

;; note: intentionally not supporting * as a list item!
(defvar organized--list-item-regexp "^[[:space:]]*\\(- \\|\\+ \\|[0-9]+\\. \\|[0-9]+) \\|[a-zA-Z]+\\. \\|[a-zA-Z]+) \\)")
(defvar organized--list-item-empty-regexp (concat organized--list-item-regexp "[[:space:]]*$"))

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

(defcustom organized-title-case-todo-headings nil
  "Whether to use title case for TODO headings"
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
    (beginning-of-line)
    (let ((end-of-line (line-end-position))
	(first-word t))
      (while (and (< (point) end-of-line))
	(forward-word)
	(let ((word (thing-at-point 'word)))
	  (when word
	    (cond
	     ((organized--acronym-p word) nil)
	     (first-word (capitalize-word -1))
	     ((member (downcase word) organized--downcase)
	      (downcase-word -1))
	     (t (capitalize-word -1))))
	  (setq first-word nil))))))

(defun organized--next-heading ()
  (re-search-forward organized--heading-regexp nil t))

(defun organized--previous-non-blank-line ()
  (beginning-of-line) 			; fixes issue when point is already at EOL
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

    (organized--ensure-blank-lines-above padding-above)
    (organized--ensure-blank-lines-below organized-heading-padding-below)))

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

(defun organized-pad-headings ()
  (save-excursion
    (goto-char (point-min))
    (while (organized--next-heading)
      (organized--pad-heading))))

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

(defun organized-remove-empty-list-items ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (organized--empty-list-item-line-p)
	  (kill-whole-line)
	(forward-line 1)))))

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

(defun organized-organize ()
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (dolist (organizer organized-organizers)
	(funcall organizer)))))

(defun organized--with-subword-disabled (fn &rest args)
  "Temporarily disable `subword-mode`, run FN with ARGS, then restore `subword-mode` to its previous state."
  (let ((subword-mode-enabled (bound-and-true-p subword-mode)))
    (when subword-mode-enabled
      (subword-mode -1))
    (unwind-protect
        (apply fn args)
      (when subword-mode-enabled
        (subword-mode 1)))))

(define-minor-mode organized-mode
  "Minor mode for autoformatting org files"
  :lighter " OrgPretty"
  :global nil
  (if organized-mode
      (add-hook 'before-save-hook #'organized-organize nil t)
    (remove-hook 'before-save-hook #'organized-organize t)))

(provide 'organized)
