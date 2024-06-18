(require 'organized)

(defvar test-file-regexp "data-test-")
(defvar tests
  (seq-filter
   (lambda (test-file)
     (string-match-p test-file-regexp test-file))
   (directory-files ".")))

(defun file-to-string (filepath)
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun run-test (filepath)
  (let* ((example (split-string (file-to-string filepath) "===\n"))
	 (test (nth 0 example))
	 (expected (nth 1 example)))
    (with-temp-buffer
      (insert test)
      (organized-organize)
      (expect (buffer-string) :to-equal expected))))

(defmacro organize-it (filepath)
  `(it ,filepath
     (run-test ,filepath)))

(defmacro describe-tests-for-directory (description directory)
  `(describe ,description
     ,@(mapcar (lambda (test-file)
		 `(it ,(format "should pass for %s" (file-name-nondirectory test-file))
		    (run-test ,test-file)))
	       (directory-files directory t test-file-regexp))))

(describe-tests-for-directory "organized-organize" "./tests")
