;;; csvq.el --- Emacs Extension for csvq

;; Copyright (C) 2018 Mithrandie <mithrandie@icloud.com>

;; Author: Mithrandie <mithrandie@icloud.com>
;; Version: 0.0.0

;;; Commentary

;; This package is a extensin for csvq (https://github.com/mithrandie/csvq).
;; You need to install the csvq command-line tool to use functions in this package.

(provide 'csvq)

;;; Variables

;; Buffer name to write logs of csvq execution.
(defvar csvq-log-buffer "*csvq-log*")

;; Default format for the csvq-insert function.
(defvar csvq-default-format "ORG")


(defun csvq-with-options (&optional query options)
  "Execute csvq with specific options."
  (interactive)
  (let ((args (csvq-read-args query options)))
    (apply 'csvq-exec args))
  (csvq-terminate)
  (csvq-open-log))

(defun csvq ()
  "Execute csvq."
  (interactive)
  (csvq-with-options nil ""))

(defun csvq-insert-to-buffer-with-options (&optional buffer query options)
  "Execute csvq with specific options and insert logs and result-set into the other buffer."
  (interactive)
  (unless buffer
    (setq buffer (read-string "buffer name: ")))
  (let ((log)
	(args (csvq-read-args query options)))
    (setq log (apply 'csvq-exec args))
    (get-buffer-create buffer)
    (switch-to-buffer buffer)
    (goto-char (point-max))
    (insert log))
  (csvq-terminate))

(defun csvq-insert-to-buffer (&optional output-format)
  "Execute csvq and insert logs and result-set into the other buffer."
  (interactive)
  (unless output-format
    (setq output-format csvq-default-format))
  (let ((options (format "-f %s -P" output-format)))
    (csvq-insert-to-buffer-with-options nil nil options)))

(defun csvq-insert-with-options (&optional query options)
  "Execute csvq with specific options and insert logs and result-set into the current buffer."
  (interactive)
  (let ((args (csvq-read-args query options)))
    (insert (apply 'csvq-exec args)))
  (csvq-terminate))

(defun csvq-insert (&optional output-format)
  "Execute csvq and insert logs and result-set formatted in default format into the current buffer."
  (interactive)
  (unless output-format
    (setq output-format csvq-default-format))
  (let ((options (format "-f %s -P" output-format)))
    (csvq-insert-with-options nil options)))

(defun csvq-insert-csv ()
  "Execute csvq and insert logs and result-set formatted in CSV into the current buffer."
  (interactive)
  (csvq-insert "CSV"))

(defun csvq-insert-tsv ()
  "Execute csvq and insert logs and result-set formatted in TSV into the current buffer."
  (interactive)
  (csvq-insert "TSV"))

(defun csvq-insert-json ()
  "Execute csvq and insert logs and result-set formatted in JSON into the current buffer."
  (interactive)
  (csvq-insert "JSON"))

(defun csvq-insert-jsonh ()
  "Execute csvq and insert logs and result-set formatted in JSONH into the current buffer."
  (interactive)
  (csvq-insert "JSONH"))

(defun csvq-insert-jsona ()
  "Execute csvq and insert logs and result-set formatted in JSONA into the current buffer."
  (interactive)
  (csvq-insert "JSONA"))

(defun csvq-insert-gfm ()
  "Execute csvq and insert logs and result-set formatted in GitHub Flavored Markdown table into the current buffer."
  (interactive)
  (csvq-insert "GFM"))

(defun csvq-insert-org ()
  "Execute csvq and insert logs and result-set formatted in Org-mode table into the current buffer."
  (interactive)
  (csvq-insert "ORG"))

(defun csvq-insert-text ()
  "Execute csvq and insert logs and result-set formatted in Org-mode table into the current buffer."
  (interactive)
  (csvq-insert "TEXT"))

(defun csvq-org-update ()
  "Execute csvq to the current Org-mode table and replace the table with the auto-selected result."
  (interactive)
  (csvq-org-replace nil nil t))

(defun csvq-org-replace (&optional query options auto-select)
  "Execute csvq to the current Org-mode table and replace the table with the result."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Not in Org-mode"))
  (unless (org-at-table-p)
    (user-error "No table at point"))
  (unless options
    (setq options (format "-f ORG")))
  (let ((args (csvq-read-args query options)))
    (setq query (nth 0 args))
    (setq options (nth 1 args)))
  (when auto-select
    (setq query (format "%s SELECT * FROM stdin;" query)))
  (let ((current-point (point))
	(view (csvq-org-table-exec query options)))
    (delete-region (org-table-begin) (org-table-end))
    (insert view)
    (goto-char (min current-point (point-max))))
  (org-table-align)
  (csvq-terminate))

(defun csvq-exec (query options)
  "Execute csvq."
  (csvq-set-log)
  (csvq-start (format "Execute query (options '%s'): %s" options query))
  (let ((args (csvq-parse-command-args query options)))
    (with-temp-buffer
      (let ((ret (apply 'call-process "csvq" nil t nil args)))
        (unless (zerop ret)
          (csvq-error (csvq-error-message)))
        (csvq-append-log (buffer-string))
        (buffer-string)))))

(defun csvq-org-table-exec (query options)
  "Execute csvq for a Org-mode table."
  (csvq-set-log)
  (csvq-start (format "Execute query for update (options '%s'): %s" options query))
  (let ((args (csvq-parse-command-args query options))
        (table (org-table-to-lisp (buffer-substring-no-properties (org-table-begin) (org-table-end)))))
    (with-temp-buffer
      (insert (orgtbl-to-csv table nil))
      (let ((ret (apply 'call-process-region (point-min) (point-max) "csvq" t t nil args)))
        (unless (zerop ret)
          (csvq-error (csvq-error-message)))
        (csvq-append-log (buffer-string))
        (csvq-filter-log (buffer-string))))))

(defun csvq-read-args (query options)
  (unless query
    (if (region-active-p)
      (setq query (buffer-substring-no-properties (region-beginning) (region-end)))
      (setq query (read-string "query: "))))
  (unless options
    (setq options (read-string "options: ")))
  (setq query (csvq-add-query-terminator query))
  (if (and (equal query "") (not (string-match "\-s" options)))
      (csvq-error "query is empty"))
  (list query options))

(defun csvq-parse-command-args (query options)
  (let ((args '())
        (option-list (split-string options nil t)))
    (dolist (elt option-list)
      (add-to-list 'args elt t))
    (if (equal query "")
      args
      (add-to-list 'args query t))))


(defun csvq-start (message)
  (csvq-append-log (format "[%s] %s\n" (current-time-string) message)))

(defun csvq-error (message)
  (let ((log (format "csvq error: %s" message)))
    (csvq-append-log (format "%s\n[%s] Query abnormally terminated.\n\n" log (current-time-string)))
    (user-error log)))

(defun csvq-terminate ()
    (csvq-append-log (format "[%s] Query terminated.\n\n" (current-time-string))))

(defun csvq-append-log (log)
  (with-current-buffer csvq-log-buffer
    (goto-char (point-max))
    (insert log)))

(defun csvq-set-log ()
  (get-buffer-create csvq-log-buffer))

(defun csvq-open-log ()
  (switch-to-buffer csvq-log-buffer)
  (goto-char (point-max)))

(defun csvq-error-message ()
  (csvq-last-line (buffer-string) (line-number-at-pos (point-max))))

(defun csvq-last-line (s line-number)
  (let ((line (csvq-trim-right (thing-at-point 'line))))
    (if (not (equal 'line ""))
      line
      (if (equal 'line-number 0)
        ""
        (csvq-last-line s (- 'line-number 1))))))

(defun csvq-add-query-terminator (s)
  (setq s (csvq-trim-right s))
  (if (or (equal s "") (equal (substring s -1) ";"))
    s
    (concat s ";")))

(defun csvq-trim-right (s)
  (if (string-match "[ \t\r\n]+$" s)
    (replace-match "" nil nil s)
    s))

(defun csvq-filter-log (log)
  (let ((view-lines '())
        (lines (split-string log "[\r\n]" t)))
    (dolist (line lines)
      (if (equal (substring line 0 1) "|")
        (add-to-list 'view-lines (format "%s\n" line) t)
        (message line)))
    (apply 'concat view-lines)))
