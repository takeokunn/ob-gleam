;;; ob-gleam.el --- Org Babel functions for Gleam -*- lexical-binding: t; -*-

;; Copyright (C) 2026 takeokunn

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/ob-gleam
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, tools

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org Babel support for evaluating Gleam code blocks.
;;
;; Gleam requires a full project structure to compile and run, so this
;; package creates a temporary Gleam project for each code block execution.
;;
;; Supported header arguments:
;;   :results - output (default), silent
;;   :deps    - space-separated Hex package names
;;   :imports - space-separated module paths for auto-import
;;   :target  - compilation target (erlang, javascript); default erlang

;;; Code:

(require 'ob)

(defgroup ob-gleam nil
  "Org Babel support for Gleam."
  :group 'org-babel
  :prefix "ob-gleam-")

(defcustom ob-gleam-command "gleam"
  "Path to the Gleam executable."
  :group 'ob-gleam
  :type 'string)

(defcustom ob-gleam-auto-main t
  "When non-nil, automatically wrap code in `pub fn main()' if not present."
  :group 'ob-gleam
  :type 'boolean)

(defcustom ob-gleam-project-name "ob_gleam_temp"
  "Project name used in generated gleam.toml."
  :group 'ob-gleam
  :type 'string)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("gleam" . "gleam"))

(defvar org-babel-default-header-args:gleam '()
  "Default header arguments for Gleam code blocks.")

(defconst org-babel-header-args:gleam
  '((deps . :any)
    (imports . :any)
    (target . :any))
  "Gleam-specific header arguments.")

(defun ob-gleam--split-arg (arg)
  "Split ARG string by whitespace into a list of strings.
Return nil if ARG is nil or empty."
  (when (and arg (not (string-empty-p (string-trim arg))))
    (split-string (string-trim arg))))

(defun ob-gleam--validate-target (target)
  "Validate and normalize TARGET string.
Return nil for default (Erlang) target, \"javascript\" for JS target.
Signal `user-error' for invalid values."
  (let ((val (and target
                  (not (string-empty-p (string-trim target)))
                  (downcase (string-trim target)))))
    (cond
     ((or (null val) (string= val "erlang")) nil)
     ((string= val "javascript") "javascript")
     (t (user-error "Invalid target `%s'; must be `erlang' or `javascript'" val)))))

(defun ob-gleam--has-main-p (body)
  "Return non-nil if BODY includes a `pub fn main()' definition."
  (string-search "pub fn main()" body))

(defun ob-gleam--wrap-body (body imports)
  "Wrap BODY in a main function with IMPORTS.
Prepends `import gleam/io' and any additional IMPORTS, then wraps
BODY in `pub fn main() { ... }'."
  (let ((import-lines (concat "import gleam/io\n"
                              (mapconcat (lambda (mod)
                                           (format "import %s" mod))
                                         imports
                                         "\n"))))
    (concat (string-trim-right import-lines)
            "\n\npub fn main() {\n"
            body
            "\n}\n")))

(defun ob-gleam--generate-gleam-toml (project-name target)
  "Generate gleam.toml content with PROJECT-NAME and TARGET."
  (concat (format "name = \"%s\"\nversion = \"1.0.0\"\n" project-name)
          (when (equal target "javascript")
            "target = \"javascript\"\n\n[javascript]\nruntime = \"node\"\n")
          "\n[dependencies]\ngleam_stdlib = \">= 0.18.0\"\n"))

(defun ob-gleam--create-project (dir project-name target)
  "Create a minimal Gleam project structure in DIR with PROJECT-NAME and TARGET."
  (let ((src-dir (expand-file-name "src" dir)))
    (make-directory src-dir t)
    (with-temp-file (expand-file-name "gleam.toml" dir)
      (insert (ob-gleam--generate-gleam-toml project-name target)))))

(defun ob-gleam--write-source (dir project-name code)
  "Write CODE to src/PROJECT-NAME.gleam in DIR."
  (with-temp-file (expand-file-name (format "src/%s.gleam" project-name) dir)
    (insert code)))

(defun ob-gleam--add-deps (dir deps)
  "Run `gleam add' for each package in DEPS list within DIR."
  (dolist (dep deps)
    (let ((default-directory dir))
      (org-babel-eval
       (format "%s add %s"
               (shell-quote-argument ob-gleam-command)
               (shell-quote-argument dep))
       ""))))

(defun ob-gleam--run (dir)
  "Run `gleam run' in DIR and return stdout."
  (let ((default-directory dir))
    (org-babel-eval
     (format "%s run --no-print-progress" (shell-quote-argument ob-gleam-command)) "")))

(defun org-babel-expand-body:gleam (body params)
  "Expand BODY with PARAMS for a Gleam code block."
  (let* ((imports (ob-gleam--split-arg (cdr (assq :imports params))))
         (has-main (ob-gleam--has-main-p body)))
    (if (and ob-gleam-auto-main (not has-main))
        (ob-gleam--wrap-body body imports)
      body)))

(defun org-babel-execute:gleam (body params)
  "Execute a Gleam code block BODY with PARAMS."
  (let* ((target (ob-gleam--validate-target (cdr (assq :target params))))
         (deps (ob-gleam--split-arg (cdr (assq :deps params))))
         (imports (ob-gleam--split-arg (cdr (assq :imports params))))
         (has-main (ob-gleam--has-main-p body))
         (code (if (and ob-gleam-auto-main (not has-main))
                   (ob-gleam--wrap-body body imports)
                 body))
         (tmp-dir (make-temp-file "ob-gleam-" t)))
    (unwind-protect
        (progn
          (ob-gleam--create-project tmp-dir ob-gleam-project-name target)
          (ob-gleam--write-source tmp-dir ob-gleam-project-name code)
          (when deps
            (ob-gleam--add-deps tmp-dir deps))
          (let ((result (ob-gleam--run tmp-dir)))
            (org-babel-reassemble-table
             result
             (org-babel-pick-name (cdr (assq :colname-names params))
                                  (cdr (assq :colnames params)))
             (org-babel-pick-name (cdr (assq :rowname-names params))
                                  (cdr (assq :rownames params))))))
      (delete-directory tmp-dir t))))

(defun ob-gleam-initiate-session (&rest _)
  "Gleam does not support sessions."
  (error "Gleam does not support sessions"))

(defun org-babel-prep-session:gleam (&rest _)
  "Gleam does not support sessions."
  (error "Gleam does not support sessions"))

(provide 'ob-gleam)
;;; ob-gleam.el ends here
