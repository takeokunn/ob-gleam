;;; ob-gleam-test.el --- Tests for ob-gleam -*- lexical-binding: t; -*-

;; Copyright (C) 2026 takeokunn

;; Author: takeokunn

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT tests for ob-gleam.el.
;; These tests cover pure functions and file-system operations that do
;; NOT require the Gleam CLI to be installed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ob-gleam)

;;; ob-gleam--split-arg

(ert-deftest ob-gleam-test-split-arg-nil ()
  "Nil input returns nil."
  (should-not (ob-gleam--split-arg nil)))

(ert-deftest ob-gleam-test-split-arg-empty ()
  "Empty string returns nil."
  (should-not (ob-gleam--split-arg "")))

(ert-deftest ob-gleam-test-split-arg-single ()
  "Single token returns a one-element list."
  (should (equal (ob-gleam--split-arg "gleam_json")
                 '("gleam_json"))))

(ert-deftest ob-gleam-test-split-arg-multiple ()
  "Multiple tokens separated by spaces return a list."
  (should (equal (ob-gleam--split-arg "gleam_json gleam_http")
                 '("gleam_json" "gleam_http"))))

(ert-deftest ob-gleam-test-split-arg-extra-spaces ()
  "Leading, trailing, and extra internal spaces are handled."
  (should (equal (ob-gleam--split-arg "  gleam_json   gleam_http  ")
                 '("gleam_json" "gleam_http"))))

;;; ob-gleam--has-main-p

(ert-deftest ob-gleam-test-has-main-present ()
  "Body containing `pub fn main()' returns non-nil."
  (should (ob-gleam--has-main-p "pub fn main() {\n  io.println(\"hi\")\n}")))

(ert-deftest ob-gleam-test-has-main-absent ()
  "Body without `pub fn main()' returns nil."
  (should-not (ob-gleam--has-main-p "fn helper() { 42 }")))

(ert-deftest ob-gleam-test-has-main-with-args ()
  "Multiline body with `pub fn main()' somewhere inside returns non-nil."
  (should (ob-gleam--has-main-p
           "import gleam/io\n\npub fn main() {\n  io.println(\"hello\")\n}")))

(ert-deftest ob-gleam-test-has-main-not-pub ()
  "Non-pub main function is not detected."
  (should-not (ob-gleam--has-main-p "fn main() { }")))

(ert-deftest ob-gleam-test-has-main-empty ()
  "Empty string returns nil."
  (should-not (ob-gleam--has-main-p "")))

;;; ob-gleam--wrap-body

(ert-deftest ob-gleam-test-wrap-body-no-imports ()
  "Wrapping with no extra imports produces correct structure."
  (let ((result (ob-gleam--wrap-body "  io.println(\"hi\")" '())))
    (should (string-prefix-p "import gleam/io" result))
    (should (string-match-p "pub fn main() {" result))
    (should (string-match-p "io\\.println(\"hi\")" result))))

(ert-deftest ob-gleam-test-wrap-body-with-imports ()
  "Wrapping with imports includes all import lines."
  (let ((result (ob-gleam--wrap-body
                 "  io.println(string.inspect(1))"
                 '("gleam/string" "gleam/int"))))
    (should (string-match-p "import gleam/io" result))
    (should (string-match-p "import gleam/string" result))
    (should (string-match-p "import gleam/int" result))
    (should (string-match-p "pub fn main() {" result))
    (should (string-match-p "io\\.println" result))))

;;; ob-gleam--generate-gleam-toml

(ert-deftest ob-gleam-test-generate-toml ()
  "Generated TOML contains project name, version, and stdlib dependency."
  (let ((toml (ob-gleam--generate-gleam-toml "my_project")))
    (should (string-match-p "name = \"my_project\"" toml))
    (should (string-match-p "version = \"1\\.0\\.0\"" toml))
    (should (string-match-p "\\[dependencies\\]" toml))
    (should (string-match-p "gleam_stdlib" toml))))

;;; ob-gleam--create-project

(ert-deftest ob-gleam-test-create-project ()
  "Creating a project produces gleam.toml and src/ directory."
  (let ((tmp-dir (make-temp-file "ob-gleam-test-" t)))
    (unwind-protect
        (progn
          (ob-gleam--create-project tmp-dir "test_project")
          (should (file-exists-p (expand-file-name "gleam.toml" tmp-dir)))
          (should (file-directory-p (expand-file-name "src" tmp-dir)))
          (let ((toml-content
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name "gleam.toml" tmp-dir))
                   (buffer-string))))
            (should (string-match-p "name = \"test_project\"" toml-content))))
      (delete-directory tmp-dir t))))

;;; ob-gleam--write-source

(ert-deftest ob-gleam-test-write-source ()
  "Writing source creates src/PROJECT-NAME.gleam with the given code."
  (let ((tmp-dir (make-temp-file "ob-gleam-test-" t))
        (code "import gleam/io\n\npub fn main() {\n  io.println(\"hello\")\n}\n"))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "src" tmp-dir) t)
          (ob-gleam--write-source tmp-dir "my_project" code)
          (let ((source-file (expand-file-name "src/my_project.gleam" tmp-dir)))
            (should (file-exists-p source-file))
            (let ((content
                   (with-temp-buffer
                     (insert-file-contents source-file)
                     (buffer-string))))
              (should (equal content code)))))
      (delete-directory tmp-dir t))))

;;; ob-gleam-initiate-session

(ert-deftest ob-gleam-test-session-error ()
  "Initiating a session signals an error."
  (should-error (ob-gleam-initiate-session)
                :type 'error))

(ert-deftest ob-gleam-test-prep-session-error ()
  "Preparing a session signals an error."
  (should-error (org-babel-prep-session:gleam)
                :type 'error))

;;; org-babel-execute:gleam (mocked)

(ert-deftest ob-gleam-test-execute-simple ()
  "Execute a simple code block with mocked gleam run."
  (cl-letf (((symbol-function 'org-babel-eval)
             (lambda (_cmd _body) "Hello, world!\n")))
    (let ((result (org-babel-execute:gleam
                   "  io.println(\"Hello, world!\")"
                   '())))
      (should (stringp result))
      (should (string-match-p "Hello, world!" result)))))

(ert-deftest ob-gleam-test-execute-no-auto-main ()
  "When auto-main is nil, body is used as-is."
  (let ((ob-gleam-auto-main nil)
        (written-code nil))
    (cl-letf (((symbol-function 'org-babel-eval)
               (lambda (_cmd _body) ""))
              ((symbol-function 'ob-gleam--write-source)
               (lambda (_dir _name code) (setq written-code code))))
      (org-babel-execute:gleam
       "import gleam/io\n\npub fn main() {\n  io.println(\"hi\")\n}"
       '())
      (should (string-match-p "pub fn main()" written-code)))))

(ert-deftest ob-gleam-test-execute-cleanup-on-error ()
  "Temp directory is removed even when gleam run fails."
  (let ((captured-dir nil))
    (cl-letf (((symbol-function 'org-babel-eval)
               (lambda (_cmd _body) (error "Gleam failed")))
              ((symbol-function 'ob-gleam--create-project)
               (lambda (dir _name)
                 (setq captured-dir dir)
                 (make-directory (expand-file-name "src" dir) t)))
              ((symbol-function 'ob-gleam--write-source)
               (lambda (_dir _name _code) nil)))
      (ignore-errors
        (org-babel-execute:gleam "io.println(\"hi\")" '()))
      (should captured-dir)
      (should-not (file-exists-p captured-dir)))))

(ert-deftest ob-gleam-test-execute-with-deps ()
  "Deps parameter triggers gleam add for each dependency."
  (let ((added-deps '()))
    (cl-letf (((symbol-function 'org-babel-eval)
               (lambda (cmd _body)
                 (when (string-match-p "add" cmd)
                   (push cmd added-deps))
                 "")))
      (org-babel-execute:gleam
       "  io.println(\"hi\")"
       '((:deps . "gleam_json gleam_http")))
      (should (= 2 (length added-deps))))))

;;; org-babel-expand-body:gleam

(ert-deftest ob-gleam-test-expand-body-auto-wrap ()
  "Expand body auto-wraps when no main function present."
  (let ((result (org-babel-expand-body:gleam "  io.println(\"hi\")" '())))
    (should (string-match-p "pub fn main() {" result))
    (should (string-match-p "import gleam/io" result))
    (should (string-match-p "io\\.println" result))))

(ert-deftest ob-gleam-test-expand-body-no-wrap ()
  "Expand body does not wrap when main function is present."
  (let* ((body "import gleam/io\n\npub fn main() {\n  io.println(\"hi\")\n}")
         (result (org-babel-expand-body:gleam body '())))
    (should (equal result body))))

(ert-deftest ob-gleam-test-expand-body-with-imports ()
  "Expand body includes imports from params."
  (let ((result (org-babel-expand-body:gleam
                 "  io.println(string.inspect(1))"
                 '((:imports . "gleam/string gleam/int")))))
    (should (string-match-p "import gleam/string" result))
    (should (string-match-p "import gleam/int" result))
    (should (string-match-p "pub fn main() {" result))))

(ert-deftest ob-gleam-test-expand-body-auto-main-nil ()
  "Expand body returns body as-is when auto-main is nil."
  (let ((ob-gleam-auto-main nil)
        (body "fn helper() { 42 }"))
    (should (equal (org-babel-expand-body:gleam body '()) body))))

(provide 'ob-gleam-test)
;;; ob-gleam-test.el ends here
