;;; mu4e-autotask-test.el --- Tests for mu4e-autotask -*- lexical-binding: t -*-

;; Copyright (C) 2026 Laurynas Biveinis

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;; URL: https://github.com/laurynas-biveinis/mu4e-autotask

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; ERT test suite for mu4e-autotask.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mm-decode)
(require 'mu4e-autotask)

;;; Test helpers

(defvar mu4e-autotask-test--buffers nil
  "Buffers backing MIME handles created during a test, killed on cleanup.")

(defmacro mu4e-autotask-test--with-cleanup (&rest body)
  "Run BODY, then kill the buffers recorded in `mu4e-autotask-test--buffers'."
  (declare (indent 0))
  `(let ((mu4e-autotask-test--buffers nil))
     (unwind-protect
         (progn ,@body)
       (dolist (buf mu4e-autotask-test--buffers)
         (when (buffer-live-p buf)
           (kill-buffer buf))))))

(defun mu4e-autotask-test--handle (content &optional mime-type)
  "Return a MIME handle whose decoded body is CONTENT.
MIME-TYPE is the handle's declared content-type, defaulting to \"text/plain\".
The backing buffer is recorded in `mu4e-autotask-test--buffers' for cleanup."
  (let ((buf (generate-new-buffer " *mu4e-autotask-test*")))
    (push buf mu4e-autotask-test--buffers)
    (with-current-buffer buf
      (insert content))
    (mm-make-handle buf (list (or mime-type "text/plain")))))

(defvar mu4e-autotask-test--called nil
  "Message passed to `mu4e-autotask-test--record-call', or nil if it never ran.
A dispatch test binds this to nil, registers `mu4e-autotask-test--record-call'
as a rule's `:action-fn', then asserts both whether the action ran and which
message it received.")

(defun mu4e-autotask-test--record-call (msg)
  "Record MSG as the dispatch action that ran, in `mu4e-autotask-test--called'."
  (setq mu4e-autotask-test--called msg))

(defun mu4e-autotask-test--abort-hook ()
  "Signal a `user-error', standing in for a pre-action hook member that aborts."
  (user-error "No clock"))

(defvar mu4e-autotask-test--parts nil
  "MIME parts that `mu4e-autotask-test--mime-parts' returns while stubbed in.")

(defun mu4e-autotask-test--mime-parts ()
  "Return `mu4e-autotask-test--parts'.
Stand-in for `mu4e-view-mime-parts', installed with `cl-letf' so a test can
supply the message's parts through `mu4e-autotask-test--parts'."
  mu4e-autotask-test--parts)

(defvar mu4e-autotask-test--saved nil
  "Reversed list of (HANDLE . PATH) conses recorded while saving attachments.")

(defun mu4e-autotask-test--record-save (handle path)
  "Record saving attachment HANDLE to PATH in `mu4e-autotask-test--saved'.
The conses accumulate newest-first; reverse the list before asserting on it.
Serves both as the FN given to `mu4e-autotask-for-each-attachment' and as a
`mm-save-part-to-file' stand-in."
  (push (cons handle path) mu4e-autotask-test--saved))

;;; Dispatch

(ert-deftest mu4e-autotask-test-dispatch-single-match ()
  "A single matching rule runs its action with the message."
  (let ((mu4e-autotask-test--called nil)
        (msg '(:from ((:email "a@b.com")) :subject "Hello"))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com"
            :action-fn mu4e-autotask-test--record-call))))
    (mu4e-autotask-dispatch msg)
    (should (equal mu4e-autotask-test--called msg))))

(ert-deftest mu4e-autotask-test-dispatch-sender-with-name ()
  "Dispatch extracts the sender email regardless of contact plist key order.
mu4e documents `:from' contacts as (:name NAME :email EMAIL), so the email must
be read by key, not by position."
  (let ((mu4e-autotask-test--called nil)
        (msg
         '(:from ((:name "Some One" :email "a@b.com")) :subject "Hello"))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com"
            :action-fn mu4e-autotask-test--record-call))))
    (mu4e-autotask-dispatch msg)
    (should (equal mu4e-autotask-test--called msg))))

(ert-deftest mu4e-autotask-test-dispatch-no-match ()
  "No matching rule leaves all actions unrun."
  (let ((mu4e-autotask-test--called nil)
        (msg '(:from ((:email "x@y.com")) :subject "Hi"))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com"
            :action-fn mu4e-autotask-test--record-call))))
    (mu4e-autotask-dispatch msg)
    (should-not mu4e-autotask-test--called)))

(ert-deftest mu4e-autotask-test-dispatch-sender-match-fires ()
  "A `:sender-match' rule fires when its regexp matches the sender."
  (let ((mu4e-autotask-test--called nil)
        (msg '(:from ((:email "a@b.com")) :subject "Hi"))
        (mu4e-autotask-rules
         '((:sender-match "@b\\.com$"
            :action-fn mu4e-autotask-test--record-call))))
    (mu4e-autotask-dispatch msg)
    (should mu4e-autotask-test--called)))

(ert-deftest mu4e-autotask-test-dispatch-sender-match-no-match ()
  "A `:sender-match' rule whose regexp does not match leaves the action unrun."
  (let ((mu4e-autotask-test--called nil)
        (msg '(:from ((:email "a@c.com")) :subject "Hi"))
        (mu4e-autotask-rules
         '((:sender-match "@b\\.com$"
            :action-fn mu4e-autotask-test--record-call))))
    (mu4e-autotask-dispatch msg)
    (should-not mu4e-autotask-test--called)))

(ert-deftest mu4e-autotask-test-dispatch-no-sender-criteria-never-matches ()
  "A rule with no sender criteria never fires, even when its subject matches."
  (let ((mu4e-autotask-test--called nil)
        (msg '(:from ((:email "a@b.com")) :subject "Hi"))
        (mu4e-autotask-rules
         '((:subject-exact "Hi"
            :action-fn mu4e-autotask-test--record-call))))
    (mu4e-autotask-dispatch msg)
    (should-not mu4e-autotask-test--called)))

(ert-deftest mu4e-autotask-test-dispatch-subject-exact-fires ()
  "A `:subject-exact' rule fires on an identical subject."
  (let ((mu4e-autotask-test--called nil)
        (msg '(:from ((:email "a@b.com")) :subject "Hi"))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com" :subject-exact "Hi"
            :action-fn mu4e-autotask-test--record-call))))
    (mu4e-autotask-dispatch msg)
    (should mu4e-autotask-test--called)))

(ert-deftest mu4e-autotask-test-dispatch-subject-match-no-match ()
  "A `:subject-match' rule whose regexp does not match leaves the action unrun."
  (let ((mu4e-autotask-test--called nil)
        (msg '(:from ((:email "a@b.com")) :subject "hello"))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com" :subject-match "^Re: "
            :action-fn mu4e-autotask-test--record-call))))
    (mu4e-autotask-dispatch msg)
    (should-not mu4e-autotask-test--called)))

(ert-deftest mu4e-autotask-test-dispatch-two-matches-error ()
  "More than one matching rule signals a `user-error' before any action runs."
  (let ((mu4e-autotask-test--called nil)
        (msg '(:from ((:email "a@b.com")) :subject "Hello"))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com"
            :action-fn mu4e-autotask-test--record-call)
           (:sender-match "@b\\.com$"
            :action-fn mu4e-autotask-test--record-call))))
    (should-error (mu4e-autotask-dispatch msg) :type 'user-error)
    (should-not mu4e-autotask-test--called)))

(ert-deftest mu4e-autotask-test-dispatch-sender-both-set-errors ()
  "Dispatch rejects a rule that sets both `:sender-exact' and `:sender-match'."
  (let ((msg '(:from ((:email "a@b.com")) :subject "Hi"))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com" :sender-match "@b"
            :action-fn ignore))))
    (should-error (mu4e-autotask-dispatch msg) :type 'user-error)))

(ert-deftest mu4e-autotask-test-dispatch-subject-both-set-errors ()
  "Dispatch rejects a both-subject-keys rule even when its sender does not match.
Validation must not depend on the rule being selected by the message sender."
  (let ((msg '(:from ((:email "x@y.com")) :subject "Hi"))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com"
            :subject-exact "Hi" :subject-match "H"
            :action-fn ignore))))
    (should-error (mu4e-autotask-dispatch msg) :type 'user-error)))

(ert-deftest mu4e-autotask-test-dispatch-missing-action-fn-errors ()
  "Dispatch rejects a rule that lacks an `:action-fn'."
  (let ((msg '(:from ((:email "a@b.com")) :subject "Hi"))
        (mu4e-autotask-rules '((:sender-exact "a@b.com"))))
    (should-error (mu4e-autotask-dispatch msg) :type 'user-error)))

(ert-deftest mu4e-autotask-test-dispatch-non-function-action-fn-errors ()
  "Dispatch rejects a rule whose `:action-fn' is not a function."
  (let ((msg '(:from ((:email "a@b.com")) :subject "Hi"))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com" :action-fn "not-a-function"))))
    (should-error (mu4e-autotask-dispatch msg) :type 'user-error)))

(ert-deftest mu4e-autotask-test-dispatch-both-criteria-subject-mismatch ()
  "A both-criteria rule does not fire when the sender matches but the subject
does not."
  (let ((mu4e-autotask-test--called nil)
        (msg '(:from ((:email "a@b.com")) :subject "Y"))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com" :subject-exact "X"
            :action-fn mu4e-autotask-test--record-call))))
    (mu4e-autotask-dispatch msg)
    (should-not mu4e-autotask-test--called)))

(ert-deftest mu4e-autotask-test-dispatch-both-criteria-sender-mismatch ()
  "A both-criteria rule does not fire when the subject matches but the sender
does not."
  (let ((mu4e-autotask-test--called nil)
        (msg '(:from ((:email "c@d.com")) :subject "X"))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com" :subject-exact "X"
            :action-fn mu4e-autotask-test--record-call))))
    (mu4e-autotask-dispatch msg)
    (should-not mu4e-autotask-test--called)))

(ert-deftest mu4e-autotask-test-dispatch-pre-action-hook-runs-first ()
  "The pre-action hook runs before any rule action."
  (let* ((order nil)
         (msg '(:from ((:email "a@b.com")) :subject "Hello"))
         (mu4e-autotask-pre-action-hook (list (lambda () (push 'hook order))))
         (mu4e-autotask-rules
          `((:sender-exact "a@b.com"
             :action-fn ,(lambda (_m) (push 'action order))))))
    (mu4e-autotask-dispatch msg)
    (should (equal order '(action hook)))))

(ert-deftest mu4e-autotask-test-dispatch-pre-action-hook-aborts ()
  "A signaling pre-action hook member aborts before any action."
  (let ((mu4e-autotask-test--called nil)
        (msg '(:from ((:email "a@b.com")) :subject "Hello"))
        (mu4e-autotask-pre-action-hook '(mu4e-autotask-test--abort-hook))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com"
            :action-fn mu4e-autotask-test--record-call))))
    (should-error (mu4e-autotask-dispatch msg) :type 'user-error)
    (should-not mu4e-autotask-test--called)))

(ert-deftest mu4e-autotask-test-dispatch-subject-match-no-subject ()
  "A non-empty `:subject-match' rule does not fire on a message with no subject."
  (let ((mu4e-autotask-test--called nil)
        (msg '(:from ((:email "a@b.com"))))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com" :subject-match "x"
            :action-fn mu4e-autotask-test--record-call))))
    (mu4e-autotask-dispatch msg)
    (should-not mu4e-autotask-test--called)))

(ert-deftest mu4e-autotask-test-dispatch-empty-subject-rule-matches ()
  "An empty-subject rule fires on a message that has no Subject header.
A missing subject is treated as the empty string, so `:subject-exact \"\"'
matches it."
  (let ((mu4e-autotask-test--called nil)
        (msg '(:from ((:email "a@b.com"))))
        (mu4e-autotask-rules
         '((:sender-exact "a@b.com" :subject-exact ""
            :action-fn mu4e-autotask-test--record-call))))
    (mu4e-autotask-dispatch msg)
    (should mu4e-autotask-test--called)))

(ert-deftest mu4e-autotask-test-dispatch-preserves-match-data ()
  "Subject matching during dispatch does not clobber global match data."
  (string-match "bar" "xxbar")
  (let ((mu4e-autotask-rules
         '((:sender-exact "a@b.com" :subject-match "foo"
            :action-fn ignore)))
        (msg '(:from ((:email "a@b.com")) :subject "foofoo")))
    (mu4e-autotask-dispatch msg)
    (should (equal (match-beginning 0) 2))))

(ert-deftest mu4e-autotask-test-initialize ()
  "`mu4e-autotask-initialize' registers the dispatcher view action once."
  (let ((mu4e-view-actions nil))
    (mu4e-autotask-initialize)
    (should
     (member
      '("Execute automation" . mu4e-autotask-dispatch) mu4e-view-actions))
    (mu4e-autotask-initialize)
    (should
     (equal
      1
      (cl-count
       '("Execute automation" . mu4e-autotask-dispatch)
       mu4e-view-actions
       :test #'equal)))))

;;; Email template

(ert-deftest mu4e-autotask-test-email-template-accessors ()
  "The template constructor and accessors round-trip the slot values."
  (let ((tmpl
         (make-mu4e-autotask-email-template
          :context "ctx" :to "a@b" :subject "Subj" :body "Body")))
    (should (equal (mu4e-autotask-email-template-context tmpl) "ctx"))
    (should (equal (mu4e-autotask-email-template-to tmpl) "a@b"))
    (should (equal (mu4e-autotask-email-template-subject tmpl) "Subj"))
    (should (equal (mu4e-autotask-email-template-body tmpl) "Body"))))

(ert-deftest mu4e-autotask-test-email-template-string-slots-default-empty ()
  "Omitted string slots default to an empty string, matching `:type string'."
  (let ((tmpl (make-mu4e-autotask-email-template)))
    (should (equal (mu4e-autotask-email-template-context tmpl) ""))
    (should (equal (mu4e-autotask-email-template-to tmpl) ""))
    (should (equal (mu4e-autotask-email-template-subject tmpl) ""))
    (should (equal (mu4e-autotask-email-template-body tmpl) ""))))

;;; Reading the message

(ert-deftest mu4e-autotask-test-raw-message ()
  "`mu4e-autotask-raw-message' returns the file at the message :path."
  (let ((file (make-temp-file "mu4e-autotask-test")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "Raw message body"))
          (should
           (equal
            (mu4e-autotask-raw-message `(:path ,file))
            "Raw message body")))
      (delete-file file))))

(ert-deftest mu4e-autotask-test-raw-message-preserves-bytes ()
  "`mu4e-autotask-raw-message' returns file bytes verbatim.
CRLF line endings and 8-bit bytes are preserved, unlike a decoding read which
would strip CR and reinterpret high bytes."
  (let ((file (make-temp-file "mu4e-autotask-test"))
        (bytes (unibyte-string ?F ?r ?o ?m 13 10 13 10 ?x 255 13 10)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region bytes nil file))
          (should (equal (mu4e-autotask-raw-message `(:path ,file)) bytes)))
      (delete-file file))))

(ert-deftest mu4e-autotask-test-msg-content ()
  "Content accessors return the decoded body of the matching MIME part."
  (mu4e-autotask-test--with-cleanup
    (let ((mu4e-autotask-test--parts
           `((:mime-type "text/plain"
              :handle ,(mu4e-autotask-test--handle "plain text"))
             (:mime-type "text/html"
              :handle ,(mu4e-autotask-test--handle "<p>html</p>" "text/html")))))
      (cl-letf (((symbol-function 'mu4e-view-mime-parts)
                 #'mu4e-autotask-test--mime-parts))
        (should (equal (mu4e-autotask-text-content) "plain text"))
        (should (equal (mu4e-autotask-html-content) "<p>html</p>"))
        (should
         (equal (mu4e-autotask-msg-content "text/plain") "plain text"))))))

(ert-deftest mu4e-autotask-test-msg-content-missing-errors ()
  "Content accessors signal a `user-error' when the MIME type is absent."
  (mu4e-autotask-test--with-cleanup
    (let ((mu4e-autotask-test--parts
           `((:mime-type "text/plain"
              :handle ,(mu4e-autotask-test--handle "plain text")))))
      (cl-letf (((symbol-function 'mu4e-view-mime-parts)
                 #'mu4e-autotask-test--mime-parts))
        (should-error (mu4e-autotask-html-content) :type 'user-error)))))

(ert-deftest mu4e-autotask-test-csv-part-found ()
  "`mu4e-autotask-csv-part' returns the first .csv part."
  (let ((mu4e-autotask-test--parts
         '((:filename "notes.txt" :attachment-like t)
           (:filename "data.csv" :attachment-like t))))
    (cl-letf (((symbol-function 'mu4e-view-mime-parts)
               #'mu4e-autotask-test--mime-parts))
      (should
       (equal
        (mu4e-autotask-csv-part)
        '(:filename "data.csv" :attachment-like t))))))

(ert-deftest mu4e-autotask-test-csv-part-missing-errors ()
  "`mu4e-autotask-csv-part' signals a `user-error' when none is present."
  (let ((mu4e-autotask-test--parts '((:filename "a.txt" :attachment-like t))))
    (cl-letf (((symbol-function 'mu4e-view-mime-parts)
               #'mu4e-autotask-test--mime-parts))
      (should-error (mu4e-autotask-csv-part) :type 'user-error))))

(ert-deftest mu4e-autotask-test-pdf-part ()
  "`mu4e-autotask-pdf-part' returns the first .pdf part or nil."
  (let ((mu4e-autotask-test--parts
         '((:filename "a.txt" :attachment-like t)
           (:filename "b.pdf" :attachment-like t))))
    (cl-letf (((symbol-function 'mu4e-view-mime-parts)
               #'mu4e-autotask-test--mime-parts))
      (should
       (equal
        (mu4e-autotask-pdf-part) '(:filename "b.pdf" :attachment-like t)))))
  (let ((mu4e-autotask-test--parts '((:filename "a.txt" :attachment-like t))))
    (cl-letf (((symbol-function 'mu4e-view-mime-parts)
               #'mu4e-autotask-test--mime-parts))
      (should-not (mu4e-autotask-pdf-part)))))

(ert-deftest mu4e-autotask-test-save-part-file ()
  "`mu4e-autotask-save-part-file' writes the part and returns its path."
  (mu4e-autotask-test--with-cleanup
    (let ((dir (make-temp-file "mu4e-autotask-test" t)))
      (unwind-protect
          (let* ((part
                  `(:target-dir ,dir :filename "out.txt"
                    :handle ,(mu4e-autotask-test--handle "saved content")))
                 (path (mu4e-autotask-save-part-file part)))
            (should (equal path (mu4e-join-paths dir "out.txt")))
            (should
             (equal
              (with-temp-buffer
                (insert-file-contents path)
                (buffer-string))
              "saved content")))
        (delete-directory dir t)))))

(ert-deftest mu4e-autotask-test-save-csv-part ()
  "`mu4e-autotask-save-csv-part' selects the .csv part, saves it, returns path."
  (mu4e-autotask-test--with-cleanup
    (let ((dir (make-temp-file "mu4e-autotask-test" t)))
      (unwind-protect
          (let ((mu4e-autotask-test--parts
                 `((:filename "notes.txt" :attachment-like t)
                   (:target-dir ,dir :filename "data.csv" :attachment-like t
                    :handle ,(mu4e-autotask-test--handle "a,b,c")))))
            (cl-letf (((symbol-function 'mu4e-view-mime-parts)
                       #'mu4e-autotask-test--mime-parts))
              (let ((path (mu4e-autotask-save-csv-part)))
                (should (equal path (mu4e-join-paths dir "data.csv")))
                (should
                 (equal
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string))
                  "a,b,c")))))
        (delete-directory dir t)))))

(ert-deftest mu4e-autotask-test-for-each-attachment ()
  "`mu4e-autotask-for-each-attachment' calls FN with handle and target path."
  (let ((mu4e-autotask-test--parts
         '((:target-dir "/tmp" :filename "a.txt" :handle h1
            :attachment-like t)
           (:target-dir "/tmp" :filename "b.jpg" :handle h2
            :attachment-like t)))
        (mu4e-autotask-test--saved nil))
    (cl-letf (((symbol-function 'mu4e-view-mime-parts)
               #'mu4e-autotask-test--mime-parts))
      (mu4e-autotask-for-each-attachment #'mu4e-autotask-test--record-save))
    (should
     (equal
      (nreverse mu4e-autotask-test--saved)
      `((h1 . ,(mu4e-join-paths "/tmp" "a.txt"))
        (h2 . ,(mu4e-join-paths "/tmp" "b.jpg")))))))

(ert-deftest mu4e-autotask-test-for-each-attachment-skips-body-parts ()
  "`mu4e-autotask-for-each-attachment' skips non-attachment body parts.
mu4e marks inline body parts with a nil `:attachment-like' and invents a
filename for them; FN must not be called for those."
  (let ((mu4e-autotask-test--parts
         '((:target-dir "/tmp" :filename "mime-part-01" :handle body
            :attachment-like nil)
           (:target-dir "/tmp" :filename "a.txt" :handle h1
            :attachment-like t)))
        (mu4e-autotask-test--saved nil))
    (cl-letf (((symbol-function 'mu4e-view-mime-parts)
               #'mu4e-autotask-test--mime-parts))
      (mu4e-autotask-for-each-attachment #'mu4e-autotask-test--record-save))
    (should
     (equal
      (nreverse mu4e-autotask-test--saved)
      `((h1 . ,(mu4e-join-paths "/tmp" "a.txt")))))))

(ert-deftest mu4e-autotask-test-open-all-attachments ()
  "`mu4e-autotask-open-all-attachments' saves, opens, deletes matching parts."
  (let ((mu4e-autotask-test--parts
         '((:target-dir "/tmp" :filename "a.pdf" :handle hp
            :attachment-like t)
           (:target-dir "/tmp" :filename "b.txt" :handle ht
            :attachment-like t)))
        (mu4e-autotask-test--saved nil)
        (opened nil)
        (deleted nil))
    (cl-letf (((symbol-function 'mu4e-view-mime-parts)
               #'mu4e-autotask-test--mime-parts)
              ((symbol-function 'mm-save-part-to-file)
               #'mu4e-autotask-test--record-save)
              ((symbol-function 'mu4e-autotask--open-file)
               (lambda (path) (push path opened)))
              ((symbol-function 'mu4e-autotask--delete-file-after-delay)
               (lambda (path _delay) (push path deleted))))
      (mu4e-autotask-open-all-attachments ".pdf"))
    (should
     (equal
      mu4e-autotask-test--saved
      `((hp . ,(mu4e-join-paths "/tmp" "a.pdf")))))
    (should (equal opened (list (mu4e-join-paths "/tmp" "a.pdf"))))
    (should (equal deleted (list (mu4e-join-paths "/tmp" "a.pdf"))))))

(ert-deftest mu4e-autotask-test-download-all-jpgs ()
  "`mu4e-autotask-download-all-jpgs' saves only the .jpg attachments."
  (let ((mu4e-autotask-test--parts
         '((:target-dir "/tmp" :filename "a.jpg" :handle hj
            :attachment-like t)
           (:target-dir "/tmp" :filename "b.pdf" :handle hp
            :attachment-like t)))
        (mu4e-autotask-test--saved nil))
    (cl-letf (((symbol-function 'mu4e-view-mime-parts)
               #'mu4e-autotask-test--mime-parts)
              ((symbol-function 'mm-save-part-to-file)
               #'mu4e-autotask-test--record-save))
      (mu4e-autotask-download-all-jpgs))
    (should
     (equal
      mu4e-autotask-test--saved
      `((hj . ,(mu4e-join-paths "/tmp" "a.jpg")))))))

;;; Sending templated mail

(ert-deftest mu4e-autotask-test-send-email ()
  "`mu4e-autotask-send-email' threads the template and wires the success fn."
  (let* ((tmpl
          (make-mu4e-autotask-email-template
           :context "ctx" :to "to@x" :subject "Subj" :body "Body text"))
         (events nil)
         (draft-buffer nil)
         (success (lambda () (push 'success events))))
    (cl-letf (((symbol-function 'mu4e-context-switch)
               (lambda (_force name) (push `(context ,name) events)))
              ;; Mirror the real `mu4e-compose-new', which leaves a new draft
              ;; buffer current, so the test pins that the body lands there.
              ((symbol-function 'mu4e-compose-new)
               (lambda (to subject)
                 (push `(compose ,to ,subject) events)
                 (setq draft-buffer (generate-new-buffer " *draft*"))
                 (set-buffer draft-buffer)))
              ((symbol-function 'message-goto-body)
               (lambda () (push '(goto-body) events) nil))
              ((symbol-function 'mml-attach-file)
               (lambda (f) (push `(attach ,f) events)))
              ((symbol-function 'message-field-value) (lambda (_f) "Subj"))
              ((symbol-function 'y-or-n-p) (lambda (_p) t))
              ((symbol-function 'message-send-and-exit)
               (lambda () (push '(send) events))))
      (unwind-protect
          (let ((ambient-buffer nil))
            (with-temp-buffer
              (setq ambient-buffer (current-buffer))
              (mu4e-autotask-send-email tmpl '("/tmp/file.pdf") success)
              (should (buffer-live-p draft-buffer))
              (with-current-buffer draft-buffer
                (should (string-match-p "Body text" (buffer-string)))
                (should (memq success message-sent-hook)))
              (with-current-buffer ambient-buffer
                (should-not (string-match-p "Body text" (buffer-string))))))
        (when (buffer-live-p draft-buffer)
          (kill-buffer draft-buffer))))
    (should
     (equal
      (nreverse events)
      '((context "ctx")
        (compose "to@x" "Subj")
        (goto-body)
        (attach "/tmp/file.pdf")
        (send))))))

(ert-deftest mu4e-autotask-test-send-email-cancel ()
  "Declining the send discards the modified draft without a second prompt.
The compose buffer has been modified by the inserted body, so the real
`message-kill-buffer' would normally ask \"kill anyway?\"; declining the send
must suppress that and kill the buffer unconditionally, then signal `user-error'."
  (let ((tmpl
         (make-mu4e-autotask-email-template
          :context "ctx" :to "to@x" :subject "Subj" :body "Body text"))
        (buf nil))
    (cl-letf (((symbol-function 'mu4e-context-switch)
               (lambda (_force _name) nil))
              ((symbol-function 'mu4e-compose-new)
               (lambda (_to _subject) nil))
              ((symbol-function 'message-goto-body) (lambda () nil))
              ((symbol-function 'message-field-value) (lambda (_f) "Subj"))
              ((symbol-function 'y-or-n-p) (lambda (_p) nil))
              ((symbol-function 'yes-or-no-p)
               (lambda (_p) (error "Unexpected second confirmation prompt"))))
      (with-temp-buffer
        (setq buf (current-buffer))
        (should-error
         (mu4e-autotask-send-email tmpl nil #'ignore) :type 'user-error)))
    (should-not (buffer-live-p buf))))

(ert-deftest mu4e-autotask-test-do-send-email-confirm ()
  "`mu4e-autotask-do-send-email' confirms, wires SUCCESS-FN, and sends.
This is the entry point for action functions that compose a message themselves
and then want the same confirm-and-send behavior."
  (let ((events nil)
        (success (lambda () (push 'success events))))
    (cl-letf (((symbol-function 'message-field-value) (lambda (_f) "Subj"))
              ((symbol-function 'y-or-n-p) (lambda (_p) t))
              ((symbol-function 'message-send-and-exit)
               (lambda () (push 'send events))))
      (with-temp-buffer
        (mu4e-autotask-do-send-email "to@x" success)
        (should (memq success message-sent-hook))))
    (should (equal events '(send)))))

(ert-deftest mu4e-autotask-test-do-send-email-decline ()
  "Declining the send discards the compose buffer and signals `user-error'."
  (let ((buf nil)
        (sent nil))
    (cl-letf (((symbol-function 'message-field-value) (lambda (_f) "Subj"))
              ((symbol-function 'y-or-n-p) (lambda (_p) nil))
              ((symbol-function 'message-send-and-exit)
               (lambda () (setq sent t))))
      (with-temp-buffer
        (setq buf (current-buffer))
        (should-error
         (mu4e-autotask-do-send-email "to@x" #'ignore) :type 'user-error)))
    (should-not sent)
    (should-not (buffer-live-p buf))))

(provide 'mu4e-autotask-test)
;;; mu4e-autotask-test.el ends here
