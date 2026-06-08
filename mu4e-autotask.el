;;; mu4e-autotask.el --- Email automation for mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2026 Laurynas Biveinis

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;; Version: 0.1
;; URL: https://github.com/laurynas-biveinis/mu4e-autotask
;; Package-Requires: ((emacs "27.1"))
;; Keywords: mail

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

;; Rule-based email automation for mu4e.  Configure `mu4e-autotask-rules' with a
;; list of rules that match an incoming message by sender and/or subject and run
;; an action function on it, then call `mu4e-autotask-initialize' to expose the
;; dispatcher as a mu4e view action.
;;
;; The package also provides building blocks for action functions: helpers to
;; read a message's raw text, MIME-part bodies, and attachments, plus an
;; `mu4e-autotask-email-template' struct and `mu4e-autotask-send-email' for
;; composing and sending templated outgoing mail.
;;
;; mu4e itself is a runtime requirement; it ships with mu and is not an ELPA
;; package, so it is not listed in Package-Requires.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'mm-decode)
(require 'mml)
(require 'message)
(require 'mu4e-contacts)
(require 'mu4e-helpers)
(require 'mu4e-message)
(require 'mu4e-context)
(require 'mu4e-compose)
(require 'mu4e-draft)
;; `mu4e-view' provides `mu4e-view-actions' and pulls in `mu4e-view-mime-parts'.
;; Depend on it rather than the internal `mu4e-mime-parts' file, which older mu4e
;; versions do not ship.
(require 'mu4e-view)

;;; Customization

(defgroup mu4e-autotask nil
  "Email automation for mu4e."
  :group 'mu4e
  :prefix "mu4e-autotask-")

(defcustom mu4e-autotask-rules nil
  "List of email automation rules.
Each rule is a property list with the following properties:
  :sender-exact  - Exact sender email address to match.
  :sender-match  - Regexp to match against the sender email address.
  :subject-exact - Exact subject text to match.
  :subject-match - Regexp to match against the email subject.
  :action-fn     - Function called with the message when the rule matches.

Only one of :sender-exact or :sender-match may be specified per rule, and only
one of :subject-exact or :subject-match.  A rule with no subject criteria
matches any subject; a rule with no sender criteria never matches.  The list is
processed in order; having more than one rule match the same message is an
error.  Every rule must supply an :action-fn."
  :type
  '(repeat
    (plist
     :options
     ((:sender-exact string)
      (:sender-match string)
      (:subject-exact string)
      (:subject-match string)
      (:action-fn function))))
  :group 'mu4e-autotask)

(defcustom mu4e-autotask-pre-action-hook nil
  "Hook run at the start of `mu4e-autotask-dispatch', before any rule action.
A function on this hook that signals an error aborts the dispatch.  For example,
add `org-autotask-require-org-clock' to require an active Org clock before any
automation runs."
  :type 'hook
  :group 'mu4e-autotask)

(defcustom mu4e-autotask-open-file-command "open"
  "Shell command used to open a file in its default application.
The file path is appended as a single shell-quoted argument.  The default,
\"open\", is the macOS opener."
  :type 'string
  :group 'mu4e-autotask)

(defcustom mu4e-autotask-open-delete-delay 1
  "Seconds to wait before deleting a temporary attachment after opening it.
`mu4e-autotask-open-all-attachments' hands each saved attachment to an external
opener that reads it asynchronously, then deletes the file after this delay.
Increase it if a slow-launching viewer or a large attachment is not done reading
before the file is removed."
  :type 'number
  :group 'mu4e-autotask)

;;; Rule matching and dispatch

(defun mu4e-autotask--validate-rule (rule)
  "Signal a `user-error' if RULE is malformed.
A rule may set at most one of `:sender-exact' / `:sender-match' and at most one
of `:subject-exact' / `:subject-match', and must supply a function as its
`:action-fn'."
  (when (and (plist-get rule :sender-exact)
             (plist-get rule :sender-match))
    (user-error "Both :sender-exact and :sender-match set in rule: %s"
                rule))
  (when (and (plist-get rule :subject-exact)
             (plist-get rule :subject-match))
    (user-error
     "Both :subject-exact and :subject-match set in rule: %s"
     rule))
  (unless (functionp (plist-get rule :action-fn))
    (user-error "Missing or invalid :action-fn in rule: %s" rule)))

(defun mu4e-autotask--sender-matches-rule (rule sender)
  "Return non-nil if SENDER matches the sender criteria of RULE.
RULE is a plist containing either `:sender-exact' or `:sender-match'."
  (let ((sender-exact (plist-get rule :sender-exact))
        (sender-match (plist-get rule :sender-match)))
    (or (and sender-exact (string= sender-exact sender))
        (and sender-match (string-match-p sender-match sender) t))))

(defun mu4e-autotask--subject-matches-rule (rule subject)
  "Return non-nil if SUBJECT matches the subject criteria of RULE.
RULE is a plist containing either `:subject-exact' or `:subject-match'."
  (let ((subject-exact (plist-get rule :subject-exact))
        (subject-match (plist-get rule :subject-match)))
    (or (and (not subject-exact) (not subject-match))
        (and subject-exact (string= subject-exact subject))
        (and subject-match (string-match-p subject-match subject)))))

;;;###autoload
(defun mu4e-autotask-dispatch (msg)
  "Run the matching `mu4e-autotask-rules' automation for mu4e MSG.
`mu4e-autotask-pre-action-hook' runs first; a member that signals an error
aborts the dispatch.  Signal a `user-error', without running any action, if
more than one rule matches.  Signal a `user-error' first if any rule in
`mu4e-autotask-rules' sets conflicting match criteria, regardless of whether it
would match MSG."
  (mapc #'mu4e-autotask--validate-rule mu4e-autotask-rules)
  (run-hooks 'mu4e-autotask-pre-action-hook)
  (let ((sender
         (mu4e-contact-email (car (mu4e-message-field msg :from))))
        (subject (mu4e-message-field msg :subject)))
    ;; From is a mandatory header (RFC 5322); a message without a parseable
    ;; sender is malformed, not ordinary input, so assert the invariant rather
    ;; than raise a `user-error'.  A missing Subject, by contrast, is normal and
    ;; `mu4e-message-field' already maps it to the empty string.
    (cl-assert (stringp sender))
    (let ((matches
           (seq-filter
            (lambda (rule)
              (and
               (mu4e-autotask--sender-matches-rule rule sender)
               (mu4e-autotask--subject-matches-rule rule subject)))
            mu4e-autotask-rules)))
      (cond
       ((null matches)
        (message "No automation rule matched for this message"))
       ((cdr matches)
        (user-error
         "More than one rule matches this message, fix `mu4e-autotask-rules'"))
       (t
        (funcall (plist-get (car matches) :action-fn) msg))))))

;;;###autoload
(defun mu4e-autotask-initialize ()
  "Register the automation dispatcher as a mu4e view action.
Adds an \"Execute automation\" entry to `mu4e-view-actions'."
  (add-to-list
   'mu4e-view-actions '("Execute automation" . mu4e-autotask-dispatch)
   t))

;;; Reading the current message

(defun mu4e-autotask--attachment-like-p (part)
  "Return non-nil if mu4e MIME PART is an attachment, not an inline body part."
  (plist-get part :attachment-like))

(defun mu4e-autotask--attachment-parts ()
  "Return the attachment parts of the current mu4e message.
Filters `mu4e-view-mime-parts' to the parts mu4e marks as attachment-like,
excluding inline body parts (those for which mu4e invents a filename and leaves
`:attachment-like' nil).  Every returned part has a non-nil string `:filename',
so callers may match against it with `string-suffix-p' without a nil guard."
  (seq-filter
   #'mu4e-autotask--attachment-like-p (mu4e-view-mime-parts)))

(defun mu4e-autotask-raw-message (msg)
  "Return the raw contents of mu4e message MSG as a unibyte string.
Read the file literally, without coding-system decoding or end-of-line
conversion, so the returned bytes match the message as stored on disk."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally (mu4e-message-readable-path msg))
    (buffer-string)))

(defun mu4e-autotask-msg-content (mime-type)
  "Return the decoded body of the first current message part with MIME-TYPE.
Signal a `user-error' if the message has no part of MIME-TYPE."
  (let ((part
         (or (seq-find
              (lambda (part)
                (string= mime-type (plist-get part :mime-type)))
              (mu4e-view-mime-parts))
             (user-error "No %s part in this message" mime-type))))
    (mm-get-part (plist-get part :handle))))

(defun mu4e-autotask-html-content ()
  "Return the text/html body of the current mu4e message."
  (mu4e-autotask-msg-content "text/html"))

(defun mu4e-autotask-text-content ()
  "Return the text/plain body of the current mu4e message."
  (mu4e-autotask-msg-content "text/plain"))

(defun mu4e-autotask--part-filename-has-suffix-p (part suffix)
  "Return non-nil if the `:filename' of mu4e MIME PART ends with SUFFIX.
The comparison ignores case."
  (string-suffix-p suffix (plist-get part :filename) t))

(defun mu4e-autotask--csv-part-p (part)
  "Return non-nil if mu4e MIME PART has a .csv file name."
  (mu4e-autotask--part-filename-has-suffix-p part ".csv"))

(defun mu4e-autotask--pdf-part-p (part)
  "Return non-nil if mu4e MIME PART has a .pdf file name."
  (mu4e-autotask--part-filename-has-suffix-p part ".pdf"))

(defun mu4e-autotask-csv-part ()
  "Return the first .csv attachment part of the current message.
Only attachment parts are considered, not inline body parts.  Signal a
`user-error' if there is none."
  (or (seq-find
       #'mu4e-autotask--csv-part-p (mu4e-autotask--attachment-parts))
      (user-error "The expected .CSV attachment not found")))

(defun mu4e-autotask-pdf-part ()
  "Return the first .pdf attachment part of the current message, or nil.
Only attachment parts are considered, not inline body parts."
  (seq-find
   #'mu4e-autotask--pdf-part-p (mu4e-autotask--attachment-parts)))

(defun mu4e-autotask-save-part-file (part)
  "Save mu4e message PART to a file and return its path."
  (let ((file-path
         (mu4e-join-paths
          (plist-get part :target-dir) (plist-get part :filename))))
    (mm-save-part-to-file (plist-get part :handle) file-path)
    file-path))

(defun mu4e-autotask-save-csv-part ()
  "Save the first .csv part of the current message and return its path."
  (mu4e-autotask-save-part-file (mu4e-autotask-csv-part)))

(defun mu4e-autotask-for-each-attachment (fn)
  "Call FN with the handle and target path of each attachment of the message.
Only attachment parts are visited, not inline body parts."
  (dolist (part (mu4e-autotask--attachment-parts))
    (funcall fn
             (plist-get part :handle)
             (mu4e-join-paths
              (plist-get part :target-dir)
              (plist-get part :filename)))))

(defun mu4e-autotask--open-file (file)
  "Open FILE in its default application.
Use the program named by `mu4e-autotask-open-file-command'."
  (shell-command
   (concat
    mu4e-autotask-open-file-command " " (shell-quote-argument file))))

(defun mu4e-autotask--delete-file-after-delay (path delay)
  "Delete the file at PATH after DELAY seconds."
  (run-with-timer delay nil #'delete-file path nil))

(defun mu4e-autotask-open-all-attachments (suffix)
  "Save and open every attachment of the message whose name ends with SUFFIX."
  (mu4e-autotask-for-each-attachment
   (lambda (handle path)
     (when (string-suffix-p suffix path t)
       (mm-save-part-to-file handle path)
       (mu4e-autotask--open-file path)
       ;; Delay deletion so the async opener can read the file first; if the
       ;; tunable delay still races, replace it with lsof polling.
       (mu4e-autotask--delete-file-after-delay
        path mu4e-autotask-open-delete-delay)))))

(defun mu4e-autotask--save-part-if-jpg (handle path)
  "Save attachment HANDLE to PATH when PATH names a .jpg file."
  (when (string-suffix-p ".jpg" path t)
    (mm-save-part-to-file handle path)))

(defun mu4e-autotask-download-all-jpgs ()
  "Save every .jpg attachment of the current mu4e message."
  (mu4e-autotask-for-each-attachment
   #'mu4e-autotask--save-part-if-jpg))

;;; Sending templated mail

(cl-defstruct
 (mu4e-autotask-email-template (:copier nil))
 "An email template to be filled out and sent."
 (context
  ""
  :read-only t
  :type string
  :documentation "The mu4e context to use.")
 (to "" :read-only t :type string :documentation "To: field.")
 (subject
  ""
  :read-only t
  :type string
  :documentation "Subject: field.")
 (body "" :read-only t :type string :documentation "Email body."))

(defun mu4e-autotask--do-send-email (to success-fn)
  "Ask to send an already filled-out email to TO, call SUCCESS-FN on success.
The \"To:\" field of the email must already be filled out; TO is used only for
diagnostics.  SUCCESS-FN is called only on success."
  (let ((subject (message-field-value "Subject")))
    (if (y-or-n-p
         (format "Send email to %s with subject %s? " to subject))
        (progn
          ;; The buffer is destroyed right after the send attempt, so set the
          ;; buffer-local hook value without bothering to clean it up.
          (add-hook 'message-sent-hook success-fn nil t)
          (message-send-and-exit))
      ;; The body and attachments leave the compose buffer modified; declining
      ;; the send means discarding it, so kill unconditionally rather than
      ;; prompting a second "kill anyway?" confirmation.
      (let ((message-kill-buffer-query nil))
        (message-kill-buffer))
      (user-error "Cancelled"))))

;;;###autoload
(defun mu4e-autotask-send-email (template attachments success-fn)
  "Compose and send mail from TEMPLATE, call SUCCESS-FN once it is sent.
TEMPLATE is a `mu4e-autotask-email-template'.  ATTACHMENTS is a list of file
paths to attach.  SUCCESS-FN is called only after the message is sent."
  (mu4e-context-switch
   'force (mu4e-autotask-email-template-context template))
  (let ((mu4e-compose-context-policy nil)
        (to (mu4e-autotask-email-template-to template)))
    (mu4e-compose-new
     to (mu4e-autotask-email-template-subject template))
    (message-goto-body)
    (insert (mu4e-autotask-email-template-body template))
    (dolist (attachment attachments)
      (mml-attach-file attachment))
    (mu4e-autotask--do-send-email to success-fn)))

(provide 'mu4e-autotask)
;;; mu4e-autotask.el ends here
