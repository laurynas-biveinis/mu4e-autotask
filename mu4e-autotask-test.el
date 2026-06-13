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

(defun mu4e-autotask-test--invite-ics (method &optional description
                                              omit-organizer)
  "Return iCalendar text with METHOD for calendar invitation fixtures.
DESCRIPTION overrides the event description and may contain backslash-n
escapes per RFC 5545.  With OMIT-ORGANIZER, leave out the ORGANIZER property."
  (concat
   "BEGIN:VCALENDAR\n"
   "METHOD:" method "\n"
   "PRODID:Test\n"
   "VERSION:2.0\n"
   "BEGIN:VEVENT\n"
   (if omit-organizer "" "ORGANIZER:MAILTO:org@example.com\n")
   "DTSTART:20260611T150000Z\n"
   "DTEND:20260611T153000Z\n"
   "SUMMARY:Team meeting\n"
   "LOCATION:Room 5\n"
   "DESCRIPTION:" (or description "Agenda") "\n"
   "ATTENDEE;RSVP=TRUE;PARTSTAT=NEEDS-ACTION;CN=Me:MAILTO:me@example.com\n"
   "UID:test-uid-1\n"
   "END:VEVENT\n"
   "END:VCALENDAR\n"))

(defvar mu4e-autotask-test--files nil
  "Temp files created during a test, deleted with their buffers on cleanup.")

(defun mu4e-autotask-test--temp-file (&optional suffix)
  "Create an empty temp file with SUFFIX, register it for cleanup, return it."
  (let ((file (make-temp-file "mu4e-autotask-test" nil suffix)))
    (push file mu4e-autotask-test--files)
    file))

(defconst mu4e-autotask-test--invite-headers-format
  (concat "From: Org <org@example.com>\n"
          "To: me@example.com\n"
          "Subject: Meeting\n"
          "MIME-Version: 1.0\n"
          "Content-Type: text/calendar; method=%s; charset=utf-8\n"
          "\n")
  "Format string for the headers of a single-part invitation message.
The format argument is the iCalendar METHOD.")

(defun mu4e-autotask-test--write-invite (method &optional description
                                                omit-organizer)
  "Write a single-part text/calendar message with METHOD and DESCRIPTION.
With OMIT-ORGANIZER, leave out the event's ORGANIZER property.  Return the
message file path, registered for cleanup."
  (let ((file (mu4e-autotask-test--temp-file)))
    (with-temp-file file
      (insert
       (format mu4e-autotask-test--invite-headers-format method)
       (mu4e-autotask-test--invite-ics method description omit-organizer)))
    file))

(defun mu4e-autotask-test--write-transformed-request (regexp rep)
  "Write a REQUEST invitation with REGEXP in its iCalendar text replaced.
REP is the replacement.  Return the message file path, registered for
cleanup."
  (let ((file (mu4e-autotask-test--temp-file)))
    (with-temp-file file
      (insert
       (format mu4e-autotask-test--invite-headers-format "REQUEST")
       (replace-regexp-in-string
        regexp rep (mu4e-autotask-test--invite-ics "REQUEST"))))
    file))

(defun mu4e-autotask-test--invite-msg (file)
  "Return a calendar-flagged fake mu4e message plist for FILE."
  `(:path ,file
    :from ((:email "org@example.com"))
    :subject "Meeting"
    :flags (seen calendar)))

(defun mu4e-autotask-test--request-msg (&optional description
                                                  omit-organizer)
  "Write a METHOD:REQUEST invitation and return its fake mu4e message.
DESCRIPTION and OMIT-ORGANIZER are passed on to
`mu4e-autotask-test--write-invite'."
  (mu4e-autotask-test--invite-msg
   (mu4e-autotask-test--write-invite "REQUEST" description omit-organizer)))

(defun mu4e-autotask-test--personal-addresses (&rest _)
  "Return the test identity, standing in for `mu4e-personal-addresses'."
  '("me@example.com"))

(defun mu4e-autotask-test--make-draft (to)
  "Create a message-mode draft buffer addressed to TO and leave it current.
Return the buffer.  Mirrors the real `mu4e-compose-reply-to', which leaves a
new draft buffer current."
  (let ((buf (generate-new-buffer " *draft*")))
    (set-buffer buf)
    (insert "To: " to "\n" "Subject: Re: Meeting\n" mail-header-separator "\n")
    (message-mode)
    buf))

(defvar mu4e-autotask-test--draft-to nil
  "Address passed to `mu4e-autotask-test--reply-to', or nil if it never ran.")

(defvar mu4e-autotask-test--draft-buffer nil
  "Draft buffer created by `mu4e-autotask-test--reply-to', killed on cleanup.")

(defun mu4e-autotask-test--reply-to (to)
  "Stand-in for `mu4e-compose-reply-to': create and record a draft to TO."
  (setq mu4e-autotask-test--draft-to to
        mu4e-autotask-test--draft-buffer (mu4e-autotask-test--make-draft to)))

(defvar mu4e-autotask-test--prompted nil
  "Non-nil once a `mu4e-autotask-test--choose-' RSVP prompt stand-in ran.")

(defvar mu4e-autotask-test--sent nil
  "Send result: t or the draft text once a send stand-in ran, nil before.")

(defvar mu4e-autotask-test--posted nil
  "Entry text captured by `mu4e-autotask-test--record-post', or nil.")

(defun mu4e-autotask-test--choose-accept (&rest _)
  "Stand-in for `read-multiple-choice': record the prompt, accept."
  (setq mu4e-autotask-test--prompted t)
  '(?a "accept"))

(defun mu4e-autotask-test--choose-tentative (&rest _)
  "Stand-in for `read-multiple-choice': record the prompt, tentative."
  (setq mu4e-autotask-test--prompted t)
  '(?t "tentative"))

(defun mu4e-autotask-test--choose-decline (&rest _)
  "Stand-in for `read-multiple-choice': record the prompt, decline."
  (setq mu4e-autotask-test--prompted t)
  '(?d "decline"))

(defun mu4e-autotask-test--choose-quit (&rest _)
  "Stand-in for `read-multiple-choice': record the prompt, quit."
  (setq mu4e-autotask-test--prompted t)
  '(?q "quit"))

(defun mu4e-autotask-test--confirm-send (_prompt)
  "Stand-in for `y-or-n-p': confirm the send."
  t)

(defun mu4e-autotask-test--refuse-send (_prompt)
  "Stand-in for `y-or-n-p': refuse the send."
  nil)

(defun mu4e-autotask-test--message-subject (_field)
  "Stand-in for `message-field-value': return the fixture subject."
  "Subj")

(defun mu4e-autotask-test--unexpected-prompt (_prompt)
  "Stand-in for a confirmation prompt that must never be reached."
  (error "Unexpected second confirmation prompt"))

(defun mu4e-autotask-test--failing-success-fn ()
  "Send success function that signals an error."
  (error "Recording failed"))

(defun mu4e-autotask-test--quitting-success-fn ()
  "Send success function that signals a quit."
  (signal 'quit nil))

(defun mu4e-autotask-test--send-recording-content ()
  "Stand-in for `message-send-and-exit': record the outgoing draft text."
  (setq mu4e-autotask-test--sent (buffer-string)))

(defun mu4e-autotask-test--send-running-hooks ()
  "Stand-in for `message-send-and-exit': record the send, run the sent hooks."
  (setq mu4e-autotask-test--sent t)
  (run-hooks 'message-sent-hook))

(defun mu4e-autotask-test--run-sent-hooks ()
  "Stand-in for `message-send-and-exit': run the sent hooks, record nothing."
  (run-hooks 'message-sent-hook))

(defun mu4e-autotask-test--record-post (&rest _)
  "Stand-in for `org-gcal-post-at-point': capture the entry buffer text."
  (setq mu4e-autotask-test--posted
        (buffer-substring-no-properties (point-min) (point-max))))

(defun mu4e-autotask-test--target-for (org-file)
  "Return an event target function mapping any message to ORG-FILE.
The calendar id is the fixture constant cal@example.com."
  (lambda (_msg) (cons org-file "cal@example.com")))

(defun mu4e-autotask-test--mm-buffer-p (buffer)
  "Return non-nil if BUFFER is an mm-decode part content buffer."
  (string-prefix-p " *mm*" (buffer-name buffer)))

(defun mu4e-autotask-test--rsvp-reply-buffer-p (buffer)
  "Return non-nil if BUFFER is a per-RSVP iTIP reply buffer."
  (string-prefix-p " *mu4e-autotask-rsvp*" (buffer-name buffer)))

(defun mu4e-autotask-test--reply-buffer ()
  "Return the single live per-RSVP reply buffer; fail unless exactly one."
  (let ((buffers (cl-remove-if-not #'mu4e-autotask-test--rsvp-reply-buffer-p
                                   (buffer-list))))
    (should (equal (length buffers) 1))
    (car buffers)))

(defconst mu4e-autotask-test--recorded-event-re
  (concat "\\`\\* Team meeting\n"
          ":PROPERTIES:\n"
          ":calendar-id: cal@example\\.com\n"
          ":LOCATION: Room 5\n"
          ":END:\n"
          ":org-gcal:\n"
          "<2026-06-1[0-9] [0-9:]+-[0-9:]+>\n"
          "Agenda\n"
          ":END:\n\\'")
  "Anchored regex matching the whole recorded entry of the default fixture.
The timestamp hour is left open because the fixture's UTC times render in
the local time zone.")

(defun mu4e-autotask-test--dispatch-rsvp (msg choice send)
  "Dispatch MSG through the RSVP flow with the standard stand-ins installed.
CHOICE stands in for `read-multiple-choice' and SEND for
`message-send-and-exit'.  The send confirmation is auto-accepted and
`org-gcal-post-at-point' records the entry into
`mu4e-autotask-test--posted'."
  (cl-letf (((symbol-function 'read-multiple-choice) choice)
            ((symbol-function 'y-or-n-p)
             #'mu4e-autotask-test--confirm-send)
            ((symbol-function 'message-send-and-exit) send)
            ((symbol-function 'org-gcal-post-at-point)
             #'mu4e-autotask-test--record-post))
    (mu4e-autotask-dispatch msg)))

(defun mu4e-autotask-test--nonexistent-target (_msg)
  "Event target stand-in naming a file that must never be written."
  '("/nonexistent.org" . "cal@example.com"))

;; Value-less declarations: the org-gcal package is absent from the test
;; environment, so `mu4e-autotask-test--with-rsvp' binds its name
;; customizations to their org-gcal defaults.
(defvar org-gcal-drawer-name)
(defvar org-gcal-calendar-id-property)

(defmacro mu4e-autotask-test--with-rsvp (&rest body)
  "Run BODY with the invariant RSVP mocks installed, then clean up.
Installs `mu4e-autotask-test--reply-to' for `mu4e-compose-reply-to' and
`mu4e-autotask-test--personal-addresses' for `mu4e-personal-addresses',
and resets the dispatch-path customizations to their defaults so the RSVP
flow runs regardless of the ambient session: `mu4e-autotask-rules' and
`mu4e-autotask-pre-action-hook' to nil, `mu4e-autotask-handle-icalendar'
to t, and `mu4e-autotask-icalendar-event-target-function' to nil.  Binds
the org-gcal drawer and property names to their org-gcal defaults, as the
package itself is absent from the test environment.  On exit kills the
recorded draft buffer and any per-RSVP reply buffers, and deletes the files
in `mu4e-autotask-test--files' along with any buffers visiting them."
  (declare (indent 0) (debug t))
  `(let ((mu4e-autotask-test--draft-to nil)
         (mu4e-autotask-test--draft-buffer nil)
         (mu4e-autotask-test--files nil)
         (mu4e-autotask-test--prompted nil)
         (mu4e-autotask-test--sent nil)
         (mu4e-autotask-test--posted nil)
         (mu4e-autotask-rules nil)
         (mu4e-autotask-pre-action-hook nil)
         (mu4e-autotask-handle-icalendar t)
         (mu4e-autotask-icalendar-event-target-function nil)
         (org-gcal-drawer-name "org-gcal")
         (org-gcal-calendar-id-property "calendar-id"))
     (unwind-protect
         (cl-letf (((symbol-function 'mu4e-compose-reply-to)
                    #'mu4e-autotask-test--reply-to)
                   ((symbol-function 'mu4e-personal-addresses)
                    #'mu4e-autotask-test--personal-addresses))
           ,@body)
       (when (buffer-live-p mu4e-autotask-test--draft-buffer)
         (kill-buffer mu4e-autotask-test--draft-buffer))
       (dolist (buf (buffer-list))
         (when (mu4e-autotask-test--rsvp-reply-buffer-p buf)
           (kill-buffer buf)))
       (dolist (file mu4e-autotask-test--files)
         (when-let* ((buf (find-buffer-visiting file)))
           (kill-buffer buf))
         (delete-file file)))))

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

;;; iCalendar RSVP

(ert-deftest mu4e-autotask-test-icalendar-gate-prompts-and-quit-bails ()
  "A calendar-flagged message with no matching rule reaches the RSVP prompt.
Quitting the prompt signals a `user-error'."
  (mu4e-autotask-test--with-rsvp
    (let ((msg (mu4e-autotask-test--request-msg)))
      (should-error
       (mu4e-autotask-test--dispatch-rsvp
        msg #'mu4e-autotask-test--choose-quit #'ignore)
       :type 'user-error)
      (should mu4e-autotask-test--prompted)
      (should-not mu4e-autotask-test--posted))))

(ert-deftest mu4e-autotask-test-icalendar-non-request-bails-before-prompt ()
  "A calendar-flagged message that is not a meeting request never prompts."
  (mu4e-autotask-test--with-rsvp
    (let ((msg (mu4e-autotask-test--invite-msg
                (mu4e-autotask-test--write-invite "REPLY"))))
      (should-error
       (mu4e-autotask-test--dispatch-rsvp
        msg #'mu4e-autotask-test--choose-accept #'ignore)
       :type 'user-error)
      (should-not mu4e-autotask-test--prompted)
      (should-not mu4e-autotask-test--posted))))

(ert-deftest mu4e-autotask-test-icalendar-accept-sends-reply-to-organizer ()
  "Accepting an invitation composes an iTIP REPLY to the organizer and sends."
  (mu4e-autotask-test--with-rsvp
    (let ((msg (mu4e-autotask-test--request-msg)))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-recording-content)
      (should (equal mu4e-autotask-test--draft-to "org@example.com"))
      (should
       (string-match-p "text/calendar; method=REPLY"
                       mu4e-autotask-test--sent))
      (with-current-buffer (mu4e-autotask-test--reply-buffer)
        (should (string-match-p "METHOD:REPLY" (buffer-string)))
        (should (string-match-p "PARTSTAT=ACCEPTED" (buffer-string)))))))

(ert-deftest mu4e-autotask-test-icalendar-accept-records-org-gcal-event ()
  "Accepting with a target function records the event after the send."
  (mu4e-autotask-test--with-rsvp
    (let* ((msg (mu4e-autotask-test--request-msg))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file)))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-running-hooks)
      (let ((posted mu4e-autotask-test--posted))
        (should posted)
        (should
         (string-match-p mu4e-autotask-test--recorded-event-re posted))
        (should (equal posted (with-temp-buffer
                                (insert-file-contents org-file)
                                (buffer-string))))))))

(ert-deftest mu4e-autotask-test-icalendar-decline-sends-without-event ()
  "Declining sends the RSVP but records no event despite a target function."
  (mu4e-autotask-test--with-rsvp
    (let ((msg (mu4e-autotask-test--request-msg))
          (mu4e-autotask-icalendar-event-target-function
           #'mu4e-autotask-test--nonexistent-target))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-decline
       #'mu4e-autotask-test--send-running-hooks)
      (should mu4e-autotask-test--sent)
      (should-not mu4e-autotask-test--posted)
      (with-current-buffer (mu4e-autotask-test--reply-buffer)
        (should (string-match-p "PARTSTAT=DECLINED" (buffer-string)))))))

(ert-deftest mu4e-autotask-test-icalendar-rule-takes-precedence ()
  "A matching user rule wins over the built-in handler on a flagged message."
  (mu4e-autotask-test--with-rsvp
    (let* ((mu4e-autotask-test--called nil)
           (msg (mu4e-autotask-test--request-msg))
           (mu4e-autotask-rules
            '((:sender-exact "org@example.com"
               :action-fn mu4e-autotask-test--record-call))))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-quit #'ignore)
      (should (equal mu4e-autotask-test--called msg))
      (should-not mu4e-autotask-test--prompted))))

(ert-deftest mu4e-autotask-test-icalendar-gate-off-no-prompt ()
  "With the handler disabled, a flagged message reports no rule matched."
  (mu4e-autotask-test--with-rsvp
    (let ((msg (mu4e-autotask-test--request-msg))
          (mu4e-autotask-handle-icalendar nil)
          (reported nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest _) (setq reported fmt))))
        (mu4e-autotask-test--dispatch-rsvp
         msg #'mu4e-autotask-test--choose-accept
         #'mu4e-autotask-test--send-running-hooks))
      (should-not mu4e-autotask-test--prompted)
      (should-not mu4e-autotask-test--sent)
      (should
       (equal reported "No automation rule matched for this message")))))

(ert-deftest mu4e-autotask-test-icalendar-no-calendar-part-errors ()
  "A flagged message without a text/calendar part signals a `user-error'."
  (mu4e-autotask-test--with-rsvp
    (let* ((file (mu4e-autotask-test--temp-file))
           (msg (mu4e-autotask-test--invite-msg file)))
      (with-temp-file file
        (insert "From: Org <org@example.com>\n"
                "To: me@example.com\n"
                "Subject: Meeting\n"
                "MIME-Version: 1.0\n"
                "Content-Type: text/plain\n"
                "\n"
                "Just text.\n"))
      (should-error (mu4e-autotask-dispatch msg) :type 'user-error))))

(ert-deftest mu4e-autotask-test-icalendar-nested-multipart-parses ()
  "A base64 calendar part nested in multipart/alternative reaches the prompt."
  (mu4e-autotask-test--with-rsvp
    (let* ((file (mu4e-autotask-test--temp-file))
           (msg (mu4e-autotask-test--invite-msg file)))
      (with-temp-file file
        (insert
         "From: Org <org@example.com>\n"
         "To: me@example.com\n"
         "Subject: Meeting\n"
         "MIME-Version: 1.0\n"
         "Content-Type: multipart/alternative; boundary=\"BND\"\n"
         "\n"
         "--BND\n"
         "Content-Type: text/plain\n"
         "\n"
         "Meeting invitation\n"
         "--BND\n"
         "Content-Type: text/calendar; method=REQUEST; charset=utf-8\n"
         "Content-Transfer-Encoding: base64\n"
         "\n"
         (base64-encode-string
          (mu4e-autotask-test--invite-ics "REQUEST"))
         "\n--BND--\n"))
      (should-error
       (mu4e-autotask-test--dispatch-rsvp
        msg #'mu4e-autotask-test--choose-quit #'ignore)
       :type 'user-error)
      (should mu4e-autotask-test--prompted))))

(ert-deftest mu4e-autotask-test-icalendar-org-gcal-missing-errors ()
  "Recording an event without org-gcal signals a `user-error' before sending.
The missing package is fully predictable, so the RSVP must not go out only for
the recording to fail afterwards."
  (mu4e-autotask-test--with-rsvp
    (let* ((real-require (symbol-function 'require))
           (msg (mu4e-autotask-test--request-msg))
           (mu4e-autotask-icalendar-event-target-function
            #'mu4e-autotask-test--nonexistent-target))
      ;; Simulate an environment without org-gcal: no function definition
      ;; and a failing feature load.
      (cl-letf (((symbol-function 'org-gcal-post-at-point) nil)
                ((symbol-function 'require)
                 (lambda (feature &optional filename noerror)
                   (if (eq feature 'org-gcal)
                       nil
                     (funcall real-require feature filename noerror))))
                ((symbol-function 'read-multiple-choice)
                 #'mu4e-autotask-test--choose-accept)
                ((symbol-function 'y-or-n-p)
                 #'mu4e-autotask-test--confirm-send)
                ((symbol-function 'message-send-and-exit)
                 #'mu4e-autotask-test--send-running-hooks))
        (should-error (mu4e-autotask-dispatch msg) :type 'user-error))
      (should-not mu4e-autotask-test--sent))))

(ert-deftest mu4e-autotask-test-icalendar-org-gcal-missing-target-nil-sends-once ()
  "Org-gcal absent with a nil-returning target sends and calls the target once.
The pre-send org-gcal check resolves the target before the send; the post-send
hook must reuse that result rather than invoke the side-effecting target again."
  (mu4e-autotask-test--with-rsvp
    (let* ((real-require (symbol-function 'require))
           (msg (mu4e-autotask-test--request-msg))
           (calls 0)
           (mu4e-autotask-icalendar-event-target-function
            (lambda (_msg)
              (cl-incf calls)
              nil)))
      ;; Simulate an environment without org-gcal: no function definition
      ;; and a failing feature load.
      (cl-letf (((symbol-function 'org-gcal-post-at-point) nil)
                ((symbol-function 'require)
                 (lambda (feature &optional filename noerror)
                   (if (eq feature 'org-gcal)
                       nil
                     (funcall real-require feature filename noerror))))
                ((symbol-function 'read-multiple-choice)
                 #'mu4e-autotask-test--choose-accept)
                ((symbol-function 'y-or-n-p)
                 #'mu4e-autotask-test--confirm-send)
                ((symbol-function 'message-send-and-exit)
                 #'mu4e-autotask-test--send-running-hooks))
        (mu4e-autotask-dispatch msg))
      (should mu4e-autotask-test--sent)
      (should (equal calls 1))
      (should-not mu4e-autotask-test--posted))))

(ert-deftest mu4e-autotask-test-icalendar-org-gcal-post-error-becomes-warning ()
  "A failure in `org-gcal-post-at-point' after the send becomes a warning.
The RSVP is already sent when recording runs, so a recording failure must
present as a warning rather than a failed send."
  (mu4e-autotask-test--with-rsvp
    (let* ((msg (mu4e-autotask-test--request-msg))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file))
           (warnings nil))
      (cl-letf (((symbol-function 'read-multiple-choice)
                 #'mu4e-autotask-test--choose-accept)
                ((symbol-function 'y-or-n-p)
                 #'mu4e-autotask-test--confirm-send)
                ((symbol-function 'message-send-and-exit)
                 #'mu4e-autotask-test--send-running-hooks)
                ((symbol-function 'org-gcal-post-at-point)
                 (lambda (&rest _) (error "Google Calendar sync failed")))
                ((symbol-function 'display-warning)
                 (lambda (type message &optional level &rest _)
                   (push (list type message level) warnings))))
        (mu4e-autotask-dispatch msg))
      (should mu4e-autotask-test--sent)
      (should-not mu4e-autotask-test--posted)
      (should (equal (length warnings) 1))
      (should (eq (nth 0 (car warnings)) 'mu4e-autotask))
      (should (string-match-p "follow-up" (nth 1 (car warnings))))
      (should (eq (nth 2 (car warnings)) :error)))))

(ert-deftest mu4e-autotask-test-icalendar-send-declined-no-event ()
  "Refusing the send confirmation bails without recording any event."
  (mu4e-autotask-test--with-rsvp
    (let* ((msg (mu4e-autotask-test--request-msg))
           (target-called nil)
           (mu4e-autotask-icalendar-event-target-function
            (lambda (_msg)
              (setq target-called t)
              '("/nonexistent.org" . "cal@example.com"))))
      (cl-letf (((symbol-function 'read-multiple-choice)
                 #'mu4e-autotask-test--choose-accept)
                ((symbol-function 'y-or-n-p)
                 #'mu4e-autotask-test--refuse-send)
                ((symbol-function 'org-gcal-post-at-point)
                 #'mu4e-autotask-test--record-post))
        (should-error (mu4e-autotask-dispatch msg) :type 'user-error))
      (should-not target-called)
      (should-not mu4e-autotask-test--posted))))

(ert-deftest mu4e-autotask-test-icalendar-description-sanitized-for-drawer ()
  "Description lines that would corrupt the org-gcal drawer are neutralized."
  (mu4e-autotask-test--with-rsvp
    (let* ((msg (mu4e-autotask-test--invite-msg
                 (mu4e-autotask-test--write-invite
                  "REQUEST" "Line1\\n:END:\\n* Heading")))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file)))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-running-hooks)
      (let ((posted mu4e-autotask-test--posted))
        (should posted)
        (should (string-match-p "^Line1$" posted))
        ;; Column-zero stars are escaped with org-gcal's lossless ✱
        ;; convention, which its drawer reader converts back to *.
        (should (string-match-p "^✱ Heading$" posted))
        ;; Only the properties and org-gcal drawer closers remain; the
        ;; :END: smuggled in via the description is gone.
        (should
         (equal
          2
          (cl-count ":END:" (split-string posted "\n")
                    :test #'string=)))))))

(ert-deftest mu4e-autotask-test-icalendar-description-trailing-newline-no-blank ()
  "A DESCRIPTION ending in a newline records without a blank line before :END:.
RFC 5545 allows a DESCRIPTION whose value ends with a `\\n' escape, decoding
to a trailing newline; the sanitizer must not leave a blank drawer line."
  (mu4e-autotask-test--with-rsvp
    (let* ((msg (mu4e-autotask-test--invite-msg
                 (mu4e-autotask-test--write-invite "REQUEST" "Agenda\\n")))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file)))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-running-hooks)
      (let ((posted mu4e-autotask-test--posted))
        (should posted)
        (should
         (string-match-p mu4e-autotask-test--recorded-event-re posted))
        (should-not (string-match-p "\n\n:END:" posted))))))

(ert-deftest mu4e-autotask-test-icalendar-description-all-newline-no-blank ()
  "A DESCRIPTION of only newlines records without a blank line before :END:.
The sanitizer reduces such a value to the empty string, which must not be
inserted as a blank drawer line."
  (mu4e-autotask-test--with-rsvp
    (let* ((msg (mu4e-autotask-test--invite-msg
                 (mu4e-autotask-test--write-invite "REQUEST" "\\n")))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file)))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-running-hooks)
      (let ((posted mu4e-autotask-test--posted))
        (should posted)
        (should-not (string-match-p "\n\n:END:" posted))))))

(ert-deftest mu4e-autotask-test-icalendar-sanitizer-ignores-case-fold ()
  "A lowercase drawer closer is dropped even when case folding is off."
  (mu4e-autotask-test--with-rsvp
    (let* ((msg (mu4e-autotask-test--invite-msg
                 (mu4e-autotask-test--write-invite
                  "REQUEST" "Line1\\n:end:\\nLine2")))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file))
           (case-fold-search nil))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-running-hooks)
      (let ((posted mu4e-autotask-test--posted))
        (should posted)
        (should (string-match-p "^Line1$" posted))
        (should (string-match-p "^Line2$" posted))
        ;; Org closes drawers case-insensitively, so the lowercase closer
        ;; must be dropped like the uppercase one.
        (should
         (equal
          0
          (cl-count ":end:" (split-string posted "\n")
                    :test #'string=)))))))

(ert-deftest mu4e-autotask-test-icalendar-tentative-partstat-and-event ()
  "A tentative RSVP replies with PARTSTAT=TENTATIVE and records the event."
  (mu4e-autotask-test--with-rsvp
    (let* ((msg (mu4e-autotask-test--request-msg))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file)))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-tentative
       #'mu4e-autotask-test--send-running-hooks)
      (should mu4e-autotask-test--posted)
      (with-current-buffer (mu4e-autotask-test--reply-buffer)
        (should
         (string-match-p "PARTSTAT=TENTATIVE" (buffer-string)))))))

(ert-deftest mu4e-autotask-test-icalendar-target-nil-skips-event ()
  "A target function returning nil sends the RSVP without recording."
  (mu4e-autotask-test--with-rsvp
    (let ((msg (mu4e-autotask-test--request-msg))
          (mu4e-autotask-icalendar-event-target-function #'ignore))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-running-hooks)
      (should mu4e-autotask-test--sent)
      (should-not mu4e-autotask-test--posted))))

(ert-deftest mu4e-autotask-test-icalendar-uninvited-still-replies ()
  "An invitation without a matching attendee still produces an RSVP reply."
  (mu4e-autotask-test--with-rsvp
    (let ((msg (mu4e-autotask-test--invite-msg
                (mu4e-autotask-test--write-transformed-request
                 "MAILTO:me@example\\.com" "MAILTO:other@example.com"))))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-recording-content)
      (should mu4e-autotask-test--sent)
      (with-current-buffer (mu4e-autotask-test--reply-buffer)
        (should
         (string-match-p "PARTSTAT=ACCEPTED" (buffer-string)))))))

(ert-deftest mu4e-autotask-test-icalendar-reply-folded-to-75-octets ()
  "Non-ASCII reply lines are folded to RFC 5545's 75-octet limit.
The limit is on the encoded wire form, so the fold must count UTF-8 octets,
not characters."
  (mu4e-autotask-test--with-rsvp
    (let* ((summary (make-string 60 ?会))
           (msg (mu4e-autotask-test--invite-msg
                 (mu4e-autotask-test--write-transformed-request
                  "SUMMARY:Team meeting" (concat "SUMMARY:" summary)))))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-recording-content)
      (with-current-buffer (mu4e-autotask-test--reply-buffer)
        (dolist (line (split-string (buffer-string) "\n"))
          (should
           (<= (length (encode-coding-string line 'utf-8)) 75)))
        ;; gnus-icalendar titles the reply "Accepted: <summary>"; unfolding
        ;; must reassemble the long summary intact.
        (should
         (string-match-p
          (concat "^SUMMARY:Accepted: " summary "$")
          (replace-regexp-in-string
           "\n " "" (buffer-string))))))))

(ert-deftest mu4e-autotask-test-icalendar-reply-long-line-round-trips ()
  "A long ATTENDEE line folds into 75-octet lines and unfolds intact.
Folding a line repeatedly must keep every physical line within the limit,
mark each continuation with exactly one leading space, and lose nothing."
  (mu4e-autotask-test--with-rsvp
    (let* ((cn (make-string 130 ?x))
           (attendee
            (concat "ATTENDEE;RSVP=TRUE;PARTSTAT=ACCEPTED;CN=" cn
                    ":MAILTO:me@example.com"))
           (msg (mu4e-autotask-test--invite-msg
                 (mu4e-autotask-test--write-transformed-request
                  "CN=Me" (concat "CN=" cn)))))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-recording-content)
      (with-current-buffer (mu4e-autotask-test--reply-buffer)
        (dolist (line (split-string (buffer-string) "\n"))
          (should
           (<= (length (encode-coding-string line 'utf-8)) 75)))
        ;; Each continuation carries exactly one fold space then content.
        (should-not (string-match-p "\n  " (buffer-string)))
        (should
         (string-match-p
          (concat "^" (regexp-quote attendee) "$")
          (replace-regexp-in-string
           "\n " "" (buffer-string))))))))

(ert-deftest mu4e-autotask-test-icalendar-location-collapsed-to-one-line ()
  "A multi-line event location lands in the LOCATION property on one line."
  (mu4e-autotask-test--with-rsvp
    (let* ((file (mu4e-autotask-test--write-transformed-request
                  "LOCATION:Room 5" "LOCATION:Room 5\\\\nFloor 2"))
           (msg (mu4e-autotask-test--invite-msg file))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file)))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-running-hooks)
      (should mu4e-autotask-test--posted)
      (should
       (string-match-p "^:LOCATION: Room 5, Floor 2$"
                       mu4e-autotask-test--posted)))))

(ert-deftest mu4e-autotask-test-icalendar-custom-org-gcal-names-honored ()
  "Customized org-gcal drawer and property names are used in the entry."
  (mu4e-autotask-test--with-rsvp
    (let* ((msg (mu4e-autotask-test--request-msg))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file))
           (org-gcal-drawer-name "custom-drawer")
           (org-gcal-calendar-id-property "gcal-id"))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-running-hooks)
      (let ((posted mu4e-autotask-test--posted))
        (should posted)
        (should (string-match-p "^:gcal-id: cal@example.com$" posted))
        (should (string-match-p "^:custom-drawer:$" posted))
        (should-not (string-match-p "^:calendar-id:" posted))
        (should-not (string-match-p "^:org-gcal:$" posted))))))

(ert-deftest mu4e-autotask-test-icalendar-multiline-summary-collapsed ()
  "A multi-line event summary lands in the org heading on one line."
  (mu4e-autotask-test--with-rsvp
    (let* ((file (mu4e-autotask-test--write-transformed-request
                  "SUMMARY:Team meeting" "SUMMARY:Team meeting\\\\nRoom 9"))
           (msg (mu4e-autotask-test--invite-msg file))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file)))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-running-hooks)
      (should mu4e-autotask-test--posted)
      (should
       (string-match-p
        "^\\* Team meeting, Room 9\n:PROPERTIES:$"
        mu4e-autotask-test--posted)))))

(ert-deftest mu4e-autotask-test-icalendar-no-summary-falls-back-to-subject ()
  "An invitation without SUMMARY titles the event after the message subject."
  (mu4e-autotask-test--with-rsvp
    (let* ((file (mu4e-autotask-test--write-transformed-request
                  "SUMMARY:Team meeting\n" ""))
           (msg (mu4e-autotask-test--invite-msg file))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file)))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-running-hooks)
      (should mu4e-autotask-test--posted)
      (should
       (string-match-p "^\\* Meeting\n:PROPERTIES:$"
                       mu4e-autotask-test--posted)))))

(ert-deftest mu4e-autotask-test-icalendar-no-summary-no-subject-no-title ()
  "With a blank SUMMARY and a blank Subject the event is titled (no title)."
  (mu4e-autotask-test--with-rsvp
    (let* ((file (mu4e-autotask-test--write-transformed-request
                  "SUMMARY:Team meeting\n" ""))
           (msg `(:path ,file
                  :from ((:email "org@example.com"))
                  :subject ""
                  :flags (seen calendar)))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file)))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-running-hooks)
      (should mu4e-autotask-test--posted)
      (should
       (string-match-p "^\\* (no title)\n:PROPERTIES:$"
                       mu4e-autotask-test--posted)))))

(ert-deftest mu4e-autotask-test-icalendar-event-respects-narrowed-buffer ()
  "Recording appends at true end-of-file despite a narrowed visiting buffer.
The pre-visited buffer's restriction and point are restored afterwards."
  (mu4e-autotask-test--with-rsvp
    (let* ((msg (mu4e-autotask-test--request-msg))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file)))
      (with-temp-file org-file
        (insert "* Existing\n** Nested\nBody\n"))
      (let ((buf (find-file-noselect org-file)))
        (with-current-buffer buf
          ;; Narrow to the "** Nested\n" line, point inside it.
          (narrow-to-region 12 22)
          (goto-char 15))
        (mu4e-autotask-test--dispatch-rsvp
         msg #'mu4e-autotask-test--choose-accept
         #'mu4e-autotask-test--send-running-hooks)
        (should
         (string-prefix-p
          "* Existing\n** Nested\nBody\n* Team meeting\n"
          (with-temp-buffer
            (insert-file-contents org-file)
            (buffer-string))))
        (with-current-buffer buf
          (should (equal (point-min) 12))
          (should (equal (point-max) 22))
          (should (equal (point) 15)))))))

(ert-deftest mu4e-autotask-test-icalendar-event-unsaved-buffer-refused ()
  "Recording refuses when the target file's buffer has unsaved edits.
Accepting an invitation must not silently save the user's unrelated edits;
the refusal surfaces as the follow-up warning and the file stays untouched."
  (mu4e-autotask-test--with-rsvp
    (let* ((msg (mu4e-autotask-test--request-msg))
           (org-file (mu4e-autotask-test--temp-file ".org"))
           (mu4e-autotask-icalendar-event-target-function
            (mu4e-autotask-test--target-for org-file))
           (warnings nil))
      (with-temp-file org-file
        (insert "* Existing\n"))
      (with-current-buffer (find-file-noselect org-file)
        (goto-char (point-max))
        (insert "Unsaved note\n"))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'display-warning)
                       (lambda (type message &optional level &rest _)
                         (push (list type message level) warnings))))
              (mu4e-autotask-test--dispatch-rsvp
               msg #'mu4e-autotask-test--choose-accept
               #'mu4e-autotask-test--send-running-hooks))
            (should (equal (length warnings) 1))
            (should (eq (nth 0 (car warnings)) 'mu4e-autotask))
            (should (string-match-p "unsaved" (nth 1 (car warnings))))
            (should (eq (nth 2 (car warnings)) :error))
            (should-not mu4e-autotask-test--posted)
            (should (buffer-modified-p (find-buffer-visiting org-file)))
            (should (equal (with-temp-buffer
                             (insert-file-contents org-file)
                             (buffer-string))
                           "* Existing\n")))
        ;; Clear the deliberate modification so cleanup's `kill-buffer'
        ;; does not prompt.
        (with-current-buffer (find-buffer-visiting org-file)
          (set-buffer-modified-p nil))))))

(ert-deftest mu4e-autotask-test-icalendar-no-organizer-uses-reply-to ()
  "Without an ORGANIZER property, the RSVP goes to the message's Reply-To."
  (mu4e-autotask-test--with-rsvp
    (let* ((file (mu4e-autotask-test--write-invite "REQUEST" nil t))
           (msg `(:path ,file
                  :from ((:email "sender@example.com"))
                  :reply-to ((:email "replyto@example.com"))
                  :subject "Meeting"
                  :flags (seen calendar))))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-recording-content)
      (should mu4e-autotask-test--sent)
      (should
       (equal mu4e-autotask-test--draft-to "replyto@example.com")))))

(ert-deftest mu4e-autotask-test-icalendar-no-organizer-uses-from ()
  "Without ORGANIZER and Reply-To, the RSVP goes to the message's From."
  (mu4e-autotask-test--with-rsvp
    (let* ((file (mu4e-autotask-test--write-invite "REQUEST" nil t))
           (msg `(:path ,file
                  :from ((:email "sender@example.com"))
                  :subject "Meeting"
                  :flags (seen calendar))))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept
       #'mu4e-autotask-test--send-recording-content)
      (should mu4e-autotask-test--sent)
      (should
       (equal mu4e-autotask-test--draft-to "sender@example.com")))))

(ert-deftest mu4e-autotask-test-icalendar-organizer-all-absent-errors ()
  "Organizer resolution errors when ORGANIZER, Reply-To, and From are absent.
`mu4e-autotask-dispatch' asserts a string sender from `:from' before the RSVP
path runs, and the organizer falls back to that same `:from', so this guard is
unreachable through dispatch; exercise `mu4e-autotask--icalendar-organizer'
directly."
  (mu4e-autotask-test--with-rsvp
    (let* ((file (mu4e-autotask-test--write-invite "REQUEST" nil t))
           (msg `(:path ,file
                  :from nil
                  :subject "Meeting"
                  :flags (seen calendar))))
      (mu4e-autotask--with-mime-handle (handle msg "text/calendar")
        (let ((event (gnus-icalendar-event-from-handle
                      handle (mu4e-personal-addresses 'no-regexp))))
          (should-error
           (mu4e-autotask--icalendar-organizer msg event)
           :type 'user-error))))))

(ert-deftest mu4e-autotask-test-icalendar-second-rsvp-keeps-pending-reply ()
  "A second RSVP must not clobber a pending draft's calendar payload.
mml resolves buffer-attached parts at send time, so each RSVP needs its own
reply buffer: with a shared buffer, a draft surviving past the flow (a
failed or quit send) would mail the next invitation's reply instead."
  (mu4e-autotask-test--with-rsvp
    (let ((msg-a (mu4e-autotask-test--request-msg)))
      ;; The send stand-in does nothing: draft A stays pending.
      (mu4e-autotask-test--dispatch-rsvp
       msg-a #'mu4e-autotask-test--choose-accept #'ignore)
      (let ((draft-a mu4e-autotask-test--draft-buffer)
            ;; Only one reply buffer exists after the first dispatch (the
            ;; `#'ignore' send left draft A pending); grab it directly
            ;; rather than parsing the draft's mml attachment markup.
            (reply-buffer-a (mu4e-autotask-test--reply-buffer)))
        (unwind-protect
            (let ((msg-b (mu4e-autotask-test--invite-msg
                          (mu4e-autotask-test--write-transformed-request
                           "UID:test-uid-1" "UID:test-uid-2"))))
              (mu4e-autotask-test--dispatch-rsvp
               msg-b #'mu4e-autotask-test--choose-accept #'ignore)
              (with-current-buffer reply-buffer-a
                (should
                 (string-match-p "UID:test-uid-1" (buffer-string)))))
          (kill-buffer draft-a))))))

(ert-deftest mu4e-autotask-test-icalendar-no-stray-mime-buffers ()
  "The RSVP flow destroys all MIME part buffers it dissected."
  (mu4e-autotask-test--with-rsvp
    (let ((msg (mu4e-autotask-test--request-msg)))
      (mu4e-autotask-test--dispatch-rsvp
       msg #'mu4e-autotask-test--choose-accept #'ignore)
      (should-not
       (cl-some #'mu4e-autotask-test--mm-buffer-p (buffer-list))))))

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
              ((symbol-function 'message-field-value)
               #'mu4e-autotask-test--message-subject)
              ((symbol-function 'y-or-n-p)
               #'mu4e-autotask-test--confirm-send)
              ((symbol-function 'message-send-and-exit)
               (lambda ()
                 (push '(send) events)
                 (run-hooks 'message-sent-hook))))
      (unwind-protect
          (let ((ambient-buffer nil))
            (with-temp-buffer
              (setq ambient-buffer (current-buffer))
              (mu4e-autotask-send-email tmpl '("/tmp/file.pdf") success)
              (should (buffer-live-p draft-buffer))
              (with-current-buffer draft-buffer
                (should (string-match-p "Body text" (buffer-string))))
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
        (send)
        success)))))

(ert-deftest mu4e-autotask-test-send-email-cancel ()
  "Declining the send discards the modified draft without a second prompt.
The compose buffer has been modified by the inserted body, so the real
`message-kill-buffer' would normally ask \"kill anyway?\"; declining the send
must suppress that and kill the buffer unconditionally, then signal `user-error'."
  (let ((tmpl
         (make-mu4e-autotask-email-template
          :context "ctx" :to "to@x" :subject "Subj" :body "Body text"))
        (buf nil))
    (cl-letf (((symbol-function 'mu4e-context-switch) #'ignore)
              ((symbol-function 'mu4e-compose-new) #'ignore)
              ((symbol-function 'message-goto-body) #'ignore)
              ((symbol-function 'message-field-value)
               #'mu4e-autotask-test--message-subject)
              ((symbol-function 'y-or-n-p)
               #'mu4e-autotask-test--refuse-send)
              ((symbol-function 'yes-or-no-p)
               #'mu4e-autotask-test--unexpected-prompt))
      (with-temp-buffer
        (setq buf (current-buffer))
        (should-error
         (mu4e-autotask-send-email tmpl nil #'ignore) :type 'user-error)))
    (should-not (buffer-live-p buf))))

(ert-deftest mu4e-autotask-test-do-send-email-confirm ()
  "`mu4e-autotask-do-send-email' confirms, wires SUCCESS-FN, and sends.
This is the entry point for action functions that compose a message themselves
and then want the same confirm-and-send behavior."
  (let* ((events nil)
         (success (lambda () (push 'success events))))
    (cl-letf (((symbol-function 'message-field-value)
               #'mu4e-autotask-test--message-subject)
              ((symbol-function 'y-or-n-p)
               #'mu4e-autotask-test--confirm-send)
              ((symbol-function 'message-send-and-exit)
               (lambda ()
                 (push 'send events)
                 (run-hooks 'message-sent-hook))))
      (with-temp-buffer
        (mu4e-autotask-do-send-email "to@x" success)))
    (should (equal (nreverse events) '(send success)))))

(ert-deftest mu4e-autotask-test-do-send-email-decline ()
  "Declining the send discards the compose buffer and signals `user-error'."
  (let ((buf nil)
        (sent nil))
    (cl-letf (((symbol-function 'message-field-value)
               #'mu4e-autotask-test--message-subject)
              ((symbol-function 'y-or-n-p)
               #'mu4e-autotask-test--refuse-send)
              ((symbol-function 'message-send-and-exit)
               (lambda () (setq sent t))))
      (with-temp-buffer
        (setq buf (current-buffer))
        (should-error
         (mu4e-autotask-do-send-email "to@x" #'ignore) :type 'user-error)))
    (should-not sent)
    (should-not (buffer-live-p buf))))

(ert-deftest mu4e-autotask-test-do-send-email-success-fn-error-contained ()
  "An error in SUCCESS-FN becomes a warning, not a failed send.
The message is already sent when SUCCESS-FN runs from `message-sent-hook',
but the send machinery's cleanup is not; an escaping error would leave the
sent message in a re-sendable compose buffer."
  (let ((warnings nil))
    (cl-letf (((symbol-function 'message-field-value)
               #'mu4e-autotask-test--message-subject)
              ((symbol-function 'y-or-n-p)
               #'mu4e-autotask-test--confirm-send)
              ((symbol-function 'message-send-and-exit)
               #'mu4e-autotask-test--run-sent-hooks)
              ((symbol-function 'display-warning)
               (lambda (type message &optional level &rest _)
                 (push (list type message level) warnings))))
      (with-temp-buffer
        (mu4e-autotask-do-send-email
         "to@x" #'mu4e-autotask-test--failing-success-fn)))
    (should (equal (length warnings) 1))
    (should (eq (nth 0 (car warnings)) 'mu4e-autotask))
    (should (string-match-p "Recording failed" (nth 1 (car warnings))))
    (should (eq (nth 2 (car warnings)) :error))))

(ert-deftest mu4e-autotask-test-do-send-email-success-fn-quit-contained ()
  "A quit in SUCCESS-FN becomes a warning, not an aborted cleanup.
`quit' is not an `error' subtype, so without an explicit handler a C-g
during the follow-up action would escape `message-sent-hook' and leave the
sent message in a re-sendable compose buffer."
  (let ((warnings nil))
    (cl-letf (((symbol-function 'message-field-value)
               #'mu4e-autotask-test--message-subject)
              ((symbol-function 'y-or-n-p)
               #'mu4e-autotask-test--confirm-send)
              ((symbol-function 'message-send-and-exit)
               #'mu4e-autotask-test--run-sent-hooks)
              ((symbol-function 'display-warning)
               (lambda (type message &optional level &rest _)
                 (push (list type message level) warnings))))
      (with-temp-buffer
        (mu4e-autotask-do-send-email
         "to@x" #'mu4e-autotask-test--quitting-success-fn)))
    (should (equal (length warnings) 1))
    (should (eq (nth 0 (car warnings)) 'mu4e-autotask))
    (should (string-match-p "Quit" (nth 1 (car warnings))))
    (should (eq (nth 2 (car warnings)) :error))))

(provide 'mu4e-autotask-test)
;;; mu4e-autotask-test.el ends here
