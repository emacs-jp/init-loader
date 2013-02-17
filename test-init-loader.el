;;; test-init-loader.el --- Test for init-loader.el

;; Copyright (C) 2013 by emacs-jp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'ert)
(require 'init-loader)

(defvar init-loader-test-files
  '("00_utils.el"
    "23_yaml.el"
    "01_ik-cmd.el"
    "96_color.el"
    "20_elisp.el"
    "21_javascript.el"
    "25_perl.el"
    "98_emacs-config.el"
    "99_global-keys.el"
    "carbon-emacs-config.el"
    "carbon-emacs-migemo.el"
    "nw-config.el"
    "emacs-migemo.el"
    "windows-powershell.el"
    "windows-fonts.el"
    "meadow-cmd.el"
    "meadow-config.el"
    "meadow-gnuserv.el"
    "meadow-shell.el"
    "meadow-w32-symlinks.el"
    "linux-fonts.el"
    "linux-commands.el"))

;; TODO flet is obsoleted from Emacs 24.3

(ert-deftest init-loader--re-load-files ()
  "Test for `init-loader--re-load-files'"
  (flet ((directory-files (dir &optional full match nosort)
                          init-loader-test-files))
    (let ((got (init-loader--re-load-files init-loader-default-regexp "" t))
          (expected '("00_utils.el" "01_ik-cmd.el" "20_elisp.el"
                      "21_javascript.el" "23_yaml.el" "25_perl.el"
                      "96_color.el" "98_emacs-config.el" "99_global-keys.el")))
      (should (equal got expected)))

    (let ((got (init-loader--re-load-files init-loader-windows-regexp "" t))
          (expected '("windows-fonts.el" "windows-powershell.el")))
      (should (equal got expected)))

    (let ((got (init-loader--re-load-files init-loader-meadow-regexp "" t))
          (expected '("meadow-cmd.el" "meadow-config.el" "meadow-gnuserv.el"
                      "meadow-shell.el" "meadow-w32-symlinks.el")))
      (should (equal got expected)))

    (let ((got (init-loader--re-load-files init-loader-carbon-emacs-regexp "" t))
          (expected '("carbon-emacs-config.el" "carbon-emacs-migemo.el")))
      (should (equal got expected)))

    (let ((got (init-loader--re-load-files init-loader-linux-regexp "" t))
          (expected '("linux-commands.el" "linux-fonts.el")))
      (should (equal got expected)))

    (let ((got (init-loader--re-load-files init-loader-nw-regexp "" t))
          (expected '("nw-config.el")))
      (should (equal got expected)))

    ;; accept '.elc' files
    (push "nw-added.elc" init-loader-test-files)
    (let ((got (init-loader--re-load-files init-loader-nw-regexp "" t))
          (expected '("nw-added.elc" "nw-config.el")))
      (should (equal got expected)))
    (pop init-loader-test-files)))

(ert-deftest init-loader-follow-symlink ()
  "Test for `init-loader-follow-symlink'"
  (flet ((directory-files (dir &optional full match nosort)
                          init-loader-test-files))

    (let ((symlink "symlink.el")
          (thisfile "test-init-loader.el"))
      ;; setup
      (make-symbolic-link thisfile symlink t)

      ;; symbolic link
      (let ((expected (expand-file-name (concat default-directory thisfile))))
        (should (string= (init-loader-follow-symlink symlink) expected)))

      ;; not symbolic link
      (let ((expected (getenv "HOME")))
        (should (string= (init-loader-follow-symlink "~") expected)))

      ;; teardown
      (delete-file symlink))))

(ert-deftest init-loader-log ()
  "Test for `init-loader-log'"
  ;; pass not string value
  (should-not (init-loader-log 1))

  ;; log message
  (init-loader-log "message1")
  (should (string= "message1" (init-loader-log)))

  ;; log message again
  (init-loader-log "message2")
  (should (string= "message1\nmessage2" (init-loader-log))))

(ert-deftest init-loader-error-log ()
  "Test for `init-loader-error-log'"
  ;; pass not string value
  (should-not (init-loader-error-log 1))

  ;; log message
  (init-loader-error-log "message1")
  (should (string= "message1" (init-loader-error-log)))

  ;; log message again
  (init-loader-error-log "message2")
  (should (string= "message1\nmessage2" (init-loader-error-log))))

(defvar is-byte-compiled nil)
(defvar is-loaded nil)
(defvar is-deleted nil)

(defun test-init-loader-clear-flags ()
  (setq is-byte-compiled nil is-loaded nil is-deleted nil))

(ert-deftest init-loader-load-file ()
  "Test for `init-loader-load-file'"

  (flet ((byte-compile-file (el)
                            (setq is-byte-compiled t))
         (load (file)
               (setq is-loaded t))
         (delete-file (file)
                      (setq is-deleted t))
         (locate-library (lib) lib))

    ;; not byte compile
    (let ((init-loader-byte-compile nil))
      (init-loader-load-file "foo")
      (should (and (not is-byte-compiled) is-loaded)))

    (test-init-loader-clear-flags)

    (let ((init-loader-byte-compile t))
      (init-loader-load-file "foo")
      (should (and is-byte-compiled (not is-deleted)
                   is-loaded)))

    (test-init-loader-clear-flags)

    (let ((init-loader-byte-compile t))
      ;; .elc file is older than .el file
      (shell-command "touch foo.elc ; sleep 1")
      (shell-command "touch foo.el")

      (init-loader-load-file "foo")
      (should (and is-byte-compiled is-deleted is-loaded))

      (shell-command "rm -f foo.el foo.elc"))))

;;; test-init-loader.el end here
