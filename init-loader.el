;;; init-loader.el --- Loader for configuration files

;; Author: IMAKADO <ken.imakado@gmail.com>
;; URL: https://github.com/emacs-jp/init-loader/
;; Version: 0.01

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;; Load `init-loader.el'
;;
;; (require 'init-loader)
;;
;; Load configuration files in `/path/to/init-directory'.
;; If you omit arguments, `init-loader.el' loads from `init-loader-directory'
;;
;; (init-loader-load "/path/to/init-directory")
;;
;;
;; Configuration files are loaded by following rules.
;;   1. Configuration files which start with two digits.
;;      Small number file is loaded earlier than large number file.
;;      e.g. "00_utils.el" "01_ik-cmd.el" "21_javascript.el" ... "99_global-keys.el"
;;
;;   2. Windows specific configuration files if system is Windows.
;;      First load files that start with 'windows-'. Second load files that start with 'meadow-'
;;      e.g. "windows-fonts.el", "windows-system.el", "meadow-commands.el", "meadow-fonts.el"
;;
;;   3. MacOSX specific configuration files if system is MacOSX
;;      First load files that start with 'carbon-'. Second load files that start with 'cocoa-'
;;      e.g. "carbon-applescript.el", "cocoa-fonts.el", "cocoa-plist.el"
;;
;;   4. No window Emacs specific configuration files which start with 'nw-'
;;      e.g. "nw-config.el", "nw-key.el"
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'benchmark)

;;; customize-variables
(defgroup init-loader nil
  "Loader of configuration files"
  :prefix "init-loader-"
  :group 'initialization)

(defcustom init-loader-directory
  (expand-file-name (concat (if (boundp 'user-emacs-directory)
                                (file-name-as-directory user-emacs-directory)
                              "~/.emacs.d/")
                            "inits"))
  "inits directory"
  :type 'directory
  :group 'init-loader)

(defcustom init-loader-show-log-after-init t
  "Show loading log message if this value is non-nil"
  :type 'boolean
  :group 'init-loader)

(defcustom init-loader-byte-compile nil
  "Byte-Compile configuration files if this value is non-nil"
  :type 'boolean
  :group 'init-loader)

(defcustom init-loader-default-regexp "\\(?:\\`[[:digit:]]\\{2\\}\\)"
  "Regexp of common configuration files

Default regexp matches files that start with two digits.
Example, 00_foo.el, 01_bar.el ... 99_keybinds.el"
  :type 'regexp
  :group 'init-loader)

(defcustom init-loader-meadow-regexp "\\`meadow-"
  "Regexp of Meadow specific configuration file"
  :group 'init-loader
  :type 'regexp)

(defcustom init-loader-windows-regexp "\\`windows-"
  "Regexp of Windows specific configuration file"
  :group 'init-loader
  :type 'regexp)

(defcustom init-loader-carbon-emacs-regexp "\\`carbon-emacs-"
  "Regexp of Carbon Emacs specific configuration file"
  :group 'init-loader
  :type 'regexp)

(defcustom init-loader-cocoa-emacs-regexp "\\`cocoa-emacs-"
  "Regexp of Cocoa Emacs specific configuration file"
  :group 'init-loader
  :type 'regexp)

(defcustom init-loader-nw-regexp "\\`nw-"
  "Regexp of no-window Emacs configuration file"
  :group 'init-loader
  :type 'regexp)

(defcustom init-loader-linux-regexp "\\`linux-"
  "Regexp of GNU/Linux specific configuration file"
  :group 'init-loader
  :type 'regexp)

;;;###autoload
(defun* init-loader-load (&optional (init-dir init-loader-directory))
  (let ((init-dir (init-loader-follow-symlink init-dir)))
    (assert (and (stringp init-dir) (file-directory-p init-dir)))
    (init-loader-re-load init-loader-default-regexp init-dir t)

    ;; Windows
    (when (featurep 'dos-w32)
      (init-loader-re-load init-loader-windows-regexp init-dir))
    ;; meadow
    (when (featurep 'meadow)
      (init-loader-re-load init-loader-meadow-regexp init-dir))

    ;; Carbon Emacs
    (when (featurep 'carbon-emacs-package)
      (init-loader-re-load init-loader-carbon-emacs-regexp init-dir))
    ;; Cocoa Emacs
    (when (equal window-system 'ns)
      (init-loader-re-load init-loader-cocoa-emacs-regexp init-dir))

    ;; GNU Linux
    (when (equal system-type 'gnu/linux)
      (init-loader-re-load init-loader-linux-regexp init-dir))

    ;; no-window
    (when (null window-system)
      (init-loader-re-load init-loader-nw-regexp init-dir))

    (when init-loader-show-log-after-init
      (add-hook  'after-init-hook 'init-loader-show-log))))

(defun init-loader-follow-symlink (dir)
  (cond ((file-symlink-p dir)
         (expand-file-name (file-symlink-p dir)))
        (t (expand-file-name dir))))

(lexical-let (logs)
  (defun init-loader-log (&optional s)
    (if s (and (stringp s) (push s logs)) (mapconcat 'identity (reverse logs) "\n"))))

(lexical-let (err-logs)
  (defun init-loader-error-log (&optional s)
    (if s (and (stringp s) (push s err-logs)) (mapconcat 'identity (reverse err-logs) "\n"))))

(defvar init-loader-before-compile-hook nil)
(defun init-loader-load-file (file)
  (when init-loader-byte-compile
    (let* ((path (file-name-sans-extension (locate-library file)))
           (el (concat path ".el")) (elc (concat path ".elc")))
      (when (or (not (file-exists-p elc))
                (file-newer-than-file-p el elc))
        (when (file-exists-p elc) (delete-file elc))
        (run-hook-with-args 'init-loader-before-compile-hook file)
        (byte-compile-file el))))
  (load file))

(defun init-loader-re-load (re dir &optional sort)
  ;; 2011/JUN/12 zqwell: Don't localize `load-path' and use it as global
  (add-to-list 'load-path dir)
  (dolist (el (init-loader--re-load-files re dir sort))
    (condition-case e
        (let ((time (car (benchmark-run (init-loader-load-file (file-name-sans-extension el))))))
          (init-loader-log (format "loaded %s. %s" (locate-library el) time)))
      (error
       ;; 2011/JUN/12 zqwell: Improve error message
       ;; See. http://d.hatena.ne.jp/kitokitoki/20101205/p1
       (init-loader-error-log (format "%s. %s" (locate-library el) (error-message-string e)))))))

;; 2011/JUN/12 zqwell Read first byte-compiled file if it exist.
;; See. http://twitter.com/#!/fkmn/statuses/21411277599
(defun init-loader--re-load-files (re dir &optional sort)
  (loop for el in (directory-files dir t)
        when (and (string-match re (file-name-nondirectory el))
                  (or (string-match "elc\\'" el)
                      (and (string-match "el\\'" el)
                           (not (locate-library (concat el "c"))))))
        collect (file-name-nondirectory el) into ret
        finally return (if sort (sort ret 'string<) ret)))

;;;###autoload
(defun init-loader-show-log ()
  "Show init-loader log buffer"
  (interactive)
  (let ((b (get-buffer-create "*init log*")))
    (with-current-buffer b
      (erase-buffer)
      (insert "------- error log -------\n\n"
              (init-loader-error-log)
              "\n\n")
      (insert "------- init log -------\n\n"
              (init-loader-log)
              "\n\n")
      ;; load-path
      (insert "------- load path -------\n\n"
              (mapconcat 'identity load-path "\n"))
      (goto-char (point-min)))
    (switch-to-buffer b)))

(provide 'init-loader)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init-loader.el ends here
