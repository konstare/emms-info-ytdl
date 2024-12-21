;;; emms-info-ytdl.el --- Info method for EMMS using a ytdl program  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING..  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; (add-to-list 'emms-info-functions #'emms-info-ytdl)

;; To use this you would need to have `emms-info-ytdl-command`
;; (typically youtube-dl or yt-dlp) installed on your system.

;;; Code:

(eval-when-compile
  (declare-function json-read "json.c"))

(require 'emms)

(defgroup emms-info-ytdl nil
  "EMMS info method for web videos using a ytdl program ('yt-dlp' by default)."
  :group 'emms-info)

(defconst emms-info-ytdl-field-map
  '((info-title        . title)
    (info-artist       . artist)
    (info-playing-time . duration))
  "Map used to parse `emms-info-ytdl-command's output.")

(defvar emms-info-ytdl-regexp
  "^https?://"
  "URLs for which `emms-info-ytdl' is used.")

(defconst emms-info-ytdl-exclude-regexp
  "\\(\\.\\w+$\\|/playlist\\|/channel\\)"
  "URLs for which `emms-info-ytdl' isn't used.")

(defcustom emms-info-ytdl-command "yt-dlp"
  "Command to run for `emms-info-ytdl'."
  :type '(file :validate
               (lambda (w)
                 (unless (executable-find (widget-value w))
                   (widget-put w :error "Command not found")
                   w))))

;;;###autoload
(defun emms-info-ytdl (track)
  "Set TRACK info using `emms-info-ytdl-program'."
  (when-let*
      ((ytdl (executable-find emms-info-ytdl-command))
       (coding-system-for-read 'utf-8)
       ((eq (emms-track-type track) 'url))
       (name (emms-track-name track))
       ((string-match-p emms-info-ytdl-regexp name))
       ((not (string-match-p emms-info-ytdl-exclude-regexp name)))
       (stderr (get-buffer-create "*emms-info-ytdl-warnings*")))
    (make-process :name "emms-info-ytdl"
                  :buffer (get-buffer-create
                           (format " *emms-info-ytdl-%s*" name))
                  :command `( ,ytdl "-j" ,name)
                  :noquery t
                  :sentinel
                  `(lambda (proc event)
                     (if (string-match-p "finished" event)
                         (with-current-buffer (process-buffer proc)
			   (goto-char (point-min))
                           (condition-case nil
                               (when-let* (((functionp 'json-available-p))
                                           ((json-available-p))
                                           (json-fields (json-read)))
                                 (dolist (field emms-info-ytdl-field-map)
                                   (when-let* ((emms-field (car field))
                                               (ytdl-field (cdr field))
                                               (track-field
                                                (assoc ytdl-field json-fields)))
                                     (emms-track-set
                                      ',track emms-field
                                      (if (eq emms-field 'info-playing-time)
                                          (truncate (cdr track-field))
                                        (cdr track-field))))))
                             (error
                              (message
                               "emms-info-ytdl: error parsing info for %s"
                               ,name)))
                           (emms-playlist-track-updated ',track)
                           (kill-buffer (current-buffer)))
                       (message "emms-info-ytdl: process failed for %s"
                                ,name)))
                  :stderr stderr)
    (set-process-sentinel (get-buffer-process stderr) #'ignore)))

(provide 'emms-info-ytdl)

;;; emms-info-ytdl.el ends here
