;;; jaword.el --- Minor-mode for handling Japanese words better

;; Copyright (C) 2014 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 1.0.0beta
;; Package-Requires: ((tinysegmenter "0.1"))

;;; Commentary:

;; This script provides a minor-mode that improves
;; backward/forward-word behavior for Japanese words.

;; tinysegmenter.el とこのファイルを load-path の通ったディレクトリに置
;; いて、ロードする。
;;
;;   (require 'jaword)
;;
;; "jaword-mode" で jaword-mode の有効を切り替える。すべてのバッファで
;; 有効にするには "global-jaword-mode" を用いる。
;;
;; jaword-mode は subword-mode と同時に有効にすることができないが、
;; jaword-mode はデフォルトで "hogeFugaPiyo" のような単語を３つの独立し
;; た単語として扱う。これを無効にするためには、
;; "jaword-enable-subword" を nil に設定する。
;;
;;   (setq jaword-enable-subword nil)

;;; Change Log:

;;; Code:

(require 'tinysegmenter)
(require 'subword)

(defconst jaword-version "1.0.0beta")

(defgroup jaword nil
  "Minor-mode for handling Japanese words better."
  :group 'emacs)

(defcustom jaword-buffer-size 50
  "size of text passed to the segmenter. set larger for better
accuracy, but slower speed."
  :group 'jaword)

(defcustom jaword-enable-subword t
  "when non-nil, handle subwords like \"hogeFugaPiyo\"."
  :group 'jaword)

(defun jaword--segment-around-point ()
  (let* ((back (replace-regexp-in-string
                "[\s\t\n]" ""
                (buffer-substring-no-properties
                 (max 1 (- (point) (lsh jaword-buffer-size -1)))
                 (point))))
         (forward (replace-regexp-in-string
                   "[\s\t\n]" ""
                   (buffer-substring-no-properties
                    (point)
                    (min (1+ (buffer-size)) (+ (point) (lsh jaword-buffer-size -1))))))
         (str (concat back forward))
         (back-len (length back))
         (_ (when (> (length forward) 0)
              (put-text-property back-len (1+ back-len) 'base-pos t str)))
         (segments (apply 'vector (tseg-segment str)))
         ;; ----
         (n 0) segment pos)
    (while (and (< n (length segments))
                (progn
                  (setq segment (aref segments n))
                  (setq pos (text-property-any 0 (length segment) 'base-pos t segment))
                  (not pos)))
      (cl-incf n))
    (cons (if (and pos (zerop pos))
              (and (> n 0) (aref segments (1- n)))
            (substring segment 0 pos))
          (and (< n (length segments))
               (if (= pos (length segment))
                   (and (< (1+ n) (length segments)) (aref segments (1+ n)))
                 (substring segment pos (length segment)))))))

;;;###autoload
(defun jaword-backward (arg)
  "Like backward-word, but handles Japanese words better."
  (interactive "p")
  (if (< arg 0)
      (jaword-forward (- arg))
    (let (segment)
      (dotimes (_ arg)
        (if (and (looking-back "\\Ca[\s\t\n]*")
                 (setq segment (car (jaword--segment-around-point))))
            (search-backward-regexp (mapconcat 'string segment "[\s\t\n]*"))
          (if jaword-enable-subword
              (subword-backward 1)
            (backward-word 1)))))))

;;;###autoload
(defun jaword-forward (arg)
  "Like forward-word, but handle Japanese words better."
  (interactive "p")
  (if (< arg 0)
      (jaword-backward (- arg))
    (let (segment)
      (dotimes (_ arg)
        (if (and (looking-at "[\s\t\n]*\\Ca")
                 (setq segment (cdr (jaword--segment-around-point))))
            (search-forward-regexp (mapconcat 'string segment "[\s\t\n]*"))
          (if jaword-enable-subword
              (subword-forward 1)
            (forward-word 1)))))))

;;;###autoload
(defun jaword-mark (&optional arg allow-extend)
  "Like mark-word, but handle Japanese words better."
  (interactive "P\np")
  ;; based on "mark-word" in "simple.el"
  (cond ((and allow-extend
              (or (and (eq last-command this-command)
                       (mark t))
                  (region-active-p)))
         (setq arg (cond (arg (prefix-numeric-value arg))
                         ((< (mark) (point)) -1)
                         (t 1)))
         (set-mark (save-excursion
                     (goto-char (mark))
                     (jaword-forward arg)
                     (point))))
        (t
         (push-mark (save-excursion
                      (jaword-forward (prefix-numeric-value arg))
                      (point))
                    nil t))))

;;;###autoload
(defun jaword-kill (n)
  "Like kill-word, but handle Japanese words better."
  (interactive "p")
  (kill-region (point) (progn (jaword-forward n) (point))))

;;;###autoload
(defun jaword-backward-kill (n)
  "Like backward-kill-word, but handle Japanese words better."
  (interactive "p")
  (jaword-kill (- n)))

;;;###autoload
(defun jaword-transpose (n)
  "Like transpose-words, but handle Japanese words better."
  (interactive "*p")
  (transpose-subr 'jaword-forward n))

(defvar jaword-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [remap backward-word] 'jaword-backward)
    (define-key kmap [remap forward-word] 'jaword-forward)
    (define-key kmap [remap kill-word] 'jaword-kill)
    (define-key kmap [remap backward-kill-word] 'jaword-backward-kill)
    (define-key kmap [remap transpose-words] 'jaword-transpose)
    kmap))

(define-minor-mode jaword-mode
  "Toggle Japanese word movement and editing."
  :init-value nil
  :global nil
  :keymap jaword-mode-map)

(define-globalized-minor-mode global-jaword-mode jaword-mode
  (lambda () (jaword-mode 1)))

(provide 'jaword)

;;; jaword.el ends here
