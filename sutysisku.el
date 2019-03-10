;;; -*- lexical-binding: t -*-
;;; sutysisku.el --- Lojban dictionary for Emacs
;; Copyright (C) 2018 - 2019 Dustin Lacewell

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24") (request "0") (helm "0") (a "0") (ivy "0"))
;; Keywords: lojban dictionary helm ivy
;; URL: http://github.com/dustinlacewell/sutysisku.el


;;; Commentary:

;; This package offers a Lojban dictionary, searchable with Helm or Ivy.

;;; Code:
;;;; Requirements
(require 'cl)
(require 'a)
(require 'helm)
(require 'ivy)
(require 'request)

;;;; Customization

(defgroup sutysisku nil
  "Settings for `sutysisku'."
  :link '(url-link "http://github.com/dustinlacewell/sutysisku.el"))

(defcustom sutysisku-data-url
  "https://rawgit.com/La-Lojban/sutysisku/master/data/parsed-en.js"
  "URL to JSON file containing dictionary data"
  :type 'string)

;;;; Variables

(defvar sutysisku--data nil
  "Contains a list of all dictionary entries.")

(setq sutysisku-show-all t)

(setq sutysisku-helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "TAB") 'hydra-sutysisku/body)
    map))


(setq sutysisku-include-gismu t)
(setq sutysisku-include-lujvo t)
(setq sutysisku-include-fuhivla t)
(setq sutysisku-include-cmevla t)
(setq sutysisku-include-cmavo t)
(setq sutysisku-include-letterals t)
(setq sutysisku-include-compounds t)
(setq sutysisku-include-experimental t)
(setq sutysisku-include-obsolete nil)



;;;; Boilerplate
;;;;; propertize-regex
(defun sutysisku--propertize-regex (regexp string &rest props)
  (let* ((matches (s-matched-positions-all regexp string)))
    (cl-loop for match in matches
             for start = (car match)
             for end = (cdr match)
             do (add-text-properties start end props string))
    string))

;;;;; format-display
(defun sutysisku--format-display (record)
  (format
   "%s %s : %s\n%s"
   (propertize (a-get record :word)
               'face '(:height 2.0 :weight bold))
   (propertize (a-get record :type)
               'face '(:foreground "cyan" :slant italic))
   (propertize (a-get record :gloss)
               'face '(:foreground "light grey" :slant italic))
   (sutysisku--propertize-regex
    "\\[[^]]*\\]"
    (sutysisku--propertize-regex
     "X[[:digit:]]+"
     (a-get record :definition)
     'face '(:foreground "deep sky blue"))
    'face '(:foreground "dim grey"))))

;;;;; candidates
(defun sutysisku--candidates (str)
  (when (and (not (equal str nil)) (not (equal str "")))
    (let ((exact)
          (gloss-exact)
          (word-prefix)
          (word-substring)
          (gloss-prefix)
          (gloss-substring)
          (definition-substring))

      (cl-loop for item in sutysisku--data
               for display = (car item)
               for record = (cdr item)
               for word = (a-get record :word)
               for gloss = (a-get record :gloss)
               for type = (a-get record :type)
               for definition = (a-get record :definition)
               do (add-text-properties 0 1 `(record ,record) display)
               do (cond
                   ((s-equals? str word)
                    (setf exact (append (list item) exact)))

                   ((s-equals? str gloss)
                    (setf gloss-exact (append (list item) exact)))

                   ((s-prefix? str word)
                    (setf word-prefix (append (list item) word-prefix)))

                   ((s-contains? str word)
                    (setf word-substring (append (list item) word-substring)))

                   ((s-prefix? str gloss)
                    (setf gloss-prefix (append (list item) gloss-prefix)))

                   ((s-contains? str gloss)
                    (setf gloss-substring (append (list item) gloss-substring)))

                   ((s-contains? str definition t)
                    (setf definition-substring (append (list item) definition-substring)))))
      (append
       (reverse exact) (reverse gloss-exact)
       (reverse word-prefix) (reverse word-substring)
       (reverse gloss-prefix) (reverse gloss-substring)
       (reverse definition-substring)))))

;;;; Request Boilerplate
;;;;; clean-definition
(defun sutysisku--clean-definition (definition)
  (replace-regexp-in-string
   "\\$[[:alpha:]]+_\\([[:digit:]]\\)=[[:alpha:]]+_[[:digit:]]+\\$" "X\\1"
   (replace-regexp-in-string
    "\\$[[:alpha:]]_\\([[:digit:]]\\)\\$" "X\\1"
    (replace-regexp-in-string
     "\\$[[:alpha:]]_\{\\([[:digit:]]+\\)\}\\$" "X\\1"
     (decode-coding-string definition 'utf-8)))))

;;;;; clean-data
(defun sutysisku--clean-data (candidates)
  (let ((candidates (cl-loop for c in candidates
                             for word = (format "%s" (car c))
                             for record = (cdr c)
                             for definition = (sutysisku--clean-definition (a-get record 'd))
                             for type = (a-get record 't)
                             for gloss = (decode-coding-string (a-get record 'g) 'utf-8)
                             for record = (a-list :word word
                                                  :gloss gloss
                                                  :type type
                                                  :definition definition)
                             for display = (sutysisku--format-display record)
                             for full-record = (a-assoc record :display display )
                             collect (cons display full-record))))
    (message "Finished cleaning.")
    candidates))

;;;; Helm Boilerplate
;;;;; hydra-sutysisku
(nougat-hydra hydra-sutysisku (:color red)
  ("  Copy"
   (("w" (let ((candidates (helm-marked-candidates)))
           (kill-new (string-join
                      (cl-loop for c in candidates
                               for record = (cdr c)
                               collect (a-get record :word))
                      ", "))) "word" :color blue)
    ("s" (let ((candidates (helm-marked-candidates)))
           (kill-new (string-join
                      (cl-loop for c in candidates
                               for record = (cdr c)
                               for word = (a-get record :word)
                               for definition = (a-get record :definition)
                               collect (format "%s - %s" word definition))
                      ", "))) "simple" :color blue)
    ("a" (let ((candidates (helm-marked-candidates)))
           (kill-new (string-join
                      (cl-loop for c in candidates
                               for record = (cdr c)
                               for word = (a-get record :word)
                               for type = (a-get record :type)
                               for gloss = (a-get record :gloss)
                               for definition = (a-get record :definition)
                               collect (format "%s (%s) / %s: %s" word type gloss definition))
                      ", "))
           (call-interactively 'helm-keyboard-quit)) "all" :color blue))
   "  Only"
   (("G" (progn
           (setq sutysisku-include-gismu t)
           (setq sutysisku-include-lujvo nil)
           (setq sutysisku-include-fuhivla nil)
           (setq sutysisku-include-cmavo nil)
           (setq sutysisku-include-cmene nil)
           (setq sutysisku-include-letterals nil)
           (helm-update)) "gismu")
    ("L" (progn
           (setq sutysisku-include-gismu nil)
           (setq sutysisku-include-lujvo t)
           (setq sutysisku-include-fuhivla nil)
           (setq sutysisku-include-cmavo nil)
           (setq sutysisku-include-cmene nil)
           (setq sutysisku-include-letterals nil)
           (helm-update)) "lujvo")
    ("F" (progn
           (setq sutysisku-include-gismu nil)
           (setq sutysisku-include-lujvo nil)
           (setq sutysisku-include-fuhivla t)
           (setq sutysisku-include-cmavo nil)
           (setq sutysisku-include-cmene nil)
           (setq sutysisku-include-letterals nil)
           (helm-update)) "fu'ivla")
    ("C" (progn
           (setq sutysisku-include-gismu nil)
           (setq sutysisku-include-lujvo nil)
           (setq sutysisku-include-fuhivla nil)
           (setq sutysisku-include-cmavo t)
           (setq sutysisku-include-cmene nil)
           (setq sutysisku-include-letterals nil)
           (helm-update)) "cmavo")
    ("N" (progn
           (setq sutysisku-include-gismu nil)
           (setq sutysisku-include-lujvo nil)
           (setq sutysisku-include-fuhivla nil)
           (setq sutysisku-include-cmavo nil)
           (setq sutysisku-include-cmene t)
           (setq sutysisku-include-letterals t)
           (helm-update)) "cmene"))
   "  Toggle"
   (("g" (progn
           (setq sutysisku-include-gismu (not sutysisku-include-gismu))
           (helm-update)) "gismu   %`sutysisku-include-gismu")
    ("l" (progn
           (setq sutysisku-include-lujvo (not sutysisku-include-lujvo))
           (helm-update)) "lujvo   %`sutysisku-include-lujvo")
    ("f" (progn
           (setq sutysisku-include-fuhivla (not sutysisku-include-fuhivla))
           (helm-update)) "fu'ivla %`sutysisku-include-fuhivla")
    ("c" (progn
           (setq sutysisku-include-cmavo (not sutysisku-include-cmavo))
           (helm-update)) "cmavo   %`sutysisku-include-cmavo")
    ("n" (progn
           (setq sutysisku-include-cmevla (not sutysisku-include-cmevla))
           (helm-update)) "cmevla  %`sutysisku-include-cmevla")
    ("l" (progn
           (setq sutysisku-include-letterals (not sutysisku-include-letterals))
           (helm-update)) "letterals  %`sutysisku-include-letterals")
    ("o" (progn
           (setq sutysisku-include-compounds (not sutysisku-include-compounds))
           (helm-update)) "compounds  %`sutysisku-include-compounds")
    ("e" (progn
           (setq sutysisku-include-experimental (not sutysisku-include-experimental))
           (helm-update)) "experimental  %`sutysisku-include-experimental"))))

;;;;; helm-source

(defvar sutysisku--helm-source
      '((name . "sutysisku.el")
        (volatile)
        (multiline)
        (candidates . (lambda ()
                        (let* ((candidates (if (and (equal helm-pattern "") sutysisku-show-all)
                                               sutysisku--data
                                             (sutysisku--candidates helm-pattern))))
                          (message "%s canidates" (length candidates))
                          (cl-loop for c in candidates
                                   for record = (cdr c)
                                   for type = (a-get record :type)
                                   if (and
                                       (or sutysisku-include-compounds (not (cl-search "compound" type)))
                                       (or sutysisku-include-experimental (not (cl-search "experiment" type)))
                                       (or sutysisku-include-gismu (not (cl-search "gismu" type)))
                                       (or sutysisku-include-lujvo (not (cl-search "lujvo" type)))
                                       (or sutysisku-include-fuhivla (not (cl-search "fu'ivla" type)))
                                       (or sutysisku-include-cmevla (not (cl-search "cmevla" type)))
                                       (or sutysisku-include-cmavo (not (cl-search "cmavo" type)))
                                       (or sutysisku-include-letterals (not (cl-search "letteral" type)))
                                       (or sutysisku-include-obsolete (not (cl-search "obsolete" type))))
                                   collect c))))))
;;;; Ivy Boilerplate
(defun sutysisku--search-ivy-kill-word-action (entry)
  (let* ((record (get-text-property 0 'record entry))
         (word (a-get record :word)))
    (kill-new word)))

(defun sutysisku--search-ivy-kill-definition-action (entry)
  (let* ((record (get-text-property 0 'record entry)))
    (kill-new (a-get record :definition))))

(defun sutysisku--search-ivy-kill-gloss-action (entry)
  (let* ((record (get-text-property 0 'record entry)))
    (kill-new (a-get record :gloss))))

(defun sutysisku--search-ivy-kill-all-action (entry)
  (let* ((record (get-text-property 0 'record entry)))
    (kill-new (format "%s (%s): %s"
                      (a-get record :word)
                      (a-get record :gloss)
                      (a-get record :definition)))))

;;;; API
;;;;; fetch
(defun sutysisku-fetch (&optional then)
  (interactive)
  (message "Downloading wordlist...")
  (request
   sutysisku-data-url
   :sync nil
   :parser (lambda () (search-forward "= ") (json-read))
   :error (lambda (&key error-thrown &accept-other-keys &rest _)
            (message (format "Error: %s" error-thrown)))
   :success (lambda (&key data &accept-other-keys &rest _)
              (message (format "%s words downloaded. Cleaning..." (length data)))
              (setq sutysisku--data (sutysisku--clean-data data))
              (message "Done!")
              (when (functionp then) (funcall then)))))
;;;;; search-helm
(defun sutysisku-search-helm ()
  (interactive)
  (if (> (length sutysisku--data) 0)
      (helm
       :candidate-number-limit nil
       :sources 'sutysisku--helm-source
       :keymap sutysisku-helm-map
       :marked-with-props 'record
       :buffer "*sutysisku.el*")
    (sutysisku-fetch 'sutysisku-search-helm)))
;;;;; search-ivy
(defun sutysisku-search-ivy ()
  (interactive)
  (if (> (length sutysisku--data) 0)
      (progn (setq this-command 'sutysisku-search-ivy)
             (ivy-read
              ": " 'sutysisku--candidates
              :dynamic-collection t
              :action 'sutysisku--search-ivy-kill-word-action))
    (sutysisku-fetch 'sutysisku-search-ivy)))

(ivy-set-actions
 'sutysisku-search-ivy
 '(("w" sutysisku--search-ivy-kill-word-action "Word")
   ("g" sutysisku--search-ivy-kill-gloss-action "Gloss")
   ("d" sutysisku--search-ivy-kill-definition-action "Definition")
   ("a" sutysisku--search-ivy-kill-all-action "All")))

(provide 'sutysisku)

;;; sutysisku.el ends here
