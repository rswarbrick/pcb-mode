;;; pcb-mode.el --- major mode providing a pcb mode hook for Emacs

;; TODO:
;; - Indentation
;; - Abbrevs
;; - Templating incl. "new footprint"
;; - Load in PCB
;; - Speedbar/Imenu navigation
;; - Delete auto-mode-alist bit.

(let ((pair (cons (concat "\\." (regexp-opt '("pcb" "fp"))) 'pcb-mode)))
  (setf auto-mode-alist (cons pair (remove pair auto-mode-alist))))

(defgroup pcb-mode nil
  "Customizations for PCB mode."
  :prefix "pcb-" :group 'text)

(defcustom pcb-mode-hook nil
  "List of hook functions run by `pcb-mode' (see `run-hooks')."
  :type 'hook
  :group 'pcb)

(defconst pcb-mode-version 0 "Current version of PCB mode")

;;; Font lock and syntax recognition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst pcb-mode-keywords
  '("Arc" "Attribute" "Connect" "Cursor" "DRC" "Element" "ElementArc"
    "ElementLine" "FileVersion" "Flags" "Grid" "Groups" "Layer" "Line"
    "Mark" "Net" "Netlist" "Pad" "Pin" "PolyArea" "Polygon" "Rat"
    "Styles" "Symbol" "SymbolLine" "Text" "Thermal" "Via")
  "The various PCB keywords.")

(defconst pcb-mode-keyword-regexp
  (regexp-opt pcb-mode-keywords)
  "An optimised regexp that matches `pcb-keywords'.")

(defconst pcb-mode-syntax-table
  (let ((table (make-syntax-table)))
    (with-syntax-table table
      (modify-syntax-entry ?\# "<")
      (modify-syntax-entry ?\n ">")
      (modify-syntax-entry ?\' "\"")
      table)))

(defconst pcb-mode-syntax-propertize-function
  (syntax-propertize-rules
   ("\\(?:.\\|\n\\)'" (0 (ignore (pcb-mode-char-constant-propertize)))))
  "In PCB mode, syntax-propertize-function is used to match
character constants, written in the form 'x'. This is because PCB
treats ''' as a character constant containing a quote, and the
standard Emacs syntax machinery doesn't support that.")

(defun pcb-mode-char-constant-propertize ()
  "Correctly marks syntax for special characters inside ''
constants. When called (via pcb-mode-syntax-propertize-function),
point is just past a quote, which is known not to be the first
character of the buffer.

Basically, this just has to suppress any special syntax of a
character immediately following a \"starting\" quote."
  (let* ((pos (1- (point)))
         (ppss (prog1 (syntax-ppss pos) (goto-char (1+ pos))))
         (in-string-p (and (not (nth 4 ppss)) (nth 8 ppss))))
    ;; This quote character should a single char constant, and there is a
    ;; following character to de-fang. "w" seems slightly wrong, but there's no
    ;; "nothing special" syntax on offer.
    (when (and (not in-string-p) (< (point) (point-max)))
      (put-text-property
       (1+ pos) (+ pos 2) 'syntax-table (string-to-syntax "w")))))

;;; Finally set everything up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pcb-mode ()
  ;; Set up commenting functionality.
  (set (make-local-variable 'comment-start) "#")

  ;; Font lock keywords
  (set (make-local-variable 'font-lock-defaults) '(pcb-mode-keywords))
  (font-lock-set-defaults)

  ;; Syntax recognition
  (set-syntax-table pcb-mode-syntax-table)
  (set (make-local-variable 'syntax-propertize-function)
       pcb-mode-syntax-propertize-function)

  ;; Mode name
  (setq mode-name "PCB"))


;; Make "require" work
(provide 'pcb-mode)

;;; pcb-mode.el ends here
