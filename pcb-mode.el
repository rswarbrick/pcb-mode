;;; pcb-mode.el --- major mode providing a pcb mode hook for Emacs
;;
;; Author: Rupert Swarbrick (2012)
;;
;; Documentation:
;; ==============
;;
;; PCB mode is a major mode for editing files for the gEDA PCB program within
;; Emacs. To load it automatically for both footprint (.fp) and pcb (.pcb)
;; files, add the following incantation to your .emacs:
;;
;; (let ((pair
;;        (cons (concat "\\." (regexp-opt '("pcb" "fp"))) 'pcb-mode)))
;;   (setf auto-mode-alist
;;         (cons pair (remove pair auto-mode-alist))))
;;
;; For more information on usage, see the docstring for `pcb-mode'.

(defgroup pcb-mode nil
  "Customizations for PCB mode."
  :prefix "pcb-" :group 'text)

(defconst pcb-mode-version 0 "Current version of PCB mode")

;;; Font lock and syntax recognition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst pcb-mode-keywords
  '(("Arc"
     (nil "X" "Y" "Width" "Height" "Thickness" "Clearance"
          "StartAngle" "DeltaAngle" "SFlags")
     ("(" "X" "Y" "Width" "Height" "Thickness"
          "StartAngle" "DeltaAngle" "SFlags"))
    ("Attribute" ("(" "Name" "Value"))
    ("Connect" ("(" "PinPad"))
    ("Cursor" (nil "X" "Y" "Zoom"))
    ("DRC"
     ("[" "Bloat" "Shrink" "Line" "Silk" "Drill" "Ring")
     ("[" "Bloat" "Shrink" "Line" "Silk")
     ("[" "Bloat" "Shrink" "Line"))
    ("Element"
     ("[" "SFlags" "\"Desc\"" "\"Name\"" "\"Value\"" "MX" "MY"
      "TX" "TY" "TDir" "TScale" "TSFlags")
     ("(" "NFlags" "\"Desc\"" "\"Name\"" "\"Value\"" "MX" "MY"
      "TX" "TY" "TDir" "TScale" "TNFlags")
     ("(" "NFlags" "\"Desc\"" "\"Name\"" "\"Value\""
      "TX" "TY" "TDir" "TScale" "TNFlags")
     ("(" "NFlags" "\"Desc\"" "\"Name\""
      "TX" "TY" "TDir" "TScale" "TNFlags")
     ("(" "\"Desc\"" "\"Name\""
      "TX" "TY" "TDir" "TScale" "TNFlags"))
    ("ElementArc"
     (nil "X" "Y" "Width" "Height"
          "StartAngle" "DeltaAngle" "Thickness"))
    ("ElementLine"
     (nil "X1" "Y1" "X2" "Y2" "Thickness"))
    ("FileVersion"
     ("[" "Version"))
    ("Flags"
     ("(" "Number"))
    ("Grid"
     (nil "Step" "OffsetX" "OffsetY" "Visible")
     ("(" "Step" "OffsetX" "OffsetY"))
    ("Groups"
     ("(" "\"String\""))
    ("Layer"
     ("(" "LayerNum" "\"Name\""))
    ("Line"
     ("[" "X1" "Y1" "X2" "Y2" "Thickness" "Clearance" "SFlags")
     ("(" "X1" "Y1" "X2" "Y2" "Thickness" "Clearance" "NFlags")
     ("(" "X1" "Y1" "X2" "Y2" "Thickness" "NFlags"))
    ("Mark" (nil "X" "Y"))
    ("Net"
     ("(" "\"Name\"" "\"Style\""))
    ("Netlist"
     ("("))
    ("Pad"
     ("[" "rX1" "rY1" "rX2" "rY2" "Thickness" "Clearance" "Mask"
      "\"Name\"" "\"Number\"" "SFlags")
     ("(" "rX1" "rY1" "rX2" "rY2" "Thickness" "Clearance" "Mask"
      "\"Name\"" "\"Number\"" "NFlags")
     ("(" "aX1" "aY1" "aX2" "aY2" "Thickness"
      "\"Name\"" "\"Number\"" "NFlags")
     ("(" "aX1" "aY1" "aX2" "aY2" "Thickness"
      "\"Name\"" "NFlags"))
    ("PCB"
     (nil "\"Name\"" "Width" "Height")
     ("(" "\"Name\""))
    ("Pin"
     ("[" "rX" "rY" "Thickness" "Clearance" "Mask" "Drill"
      "\"Name\"" "\"Number\"" "SFlags")
     ("(" "rX" "rY" "Thickness" "Clearance" "Mask" "Drill"
      "\"Name\"" "\"Number\"" "NFlags")
     ("(" "aX" "aY" "Thickness" "Drill"
      "\"Name\"" "\"Number\"" "NFlags")
     ("(" "aX" "aY" "Thickness" "Drill" "\"Name\"" "NFlags")
     ("(" "aX" "aY" "Thickness" "\"Name\"" "NFlags"))
    ("PolyArea" ("[" "Area"))
    ("Polygon" ("(" "SFlags"))
    ("Rat"
     ("[" "X1" "Y1" "Group1" "X2" "Y2" "Group2" "SFlags")
     ("(" "X1" "Y1" "Group1" "X2" "Y2" "Group2" "NFlags"))
    ("Styles" ("(" "\"String\""))
    ("Symbol" (nil "Char" "Delta"))
    ("SymbolLine" (nil "X1" "Y1" "X2" "Y1" "Thickness"))
    ("Text"
     ("[" "X" "Y" "Direction" "Scale" "\"String\"" "SFlags")
     ("(" "X" "Y" "Direction" "Scale" "\"String\"" "NFlags")
     ("(" "X" "Y" "Direction" "\"String\"" "NFlags"))
    ("Thermal" ("[" "Scale"))
    ("Via"
     ("[" "X" "Y" "Thickness" "Clearance" "Mask" "Drill" "\"Name\"" "SFlags")
     ("(" "X" "Y" "Thickness" "Clearance" "Mask" "Drill" "\"Name\"" "NFlags")
     ("(" "X" "Y" "Thickness" "Clearance" "Drill" "\"Name\"" "NFlags")
     ("(" "X" "Y" "Thickness" "Drill" "\"Name\"" "NFlags")
     ("(" "X" "Y" "Thickness" "\"Name\"" "NFlags")))
  "The various PCB keywords. Each entry is a list. Its first
entry is the keyword and then the following elements are
themselves lists. Each of those lists is a list of arguments,
except the first entry, which is nil, \"[\" or \"(\". Either type
of bracket means these arguments can only be chosen with that
surrounding bracket. An entry of NIL means that either type
works.")

(defconst pcb-mode-keyword-regexp
  (regexp-opt (mapcar #'car pcb-mode-keywords))
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

;;; Indentation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom pcb-mode-offset 2
  "The amount of offset to indent by when there is a ( ending the
previous line in a PCB file. Analogous to `c-basic-offset'."
  :type 'integer
  :group 'pcb)

(defun pcb-mode-parens-on-line ()
  "Return a count of the parentheses on the line, with ( counted
as +1 and ) counted as -1, ignoring any that font-lock has
decided are inside strings"
  (let ((line (thing-at-point 'line))
        (pos (line-beginning-position))
        (n 0)
        (count 0))
    (while (< n (length line))
      (unless (get-text-property pos 'face)
        (cond
         ((= ?\( (elt line n)) (setq count (1+ count)))
         ((= ?\) (elt line n)) (setq count (1- count)))))
      (setq n (1+ n) pos (1+ pos)))
    count))

(defun pcb-mode-calculate-indent ()
  "Return appropriate indentation for current line in a PCB file:
always returns a non-negative integer. This is either the
indentation of the previous line or increased/decreased by
`pcb-mode-offset' (the analogue of `c-basic-offset')."
  (let ((diff 0)
        (last-line-indent 0))
    (unless (= 1 (line-number-at-pos))
      (save-excursion
        (beginning-of-line 0)
        (setq diff (+ diff (max (pcb-mode-parens-on-line) 0)))
        (setq last-line-indent (current-indentation))))
    (save-excursion
      (setq diff (+ diff (min (pcb-mode-parens-on-line) 0))))
    (+ last-line-indent (* diff pcb-mode-offset))))

(defun pcb-mode-indent-line ()
  "Indent current line as PCB data."
  (interactive)
  (let ((indent (pcb-mode-calculate-indent))
        (minus-pos (- (point-max) (point)))
        (bol (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (let ((shift-amount (- indent (current-column))))
      (unless (zerop shift-amount)
        (delete-region bol (point))
        (indent-to indent))
      ;; Move to the start of the real line if we started inside the
      ;; indentation.
      (if (> (- (point-max) minus-pos) (point))
          (goto-char (- (point-max) minus-pos))))))

;;; Abbrevs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pcb-mode-abbrev-table nil
  "Abbrev table in use in PCB buffers.")
(define-abbrev-table 'pcb-mode-abbrev-table ())

;;; Templates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tempo)

(defvar pcb-mode-tempo-taglist nil
  "A tag list for tempo in PCB mode.")

(defun pcb-mode-register-tempo-abbrev (name)
  "Register a mode-specific abbrev for PCB for the tempo template
with the given NAME. NAME should be a symbol, without the PCB-
prefix added to tempo template names."
  (let ((template (intern (concat "tempo-template-pcb-" (symbol-name name)))))
    (define-abbrev pcb-mode-abbrev-table
      (symbol-name name) "" template :system t)))

(defmacro pcb-mode-define-template (name documentation expansion)
  "Define a tempo template that expands the symbol name of NAME with
EXPANSION. See `tempo-define-template' for details of EXPANSION. Also registers
a mode-specific abbreviation, with the same name."
  (declare (indent 2))
  (unless (symbolp name) (error "NAME must be a symbol."))
  (let* ((name-string (symbol-name name))
        (template-name (concat "pcb-" name-string)))
    `(progn
       (tempo-define-template ,template-name ',expansion ,name-string
                              ,documentation 'pcb-mode-tempo-taglist)
       (pcb-mode-register-tempo-abbrev ',name)
       nil)))

(defmacro pcb-mode-standard-template (name pcb-string body-p &rest prompts)
  "Define a tempo template expanding NAME as with
`pcb-mode-define-template'. However, this just takes a list of
prompts and follows the standard format. PCB-STRING is the string
that PCB uses to name the object.

PROMPTS is a list. Each member is either a string, in which case
it is used as a prompt (with an appended \": \"), or it is a
list, in which case the contents are included verbatim. If BODY-P
is true, we also insert parentheses to hold a body."
  (declare (indent 3))
  (let ((doc (format "Insert a %s element." (symbol-name name))))
    `(pcb-mode-define-template ,name ,doc
       (,pcb-string " ["
        ,@(let ((collected))
            (dolist (prompt prompts (nreverse (cdr collected)))
              (setq collected
                    (append (if (listp prompt)
                                (cons ", " (reverse prompt))
                              `(", " (p ,(concat prompt ": "))))
                            collected))))
        "]"
        ,@(if body-p
              '(" (" > n ")" >)
            nil)))))

;; Element [SFlags "Desc" "Name" "Value" MX MY TX TY TDir TScale TSFlags]
(pcb-mode-standard-template elt "Element" t
  ("\"\"") "Description string" "Refdes" "Value"
  ;; Don't place the text or mark yet: we're never going to know until we've
  ;; placed contents anyway.
  ("0, 0, 0, 0, 0, 100, \"\""))

;; ElementArc [X Y Width Height StartAngle DeltaAngle Thickness]
(pcb-mode-standard-template eltarc "ElementArc" nil
  "Arc centre x coord" "Arc centre y coord"
  "Arc width" "Arc height"
  "Starting angle (degrees)" "Swept angle (degrees)"
  "Line width")

;; ElementLine [X1 Y1 X2 Y2 Thickness]
(pcb-mode-standard-template eltline "ElementLine" nil
  "Start X coordinate" "Start Y coordinate"
  "End X coordinate" "End Y coordinate"
  "Line width")

;; Pad [rX1 rY1 rX2 rY2 Thickness Clearance Mask "Name" "Number" SFlags]
(pcb-mode-standard-template pad "Pad" nil
  "Start X coordinate" "Start Y coordinate"
  "End X coordinate" "End Y coordinate"
  "Pad Thickness" "Additional clearance width"
  "Solder mask opening width" "Name of pad" "Number of pad"
  ("\"\""))

;; Pin [rX rY Thickness Clearance Mask Drill "Name" "Number" SFlags]
(pcb-mode-standard-template pin "Pin" nil
  "X coordinate" "Y coordinate"
  "Copper outer diameter"
  "Additional clearance width"
  "Solder mask opening diameter"
  "Drill diameter" "Name of pin" "Number of pin" ("\"\""))

;;; Inferior PCB process
(defvar pcb-mode-pcb-program "pcb"
  "The name/path of the PCB program.")

(defun pcb-mode-make-inferior-pcb (filename)
  "Run a new PCB process using `pcb-mode-pcb-program', visiting
FILENAME."
  (process-put
   (start-process
    "inferior-pcb" " *inferior-pcb*" pcb-mode-pcb-program filename)
   'pcb-mode-filename (expand-file-name filename)))

(defun pcb-mode-inferior-targets ()
  "Return a list of filenames that are currently being visited by
inferior PCB processes."
  (let ((acc))
    (dolist (proc (process-list) acc)
      (let ((filename (process-get proc 'pcb-mode-filename)))
        (when filename (setq acc (cons filename acc)))))))

(defun pcb-mode-inferior-pcb (&optional filename)
  "If there isn't already one we're running, start a PCB process
visiting FILENAME. If nil, then visit the file of the current
buffer."
  (interactive)
  (let ((filename
         (or filename (buffer-file-name)
             (error "No file associated to current buffer."))))
    (unless (member (expand-file-name filename) (pcb-mode-inferior-targets))
      (pcb-mode-make-inferior-pcb filename))))

;;; Keymap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar pcb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") 'pcb-mode-inferior-pcb)
    map)
  "Keymap for `pcb-mode'.")

;;; Imenu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar pcb-mode-imenu-generic-expression
  `((nil
     ,(concat "Element[[:space:]]*\\[[[:space:]]*"
              "\".*?\"[[:space:]]+\".*?\"[[:space:]]+\"\\(.*?\\)\"")
     1)))

;;; Finally set everything up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pcb-mode ()
  "Major mode for editing files for the gEDA PCB program.

The mode features syntax highlighting and automatic
indentation. To control the basic offset of the indentation,
customize the variable `pcb-mode-offset'.

There is also support for \"templated insertion\" using the tempo
library. To use this, make sure abbrev mode is
enabled (\\[abbrev-mode]) and then insert one of the following
keywords into your file, followed by a space or new line:

  elt, eltarc, eltline, pad, pin

These correspond to Element, ElementArc, ElementLine, Pad and Pin
in the PCB file, respectively. To be prompted for the parameters
in turn, ensure that `tempo-interactive' is on. (The default is
off)"

  ;; Kill local variables, as instructed in "Major Mode Conventions"
  (kill-all-local-variables)

  ;; Set major mode
  (setq major-mode 'pcb-mode)
  (setq mode-name "PCB")

  ;; Key map
  (use-local-map pcb-mode-map)

  ;; Set up commenting functionality.
  (set (make-local-variable 'comment-start) "#")

  ;; Font lock keywords
  (set (make-local-variable 'font-lock-defaults) '(pcb-mode-keywords))
  (font-lock-set-defaults)

  ;; Syntax recognition
  (set-syntax-table pcb-mode-syntax-table)
  (set (make-local-variable 'syntax-propertize-function)
       pcb-mode-syntax-propertize-function)

  ;; Indentation
  (set (make-local-variable 'indent-line-function)
       'pcb-mode-indent-line)

  ;; Abbrevs
  (set (make-local-variable 'local-abbrev-table)
       pcb-mode-abbrev-table)

  ;; Tempo
  (setq tempo-local-tags
        (cons '(pcb-mode-tempo-taglist) tempo-local-tags))

  ;; Imenu
  (setq imenu-generic-expression pcb-mode-imenu-generic-expression))


;; Make "require" work
(provide 'pcb-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; pcb-mode.el ends here
