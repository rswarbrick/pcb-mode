;;; pcb-mode.el --- major mode providing a pcb mode hook for Emacs

;; Copyright (C) 2012  Rupert Swarbrick
;; Author: Rupert Swarbrick <rswarbrick@gmail.com>

;;; Commentary:

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

;;; Code:

(defgroup pcb-mode nil
  "Customizations for PCB mode."
  :prefix "pcb-" :group 'text)

(defconst pcb-mode-version 1 "Current version of PCB mode")

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
  (regexp-opt (mapcar #'car pcb-mode-keywords) 'words)
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
                                (cons " " (reverse prompt))
                              `(" " (p ,(concat prompt ": "))))
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

;;; Eldoc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pcb-mode-keyword-at-point ()
  "If point is at a keyword, return its entry in
`pcb-mode-keywords' (see that variable for the
format). Otherwise, return nil. "
  (let ((key (car (member (word-at-point)
                          (mapcar #'car pcb-mode-keywords)))))
    (when key (assoc key pcb-mode-keywords))))

(defun pcb-mode-find-plain-match (re start end &optional backwards)
  "Hunt for a match for the regular expression RE between START
and END in the current buffer. \"Plain\" refers to the fact that
we only consider matches where the first character has a null
'face property. This allows us to ignore things like the contents
of string constants.

Either START or END can be null, in which case they are taken to
be `point-min' and `point-max' respectively. If BACKWARDS is
true, search backwards from the end."
  (let ((start-pos
         (if backwards (or end (point-max)) (or start (point-min))))
        (searcher (if backwards 're-search-backward 're-search-forward))
        (bound (if backwards start end)))
    (save-excursion
      (goto-char start-pos)
      (catch 'done
        (while (funcall searcher re bound t)
          (unless (get-text-property (match-beginning 0) 'face)
            (throw 'done (match-beginning 0))))
        nil))))

(defun pcb-mode-pos-in-arglist (start end)
  "Return the index of the current position in the arglist
bounded by START and END, together with its total length.

START and END should be valid positions in the buffer. Arguments
are separated by whitespace (not including that inside strings,
of course)."
  (save-excursion
    (let ((args-found 0) pos (old-point (point)))
      (goto-char start)
      (catch 'loop
        (while (< (point) end)
          (skip-syntax-forward " ")
          (let ((next-space
                 (pcb-mode-find-plain-match "[[:space:]]+" (point) end)))
            (when (and (not pos) (> (point) old-point))
              (setq pos (1- args-found)))
            (unless next-space
              (throw 'loop nil))
            (setq args-found (1+ args-found))
            (goto-char next-space))))
      (list (or pos args-found) (1+ args-found)))))

(defun pcb-mode-find-keyword ()
  "Search near point for a keyword and argument list. If we are
within white space of a keyword, use that. Otherwise, if we
are [!] in the situation

  <PCB-keyword> ( <no-parens> [!] <still-no-brackets> <closing-)?>

or the same with [ and maybe ], then pick up on that.

If there is a match, return (KEYWORD POS NUM-ARGS BRACKET-CHAR
CLOSED-P) where KEYWORD is the keyword found, POS and NUM-ARGS
might be non-nil if we're inside an arglist. NUM-ARGS is the
number of arguments found so far and POS is the index of the
argument where point is found. If an opening bracket was found,
BRACKET-CHAR is its value. If there is a closing bracket then
CLOSED-P is non-nil."
  ;; First look to see whether we're on or immediately after a PCB keyword (on
  ;; the same line)
  (let ((pos (point)) keyword open close bracket-char)
    (save-excursion
      (skip-syntax-forward "w")
      (skip-syntax-backward " ")
      (setq keyword (car (pcb-mode-keyword-at-point))))
    (unless keyword
      (let ((open-bracket (pcb-mode-find-plain-match
                           "\\[\\|(" (line-beginning-position) pos t))
            (close-bracket))
        (when open-bracket
          (save-excursion
            ;; If we jump back a word from the [ or (, do we find a PCB keyword?
            (goto-char open-bracket)
            (backward-word)
            (when (looking-at pcb-mode-keyword-regexp)
              ;; Lookin' good!
              (setq keyword (match-string 0)
                    open (1+ open-bracket))))
          ;; Go back to the opening bracket and then jump forward a sexp. 
          (save-excursion
            (goto-char open-bracket)
            (setq close (ignore-errors (forward-sexp) (1- (point)))))
          ;; If close < pos, then the point was after the whole of the argument
          ;; list, so give up!
          (when (and close (< close pos))
            (setq keyword nil open nil close nil))
          ;; Otherwise, record the value of bracket-char
          (setq bracket-char
                (elt (buffer-substring-no-properties
                      open-bracket (1+ open-bracket)) 0)))))
    (cons keyword
          (when open
            (append (pcb-mode-pos-in-arglist
                     open (or close (line-end-position)))
                    (list bracket-char
                          (and close t)))))))

(defun pcb-mode-eldoc-situation ()
  "Search around point for a PCB keyword, with argument list. If
the point is in a situation like

  Element (arg1 arg2<!> arg3)

\(with or without the closing parenthesis), return a list

  '(\"Element\" ARGS 1).

Here, the 1 refers to the fact that point is at the argument at
position one in the list. ARGS is the best guess for the argument
list to the keyword (including the start bracket). If we aren't
in such a situation, return nil."
  (let ((pmfk (pcb-mode-find-keyword)))
    ;; pmfk is (keyword pos num-args bracket-char closed-p)
    (let ((keyword (first pmfk))
          (pos (second pmfk))
          (num-args (third pmfk))
          (bracket-char (fourth pmfk)))
      (when keyword
        (let ((arglists (cdr (assoc keyword pcb-mode-keywords))))
          (list keyword
                (or (catch 'result
                      (dolist (arglist arglists)
                        (when (and (or (null (first arglist))
                                       (null bracket-char)
                                       (= (elt (first arglist) 0) bracket-char))
                                   (or (null num-args)
                                       (<= num-args (length (rest arglist)))))
                          (throw 'result arglist))))
                    (first arglists))
                pos))))))

(defun pcb-mode-eldoc-documentation-function ()
  "Return a one-line string for displaying information about the
current PCB keyword and its arguments. Is used as
`eldoc-documentation-function' by `pcb-mode'."
  (let ((pmes (pcb-mode-eldoc-situation)))
    (let ((keyword (first pmes))
          (arglist (second pmes))
          (pos (third pmes)))
      (when keyword
        (let ((acc (concat keyword ": "
                           (or (first arglist) "[")))
              (arg-pos 0)
              (pos (or pos -1)))
          (set-text-properties 0 (length keyword)
                               '(face font-lock-function-name-face)
                               acc)
          (dolist (arg (rest arglist))
            (setq acc (concat acc (unless (= arg-pos 0) " ") arg))
            (when (= arg-pos pos)
              (add-text-properties (- (length acc) (length arg)) (length acc)
                                   '(face eldoc-highlight-function-argument)
                                   acc))
            (setq arg-pos (1+ arg-pos)))
          (setq acc
                (concat acc (if (eq (elt (first arglist) 0) ?\() ")" "]")))
          acc)))))

;; Symbol completion
(defun pcb-mode-completion-at-point ()
  "Function used for the `completion-at-point-functions' variable
in `pcb-mode'."
  (with-syntax-table pcb-mode-syntax-table
    (let* ((pos (point))
           (beg
            (save-excursion
              (cond
               ((eq (char-syntax (char-before)) ?\w)
                (backward-word 1) (point))
               ((eq (char-syntax (char-after)) ?\w)
                (point))
               (t nil))))
           (end
            (and beg
                 (save-excursion
                   (cond
                    ((eq (char-syntax (char-after)) ?\w)
                     (forward-word 1) (point))
                    (t (point)))))))
      (when (and beg end)
        (list beg end (mapcar #'car pcb-mode-keywords))))))

;; Mode hook
(defvar pcb-mode-hook nil
  "List of functions to call when PCB mode is invoked.
This hook is automatically executed after `pcb-mode' is fully
loaded.")

;;; Finally set everything up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pcb-mode ()
  "Major mode for editing files for the gEDA PCB program.

The mode features syntax highlighting and automatic
indentation. To control the basic offset of the indentation,
customize the variable `pcb-mode-offset'.

There is support for `imenu', useful in layout files. The indexed
items are elements and they are listed by refdes. To use it,
either use \\[imenu] or use the \"Elements\" menu item.

There is symbol completion at point (use \\[complete-symbol]),
which completes the PCB keywords. For quicker insertion of
keywords with lots of parameters, there is also support for
\"templated insertion\" using the tempo library. To use this,
make sure abbrev mode is enabled (\\[abbrev-mode]) and then
insert one of the following keywords into your file, followed by
a space or new line:

  elt, eltarc, eltline, pad, pin

These correspond to Element, ElementArc, ElementLine, Pad and Pin
in the PCB file, respectively. To be prompted for the parameters
in turn, ensure that `tempo-interactive' is on. (The default is
off)

PCB mode supports the minor mode `eldoc-mode'. This is off by
default; to turn it on use (\\[eldoc-mode]). For
backwards-compatibility reasons, PCB allows several different
sets of arguments to each keyword, which makes guessing argument
lists rather problematic. We try to guess which one is right by
looking at the brackets used and the number of arguments, but
this is probably rather brittle.

As is standard for major modes, there is a `pcb-mode-hook', which
is a list of functions that get called just before `pcb-mode'
finishes setting things up. This is a good place to do things
like enabling `eldoc-mode' or `abbrev-mode'."
  (interactive)

  ;; Kill local variables, as instructed in "Major Mode Conventions"
  (kill-all-local-variables)

  ;; Set major mode
  (setq major-mode 'pcb-mode)
  (setq mode-name "PCB")

  ;; Key map
  (use-local-map pcb-mode-map)

  ;; Set up commenting functionality.
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(?:^\\|[^'\n]\\)[[:space:]]*\\)#")

  ;; Font lock keywords
  (set (make-local-variable 'font-lock-defaults)
       `((,pcb-mode-keyword-regexp 0 font-lock-keyword-face)))
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
  (setq imenu-generic-expression pcb-mode-imenu-generic-expression)
  (imenu-add-to-menubar "Elements")

  ;; Eldoc
  (set (make-local-variable 'eldoc-documentation-function)
       'pcb-mode-eldoc-documentation-function)

  ;; Completion
  (set (make-local-variable 'completion-at-point-functions)
       (cons 'pcb-mode-completion-at-point completion-at-point-functions))

  ;; Only check comments with ispell
  (set (make-local-variable 'ispell-check-comments) 'exclusive)

  ;; Mode hook
  (run-mode-hooks 'pcb-mode-hook))

;; Make "require" work
(provide 'pcb-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; pcb-mode.el ends here
