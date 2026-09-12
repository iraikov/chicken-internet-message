;;
;;  Bidirectional (lens-like) counterpart to internet-message.scm's parser
;;  combinators, built on the abnf-lens library: a representative subset of
;;  RFC 5322, "Internet Message Format", covering addresses, the common
;;  structured headers, and full message assembly, so that values built by
;;  a program can be printed back out as valid message text, not just
;;  parsed from it.
;;
;;  Simplifications relative to the RFC:
;;
;;  - Comments and folding whitespace (cfws/fws/comment) are canonicalized:
;;    dropped on parse, printed as nothing or as a single
;;    required space. 
;;  - Received/trace fields (Received, Return-Path) are out of scope, as
;;    are the Unicode variants of the text primitives, and internet-
;;    message.scm's `parts`.
;;  - A quoted-string or domain-literal's surrounding quotes/brackets are
;;    stripped from the stored value and reconstructed canonically on
;;    print, so that callers work with plain content strings.
;;  - The generic optional-field fallback keeps the header name as a
;;    plain, case-preserving string rather than internet-message.scm's
;;    titlecased symbol, since round-tripping needs the printed name to
;;    reparse to the same value.
;;
;;   Copyright 2009-2026 Ivan Raikov.
;;
;;
;;  This program is free software: you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License as
;;  published by the Free Software Foundation, either version 3 of the
;;  License, or (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.
;;
;;  A full copy of the GPL license can be found at
;;  <http://www.gnu.org/licenses/>.

(module internet-message-lens

	(
	 ;; words, phrases, unstructured text
	 bi-atom bi-dot-atom bi-dot-atom-text-string bi-quoted-string bi-word
	 bi-phrase bi-display-name bi-unstructured

	 ;; date and time
	 date-spec? make-date-spec date-spec-day date-spec-month date-spec-year bi-date-spec
	 time-spec? make-time-spec time-spec-hour time-spec-minute time-spec-second
	 time-spec-zone-sign time-spec-zone-hour time-spec-zone-minute bi-time-spec
	 date-time-spec? make-date-time-spec date-time-spec-day-of-week
	 date-time-spec-date date-time-spec-time bi-date-time-spec

	 ;; addresses
	 addr-spec? make-addr-spec addr-spec-local-part addr-spec-domain bi-addr-spec
	 mailbox mailbox? NamedMailbox BareMailbox bi-mailbox bi-mailbox-list
	 group? make-group group-display-name group-mailboxes bi-group bi-group-list
	 address address? MailboxAddress GroupAddress bi-address
	 bi-address-list bi-address-list-or-empty

	 ;; message identifiers
	 msg-id? make-msg-id msg-id-id-left msg-id-id-right bi-msg-id bi-msg-id-list1

	 ;; body
	 bi-body

	 ;; header fields and messages
	 header-field header-field?
	 FromField SenderField ReplyToField ToField CcField BccField
	 MessageIdField InReplyToField ReferencesField SubjectField CommentsField
	 KeywordsField DateField ResentDateField ResentFromField ResentSenderField
	 ResentToField ResentCcField ResentBccField ResentMessageIdField
	 ResentReplyToField OptionalField
	 bi-header-field bi-fields

	 message? make-message message-fields message-body bi-message

	 ;; re-exported entry points, for convenience
	 bp-parse bp-print
	 )

	(import scheme (chicken base) srfi-1
                abnf-lens abnf
		(only utf8-srfi-14 char-set char-set-difference
		      char-set:graphic char-set:printing char-set:ascii))


;;;; Character classes, shared between the raw (cfws/comment) grammar below
;;;; and the bidirectional rules that follow it.

;; atext: letters, digits, and a fixed set of punctuation marks (section 3.2.3)
(define (alpha-or-decimal? c)
  (or (and (char>=? c #\a) (char<=? c #\z))
      (and (char>=? c #\A) (char<=? c #\Z))
      (and (char>=? c #\0) (char<=? c #\9))))

(define atext-specials (string->list "!#$%&'*+-/=?^_`{|}~"))

(define (atext-char? c)
  (or (alpha-or-decimal? c) (memv c atext-specials)))

(define bi-atext
  (bi-alternatives bi-alpha bi-decimal (bi-set-from-string "!#$%&'*+-/=?^_`{|}~")))

;; ctext: any non-whitespace, non-control character except ( ) and \
(define ctext-char-set
  (char-set-difference char-set:graphic (char-set #\( #\) #\\)))

;; qtext: any non-whitespace, non-control character except \ and "
(define qtext-char-set
  (char-set-difference char-set:printing (char-set #\\ #\")))

(define bi-qtext (bi-set qtext-char-set))

;; dtext: any printing character except [ ] and \
(define dtext-char-set
  (char-set-difference char-set:printing (char-set #\[ #\] #\\)))

(define bi-dtext (bi-set dtext-char-set))

;; ftext: any graphic character except :
(define ftext-char-set
  (char-set-difference char-set:graphic (char-set #\:)))

(define bi-ftext (bi-set ftext-char-set))

;; text: any US-ASCII character except NUL, CR, LF (section 3.2.1)
(define text-char-set
  (char-set-difference char-set:ascii
                        (char-set (integer->char 0) (integer->char 10) (integer->char 13))))

(define bi-text-char (bi-set text-char-set))


;;;; Folding whitespace and comments (section 3.2.3), private and
;;;; non-bidirectional: canonicalized away, so only
;;;; a parser (never a printer) is needed for them. 

(define raw-fws
  (concatenation
   (optional-sequence
    (concatenation
     (repetition wsp)
     (drop-consumed (alternatives crlf lf cr))))
   (repetition1 wsp)))

(define raw-ctext (set ctext-char-set))

(define raw-quoted-pair
  (concatenation (drop-consumed (char #\\)) (alternatives vchar wsp)))

(define-syntax vac
  (syntax-rules ()
    ((_ fn) (lambda args (apply fn args)))))

;; ccontent and comment are mutually recursive (comments may nest); vac
;; defers evaluation of the forward reference to comment until call time.
(define raw-ccontent
  (vac (alternatives raw-ctext raw-quoted-pair raw-comment)))

(define raw-comment
  (concatenation
   (char #\()
   (repetition (concatenation (optional-sequence raw-fws) raw-ccontent))
   (optional-sequence raw-fws)
   (char #\))))

(define raw-cfws
  (alternatives
   (concatenation
    (repetition1 (concatenation (optional-sequence raw-fws) (drop-consumed raw-comment)))
    (optional-sequence raw-fws))
   raw-fws))

;; drop-consumed (abnf's/lexgen's `bind`) fails when the stream is already
;; at end-of-input, even if the wrapped parser could otherwise match zero
;; characters there. bi-drop-cfws needs to work at the trailing edge of a
;; token that may be the last thing in the input (bi-atom's trailing edge
;; when nothing follows it, for instance), so it is built from bind*
;; instead, which succeeds trivially at end-of-input.
(define (drop-consumed* p) (bind* (lambda args #f) p))

;; Drops any amount of folding whitespace and/or (possibly nested)
;; comments.
(define bi-drop-cfws
  (make-bp (drop-consumed* (optional-sequence raw-cfws)) (lambda (vals) (cons '() vals))))

;; RFC folding whitespace is meaningful inside unstructured text
;; (unlike cfws elsewhere, which is purely decorative): it must contribute
;; exactly one canonical space to the flat value list rather than being
;; dropped.
(define bi-folded-space
  (make-bp
   (alternatives raw-fws wsp)
   (lambda (vals)
     (and (pair? vals) (char? (car vals)) (char=? (car vals) #\space)
          (cons (list #\space) (cdr vals))))))

;; Two adjacent tokens (phrase words, a header's fixed keyword and its
;; value, ...) often have no separator character between them, only cfws.
;; This glue bp consumes nothing and always prints exactly one canonical space.
(define bi-space
  (make-bp pass (lambda (vals) (cons (list #\space) vals))))

;; Like bi-space, but for a position where a real mandatory separator is
;; actually present in valid input and must be consumed on parse: drops any
;; real cfws there while still printing exactly one canonical space
;; rather than bi-drop-cfws's nothing. This is needed wherever neither
;; neighboring rule already drops cfws at its own edge.
(define bi-cfws-as-space
  (make-bp (bp-parser bi-drop-cfws) (lambda (vals) (cons (list #\space) vals))))


;;;; Reusable folding patterns

;; Wraps a bp built from bi-repetition/bi-optional-sequence/bi-alt (whose
;; own contribution to an enclosing flat list is a variable number of
;; items) into a single flat slot holding the matched items as a list.
(define (bi-fold-list p)
  (bi-iso (lambda (flat) flat)
          (lambda (v) (and (list? v) v))
          p))

;; One or more, comma-separated; printed with a canonical space after each
;; comma ("a, b", not "a,b"), matching ordinary usage.
(define (bi-list1-comma element)
  (bi-fold-list (bi-seq element (bi-repetition (bi-concatenation (bi-drop-char #\,) bi-space element)))))

;; One or more, no separator character required between elements, but
;; printed with a canonical space between them for readability.
(define (bi-list1-nosep element)
  (bi-fold-list (bi-seq element (bi-repetition (bi-seq bi-space element)))))

;; RFC 5322's recurring "list-of-things / CFWS" shape (section 3.4, 3.6.3):
;; either a real, non-empty list, or nothing at all (with any stray
;; whitespace/comments in between consumed, via bi-drop-cfws, so a
;; following delimiter -- ";" or CRLF -- still matches). 
(define (bi-list-or-empty raw-element-seq)
  (bi-fold-list (bi-alt raw-element-seq bi-drop-cfws)))


;;;; Atoms, dot-atoms, quoted strings, words, phrases (section 3.2.3-3.2.5)

(define bi-atom
  (bi-iso
   (lambda (chars) (and (pair? chars) (list->string chars)))
   (lambda (s) (and (string? s) (positive? (string-length s)) (every atext-char? (string->list s))
                     (string->list s)))
   (bi-concatenation bi-drop-cfws (bi-repetition1 bi-atext) bi-drop-cfws)))

;; dot-atom-text, without surrounding cfws: one or more atext runs joined
;; by dots, folded to one string ("jane.doe") with the dots kept as part
;; of it (via bi-char, not bi-drop-char, so they appear in the folded text).
(define bi-dot-atom-text-string
  (bi-iso
   (lambda (chars) (list->string chars))
   (lambda (s) (and (valid-dot-atom-text? s) (string->list s)))
   (bi-seq (bi-repetition1 bi-atext)
           (bi-repetition (bi-seq (bi-char #\.) (bi-repetition1 bi-atext))))))

(define (valid-dot-atom-text? s)
  (and (string? s) (positive? (string-length s))
       (not (char=? (string-ref s 0) #\.))
       (not (char=? (string-ref s (- (string-length s) 1)) #\.))
       (let loop ((cs (string->list s)) (prev-dot #f))
         (cond ((null? cs) #t)
               ((char=? (car cs) #\.) (and (not prev-dot) (loop (cdr cs) #t)))
               ((atext-char? (car cs)) (loop (cdr cs) #f))
               (else #f)))))

(define bi-dot-atom
  (bi-concatenation bi-drop-cfws bi-dot-atom-text-string bi-drop-cfws))

(define bi-quoted-pair-char
  (bi-concatenation (bi-drop-char #\\) (bi-alternatives bi-vchar bi-wsp)))

;; Exactly one logical content character of a quoted-string, choosing
;; between plain (qtext) and backslash-escaped (quoted-pair) spelling on
;; print. 
(define qcontent-char
  (bi-iso (lambda (flat) (car flat))
          (lambda (c) (list c))
          (bi-alternatives bi-qtext bi-quoted-pair-char)))

;; A quoted-string's surrounding quotes are dropped, not captured, so
;; callers work with plain content strings; print reconstructs them
;; canonically.
(define bi-quoted-string
  (bi-iso
   list->string
   (lambda (s) (and (string? s) (string->list s)))
   (bi-concatenation
    bi-drop-cfws
    (bi-drop-char #\")
    (bi-repetition qcontent-char)
    (bi-drop-char #\")
    bi-drop-cfws)))

;; atom tried before quoted-string: on print, an atext-only string prints
;; unquoted; anything else (embedded space, quote, backslash, ...) falls
;; through and prints quoted.
(define bi-word (bi-alternatives bi-atom bi-quoted-string))

(define bi-phrase
  (bi-iso
   (lambda (flat) flat)
   (lambda (v) (and (list? v) (pair? v) (every string? v) v))
   (bi-seq bi-word (bi-repetition (bi-seq bi-space bi-word)))))

(define bi-display-name bi-phrase)

;; Leading/trailing cfws is dropped, not captured as part of the value,
;; so the mandatory single space after a header's fixed keyword comes
;; from that header's own bi-space clause, the same way it does for
;; every other header type, rather than being built into the text itself.
(define bi-unstructured
  (bi-iso
   (lambda (chars) (list->string chars))
   (lambda (s) (and (string? s) (string->list s)))
   (bi-concatenation
    bi-drop-cfws
    (bi-repetition (bi-alternatives bi-vchar bi-folded-space))
    bi-drop-cfws)))


;;;; Date and time (section 3.3)

(define bi-2digit
  (bi-iso list->string string->list (bi-repetition-n 2 bi-decimal)))

(define bi-day-num
  (bi-iso
   (lambda (chars) (list->string chars))
   (lambda (s) (and (string? s) (memv (string-length s) '(1 2)) (string->list s)))
   (bi-concatenation bi-drop-cfws (bi-variable-repetition 1 2 bi-decimal) bi-drop-cfws)))

(define day-names '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(define month-names '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define bi-day-name
  (bi-iso
   (lambda (chars) (list->string chars))
   (lambda (s) (and (member s day-names) (string->list s)))
   (bi-alternatives (bi-lit "Mon") (bi-lit "Tue") (bi-lit "Wed") (bi-lit "Thu")
                     (bi-lit "Fri") (bi-lit "Sat") (bi-lit "Sun"))))

(define bi-day-of-week
  (bi-concatenation bi-drop-cfws bi-day-name bi-drop-cfws))

;; The mandatory ", " that follows a weekday is built into this token's
;; own print (via the trailing bi-drop-char/bi-space, which contribute no
;; flat value of their own): the stored value is just the bare weekday
;; string ("Fri"), but printing it always produces "Fri, ". 
(define bi-day-of-week-token
  (bi-iso
   (lambda (flat) (car flat))
   (lambda (s) (list s))
   (bi-concatenation bi-day-of-week (bi-drop-char #\,) bi-space)))

(define bi-date-time-day-of-week (bi-maybe bi-day-of-week-token))

(define bi-month-name
  (bi-iso
   (lambda (chars) (list->string chars))
   (lambda (s) (and (member s month-names) (string->list s)))
   (bi-alternatives (bi-lit "Jan") (bi-lit "Feb") (bi-lit "Mar") (bi-lit "Apr")
                     (bi-lit "May") (bi-lit "Jun") (bi-lit "Jul") (bi-lit "Aug")
                     (bi-lit "Sep") (bi-lit "Oct") (bi-lit "Nov") (bi-lit "Dec"))))

(define bi-month
  (bi-concatenation bi-drop-cfws bi-month-name bi-drop-cfws))

(define bi-year-num
  (bi-iso
   (lambda (chars) (list->string chars))
   (lambda (s) (and (string? s) (= (string-length s) 4) (string->list s)))
   (bi-concatenation bi-drop-cfws (bi-repetition-n 4 bi-decimal) bi-drop-cfws)))

(define-bi-rule date-spec
  (day   bi-day-num)
  bi-space
  (month bi-month)
  bi-space
  (year  bi-year-num))

(define bi-time-second
  (bi-maybe (bi-concatenation (bi-drop-char #\:) bi-2digit)))

(define bi-zone-sign (bi-alternatives (bi-char #\+) (bi-char #\-)))

(define-bi-rule time-spec
  (hour bi-2digit)
  (bi-drop-char #\:)
  (minute bi-2digit)
  (second bi-time-second)
  bi-cfws-as-space
  (zone-sign bi-zone-sign)
  (zone-hour bi-2digit)
  (zone-minute bi-2digit))

;; day-of-week's own token already prints its trailing ", " when present
;; (see bi-day-of-week-token above) and nothing at all when absent, so no
;; extra separator is needed between it and date here.
(define-bi-rule date-time-spec
  (day-of-week bi-date-time-day-of-week)
  (date bi-date-spec)
  bi-space
  (time bi-time-spec)
  bi-drop-cfws)


;;;; Address grammar (section 3.4)

;; A domain-literal's surrounding brackets are dropped, the
;; same way a quoted-string's quotes are.
(define bi-domain-literal
  (bi-iso
   list->string
   (lambda (s) (and (string? s) (string->list s)))
   (bi-concatenation
    bi-drop-cfws
    (bi-drop-char #\[)
    (bi-repetition bi-dtext)
    (bi-drop-char #\])
    bi-drop-cfws)))

(define bi-local-part (bi-alternatives bi-dot-atom bi-quoted-string))
(define bi-domain (bi-alternatives bi-dot-atom bi-domain-literal))

(define-bi-rule addr-spec
  (local-part bi-local-part)
  (bi-drop-char #\@)
  (domain bi-domain))

(define bi-angle-addr
  (bi-concatenation
   bi-drop-cfws (bi-drop-char #\<) bi-addr-spec (bi-drop-char #\>) bi-drop-cfws))

(define-bi-datatype mailbox mailbox?
  (NamedMailbox (display-name bi-phrase) bi-space (address bi-angle-addr))
  (BareMailbox  (address bi-addr-spec)))

(define bi-mailbox-list (bi-list1-comma bi-mailbox))

(define raw-mailbox-seq
  (bi-seq bi-mailbox (bi-repetition (bi-concatenation (bi-drop-char #\,) bi-space bi-mailbox))))

(define bi-group-list (bi-list-or-empty raw-mailbox-seq))

(define-bi-rule group
  (display-name bi-phrase)
  (bi-drop-char #\:)
  (mailboxes bi-group-list)
  (bi-drop-char #\;)
  bi-drop-cfws)

(define-bi-datatype address address?
  (MailboxAddress (mailbox bi-mailbox))
  (GroupAddress   (group bi-group)))

(define bi-address-list (bi-list1-comma bi-address))

(define raw-address-seq
  (bi-seq bi-address (bi-repetition (bi-concatenation (bi-drop-char #\,) bi-space bi-address))))

(define bi-address-list-or-empty (bi-list-or-empty raw-address-seq))


;;;; Message identifiers (section 3.6.4)

(define bi-no-fold-literal
  (bi-iso
   list->string
   (lambda (s) (and (string? s) (string->list s)))
   (bi-concatenation (bi-drop-char #\[) (bi-repetition bi-dtext) (bi-drop-char #\]))))

(define bi-id-right (bi-alternatives bi-dot-atom-text-string bi-no-fold-literal))

(define-bi-rule msg-id
  bi-drop-cfws
  (bi-drop-char #\<)
  (id-left bi-dot-atom-text-string)
  (bi-drop-char #\@)
  (id-right bi-id-right)
  (bi-drop-char #\>)
  bi-drop-cfws)

(define bi-msg-id-list1 (bi-list1-nosep bi-msg-id))


;;;; Body (section 3.5)

;; One-or-more consecutive CRLFs, canonically printed as exactly one: used
;; as the separator between body lines. 
(define bi-crlf-run
  (make-bp
   (drop-consumed (repetition1 crlf))
   (lambda (vals) (cons (list (integer->char 13) (integer->char 10)) vals))))

(define bi-body-line
  (bi-iso list->string string->list (bi-repetition bi-text-char)))

(define raw-body-line-seq
  (bi-seq bi-body-line (bi-repetition (bi-seq bi-crlf-run bi-body-line))))

(define bi-body (bi-fold-list (bi-optional-sequence raw-body-line-seq)))


;;;; Header fields (section 3.6, minus Received/Return-Path) and messages

(define bi-field-name
  (bi-iso list->string string->list (bi-repetition1 bi-ftext)))

(define bi-phrase-list1 (bi-list1-comma bi-phrase))

;; A canonical single space follows every fixed header keyword, matching
;; ordinary usage ("From: ..." rather than "From:...").
(define-bi-datatype header-field header-field?
  (FromField            (bi-drop-lit "From:")            bi-space (mailboxes bi-mailbox-list)          bi-drop-crlf)
  (SenderField          (bi-drop-lit "Sender:")           bi-space (mailbox   bi-mailbox)               bi-drop-crlf)
  (ReplyToField         (bi-drop-lit "Reply-To:")         bi-space (addresses bi-address-list)          bi-drop-crlf)
  (ToField              (bi-drop-lit "To:")               bi-space (addresses bi-address-list)          bi-drop-crlf)
  (CcField              (bi-drop-lit "Cc:")               bi-space (addresses bi-address-list)          bi-drop-crlf)
  (BccField             (bi-drop-lit "Bcc:")              bi-space (addresses bi-address-list-or-empty) bi-drop-crlf)
  (MessageIdField       (bi-drop-lit "Message-ID:")       bi-space (id bi-msg-id)                       bi-drop-crlf)
  (InReplyToField       (bi-drop-lit "In-Reply-To:")      bi-space (ids bi-msg-id-list1)                bi-drop-crlf)
  (ReferencesField      (bi-drop-lit "References:")       bi-space (ids bi-msg-id-list1)                bi-drop-crlf)
  (SubjectField         (bi-drop-lit "Subject:")          bi-space (text bi-unstructured)               bi-drop-crlf)
  (CommentsField        (bi-drop-lit "Comments:")         bi-space (text bi-unstructured)               bi-drop-crlf)
  (KeywordsField        (bi-drop-lit "Keywords:")         bi-space (phrases bi-phrase-list1)            bi-drop-crlf)
  (DateField            (bi-drop-lit "Date:")             bi-space (date bi-date-time-spec)             bi-drop-crlf)
  (ResentDateField      (bi-drop-lit "Resent-Date:")      bi-space (date bi-date-time-spec)             bi-drop-crlf)
  (ResentFromField      (bi-drop-lit "Resent-From:")      bi-space (mailboxes bi-mailbox-list)          bi-drop-crlf)
  (ResentSenderField    (bi-drop-lit "Resent-Sender:")    bi-space (mailbox   bi-mailbox)               bi-drop-crlf)
  (ResentToField        (bi-drop-lit "Resent-To:")        bi-space (addresses bi-address-list)          bi-drop-crlf)
  (ResentCcField        (bi-drop-lit "Resent-Cc:")        bi-space (addresses bi-address-list)          bi-drop-crlf)
  (ResentBccField       (bi-drop-lit "Resent-Bcc:")       bi-space (addresses bi-address-list-or-empty) bi-drop-crlf)
  (ResentMessageIdField (bi-drop-lit "Resent-Message-ID:") bi-space (id bi-msg-id)                      bi-drop-crlf)
  (ResentReplyToField   (bi-drop-lit "Resent-Reply-To:")  bi-space (addresses bi-address-list)          bi-drop-crlf)
  (OptionalField        (name bi-field-name) (bi-drop-char #\:) bi-space (text bi-unstructured)         bi-drop-crlf))

(define bi-fields (bi-fold-list (bi-repetition bi-header-field)))

(define bi-message-body
  (bi-maybe (bi-concatenation bi-drop-crlf bi-body)))

(define-bi-rule message
  (fields bi-fields)
  (body bi-message-body))

)
