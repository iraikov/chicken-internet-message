;;
;;  Round-trip unit tests for internet-message-lens, the bidirectional
;;  (lens-like) counterpart to internet-message.scm's parser combinators.
;;

(import internet-message-lens abnf-lens datatype test srfi-1)

(define (test-error s) (list 'parse-error s))

;; Parses s with bp and returns the resulting value (the first, and only,
;; item the grammar produces for a self-contained input).
(define (parse1 bp s) (car (car (bp-parse bp s test-error))))


;;;; Comments and folding whitespace are canonicalized away, not
;;;; preserved: dropped on parse, never reintroduced on print. Tested
;;;; indirectly through bi-atom/bi-phrase, since the cfws-dropping bp
;;;; itself is a private implementation detail.

(test-group "cfws canonicalization"

  (test "leading and trailing whitespace around an atom is dropped"
        "jdoe"
        (parse1 bi-atom "  jdoe  "))

  (test "a comment around an atom is dropped"
        "jdoe"
        (parse1 bi-atom "(a comment) jdoe (another)"))

  (test "nested comments are dropped"
        "jdoe"
        (parse1 bi-atom "(outer (inner) comment) jdoe"))

  (test "printing never reintroduces dropped cfws"
        "jdoe"
        (bp-print bi-atom (parse1 bi-atom "  jdoe  ")))
  )


;;;; atom, dot-atom, quoted-string, word

(test-group "atom, dot-atom, quoted-string, word"

  (test "bi-atom: parse then print reproduces the text"
        "jdoe"
        (bp-print bi-atom (parse1 bi-atom "jdoe")))

  (test "bi-dot-atom: parse then print reproduces the text"
        "jane.doe"
        (bp-print bi-dot-atom (parse1 bi-dot-atom "jane.doe")))

  (test "bi-dot-atom-text-string rejects an empty dotted segment"
        #f
        ((bp-printer bi-dot-atom-text-string) (list "jane..doe")))

  (test "bi-dot-atom-text-string rejects a leading dot"
        #f
        ((bp-printer bi-dot-atom-text-string) (list ".jane")))

  (test "bi-dot-atom-text-string rejects a trailing dot"
        #f
        ((bp-printer bi-dot-atom-text-string) (list "jane.")))

  (test "bi-quoted-string: print then parse reproduces the value, including escaped quotes and backslashes"
        "Giant; \"Big\" Box"
        (parse1 bi-quoted-string (bp-print bi-quoted-string "Giant; \"Big\" Box")))

  (test "bi-quoted-string: parse then print reproduces the text"
        "\"Giant; \\\"Big\\\" Box\""
        (bp-print bi-quoted-string (parse1 bi-quoted-string "\"Giant; \\\"Big\\\" Box\"")))

  (test "bi-word prints an atext-only string unquoted"
        "jdoe"
        (bp-print bi-word "jdoe"))

  (test "bi-word prints a string containing a space quoted"
        "\"Joe Q. Public\""
        (bp-print bi-word "Joe Q. Public"))

  (test "bi-word parses a quoted string back to its plain (unquoted) content"
        "Joe Q. Public"
        (parse1 bi-word "\"Joe Q. Public\""))
  )


;;;; phrase

(test-group "phrase"

  (test "bi-phrase: print then parse reproduces the value"
        '("John" "Doe")
        (parse1 bi-phrase (bp-print bi-phrase (list "John" "Doe"))))

  (test "bi-phrase parses simple whitespace-separated words"
        '("John" "Doe")
        (parse1 bi-phrase "John Doe"))

  (test "bi-phrase drops comments and folding whitespace between words"
        '("John" "Doe")
        (parse1 bi-phrase "  John   (nickname)  Doe  "))

  (test "bi-phrase: parse then print reproduces canonical text"
        "John Doe"
        (bp-print bi-phrase (parse1 bi-phrase "  John   (nickname)  Doe  ")))
  )


;;;; addr-spec, mailbox, group

(test-group "addr-spec, mailbox, group"

  (test "bi-addr-spec: parse then print reproduces the text"
        "jdoe@machine.example"
        (bp-print bi-addr-spec (parse1 bi-addr-spec "jdoe@machine.example")))

  (test "bi-addr-spec rejects a value of the wrong type"
        #f
        ((bp-printer bi-addr-spec) (list 42)))

  (test "bi-mailbox prints a bare addr-spec mailbox"
        "jdoe@machine.example"
        (bp-print bi-mailbox (BareMailbox (make-addr-spec "jdoe" "machine.example"))))

  (test "bi-mailbox prints a named mailbox with a space before the angle address"
        "John Doe <jdoe@machine.example>"
        (bp-print bi-mailbox (NamedMailbox (list "John" "Doe") (make-addr-spec "jdoe" "machine.example"))))

  (test "bi-mailbox parses a From:-style named mailbox"
        (NamedMailbox (list "John" "Doe") (make-addr-spec "jdoe" "machine.example"))
        (parse1 bi-mailbox "John Doe <jdoe@machine.example>"))

  (test "bi-mailbox parses a bare addr-spec mailbox"
        (BareMailbox (make-addr-spec "jdoe" "example.org"))
        (parse1 bi-mailbox "jdoe@example.org"))

  (test "bi-mailbox: a quoted display name with escaped quotes round-trips"
        "\"Giant; \\\"Big\\\" Box\" <sysservices@example.net>"
        (bp-print bi-mailbox (parse1 bi-mailbox "\"Giant; \\\"Big\\\" Box\" <sysservices@example.net>")))

  (test "bi-mailbox rejects a value of the wrong type"
        #f
        ((bp-printer bi-mailbox) (list 42)))

  (test "bi-mailbox-list: parse then print reproduces a canonical mixed named/bare list"
        "Mary Smith <mary@x.test>, jdoe@example.org"
        (bp-print bi-mailbox-list (parse1 bi-mailbox-list "Mary Smith <mary@x.test>, jdoe@example.org")))

  (test "bi-group: parse then print reproduces a canonical non-empty group"
        "A Group:Ed Jones <c@a.test>, joe@where.test, John <jdoe@one.test>;"
        (bp-print bi-group (parse1 bi-group "A Group:Ed Jones <c@a.test>,joe@where.test,John <jdoe@one.test>;")))

  (test "bi-group prints an empty group with no addresses"
        "Undisclosed recipients:;"
        (bp-print bi-group (make-group (list "Undisclosed" "recipients") '())))

  (test "bi-group parses an empty group back to zero mailboxes"
        '()
        (group-mailboxes (parse1 bi-group "Undisclosed recipients:;")))

  (test "bi-address-list: parse then print reproduces a group plus a mailbox"
        "A Group:Ed Jones <c@a.test>;, joe@where.test"
        (bp-print bi-address-list (list (GroupAddress (make-group (list "A" "Group")
                                                                   (list (NamedMailbox (list "Ed" "Jones")
                                                                                       (make-addr-spec "c" "a.test")))))
                                         (MailboxAddress (BareMailbox (make-addr-spec "joe" "where.test"))))))
  )


;;;; message identifiers

(test-group "msg-id"

  (test "bi-msg-id: parse then print reproduces the text"
        "<1234@local.machine.example>"
        (bp-print bi-msg-id (parse1 bi-msg-id "<1234@local.machine.example>")))

  (test "bi-msg-id accepts a dotted id-left"
        "<5678.21-Nov-1997@example.com>"
        (bp-print bi-msg-id (parse1 bi-msg-id "<5678.21-Nov-1997@example.com>")))

  (test "bi-msg-id-list1: parse then print reproduces a canonical space-separated list"
        "<1234@local.machine.example> <3456@example.net>"
        (bp-print bi-msg-id-list1 (parse1 bi-msg-id-list1 "<1234@local.machine.example> <3456@example.net>")))

  (test "bi-msg-id rejects a value of the wrong type"
        #f
        ((bp-printer bi-msg-id) (list 42)))

  (test "an unterminated angle address fails to parse rather than hanging"
        #t
        (equal? 'parse-error (car (bp-parse bi-msg-id "<1234@local.machine.example" test-error))))
  )


;;;; date and time

(test-group "date-time"

  (test "bi-date-time-spec: parse then print reproduces the single-space form"
        "Fri, 29 Aug 2008 12:21:46 +0200"
        (bp-print bi-date-time-spec (parse1 bi-date-time-spec "Fri, 29 Aug 2008 12:21:46 +0200")))

  (test "bi-date-time-spec absorbs extra folding whitespace on parse"
        "2"
        (date-spec-day (date-time-spec-date (parse1 bi-date-time-spec "Fri,  2 Aug 2008 12:21:46 +0200"))))

  (test "bi-date-time-spec: a missing day-of-week still prints with exactly one leading space"
        "Date: 29 Aug 2008 12:21:46 +0200\r\n"
        (bp-print bi-header-field (parse1 bi-header-field "Date: 29 Aug 2008 12:21:46 +0200\r\n")))

  (test "bi-time-spec rejects a value of the wrong type"
        #f
        ((bp-printer bi-time-spec) (list 42)))
  )


;;;; Subject / Comments

(test-group "subject, comments"

  (test "Subject: parse then print reproduces canonical text"
        "Subject: Test\r\n"
        (bp-print bi-header-field (parse1 bi-header-field "Subject: Test\r\n")))

  (test "Subject: header keyword is matched case-insensitively and printed canonically"
        "Subject: Test\r\n"
        (bp-print bi-header-field (parse1 bi-header-field "SUBJECT: Test\r\n")))

  (test "Subject: a folded (multi-line) value parses to one canonical-spaced line"
        "Subject: This is a test\r\n"
        (bp-print bi-header-field (parse1 bi-header-field "SUBJECT: This is a\r\n test\r\n")))

  (test "Comments: parse then print reproduces canonical text"
        "Comments: a remark\r\n"
        (bp-print bi-header-field (parse1 bi-header-field "Comments: a remark\r\n")))
  )


;;;; Keywords

(test-group "keywords"

  (test "Keywords: parse then print reproduces a canonical single-phrase list"
        "Keywords: ErbB receptors\r\n"
        (bp-print bi-header-field (parse1 bi-header-field "Keywords: ErbB receptors\r\n")))

  (test "Keywords: parses each comma-separated phrase into its own word list"
        (KeywordsField (list (list "ErbB" "receptors") (list "EGF")))
        (parse1 bi-header-field "Keywords:  ErbB receptors, EGF\r\n"))

  (test "Keywords: parse then print reproduces canonical spacing for a multi-phrase list"
        "Keywords: ErbB receptors, EGF\r\n"
        (bp-print bi-header-field (parse1 bi-header-field "Keywords:  ErbB receptors, EGF\r\n")))
  )


;;;; Optional (unrecognized) fields

(test-group "optional-field"

  (test "OptionalField: parse then print reproduces the text"
        "Content-Type: text/plain; charset=ISO-8859-1\r\n"
        (bp-print bi-header-field (parse1 bi-header-field "Content-Type: text/plain; charset=ISO-8859-1\r\n")))

  (test "OptionalField keeps the header name's exact original casing, not a titlecased symbol"
        (OptionalField "Content-Type" "text/plain; charset=ISO-8859-1")
        (parse1 bi-header-field "Content-Type: text/plain; charset=ISO-8859-1\r\n"))
  )


;;;; A full message

(test-group "message"

  (let* ((text (string-append
                "From: John Doe <jdoe@machine.example>\r\n"
                "To: Mary Smith <mary@example.net>\r\n"
                "Subject: Saying Hello\r\n"
                "Date: Fri, 21 Nov 1997 09:55:06 -0600\r\n"
                "Message-ID: <1234@local.machine.example>\r\n"
                "\r\n"
                "This is a message just to say hello.\r\n"
                "So, \r\n"
                "\r\n"
                "\"Hello\"."))
         ;; The source text's body has a blank line (a double CRLF) between
         ;; "So, " and "\"Hello\".": internet-message.scm's own body grammar
         ;; collapses a run of consecutive CRLFs into a single separator
         ;; rather than an empty line, and this module's canonical printer
         ;; reproduces that same single-CRLF form, not the original doubled
         ;; one -- an instance of the general "same meaning, not necessarily
         ;; same bytes" canonicalization documented at the top of this file.
         (canonical-text (string-append
                          "From: John Doe <jdoe@machine.example>\r\n"
                          "To: Mary Smith <mary@example.net>\r\n"
                          "Subject: Saying Hello\r\n"
                          "Date: Fri, 21 Nov 1997 09:55:06 -0600\r\n"
                          "Message-ID: <1234@local.machine.example>\r\n"
                          "\r\n"
                          "This is a message just to say hello.\r\n"
                          "So, \r\n"
                          "\"Hello\"."))
         (expected
          (make-message
           (list (FromField (list (NamedMailbox (list "John" "Doe") (make-addr-spec "jdoe" "machine.example"))))
                 (ToField (list (MailboxAddress (NamedMailbox (list "Mary" "Smith") (make-addr-spec "mary" "example.net")))))
                 (SubjectField "Saying Hello")
                 (DateField (make-date-time-spec "Fri" (make-date-spec "21" "Nov" "1997")
                                                  (make-time-spec "09" "55" "06" #\- "06" "00")))
                 (MessageIdField (make-msg-id "1234" "local.machine.example")))
           (list "This is a message just to say hello." "So, " "\"Hello\"."))))

    (test "bi-message parses a complete message into the expected structure"
          expected
          (parse1 bi-message text))

    (test "bi-message: parse then print reproduces the text up to body-blank-line canonicalization"
          canonical-text
          (bp-print bi-message (parse1 bi-message text)))

    (test "bi-message: printing then reparsing is idempotent"
          (bp-print bi-message (parse1 bi-message text))
          (bp-print bi-message (parse1 bi-message (bp-print bi-message (parse1 bi-message text)))))
    )

  (test "bi-message rejects a value of the wrong type"
        #f
        ((bp-printer bi-message) (list 42)))
  )

(test-exit)
