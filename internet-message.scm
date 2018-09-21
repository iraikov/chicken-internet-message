;;
;;  Parser for the grammar defined in RFC 5322, "Internet Message Format".
;;  
;;
;;  Based on the Haskell Rfc2822 module by Peter Simons.
;;
;;  Copyright 2009-2018 Ivan Raikov.
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

(module internet-message

	(header body message parts fields comment
         ftext )

	(import scheme (chicken base) (chicken string) srfi-1
                (prefix abnf abnf:) 
		(prefix abnf-consumers abnf:) 
		(only utf8-srfi-13 string-downcase)
		(only utf8-srfi-14 char-set char-set-difference char-set-union
		      char-set:graphic char-set:printing char-set:ascii char-set:full)
		)


(define (char-list-titlecase x)
  (if (null? x) x (cons (char-upcase (car x)) (map char-downcase (cdr x)))))

;; construct symbols from consumed chars
(define consumed-chars->tsymbol
  (abnf:consumed-chars->list 
   (compose string->symbol 
	    list->string 
	    char-list-titlecase 
	    abnf:trim-ws-char-list)))

;; shortcut for (abnf:bind consumed-chars->tsymbol ... )
(define-syntax bind-consumed->tsymbol
  (syntax-rules () 
    ((_ p)    (abnf:bind consumed-chars->tsymbol p))
    ))

(define consumed-objects-lift-any
  (abnf:consumed-objects-lift
   (abnf:consumed-objects identity)))

;; Construct a parser for a message header line from the header's name
;; and a parser for the body.

(define (header s p)  
    (let ((ss (->string s)))
      (lambda (#!key (crlf abnf:crlf) (prefix #f))
	(if prefix
	    `(,ss . ,(abnf:concatenation
		      (abnf:drop-consumed (abnf:char #\:))
		      p
		      (abnf:drop-consumed crlf)))
	    (abnf:bind (consumed-objects-lift-any)
		       (abnf:concatenation
			   (bind-consumed->tsymbol (abnf:lit ss))
			   (abnf:drop-consumed (abnf:char #\:))
			   p
			   (abnf:drop-consumed crlf)))
	    ))
    ))


;; Primitive parsers (section 3.2.1)


;; Matches any US-ASCII character except for nul \r \n

(define text
  (abnf:set (char-set-difference
             char-set:ascii 
             (char-set (integer->char 0) 
                       (integer->char 10) 
                       (integer->char 13) ))))

;; Unicode variant of text
(define unicode-text
  (abnf:set (char-set-union
             (char-set-difference
              char-set:ascii 
              (char-set (integer->char 0) 
                        (integer->char 10) 
                        (integer->char 13) ))
             (char-set-difference
              char-set:full 
              char-set:ascii))))



;; Folding white space and comments (section 3.2.3)

(define fws
  (abnf:concatenation
   (abnf:optional-sequence 
    (abnf:concatenation
     (abnf:repetition abnf:wsp)
     (abnf:drop-consumed 
      (abnf:alternatives abnf:crlf abnf:lf abnf:cr))))
   (abnf:repetition1 abnf:wsp)))

(define (between-fws-drop p fws)
  (abnf:concatenation
   (abnf:drop-consumed (abnf:optional-sequence fws)) p 
   (abnf:drop-consumed (abnf:optional-sequence fws))))
			     
;; helper macro for mutually-recursive parser definitions

(define-syntax vac
  (syntax-rules ()
    ((_ fn) (lambda args (apply fn args)))))


;; Quoted characters
(define quoted-pair
  (abnf:concatenation
   (abnf:drop-consumed (abnf:char #\\))
   (abnf:alternatives abnf:vchar abnf:wsp)))

;; Matches any non-whitespace, non-control character except for ( ) and \

(define ctext
  (abnf:set (char-set-difference char-set:graphic (char-set #\( #\) #\\))))

;; Unicode variant of ctext
(define unicode-ctext
  (abnf:set
   (char-set-union
    (char-set-difference char-set:graphic (char-set #\( #\) #\\))
    (char-set-difference char-set:full  char-set:ascii))))


;; Matches comments. That is any combination of ctext, quoted pairs,
;; and fws between brackets. Comments may nest.


(define ccontent 
  (vac (abnf:alternatives ctext quoted-pair comment)))

(define comment
  (abnf:concatenation 
   (abnf:char #\( )
   (abnf:repetition
    (abnf:concatenation
     (abnf:optional-sequence fws)
     ccontent
     ))
   (abnf:optional-sequence fws)
   (abnf:char #\))
   ))

;; Matches any combination of fws and comments

(define cfws
  (abnf:alternatives
   (abnf:concatenation
    (abnf:repetition1
     (abnf:concatenation
      (abnf:optional-sequence fws)
      (abnf:drop-consumed comment)))
    (abnf:optional-sequence fws))
   fws))
		 

;;  A combinator for sequences (optional cfws) p (optional cfws)

(define (between-cfws p cfws)
  (abnf:concatenation
   (abnf:optional-sequence cfws) p 
   (abnf:optional-sequence cfws) ))
			     
(define (between-cfws-drop p cfws)
  (abnf:concatenation
   (abnf:drop-consumed (abnf:optional-sequence cfws)) p 
   (abnf:drop-consumed (abnf:optional-sequence cfws) )))
			     

;; Atom (section 3.2.4)

;; Matches any US-ASCII character except for control characters,
;; specials, or space. atom and dot-atom are made up of this.

(define atext
  (abnf:alternatives
   abnf:alpha abnf:decimal (abnf:set-from-string "!#$%&'*+-/=?^_`{|}~")))

;; Unicode variant of atext
(define unicode-atext
  (abnf:alternatives
   abnf:alpha abnf:decimal (abnf:set-from-string "!#$%&'*+-/=?^_`{|}~")
   (abnf:set (char-set-difference char-set:full char-set:ascii))))

;; Matches one or more atext characters and skip any preceeding or
;; trailing cfws

(define atom
  (abnf:bind-consumed->string
   (between-cfws (abnf:repetition1 atext) cfws)))


;; Matches two or more atext elements interspersed by dots.

(define dot-atom-text
  (abnf:concatenation
   (abnf:repetition1 atext)
   (abnf:repetition 
    (abnf:concatenation
     (abnf:char #\.)
     (abnf:repetition1 atext) 
     ))))

;; Matches dot-atom-text and skips any preceeding or trailing cfws.

(define dot-atom
  (abnf:bind-consumed->string
   (between-cfws dot-atom-text cfws)))

;; Quoted strings (section 3.2.4)

;;; Matches any non-whitespace, non-control US-ASCII character except
;;; for \ and "

(define char-set:quoted (char-set-difference char-set:printing (char-set #\\ #\")))

(define qtext (abnf:set char-set:quoted))

;; Unicode variant of qtext 

(define unicode-qtext
  (abnf:set (char-set-union
             char-set:quoted
             (char-set-difference char-set:full char-set:ascii))))

;; Matches either qtext or quoted-pair

(define qcontent
  (abnf:repetition1 
   (abnf:alternatives
    qtext quoted-pair)))

;; Matches any number of qcontent between double quotes.

(define quoted-string
  (abnf:bind-consumed->string
   (between-cfws
    (abnf:concatenation
     (abnf:drop-consumed abnf:dquote)
     (abnf:repetition
      (abnf:concatenation
       (abnf:optional-sequence fws)
       qcontent))
     (abnf:optional-sequence fws)
     (abnf:drop-consumed abnf:dquote))
    cfws)
   ))

;; Miscellaneous tokens (section 3.2.5)

;;; Matches either atom or quoted-string

(define word
  (abnf:alternatives atom quoted-string))
  
;; Matches either one or more word elements 

(define phrase
  (abnf:bind-consumed-strings->list 
   (abnf:repetition1 word)))


;; Matches any number of utext tokens.
;;
;; Unstructured text is used in free text fields such as subject.

(define unstructured
  (abnf:bind-consumed->string
   (abnf:concatenation
    (abnf:repetition 
     (abnf:concatenation
      (abnf:optional-sequence fws)
      abnf:vchar))
    (abnf:repetition abnf:wsp))))


(define unicode-unstructured
  (abnf:bind-consumed->string
   (abnf:concatenation
    (abnf:repetition 
     (abnf:concatenation
      (abnf:optional-sequence fws)
      abnf:unicode-vchar))
    (abnf:repetition abnf:wsp))))


;; Date and Time Specification (section 3.3)

;; Parses a date and time specification of the form
;;
;;   Thu, 19 Dec 2002 20:35:46 +0200
;;
;; where the weekday specification (Thu) is optional. The parser
;; This parser will not perform any consistency checking.
;; It will accept
;;
;;    40 Apr 2002 13:12 +0100
;;
;;  as a perfectly valid date.

			     
;; Matches the abbreviated weekday names

(define day-name
  (abnf:alternatives
   (abnf:lit "Mon")
   (abnf:lit "Tue")
   (abnf:lit "Wed")
   (abnf:lit "Thu")
   (abnf:lit "Fri")
   (abnf:lit "Sat")
   (abnf:lit "Sun")))

;; Matches a day-name, optionally wrapped in folding whitespace

(define day-of-week
  (abnf:bind-consumed-strings->list 
   'day-of-week 
   (between-fws-drop 
    (abnf:bind-consumed->string day-name)
    fws)))


;; Matches a four digit decimal number

(define year
  (between-fws-drop
   (abnf:bind-consumed->string (abnf:repetition-n 4 abnf:decimal))
   fws))

;; Matches the abbreviated month names


(define month-name
  (abnf:alternatives
   (abnf:lit "Jan")
   (abnf:lit "Feb")
   (abnf:lit "Mar")
   (abnf:lit "Apr")
   (abnf:lit "May")
   (abnf:lit "Jun")
   (abnf:lit "Jul")
   (abnf:lit "Aug")
   (abnf:lit "Sep")
   (abnf:lit "Oct")
   (abnf:lit "Nov")
   (abnf:lit "Dec")))

;; Matches a month-name, optionally wrapped in folding whitespace

(define month
  (between-fws-drop
   (abnf:bind-consumed->string month-name)
   fws))


;; Matches a one or two digit number

(define day
  (abnf:concatenation
   (abnf:drop-consumed (abnf:optional-sequence fws))
   (abnf:alternatives 
    (abnf:bind-consumed->string (abnf:variable-repetition 1 2 abnf:decimal))
    (abnf:drop-consumed fws))))

;; Matches a date of the form dd:mm:yyyy
(define date
  (abnf:bind-consumed-strings->list 'date 
     (abnf:concatenation day month year)))

;; Matches a two-digit number 

(define hour (abnf:bind-consumed->string (abnf:repetition-n 2 abnf:decimal)))
(define minute (abnf:bind-consumed->string (abnf:repetition-n 2 abnf:decimal)))
(define isecond (abnf:bind-consumed->string (abnf:repetition-n 2 abnf:decimal)))


;; Matches a time-of-day specification of hh:mm or hh:mm:ss.

(define time-of-day
  (abnf:concatenation
   hour (abnf:drop-consumed (abnf:char #\:))
   minute (abnf:optional-sequence 
           (abnf:concatenation (abnf:drop-consumed (abnf:char #\:))
                               isecond))))

;; Matches a timezone specification of the form
;; +hhmm or -hhmm 

(define zone
  (abnf:concatenation 
   (abnf:drop-consumed fws)
   (abnf:bind-consumed->string (abnf:alternatives (abnf:char #\-) (abnf:char #\+)))
   hour minute))

;; Matches a time-of-day specification followed by a zone.

(define itime
  (abnf:bind-consumed-strings->list 'time 
    (abnf:concatenation time-of-day zone)))

(define date-time
  (abnf:concatenation
   (abnf:optional-sequence
    (abnf:concatenation
     day-of-week
     (abnf:drop-consumed (abnf:char #\,))))
   date
   itime
   (abnf:drop-consumed (abnf:optional-sequence cfws))))


;; Address Specification (section 3.4)


;; Parses and returns a "local part" of an addr-spec. That is either
;; a dot-atom or a quoted-string.

(define local-part
  (abnf:alternatives dot-atom quoted-string))


;; Parses and returns any ASCII characters except [ ] and \

(define dtext
  (abnf:set 
   (char-set-difference char-set:printing (char-set #\[ #\] #\\))))



;; Unicode variant of dtext
(define unicode-dtext
  (abnf:set 
   (char-set-union
    (char-set-difference char-set:printing (char-set #\[ #\] #\\))
    (char-set-difference char-set:full char-set:ascii))))


;; Parses a domain literal. That is a [ character, followed by any
;; amount of dcontent, followed by a terminating ] character.

(define domain-literal
  (between-cfws
   (abnf:concatenation
    (abnf:drop-consumed (abnf:char #\[))
    (abnf:bind-consumed->string 
     (abnf:repetition 
      (abnf:concatenation
       (abnf:drop-consumed (abnf:optional-sequence fws))
       dtext)))
    (abnf:drop-consumed (abnf:optional-sequence fws))
    (abnf:drop-consumed (abnf:char #\])))
   cfws))

;; Parses and returns a domain part of an addr-spec. That is either
;; a dot-atom or a domain-literal.

(define domain
  (abnf:alternatives dot-atom domain-literal))


;; Addr-spec specification (section 3.4.1)

;; Parses an address specification. That is, a local-part, followed
;; by an \ character, followed by a domain. 

(define addr-spec
  (abnf:concatenation
   (abnf:bind-consumed-strings->list 'local-part local-part)
   (abnf:drop-consumed (abnf:char #\@))
   (abnf:bind-consumed-strings->list 'domain domain)))

;; Parses an angle-addr

(define angle-addr
  (between-cfws-drop
   (abnf:concatenation
    (abnf:drop-consumed (abnf:char #\<))
    addr-spec
    (abnf:drop-consumed (abnf:char #\>))
    )
   cfws))


;; Parses and returns a phrase.
(define display-name
  (abnf:bind-consumed-pairs->list 'display-name phrase))

;; Matches an angle-addr, optionally prefaced with a display-name

(define name-addr
  (abnf:concatenation
   (abnf:optional-sequence display-name)
   angle-addr))

;; Matches a name-addr or an addr-spec and returns the address.

(define mailbox
  (abnf:bind-consumed-pairs->list 'mailbox 
     (abnf:alternatives name-addr addr-spec)))

;; Parses a list of mailbox addresses, every two addresses being
;; separated by a comma, and returns the list of found address(es).

(define mailbox-list
  (abnf:bind-consumed-pairs->list 'mailbox-list
     (abnf:concatenation
      mailbox
      (abnf:repetition
       (abnf:concatenation
	(abnf:drop-consumed (abnf:char #\,))
	mailbox)))))


;; Parses a group of addresses. That is, a display-name, followed
;; by a colon, optionally followed by a mailbox-list, followed by a
;; semicolon. The found address(es) are returned - what may be none.
;; Here is an example:
;;
;;    my group: user1@example.org, user2@example.org;

(define group
  (vac
   (abnf:bind-consumed-pairs->list 'group 
      (abnf:concatenation
       display-name
       (abnf:drop-consumed (abnf:char #\:))
       (abnf:optional-sequence group-list)
       (abnf:drop-consumed (abnf:char #\;))
       (abnf:drop-consumed (abnf:optional-sequence cfws))))))
  
(define group-list
  (abnf:alternatives 
   mailbox-list
   (abnf:drop-consumed cfws)))

;; Matches a single mailbox or an address group 

(define address (abnf:alternatives mailbox group))

;; Parses a list of address addresses, every two addresses being
;; separated by a comma, and returns the list of found address(es).

(define address-list
  (abnf:concatenation
   address
   (abnf:repetition
    (abnf:concatenation
     (abnf:drop-consumed (abnf:char #\,))
     address))))

;;  Overall message syntax (section 3.5)
			     
;; This parser will return a message body as specified by the RFC;
;; that is basically any number of text characters, which may be
;; divided into separate lines by crlf.

(define body
  (abnf:repetition
   (abnf:concatenation
    (abnf:repetition 
     (abnf:concatenation
      (abnf:bind-consumed->string
       (abnf:repetition text))
      (abnf:drop-consumed 
       (abnf:repetition abnf:crlf))))
    (abnf:bind-consumed->string
     (abnf:repetition text)))))

;; Field definitions (section 3.6)

;; The origination date field (section 3.6.1)

;; Parses a Date: header and returns the date as a list
;; (year month dom hour min sec tz dow)

(define orig-date  (header "Date" date-time))

;; Originator fields (section 3.6.2)

;; Parses a From: header and returns the mailbox-list address(es)
;; contained in it.

(define from      (header "From" mailbox-list))

;; Parses a Sender: header and returns the mailbox address contained in
;; it.

(define sender    (header "Sender" mailbox))

;; Parses a Reply-To: header and returns the address-list address(es)
;; contained in it.

(define reply-to  (header "Reply-To" address-list))

;; Destination address fields (section 3.6.3)

;; Parses a To: header and returns the address-list address(es)
;; contained in it.

(define to        (header "To" address-list))

;; Parses a Cc: header and returns the address-list address(es)
;; contained in it.

(define cc        (header "Cc" address-list))

;; Parses a Bcc: header and returns the address-list address(es)
;; contained in it.

(define bcc
  (header "Bcc" (abnf:optional-sequence 
		 (abnf:alternatives
		  address-list
		  (abnf:drop-consumed cfws)))))

;; Identification fields (section 3.6.4)

;; Parses one or more occurences of dtext or quoted-pair and returns the
;; concatenated string. This makes up the id-right of a msg-id.

(define no-fold-literal
  (abnf:concatenation
   (abnf:drop-consumed (abnf:char #\[))
   (abnf:repetition dtext)
   (abnf:drop-consumed (abnf:char #\]))))

;; Parses a left ID part of a msg-id. This is almost identical to
;; the local-part of an e-mail address, but with stricter rules
;; about folding and whitespace.

(define id-left  dot-atom-text)

;; Parses a right ID part of a msg-id. This is almost identical to the
;; domain of an e-mail address, but with stricter rules about folding
;; and whitespace.

(define id-right
  (abnf:alternatives dot-atom-text no-fold-literal))

;; Parses a message ID and returns it. A message ID is almost identical
;; to an angle-addr, but with stricter rules about folding and
;; whitespace.

(define msg-id
  (abnf:bind-consumed-strings->list 'message-id
      (between-cfws-drop
       (abnf:concatenation
	(abnf:drop-consumed (abnf:char #\<))
	(abnf:bind-consumed->string id-left)
	(abnf:drop-consumed (abnf:char #\@))
	(abnf:bind-consumed->string id-right)
	(abnf:drop-consumed (abnf:char #\>))
	)
       cfws)))


;; Parses a In-Reply-To header and returns the list of msg-id's
;; contained in it.

(define in-reply-to (header "In-Reply-To" (abnf:repetition1 msg-id)))

;; Parses a References: header and returns the list of msg-id's
;; contained in it.

(define references  (header "References"  (abnf:repetition1 msg-id)))

;; Parses a Message-Id: header and returns the msg-id contained
;; in it.

(define message-id (header "Message-ID" msg-id))
					   
;; Informational fields (section 3.6.5)

;; Parses a Subject: header and returns its contents verbatim.

(define subject (header "Subject"  unstructured))

;; Parses a Comments: header and returns its contents verbatim.

(define comments (header "Comments" unstructured))

;; Parses a Keywords: header and returns the list of phrases
;; found. Please note that each phrase is again a list of atoms, as
;; returned by the phrase parser.

(define kwd-list
  (abnf:concatenation
   phrase
   (abnf:repetition
    (abnf:concatenation
     (abnf:drop-consumed (abnf:char #\,))
     phrase))))

(define keywords  (header "Keywords" kwd-list))
			   

;; Resent fields (section 3.6.6)

;; Parses a Resent-Date: header and returns the date it contains as
;; CalendarTime

(define resent-date (header "Resent-Date" date-time))

;; Parses a Resent-From: header and returns the mailbox-list address(es)
;; contained in it.

(define resent-from  (header "Resent-From" mailbox-list))

;; Parses a Resent-Sender: header and returns the mailbox-list
;; address(es) contained in it.

(define resent-sender (header "Resent-Sender" mailbox))

;; Parses a Resent-To header and returns the mailbox address contained
;; in it.

(define resent-to  (header "Resent-To" address-list))

;; Parses a Resent-Cc header and returns the address-list address(es)
;; contained in it.

(define resent-cc (header "Resent-Cc" address-list))

;; Parses a Resent-Bcc: header and returns the address-list
;; address(es) contained in it. (This list may be empty.)

(define resent-bcc
  (header "Resent-Bcc"
	  (abnf:alternatives 
	   address-list
	   (abnf:drop-consumed 
	    (abnf:optional-sequence cfws)))))


;; Parses a Resent-Message-ID: header and returns the msg-id contained
;; in it.

(define resent-msg-id  
  (header "Resent-Message-ID" msg-id))


;; Parses a Resent-Reply-To: header and returns the address-list
;; contained in it.

(define resent-reply-to
  (header "Resent-Reply-To" address-list))


;; Trace fields (section 3.6.7)

			 
(define path
  (abnf:alternatives 
   angle-addr
   (between-cfws-drop
    (abnf:concatenation
     (abnf:drop-consumed (abnf:char #\<))
     (abnf:drop-consumed (abnf:optional-sequence cfws))
     (abnf:drop-consumed (abnf:char #\>)))
    cfws)))

(define return-path  (header "Return-Path" path))

(define received-token
  (abnf:bind-consumed-strings->list
   (lambda (x) (and (pair? x) `(received-token . ,x)))
   (abnf:alternatives angle-addr addr-spec domain word)))

(define received-token-list
  (abnf:concatenation
   (abnf:repetition received-token)
   (abnf:drop-consumed (abnf:char #\;))
   date-time))
  

(define received 
  (header "Received"  received-token-list))


;; Optional fields (section 3.6.8)

;; Matches and returns any ASCII character except for control
;; characters, whitespace, and :

(define ftext
  (abnf:set (char-set-difference char-set:graphic
                                 (char-set #\:))))


;; Parses and returns an arbitrary header field name. That is one or
;; more ftext characters.

(define field-name
  (bind-consumed->tsymbol (abnf:repetition1 ftext)))

;; Parses an arbitrary header field and returns a tuple containing the
;; field-name and unstructured text of the header. The name will not
;; contain the terminating colon.

(define optional-field
    (lambda (#!key (crlf abnf:crlf) )
      (abnf:bind (consumed-objects-lift-any)
		 (abnf:concatenation
		  (abnf:concatenation
		   field-name
		   (abnf:drop-consumed (abnf:char #\:)))
		  unstructured
		  (abnf:drop-consumed crlf)))))
  
;; This parser will parse an arbitrary number of header fields as
;; defined in this RFC. For each field, an appropriate 'Field' value
;; is created, all of them making up the 'Field' list that this parser
;; returns.

;; Fields that contain syntax errors fall back to the catch-all
;; optional-field. Thus, this parser will hardly ever return a syntax
;; error -- what conforms with the idea that any message that can
;; possibly be accepted /should/ be.

(define (fields #!key (crlf abnf:crlf))
  (abnf:repetition
   (abnf:alternatives
      (from           crlf: crlf)
      (sender         crlf: crlf)
      (return-path    crlf: crlf)
      (reply-to       crlf: crlf)
      (to             crlf: crlf)
      (cc             crlf: crlf)
      (bcc            crlf: crlf)
      (message-id     crlf: crlf)
      (in-reply-to    crlf: crlf)
      (references     crlf: crlf)
      (subject        crlf: crlf)
      (comments       crlf: crlf)
      (keywords       crlf: crlf)
      (orig-date      crlf: crlf)
      (resent-date    crlf: crlf)
      (resent-from    crlf: crlf)
      (resent-sender  crlf: crlf)
      (resent-to      crlf: crlf)
      (resent-cc      crlf: crlf)
      (resent-bcc     crlf: crlf)
      (resent-msg-id    crlf: crlf)
      (resent-reply-to  crlf: crlf)
      (received         crlf: crlf)
      (optional-field   crlf: crlf))
    )
  )


;; Parses a complete message as defined by the RFC and returns
;; the separate header fields and the message body. 

(define (message #!key (crlf abnf:crlf))
    (abnf:bind-consumed-pairs->list 
     'message
     (abnf:concatenation 
      (abnf:bind-consumed-pairs->list 'fields 
	 (fields crlf: crlf))
      (abnf:optional-sequence
       (abnf:concatenation 
        (abnf:drop-consumed crlf)
        (abnf:bind-consumed-strings->list 'body body)))
      ))
    )

;; Given an alist of headers and a body, parses all header values and
;; the body, and returns a list of the form
;;
;; (PARSED-HEADERS PARSED-BODY)
;;
(define parts
  (lambda (#!key (crlf abnf:crlf))
    (let* (
           (header-parsers
            (map
             (lambda (p) (p alist: #t crlf: crlf))
             (list from
                   sender
                   return-path
                   reply-to
                   to
                   cc
                   bcc
                   message-id
                   in-reply-to
                   references
                   subject
                   comments
                   keywords
                   orig-date
                   resent-date
                   resent-from
                   resent-sender
                   resent-to
                   resent-cc
                   resent-bcc
                   resent-msg-id
                   resent-reply-to
                   received
                   optional-field)))
           (try-header
            (lambda (kv)
              (let loop ((fs header-parsers))
                (if (null? fs) kv
                    (let ((kv1 (apply (car fs) kv)))
                      (or kv1 (loop (cdr fs)))))))))
      (lambda (unparsed-headers unparsed-body)
        (let ((parsed-headers (map try-header unparsed-headers))
              (parsed-body (body (unparsed-body))))
          (list parsed-headers parsed-body))))))


  
)
