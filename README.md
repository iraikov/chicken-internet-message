# chicken-internet-message

Parser combinators for Internet Message Format (RFC 5322).


## Description

{{internet-message}} is a collection of parser combinators for the
grammar defined in [[http://www.ietf.org/rfc/rfc5322.txt|RFC 5322]]
(Internet Message Format).

## Usage

The combinator procedures in this library are based on the interface
provided by the [[abnf]] library.

Each procedure contained exported by `internet-message`
is a parser combinator of the form {{(lambda (cont s) ...)}}, which takes a
continuation and input stream. For instance, the {{message}} parser
combinator can be used to parse messages:

```scheme
;; A procedure which creates an input stream from a string
(define (string->input-stream s) `(() ,(string->list s)))

;; A procedure used to report parse errors 
(define (err s)
  (print "internet message error on stream: " s)
  (list))

(let* (;; Parser combinator procedure which takes continuation and input stream
         (parse-message (lambda (cont s) ((message) (compose cont car) err s)))
         (my-message "From: John Doe <jdoe@machine.example>\r\nTo: Mary Smith <mary@example.net>\r\nSubject: Saying Hello\r\nDate: Fri, 21 Nov 1997 09:55:06 -0600\r\nMessage-ID: <1234@local.machine.example>\r\n\r\nThis is a message just to say hello.\r\nSo, \r\n\r\n\"Hello\".")
          )        

  (parse-message (lambda (s) (test (apply sprintf "~S -> ~S" p) res s))
                 (string->input-stream inp))
  )
->
(message 
  (fields (From (mailbox-list (mailbox (display-name (" John " "Doe ")) (local-part "jdoe") (domain "machine.example"))))
          (To (mailbox (display-name (" Mary " "Smith ")) (local-part "mary") (domain "example.net")))
          (Subject " Saying Hello") 
	  (Date (day-of-week "Fri") (date "21" "Nov" "1997") (time "09" "55" "06" "-" "06" "00"))
	  (Message-id  (message-id "1234" "local.machine.example")))
  (body "This is a message just to say hello." "So, " "\"Hello\"."))
```

               
## Library Procedures

The following procedures are exported:

<procedure>fields</procedure>

This parser will parse an arbitrary number of header fields as defined
in the RFC. For each field, an appropriate alist is created. The
following fields are recognized:

* {{from}}
* {{sender}}
* {{return-path}}
* {{reply-to}}
* {{to}}
* {{cc}}
* {{bcc}}
* {{message-id}}
* {{in-reply-to}}
* {{references}}
* {{subject}}
* {{comments}}
* {{keywords}}
* {{orig-date}}
* {{resent-date}}
* {{resent-from}}
* {{resent-sender}}
* {{resent-to}}
* {{resent-cc}}
* {{resent-bcc}}
* {{resent-msg-id}}
* {{resent-reply-to}}
* {{received}}
* {{optional-field}}

<procedure>body</procedure>

This parser will parse a message body as specified by the RFC; that
is, any number of text characters, which may be divided into separate
lines by {{CRLF}}.

<procedure>message</procedure>

This parser will parse a complete message as defined by the RFC and it
will break it down into the separate header fields and the message
body.

<procedure>comment</procedure>

This parser parses comment text, as defined by the RFC. Comments may
nest.


## Requires

* [[abnf]]

## Version History

* 7.0 Ported to CHICKEN 5
* 5.3 Bug fix in received-token
* 5.2 Updated test script to return proper exit code
* 5.1 Compatibility with improved CharLex->CoreABNF constructor
* 5.0 Compatibility with abnf 5
* 4.1-4.2 Typeclass interface fixes
* 4.0 Implemented typeclass interface
* 3.1 Additional parsing combinators exported
* 3.0 Changes to the interface of the fields parser
* 2.0 Extensions to the header function and many bugfixes
* 1.3 Update to reflect changes in lexgen
* 1.1 Fix in date parsing
* 1.0 Initial release

## License

Based on the Haskell Rfc2822 module by Peter Simons.

>
>  Copyright 2009-2017 Ivan Raikov.
>
>
> This program is free software: you can redistribute it and/or
>  modify it under the terms of the GNU General Public License as
>  published by the Free Software Foundation, either version 3 of the
>  License, or (at your option) any later version.
>
>  This program is distributed in the hope that it will be useful, but
>  WITHOUT ANY WARRANTY; without even the implied warranty of
>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
>  General Public License for more details.
>
>  A full copy of the GPL license can be found at
>  <http://www.gnu.org/licenses/>.
>