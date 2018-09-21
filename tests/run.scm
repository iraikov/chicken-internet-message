;; input stream based on character list

(import scheme (chicken base) (chicken format)
        srfi-1 srfi-13 srfi-14 test internet-message)


(define (string->input-stream s) `(() ,(string->list s)))

(define (err s)
  (print "internet message error on stream: " s)
  (list))

  (let* ((parse-fields (lambda (cont s)  ((fields) (compose cont car) err s)))

	 (parse-message (lambda (cont s) ((message) (compose cont car) err s)))

	 (comment-cases
	  `(
	    ("(a comment)"  (#\) #\t #\n #\e #\m #\m #\o #\c #\space #\a #\() ())
	    ))

	 (fields-cases
	  `(
	    ;; subject
	    ("Subject: Test\r\n"   ((Subject " Test")))
	    ("Subject:Test\r\n"    ((Subject "Test")))
	    ("SUBJECT: Test\r\n"   ((Subject " Test")))
	    ("SUBJECT: This is a\r\n test\r\n" ((Subject " This is a test")))
	    
	    ;; keywords
	    ("Keywords: ErbB receptors\r\n"       ((Keywords (" ErbB " "receptors"))))
	    ("Keywords:  ErbB receptors, EGF\r\n"  ((Keywords ("  ErbB " "receptors") (" EGF"))))
	    ("Keywords: ErbB receptors, EGF, Signal transduction, Dimer, Subcellular\r\n localization, Fluorescent protein\r\n"
	     ((Keywords (" ErbB " "receptors") (" EGF") (" Signal " "transduction") (" Dimer") 
			(" Subcellular " "localization") (" Fluorescent " "protein"))))
	    
	    ;; date
	    ("Date: Fri, 29 Aug 2008 12:21:46 +0200\r\n" ((Date (day-of-week "Fri") (date "29" "Aug" "2008") 
								(time "12" "21" "46" "+" "02" "00"))))
	    ("Date: Fri,  2 Aug 2008 12:21:46 +0200\r\n" ((Date (day-of-week "Fri") (date "2" "Aug" "2008") 
								(time "12" "21" "46" "+" "02" "00"))))
	    
	    ;; different types of mailboxes
	    ("From: John Doe <jdoe@machine.example>\r\n" 
	     ((From (mailbox-list (mailbox (display-name (" John "  "Doe ")) (local-part "jdoe") (domain "machine.example"))))))
	    ("To: Mary Smith <mary@example.net>\r\n"     
	     ((To (mailbox (display-name (" Mary " "Smith ")) (local-part "mary") (domain "example.net")))))
	    
	    ("From: \"Joe Q. Public\" <john.q.public@example.com>\r\n"   
	     ((From (mailbox-list (mailbox (display-name (" Joe Q. Public ")) (local-part "john.q.public") (domain "example.com"))))))
	    ("To: Mary Smith <mary@x.test>, jdoe@example.org\r\n"        
	     ((To (mailbox (display-name (" Mary " "Smith ")) (local-part "mary") (domain "x.test"))
		  (mailbox (local-part " jdoe") (domain "example.org")))))
	    ("To:  Mary Smith <mary@x.test>, jdoe@example.org, Who? <one@y.test>\r\n"         
	     ((To (mailbox (display-name ("  Mary " "Smith ")) (local-part "mary") (domain "x.test"))
		  (mailbox (local-part " jdoe") (domain "example.org"))
		  (mailbox (display-name (" Who? ")) (local-part "one") (domain "y.test")))))
	    ("Cc: <boss@nil.test>, \"Giant; \\\"Big\\\" Box\" <sysservices@example.net>\r\n"  
	     ((Cc (mailbox (local-part "boss") (domain "nil.test")) 
		  (mailbox (display-name (" Giant; \"Big\" Box ")) (local-part "sysservices") (domain "example.net")))))
	    ("To: A Group:Ed Jones <c@a.test>,joe@where.test,John <jdoe@one.test>;\r\n"  
	     ((To (group (display-name (" A " "Group")) 
			 (mailbox-list (mailbox (display-name ("Ed " "Jones ")) (local-part "c") (domain "a.test")) 
				       (mailbox (local-part "joe") (domain "where.test")) 
				       (mailbox (display-name ("John ")) (local-part "jdoe") (domain "one.test")))))))
	    
	    ;; trace fields
	    ("Message-ID: <1234@local.machine.example>\r\n"            ((Message-id (message-id "1234" "local.machine.example"))))
	    ("Message-ID: <5678.21-Nov-1997@example.com>\r\n"          ((Message-id (message-id "5678.21-Nov-1997" "example.com"))))
	    ("Message-ID: <testabcd.1234@silly.example>\r\n"           ((Message-id (message-id "testabcd.1234" "silly.example"))))
	    ("References: <1234@local.machine.example> <3456@example.net>\r\n" 
	     ((References (message-id "1234" "local.machine.example") (message-id "3456" "example.net"))))
	    ("Received: from node.example by x.y.test; 21 Nov 1997 10:01:22 -0600\r\n"  
	     ((Received (received-token " from ") (received-token "node.example ") (received-token "by ") 
			(received-token "x.y.test") (date "21" "Nov" "1997") (time "10" "01" "22" "-" "06" "00"))))
	    
	    ("Received: from x.y.test\r\n    by example.net\r\n      via TCP\r\n      with ESMTP\r\n      id ABC12345\r\n       for <mary@example.net>;  21 Nov 1997 10:05:43 -0600\r\n"
	     ((Received (received-token " from ") (received-token "x.y.test    ") 
			(received-token "by ") (received-token "example.net      ") 
			(received-token "via ") (received-token "TCP      ") 
			(received-token "with ") (received-token "ESMTP      ") 
			(received-token "id ") (received-token "ABC12345       ") 
			(received-token "for ") (local-part "mary") (domain "example.net")
			(date "21" "Nov" "1997") (time "10" "05" "43" "-" "06" "00"))))
	    ;; optional fields
	    ("Content-Type: text/plain; charset=ISO-8859-1\r\n"  
	     ((Content-type " text/plain; charset=ISO-8859-1")))
	    ))

	 (message-cases
	  `(
	    
	    ("From: John Doe <jdoe@machine.example>\r\nTo: Mary Smith <mary@example.net>\r\nSubject: Saying Hello\r\nDate: Fri, 21 Nov 1997 09:55:06 -0600\r\nMessage-ID: <1234@local.machine.example>\r\n\r\nThis is a message just to say hello.\r\nSo, \r\n\r\n\"Hello\"."
	     ((message 
	       (fields (From (mailbox-list (mailbox (display-name (" John " "Doe ")) (local-part "jdoe") (domain "machine.example"))))
		       (To (mailbox (display-name (" Mary " "Smith ")) (local-part "mary") (domain "example.net")))
		       (Subject " Saying Hello") 
		       (Date (day-of-week "Fri") (date "21" "Nov" "1997") (time "09" "55" "06" "-" "06" "00"))
		       (Message-id  (message-id "1234" "local.machine.example")))
	       (body "This is a message just to say hello." "So, " "\"Hello\"."))))
	    
	    (,(string-concatenate
	       (list
		"Return-Path: <chicken-users-bounces+ivan.g.raikov=gmail.com@nongnu.org>\r\n"
		"Received: from lists.gnu.org (lists.gnu.org [199.232.76.165])\r\n   by mx.google.com with ESMTP id c14si3375477ana.41.2009.03.25.08.31.52;\r\n   Wed, 25 Mar 2009 08:31:53 -0700 (PDT)\r\n"
		"Received: by fxm17 with SMTP id 17so92196fxm.34\r\n  for <chicken-users@nongnu.org>; Wed, 25 Mar 2009 08:31:25 -0700 (PDT)\r\n"
		"DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed; d=gmail.com; s=gamma;\r\n	h=domainkey-signature:mime-version:received:date:message-id:subject\r\n	:from:to:content-type:content-transfer-encoding;\r\n	bh=3/gbdTTH19Zaqq1OYytiEg9lxqROJljdO4gXiBtCWl0=;\r\n	b=nFDjeva/nLcPlkXRsGTdHHkM15GSujBCy85V3vFXElSHEX2FRcGnkNc01N6xbJVpA8\r\n	s0hDM1gQwPvcuesuxJZ7UP79tbnBQqHAcOyMMQG2vcrhPjiGM2Zfx48cnfk7inydM5uL\r\n	aqKQLpqdQmoAfUoc7uqkCNwaD1wqRY86GbmFk=\r\n"
		"MIME-Version: 1.0\r\n"
		"Date: Wed, 25 Mar 2009 16:31:24 +0100\r\n"
		"X-Gnus-Mail-Source: imap:imap.gmail.com:INBOX\r\n"
		"Message-ID: <bd22bd7a0903250831k28553845he5099d4414f9b7d5@mail.gmail.com>\r\n"
		"From: felix winkelmann <bunny351@gmail.com>\r\n"
		"To: chicken chicken <chicken-users@nongnu.org>\r\n"
		"Content-Type: text/plain; charset=ISO-8859-1\r\n"
		"Content-Transfer-Encoding: 7bit\r\n"
		"Subject: [Chicken-users] testing release candidate for 4.0.0\r\n"
		"Sender: chicken-users-bounces+ivan.g.raikov=gmail.com@nongnu.org\r\n"
		"Lines: 22\r\n"
		"\r\n"
		"Hi!\r\n"
		"\r\n"
		"\r\n"
		"The current release candidate can be found at:\r\n"
		"\r\n"
		"http://www.call-with-current-continuation.org/chicken-4.0.0.tar.gz\r\n"
		"\r\n"
		"I tested it on several systems (mingw(+msys), linux), but would\r\n"
		"appreciate if others could give it a try. Note that some minor\r\n"
		"recent trunk changes didn't make it, due to unclear portability.\r\n"
		"\r\n"
		"cheers,\r\n"
		"felix"))
	     ((message 
	       (fields
		(Return-path (local-part "chicken-users-bounces+ivan.g.raikov=gmail.com") (domain "nongnu.org")) 
		(Received (received-token " from ") (received-token "lists.gnu.org    ") (received-token "by ") 
			  (received-token "mx.google.com ") (received-token "with ") (received-token "ESMTP ") 
			  (received-token "id ") (received-token "c14si3375477ana.41.2009.03.25.08.31.52") 
			  (day-of-week "Wed") (date "25" "Mar" "2009") (time "08" "31" "53" "-" "07" "00")) 
		(Received (received-token " by ") (received-token "fxm17 ") (received-token "with ") 
			  (received-token "SMTP ") (received-token "id ") (received-token "17so92196fxm.34  ") 
			  (received-token "for ") (local-part "chicken-users") (domain "nongnu.org")
			  (day-of-week "Wed") (date "25" "Mar" "2009") (time "08" "31" "25" "-" "07" "00"))
		(Dkim-signature " v=1; a=rsa-sha256; c=relaxed/relaxed; d=gmail.com; s=gamma;\th=domainkey-signature:mime-version:received:date:message-id:subject\t:from:to:content-type:content-transfer-encoding;\tbh=3/gbdTTH19Zaqq1OYytiEg9lxqROJljdO4gXiBtCWl0=;\tb=nFDjeva/nLcPlkXRsGTdHHkM15GSujBCy85V3vFXElSHEX2FRcGnkNc01N6xbJVpA8\ts0hDM1gQwPvcuesuxJZ7UP79tbnBQqHAcOyMMQG2vcrhPjiGM2Zfx48cnfk7inydM5uL\taqKQLpqdQmoAfUoc7uqkCNwaD1wqRY86GbmFk=") 
		(Mime-version " 1.0") 
		(Date (day-of-week "Wed") (date "25" "Mar" "2009") (time "16" "31" "24" "+" "01" "00")) 
		(X-gnus-mail-source " imap:imap.gmail.com:INBOX") 
		(Message-id (message-id "bd22bd7a0903250831k28553845he5099d4414f9b7d5" "mail.gmail.com")) 
		(From (mailbox-list (mailbox (display-name (" felix " "winkelmann ")) (local-part "bunny351") (domain "gmail.com")))) 
		(To (mailbox (display-name (" chicken " "chicken ")) (local-part "chicken-users") (domain "nongnu.org"))) 
		(Content-type " text/plain; charset=ISO-8859-1") 
		(Content-transfer-encoding " 7bit") 
		(Subject " [Chicken-users] testing release candidate for 4.0.0") 
		(Sender (mailbox (local-part " chicken-users-bounces+ivan.g.raikov=gmail.com") (domain "nongnu.org"))) 
		(Lines " 22")) 
	       (body "Hi!" "The current release candidate can be found at:" "http://www.call-with-current-continuation.org/chicken-4.0.0.tar.gz" "I tested it on several systems (mingw(+msys), linux), but would" "appreciate if others could give it a try. Note that some minor" "recent trunk changes didn't make it, due to unclear portability." "cheers," "felix"))))
	    ))
	 
	 )
    (test-group "comments"
		(for-each (lambda (p)
			    (let ((inp (first p))
				  (res (second p)))
			      (let ((is (string->input-stream inp)))
				(comment (lambda (s) (test (apply sprintf "~S -> ~S" p) res (car s))) err is))))
			  comment-cases)
		)

    (test-group "fields"
		(for-each (lambda (p)
			    (let ((inp (first p))
				  (res (second p)))
			      (let ((is (string->input-stream inp)))
				(parse-fields (lambda (s) (test (apply sprintf "~S -> ~S" p) res s)) is))))
			  fields-cases)
		)

    
    (test-group "message"
		(for-each (lambda (p)
			    (let ((inp (first p))
				  (res (second p)))
			      (parse-message (lambda (s) (test (apply sprintf "~S -> ~S" p) res  s)) (string->input-stream inp))
			      ))
			  message-cases))
    )


(test-exit)
