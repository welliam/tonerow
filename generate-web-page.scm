; NOTE: as it requires filesystem features, this file is written in chicken 
; scheme!

(use posix irregex)

(define head
"<!DOCTYPE html>

<title>MOLTs</title>

<style>
  body {
      margin-left: 10%;
      width: 70%;
  }

  h3 { margin: 5.7%; } /* 5.7 is correct */
  a { color: black }
  a:hover { text-decoration: none }
</style>

<body>

  <h3>Modes of Limited Transposition</h3>

  A list of all modes of limited transposition for well tempered scales.
  Prime numbered tone scales are not included because they have no MOLTs;
  rather, they are the basis for MOLTs in composite numbered tone scales.
  Through interpolation of these prime tone scales all modes of limited
  transposition in all tone scales are derived.

  <ul>\n")

(define tail
"  </ul>

  The source used to find these scales is gross, but available
  <a href=\"https://github.com/welliam/tonerow\">here</a>, specifically in the
  file <a href=\"https://raw.githubusercontent.com/welliam/tonerow/master/molts.scm\">molts.scm</a>.

</body>\n")

(define (reg r)
  (lambda (s)
    (let ((search (irregex-extract r s)))
      (and (not (null? search)) (car search)))))

(define get-number (reg "\\d+"))
(define translated? (reg "-translated"))

(define trim-leading-zeros
  (compose list->string
            (cut* let loop ((s <>))
              (if (char=? (car s) #\0)
                  (loop (cdr s))
                  s))
            string->list))

(define (format-list-entry filename)
  (string-append
   "    <li><a href=\"scales/" filename
   "\">" (trim-leading-zeros (get-number filename) )
   " tone scale" 
   (if (translated? filename) " (in C)" "")
   "</a></li>"))

(define (list-scales)
  (fold-right (lambda (s res)
                (if (get-number s)
                    (string-append (format-list-entry s) "\n" res)
                    res))
              ""
              (directory "scales")))

(define (generate-page)
  (string-append head (list-scales) tail))

(define (display-to-page)
  (let ((filename "page.html"))
    (with-output-to-file filename
      (cut display (generate-page)))
    (print "Wrote to " filename)))

(display-to-page)
