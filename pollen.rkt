#lang racket

(require pollen/tag)
(require racket/match)

(provide (all-defined-out))

(define (scripture-url book chapter verses)
  (let* ((collection (match book
                       [(or "Isaiah") "ot"]
                       [(or "Matthew" "Mark" "Luke" "John") "nt"]
                       [(or "Helaman" "3 Nephi") "bofm"]
                       [_ (error (format "Unknown collection for '~a'" book))]))
         (book-abbrev (match book
                        ["Isaiah" "isa"]
                        ["Matthew" "matt"]
                        ["Mark" "mark"]
                        ["Luke" "luke"]
                        ["John" "john"]
                        ["Helaman" "hel"]
                        ["3 Nephi" "3-ne"]
                        [_ (error (format "Unknown book abbreviation for '~a'" book))]))
         (first-verse (car verses))
         (last-verse (cdr verses))
         (verse-span (if last-verse
                         (format "p~a-p~a" first-verse last-verse)
                         (format "p~a" first-verse))))
    (format "https://www.churchofjesuschrist.org/study/scriptures/~a/~a/~a?id=~a#p~a"
            collection
            book-abbrev
            chapter
            verse-span
            first-verse)))

(define (day number . content)
  (let ([day-id (format "day-~a" number)]
        [close-button '(form ((method "dialog")) (button "×"))])
    `(section ((class "day") (id ,day-id))
              (h2 ,(format "Day ~a" number))
              ,@content)))


(define para (default-tag-function 'p))
(define question (default-tag-function 'p #:class "question"))
(define action (default-tag-function 'p #:class "action"))

(define (scripture book chapter verses)
  (let* ([url (scripture-url book chapter verses)]
         [first-verse (car verses)]
         [last-verse (cdr verses)]
         [verse-range (if last-verse
                          (format "~a–~a" first-verse last-verse)
                          (format "~a" first-verse))]

         )
    `(p ((class "scripture")) "Read " (a ((href ,url)) ,(format "~a ~a:~a" book chapter verse-range)))))
