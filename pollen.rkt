#lang racket

(require pollen/tag)
(require racket/match)

(provide (all-defined-out))

(define (day number . content)
  (let ([day-id (format "day-~a" number)]
        [close-button '(button ((class "close-day")) "×")])
    `(section ((class "day") (id ,day-id))
              (div ((class "card-holder"))
                   (div ((class "card")))
                   (button ((class "open-day")) ,(number->string number)))
              (dialog
               (div ((class "day-content"))
                    ,close-button
                    (h2 ,(format "December ~a" number))
                    ,@content)))))


(define para (default-tag-function 'p))

(define (question . content)
    (let ([icon '(img ((class "icon") (src "images/question-icon.svg") (alt "Question mark icon")))])
    `(p
        ((class "question"))
        ,icon
        ,@content)))

(define (action . content)
    (let ([icon '(img ((class "icon") (src "images/star-icon.svg") (alt "Star icon")))])
    `(p
        ((class "action"))
        ,icon
        ,@content)))


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

(define (scripture book chapter verses)
  (let* ([url (scripture-url book chapter verses)]
         [first-verse (car verses)]
         [last-verse (cdr verses)]
         [verse-range (if last-verse
                          (format "~a–~a" first-verse last-verse)
                          (format "~a" first-verse))]
         [icon '(img ((class "scripture icon") (src "images/book-icon.svg") (alt "Scriptures icon")))])
    `(p ((class "scripture"))
        ,icon
        "Read " (a ((href ,url)) ,(format "~a ~a:~a" book chapter verse-range)) ".")))
