;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cs135search) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))


;; A doc-list (DL) is one of:
;; * empty
;; * (cons Str DL)
;; Requires: each doc (i.e. Str) only occurs once in the doc-list
;; the doc-list is in lexicographic order

;; An Inverted List (IL) is one of:
;; * empty
;; * (cons (list Str DL) IL)
;; Requires: each key (i.e. Str) only occurs once in the IL.
;; the keys occur in lexicographic order in the IL.

;; question a)

;; (both DL1 DL2) consumes two doc-lists and produces a list of documents
;; (possibly empty) that occur in both doc-lists
;; Examples:

(check-expect (both (list "a.txt") (list "a.txt" "b.txt")) (list "a.txt"))
(check-expect (both (list "cat.txt") (list "dog.txt" "duck.txt")) empty)
(check-expect (both (list "cat.txt" "donkey.txt")
                    (list "cat.txt" "dog.txt" "duck.txt")) (list "cat.txt"))

;; both: DL DL -> DL
(define (both DL1 DL2)
  (cond [(or (empty? DL1) (empty? DL2)) empty]
        [(string=? (first DL1) (first DL2))
         (cons (first DL1) (both (rest DL1) (rest DL2)))]
        [(string>? (first DL1) (first DL2)) (both DL1 (rest DL2))]
        [(string<? (first DL1) (first DL2)) (both (rest DL1) DL2)]))

;; Tests:
(check-expect (both (list "cat.txt") empty) empty)
(check-expect (both (list "cat.txt" "girl.txt") (list "boy.txt" "girl.txt"))
              (list "girl.txt"))
(check-expect (both (list "cat.txt" "dog.txt") (list "cat.txt")) (list "cat.txt"))
(check-expect (both empty (list "cat.txt")) empty)
(check-expect (both (list "cat.txt" "dog.txt") (list "cat.txt")) (list "cat.txt"))
(check-expect (both (list "cat.txt" "donkey.txt")
                    (list "cat.txt" "dog.txt" "donkey.txt"))
              (list "cat.txt" "donkey.txt"))

;; question b)

;; (exclude DL1 DL2) consumes two doc-lists and produces a list of documents
;; (possibly empty) that occur in the first doc-list but not the second one
;; Examples:

(check-expect (exclude (list "a.txt") (list "a.txt" "b.txt")) empty)
(check-expect (exclude (list "cat.txt" "pharmacy.txt")
                       (list "b.txt" "bob.txt")) (list "cat.txt" "pharmacy.txt"))
(check-expect (exclude (list "bee.txt" "cat.txt" "tail.txt")
                       (list "a.txt" "b.txt" "call.txt"))
              (list "bee.txt" "cat.txt" "tail.txt"))

;; exclude: DL DL -> DL
(define (exclude DL1 DL2)
  (cond [(and (empty? DL1) (empty? DL2)) empty]
        [(empty? DL2) DL1]
        [(empty? DL1) empty]
        [(string=? (first DL1) (first DL2)) (exclude (rest DL1) (rest DL2))]
        [(string>? (first  DL1) (first DL2)) (exclude DL1 (rest DL2))]
        [else (cons (first DL1) (exclude (rest DL1) DL2))]))

;; Tests:
(check-expect (exclude (list "lion.txt") empty) (list "lion.txt"))
(check-expect (exclude empty (list "lion.txt")) empty)
(check-expect (exclude (list "cat.txt" "donkey.txt")
                    (list "cat.txt" "dog.txt" "swan.txt")) (list "donkey.txt"))
(check-expect (exclude empty empty) empty)

;; question c)

;; (keys-retrieve doc an-il) consumes a String and an Inverted List and
;; produces a list of Strings with lexicographic ordering.
;; The values in the produced list are they keys from an-il whose
;; doc-lists contain doc. If doc is not contained in doc-list that
;; is associated with any keys in an-il, then function returns empty
;; Examples:

(check-expect (keys-retrieve "c.txt" (list (list "barks" (list "b.txt"))
                                           (list "cat" (list "a.txt" "c.txt"))
                                           (list "chases" (list "c.txt"))
                                           (list "dog" (list "b.txt" "c.txt"))
                                           (list "sleeps" (list "a.txt"))
                                           (list "suddenly" (list "c.txt"))
                                           (list "the"
                                                 (list "a.txt" "b.txt" "c.txt"))))
              (list "cat" "chases" "dog" "suddenly" "the"))
(check-expect (keys-retrieve "call.txt" (list (list "people"
                                                    (list "call.txt" "phone.txt"))
                                              (list "the" (list "phone.txt"))))
              (list "people"))
(check-expect (keys-retrieve "banana.txt"
                             (list (list "hello"
                                         (list "greetings.txt" "names.txt"))
                                   (list "the" (list "names.txt"))))
              empty)

;; keys-retrieve: Str IL -> listof Str
(define (keys-retrieve doc an-il)
  (cond [(empty? an-il) empty]
        [(match? doc (second (first an-il)))
         (cons (first (first an-il)) (keys-retrieve doc (rest an-il)))]
        [else (keys-retrieve doc (rest an-il))]))

;; (match? doc part-of-il) consumes a String and a Inverted List and produces true if
;; the doc matches with the doc-list in an-il, and false if none matches
;; Examples:

(check-expect (match? "farm.txt" (list "meat.txt" "farm.txt")) true)
(check-expect (match? "meow.txt" (list "meat.txt" "farm.txt")) false)
(check-expect (match? "farm.txt" empty) false)

;; match?: Str IL -> Bool
(define (match? doc part-of-il)
  (cond [(empty? part-of-il) false]
        [(string=? doc (first part-of-il)) true]
        [else (match? doc (rest part-of-il))]))

;; question d)

;; (search find-type str1 str2 an-il) consumes a Symbol (either 'both or 'exclude),
;; two Strings, and an Inverted list, to produce a doc-list.
;; If the Symbol is 'both, then produces a doc-list containing
;; docs present in boht of the keys' associated doc-lists.
;; If the Symbol is 'exclude, then produces a doc-list containing docs
;; associated with the key str1 but not key str2.
;; Examples:

(check-expect (search 'both "cat" "sleeps" (list (list "barks" (list "b.txt"))
                                                 (list "cat" (list "a.txt" "c.txt"))
                                                 (list "chases" (list "c.txt"))
                                                 (list "dog" (list "b.txt" "c.txt"))
                                                 (list "sleeps" (list "a.txt"))
                                                 (list "suddenly" (list "c.txt"))
                                                 (list "the"
                                                       (list "a.txt" "b.txt" "c.txt"))))
              (list "a.txt"))
(check-expect
 (search 'exclude "banana" "potato"
         (list (list "apple" (list "fruits.txt" "juice.txt" "snacks.txt"))
               (list "banana" (list "fruits.txt" "milk.txt" "snacks.txt"))
               (list "potato" (list "snacks.txt" "vegetable.txt"))))
 (list "fruits.txt" "milk.txt"))
(check-expect
 (search 'both "banana" "potato"
         (list (list "apple" (list "fruits.txt" "juice.txt" "snacks.txt"))
               (list "banana" (list "fruits.txt" "milk.txt" "snacks.txt"))
               (list "beef" (list "meat.txt"))
               (list "potato" (list "snacks.txt" "vegetable.txt"))
               ))(list "snacks.txt"))

;; search: (Anyof 'both 'exclude) Str Str IL -> DL
(define (search find-type str1 str2 an-il)
  (cond [(empty? an-il) empty]
        [(symbol=? 'both find-type) (both (str-in-il str1 an-il)
                                          (str-in-il str2 an-il))]
        [(symbol=? 'exclude find-type)
         (exclude (str-in-il str1 an-il) (str-in-il str2 an-il))]))

;; Tests:
(check-expect (search 'both "donut" "frog" empty) empty)
(check-expect
 (search 'both "banana" "potato"
         (list (list "apple" (list "fruits.txt" "juice.txt" "snacks.txt"))
               (list "beef" (list "meat.txt"))
               (list "potato" (list "snacks.txt" "vegetable.txt"))
               )) empty)

;; (str-in-il s an-il) consumes a String and an Inverted list and produces a
;; doc-list of one of the keys in the Inverted list if the key matches with
;; the String
;; Examples:

(check-expect (str-in-il "donut" (list (list "donut" (list "food.txt" "snack.txt"))))
              (list "food.txt" "snack.txt"))
(check-expect (str-in-il "chips" (list (list "donut" (list "food.txt" "snack.txt"))))
              empty)
(check-expect (str-in-il "gown" empty) empty) 
               
;; str-in-il: Str IL -> DL
(define (str-in-il s an-il)
  (cond [(empty? an-il) empty]
        [(string=? s (first (first an-il))) (second (first an-il))]
        [else (str-in-il s (rest an-il))]))