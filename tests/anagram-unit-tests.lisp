(defpackage anagram.unit-tests
  (:use cl)
  (:local-nicknames
   (test epsilon.test)
   (json epsilon.json)
   (map epsilon.map))
  (:export run-tests))

(in-package anagram.unit-tests)

(test:deftest test-shuffle-string ()
  "Test the shuffle-string function"
  (let ((original "hello")
        (shuffled (anagram:shuffle-string "hello")))
    ;; Should be same length
    (test:is (= (length original) (length shuffled)))
    ;; Should contain same characters (but allow same result occasionally)
    (test:is (every (lambda (char) (find char shuffled)) original))
    (test:is (every (lambda (char) (find char original)) shuffled))))

(test:deftest test-compute-anagram ()
  "Test the compute-anagram function"
  (let* ((original "hello world")
         (anagram (anagram:compute-anagram original)))
    ;; Should be same length  
    (test:is (= (length original) (length anagram)))
    ;; Should contain same characters
    (test:is-equal (sort (copy-seq original) #'char<)
                   (sort (copy-seq anagram) #'char<))))

(test:deftest test-make-json-response ()
  "Test JSON response creation"
  (let* ((data (map:make-map "message" "hello"))
         (response (anagram:make-json-response data :status 200)))
    (test:is (= (anagram:response-status response) 200))
    (test:is-equal (map:get (anagram:response-headers response) "Content-Type")
                   "application/json")
    (test:is (stringp (anagram:response-body response)))))

(defun run-tests ()
  "Run unit tests for anagram functions"
  (format t "~&Running anagram unit tests...~%")
  (test:run :package "anagram.unit-tests"))