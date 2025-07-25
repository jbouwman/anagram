(defpackage anagram.unit-tests
  (:use cl)
  (:local-nicknames
   (test epsilon.test)
   (json epsilon.json)
   (http epsilon.http.client)
   (map epsilon.map))
  (:export run-tests))

(in-package anagram.unit-tests)

(defparameter *test-port* 8081)
(defparameter *base-url* (format nil "http://localhost:~A" *test-port*))

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

(test:deftest test-health-endpoint ()
  "Test the health check endpoint"
  (multiple-value-bind (status-code headers body)
      (http:get (format nil "~A/health" *base-url*))
    (declare (ignore headers))
    (test:is (= status-code 200))
    (let ((body-data (json:parse body)))
      (test:is-equal (map:get body-data "status") "healthy"))))

(test:deftest test-anagram-generation ()
  "Test successful anagram generation"
  (let ((request-body (with-output-to-string (s)
                        (json:encode (map:make-map "text" "hello world") s))))
    (multiple-value-bind (status-code headers body)
        (http:post (format nil "~A/api/anagram" *base-url*)
                   :body request-body
                   :headers (map:make-map "Content-Type" "application/json"))
      (declare (ignore headers))
      (test:is (= status-code 200))
      (let ((body-data (json:parse body)))
        (test:is-equal (map:get body-data "original") "hello world")
        (test:is (stringp (map:get body-data "anagram")))
        ;; Check anagram has same characters
        (test:is-equal (sort (copy-seq (map:get body-data "original")) #'char<)
                       (sort (copy-seq (map:get body-data "anagram")) #'char<))))))

(test:deftest test-empty-text ()
  "Test anagram generation with empty text"
  (let ((request-body (with-output-to-string (s)
                        (json:encode (map:make-map "text" "") s))))
    (multiple-value-bind (status-code headers body)
        (http:post (format nil "~A/api/anagram" *base-url*)
                   :body request-body
                   :headers (map:make-map "Content-Type" "application/json"))
      (declare (ignore headers))
      (test:is (= status-code 400))
      (let ((body-data (json:parse body)))
        (test:is (map:get body-data "error"))))))

(test:deftest test-missing-text-field ()
  "Test anagram generation without text field"
  (let ((request-body (with-output-to-string (s)
                        (json:encode (map:make-map "wrong" "field") s))))
    (multiple-value-bind (status-code headers body)
        (http:post (format nil "~A/api/anagram" *base-url*)
                   :body request-body
                   :headers (map:make-map "Content-Type" "application/json"))
      (declare (ignore headers))
      (test:is (= status-code 400))
      (let ((body-data (json:parse body)))
        (test:is (map:get body-data "error"))))))
!
(test:deftest test-invalid-json ()
  "Test anagram generation with invalid JSON"
  (multiple-value-bind (status-code headers body)
      (http:post (format nil "~A/api/anagram" *base-url*)
                 :body "not valid json"
                 :headers (map:make-map "Content-Type" "application/json"))
    (declare (ignore headers))
    (test:is (= status-code 400))
    (let ((body-data (json:parse body)))
      (test:is (map:get body-data "error")))))

(test:deftest test-404-endpoint ()
  "Test that non-existent endpoints return 404"
  (multiple-value-bind (status-code headers body)
      (http:get (format nil "~A/nonexistent" *base-url*))
    (declare (ignore headers body))
    (test:is (= status-code 404))))
