(defpackage anagram.tests
  (:use cl)
  (:local-nicknames
   (test epsilon.test)
   (http epsilon.http.client)
   (json epsilon.json)
   (map epsilon.map))
  (:export run-tests))

(in-package anagram.tests)

(defparameter *test-port* 8081)
(defparameter *base-url* (format nil "http://localhost:~A" *test-port*))

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

(defun run-tests ()
  "Run all anagram service tests"
  (format t "~&Starting anagram service tests on port ~A...~%" *test-port*)
  
  ;; Start the service in a separate thread
  (let ((server-thread (sb-thread:make-thread
                        (lambda ()
                          (anagram:start-service 
                           :port *test-port* 
                           :address "127.0.0.1"))
                        :name "test-server")))
    
    ;; Give server time to start
    (sleep 1)
    
    ;; Run the tests
    (unwind-protect
         (test:run :package "anagram.tests")
      
      ;; Stop the server
      (sb-thread:terminate-thread server-thread)
      (anagram:stop-service))))
