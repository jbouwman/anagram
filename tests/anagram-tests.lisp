(defpackage :epsilon-anagram-service.tests
  (:use :cl)
  (:local-nicknames
   (#:test #:epsilon.tool.test)
   (#:http #:epsilon.http.client)
   (#:json #:epsilon.lib.json)
   (#:map #:epsilon.lib.map))
  (:export #:run-tests))

(in-package :epsilon-anagram-service.tests)

(defparameter *test-port* 8081)
(defparameter *base-url* (format nil "http://localhost:~A" *test-port*))

(test:deftest test-health-endpoint ()
  "Test the health check endpoint"
  (let ((response (http:get (format nil "~A/health" *base-url*))))
    (test:is (= (http:status-code response) 200))
    (let ((body (json:decode (http:body response))))
      (test:is-equal (map:get body "status") "healthy"))))

(test:deftest test-anagram-generation ()
  "Test successful anagram generation"
  (let ((response (http:post (format nil "~A/api/anagram" *base-url*)
                             :body (json:encode (map:make-map "text" "hello world"))
                             :headers (map:make-map "Content-Type" "application/json"))))
    (test:is (= (http:status-code response) 200))
    (let ((body (json:decode (http:body response))))
      (test:is-equal (map:get body "original") "hello world")
      (test:is (stringp (map:get body "anagram")))
      ;; Check anagram has same characters
      (test:is-equal (sort (copy-seq (map:get body "original")) #'char<)
                     (sort (copy-seq (map:get body "anagram")) #'char<)))))

(test:deftest test-empty-text ()
  "Test anagram generation with empty text"
  (let ((response (http:post (format nil "~A/api/anagram" *base-url*)
                             :body (json:encode (map:make-map "text" ""))
                             :headers (map:make-map "Content-Type" "application/json"))))
    (test:is (= (http:status-code response) 400))
    (let ((body (json:decode (http:body response))))
      (test:is (map:get body "error")))))

(test:deftest test-missing-text-field ()
  "Test anagram generation without text field"
  (let ((response (http:post (format nil "~A/api/anagram" *base-url*)
                             :body (json:encode (map:make-map "wrong" "field"))
                             :headers (map:make-map "Content-Type" "application/json"))))
    (test:is (= (http:status-code response) 400))
    (let ((body (json:decode (http:body response))))
      (test:is (map:get body "error")))))

(test:deftest test-invalid-json ()
  "Test anagram generation with invalid JSON"
  (let ((response (http:post (format nil "~A/api/anagram" *base-url*)
                             :body "not valid json"
                             :headers (map:make-map "Content-Type" "application/json"))))
    (test:is (= (http:status-code response) 400))
    (let ((body (json:decode (http:body response))))
      (test:is (map:get body "error")))))

(test:deftest test-404-endpoint ()
  "Test that non-existent endpoints return 404"
  (let ((response (http:get (format nil "~A/nonexistent" *base-url*))))
    (test:is (= (http:status-code response) 404))))

(defun run-tests ()
  "Run all anagram service tests"
  (format t "~&Starting anagram service tests on port ~A...~%" *test-port*)
  
  ;; Start the service in a separate thread
  (let ((server-thread (sb-thread:make-thread
                        (lambda ()
                          (epsilon-anagram-service:start-service 
                           :port *test-port* 
                           :address "127.0.0.1"))
                        :name "test-server")))
    
    ;; Give server time to start
    (sleep 1)
    
    ;; Run the tests
    (unwind-protect
         (test:run-package-tests :epsilon-anagram-service.tests)
      
      ;; Stop the server
      (sb-thread:terminate-thread server-thread)
      (epsilon-anagram-service:stop-service))))