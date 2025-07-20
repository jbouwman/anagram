(defpackage :epsilon-anagram-service
  (:use :cl)
  (:local-nicknames
   (#:map #:epsilon.lib.map)
   (#:str #:epsilon.lib.string)
   (#:json #:epsilon.lib.json))
  (:export
   #:main
   #:start-service
   #:stop-service))

(in-package :epsilon-anagram-service)

(defvar *service-server* nil
  "Global reference to the running server")

;;; Anagram Logic

(defun shuffle-string (string)
  "Randomly shuffle characters in a string"
  (let ((chars (coerce string 'list)))
    (coerce (loop for i from (length chars) downto 2
                  do (rotatef (nth (random i) chars)
                              (nth (1- i) chars))
                  finally (return chars))
            'string)))

(defun compute-anagram (text)
  "Generate an anagram by shuffling words individually"
  (str:join " " 
    (mapcar #'shuffle-string 
            (str:split text #\Space))))

;;; HTTP Server Implementation using epsilon libraries

(defun handle-request (stream)
  "Handle a single HTTP request"
  (let ((request-line (read-line stream nil)))
    (when request-line
      (let* ((parts (str:split request-line #\Space))
             (method (first parts))
             (path (second parts)))
        
        ;; Read headers
        (let ((headers map:+empty+))
          (loop for line = (read-line stream nil)
                while (and line (> (length line) 0))
                do (let ((colon-pos (position #\: line)))
                     (when colon-pos
                       (let ((key (subseq line 0 colon-pos))
                             (value (string-trim " " (subseq line (1+ colon-pos)))))
                         (setf headers (map:assoc headers key value))))))
          
          ;; Get content length for POST requests
          (let ((content-length (parse-integer (or (map:get headers "Content-Length") "0") 
                                               :junk-allowed t)))
            
            ;; Handle different routes
            (cond
              ((string= path "/")
               (write-line "HTTP/1.1 200 OK" stream)
               (write-line "Content-Type: text/html" stream)
               (write-line "" stream)
               (write-line "<html><body><h1>Epsilon Anagram Service</h1><p>POST to /api/anagram with JSON: {\"text\":\"hello\"}</p></body></html>" stream))
              
              ((string= path "/health")
               (let ((response (json:encode (map:make-map "status" "healthy"))))
                 (write-line "HTTP/1.1 200 OK" stream)
                 (write-line "Content-Type: application/json" stream)
                 (format stream "Content-Length: ~D~%" (length response))
                 (write-line "" stream)
                 (write-string response stream)))
              
              ((and (string= method "POST") (string= path "/api/anagram"))
               ;; Read body based on content-length
               (let ((body (when (> content-length 0)
                             (let ((buffer (make-string content-length)))
                               (read-sequence buffer stream)
                               buffer))))
                 (if body
                     (handler-case
                         (let* ((json-data (json:decode body))
                                (text (map:get json-data "text")))
                           (if text
                               (let* ((anagram (compute-anagram text))
                                      (response-data (map:make-map 
                                                      "original" text
                                                      "anagram" anagram))
                                      (response (json:encode response-data)))
                                 (write-line "HTTP/1.1 200 OK" stream)
                                 (write-line "Content-Type: application/json" stream)
                                 (format stream "Content-Length: ~D~%" (length response))
                                 (write-line "" stream)
                                 (write-string response stream))
                               (let ((error-response (json:encode (map:make-map "error" "No text provided"))))
                                 (write-line "HTTP/1.1 400 Bad Request" stream)
                                 (write-line "Content-Type: application/json" stream)
                                 (format stream "Content-Length: ~D~%" (length error-response))
                                 (write-line "" stream)
                                 (write-string error-response stream))))
                       (error (e)
                         (let ((error-response (json:encode (map:make-map "error" (format nil "~A" e)))))
                           (write-line "HTTP/1.1 400 Bad Request" stream)
                           (write-line "Content-Type: application/json" stream)
                           (format stream "Content-Length: ~D~%" (length error-response))
                           (write-line "" stream)
                           (write-string error-response stream))))
                     (let ((error-response (json:encode (map:make-map "error" "No body provided"))))
                       (write-line "HTTP/1.1 400 Bad Request" stream)
                       (write-line "Content-Type: application/json" stream)
                       (format stream "Content-Length: ~D~%" (length error-response))
                       (write-line "" stream)
                       (write-string error-response stream)))))
              
              (t
               (write-line "HTTP/1.1 404 Not Found" stream)
               (write-line "" stream)
               (write-line "Not Found" stream)))
            
            (force-output stream)))))))

(defun start-service (&key (port 8080) (address "127.0.0.1"))
  "Start the web service on specified port and address"
  (when *service-server*
    (error "Service is already running"))
  
  (format t "~&Starting epsilon-based web service on ~A:~A~%" address port)
  
  ;; Use standard sockets but with epsilon libraries for data handling
  (setf *service-server* 
        (sb-bsd-sockets:make-inet-socket :stream :tcp))
  
  (sb-bsd-sockets:socket-bind *service-server* 
                              (sb-bsd-sockets:make-inet-address address) 
                              port)
  (sb-bsd-sockets:socket-listen *service-server* 5)
  
  (format t "~&Service started. Available endpoints:~%")
  (format t "  GET  /              - Simple info page~%")
  (format t "  GET  /health        - Health check (using epsilon JSON)~%")
  (format t "  POST /api/anagram   - Generate anagram (JSON API with epsilon)~%")
  
  ;; Simple server loop
  (loop
    (let ((client-socket (sb-bsd-sockets:socket-accept *service-server*)))
      (unwind-protect
          (let ((stream (sb-bsd-sockets:socket-make-stream client-socket
                                                           :input t :output t
                                                           :element-type 'character)))
            (handle-request stream))
        (sb-bsd-sockets:socket-close client-socket)))))

(defun stop-service ()
  "Stop the web service"
  (when *service-server*
    (format t "~&Stopping web service...~%")
    (sb-bsd-sockets:socket-close *service-server*)
    (setf *service-server* nil)
    (format t "~&Service stopped~%")))

(defun main ()
  "Main entry point for the web service"
  (format t "~&Starting Epsilon Anagram Service...~%")
  
  ;; Parse command line arguments for port
  (let ((port (parse-integer 
               (or (sb-ext:posix-getenv "PORT") "8080")
               :junk-allowed t)))
    (when (null port)
      (setf port 8080))
    
    ;; Start the service
    (handler-case
        (start-service :port port :address "0.0.0.0")
      (sb-sys:interactive-interrupt ()
        (format t "~%Received interrupt signal~%")
        (stop-service))
      (error (e)
        (format t "~%Error occurred: ~A~%" e)
        (stop-service)))))