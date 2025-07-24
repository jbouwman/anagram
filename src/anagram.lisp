(defpackage anagram
  (:use :cl)
  (:local-nicknames
   (map epsilon.map)
   (str epsilon.string)
   (json epsilon.json)
   (seq epsilon.sequence))
  (:export
   main
   shuffle-string
   compute-anagram
   make-json-response
   make-html-response
   make-error-response
   response-status
   response-headers
   response-body
   start-service
   stop-service))

(in-package anagram)

(defvar *service-server* nil
  "Global reference to the running server")

;;; Core Anagram Logic

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
  (str:join #\Space 
    (seq:map #'shuffle-string 
             (str:split #\Space text))))

;;; HTTP Infrastructure

(defstruct request
  method
  path
  headers
  body)

(defstruct response
  status
  headers
  body)

(defun make-json-response (data &key (status 200))
  "Create a JSON response"
  (let ((body (with-output-to-string (s)
                (json:encode data s))))
    (make-response :status status
                   :headers (map:make-map "Content-Type" "application/json"
                                          "Content-Length" (length body))
                   :body body)))

(defun make-html-response (html &key (status 200))
  "Create an HTML response"
  (make-response :status status
                 :headers (map:make-map "Content-Type" "text/html"
                                        "Content-Length" (length html))
                 :body html))

(defun make-error-response (message &key (status 400))
  "Create an error response"
  (make-json-response (map:make-map "error" message) :status status))

;;; Route Handlers

(defun home-handler (request)
  "Handler for home page"
  (declare (ignore request))
  (make-html-response 
   "<html>
<head><title>Anagram Service</title></head>
<body>
<h1>Anagram Service</h1>
<p>A simple web service for generating anagrams.</p>

<h2>API</h2>
<h3>POST /api/anagram</h3>
<p>Generate an anagram:</p>
<pre>{\"text\": \"hello world\"}</pre>

<h3>GET /health</h3>
<p>Health check endpoint.</p>
</body>
</html>"))

(defun health-handler (request)
  "Handler for health check"
  (declare (ignore request))
  (make-json-response (map:make-map "status" "healthy")))

(defun anagram-handler (request)
  "Handler for anagram generation"
  (handler-case
      (let* ((json-data (json:parse (request-body request)))
             (text (map:get json-data "text")))
        (if (and text (> (length text) 0))
            (let ((anagram (compute-anagram text)))
              (make-json-response (map:make-map "original" text
                                                "anagram" anagram)))
            (make-error-response "No text provided")))
    (error (e)
      (make-error-response (format nil "Invalid request: ~A" e)))))

(defun not-found-handler (request)
  "Handler for 404 responses"
  (declare (ignore request))
  (make-response :status 404
                 :headers map:+empty+
                 :body "Not Found"))

;;; Routing System

(defstruct route
  method
  path
  handler)

(defparameter *routes* 
  (list
   (make-route :method :get :path "/" :handler #'home-handler)
   (make-route :method :get :path "/health" :handler #'health-handler)
   (make-route :method :post :path "/api/anagram" :handler #'anagram-handler)))

(defun find-route (method path)
  "Find handler for given method and path"
  (find-if (lambda (route)
             (and (eq (route-method route) method)
                  (string= (route-path route) path)))
           *routes*))

;;; HTTP Processing

(defun parse-request (stream headers-map content-length)
  "Parse HTTP request with pre-read headers"
  (let ((body (when (and content-length (> content-length 0))
                (let ((buffer (make-string content-length)))
                  (read-sequence buffer stream)
                  buffer))))
    (make-request :headers headers-map
                  :body body)))

(defun send-response (stream response)
  "Send HTTP response to stream"
  (let ((status-text (case (response-status response)
                       (200 "OK")
                       (400 "Bad Request")
                       (404 "Not Found")
                       (t "Unknown"))))
    (format stream "HTTP/1.1 ~D ~A~%" (response-status response) status-text)
    
    ;; Send headers
    (map:each (lambda (k v)
                (format stream "~A: ~A~%" k v))
              (response-headers response))
    
    ;; End of headers
    (write-line "" stream)
    
    ;; Send body
    (when (response-body response)
      (write-string (response-body response) stream))
    
    (force-output stream)))

(defun handle-request (stream)
  "Handle a single HTTP request"
  (let ((request-line (read-line stream nil)))
    (when request-line
      (let* ((parts (str:split request-line #\Space))
             (method-str (first parts))
             (path (second parts))
             (method (cond ((string= method-str "GET") :get)
                           ((string= method-str "POST") :post)
                           (t :unknown))))
        
        ;; Read headers
        (let ((headers map:+empty+))
          (loop for line = (read-line stream nil)
                while (and line (> (length line) 0))
                do (let ((colon-pos (position #\: line)))
                     (when colon-pos
                       (let ((key (subseq line 0 colon-pos))
                             (value (string-trim " " (subseq line (1+ colon-pos)))))
                         (setf headers (map:assoc headers key value))))))
          
          ;; Get content length
          (let* ((content-length (parse-integer (or (map:get headers "Content-Length") "0") 
                                                :junk-allowed t))
                 (request (parse-request stream headers content-length)))
            
            ;; Update request with method and path
            (setf (request-method request) method)
            (setf (request-path request) path)
            
            ;; Find and invoke handler
            (let* ((route (find-route method path))
                   (response (if route
                                 (funcall (route-handler route) request)
                                 (not-found-handler request))))
              
              (send-response stream response))))))))

;;; Server Management

(defun start-service (&key (port 8080) (address "127.0.0.1"))
  "Start the web service on specified port and address"
  (when *service-server*
    (error "Service is already running"))
  
  (format t "~&Starting anagram service on ~A:~A~%" address port)
  
  (setf *service-server* 
        (sb-bsd-sockets:make-inet-socket :stream :tcp))
  
  (sb-bsd-sockets:socket-bind *service-server* 
                              (sb-bsd-sockets:make-inet-address address) 
                              port)
  (sb-bsd-sockets:socket-listen *service-server* 5)
  
  (format t "~&Service started. Available endpoints:~%")
  (loop for route in *routes*
        do (format t "  ~A ~A~%" 
                   (route-method route) 
                   (route-path route)))
  
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
  (format t "~&Starting Anagram Service...~%")
  
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