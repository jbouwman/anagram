(defpackage anagram
  (:use
   cl)
  (:local-nicknames
   (web epsilon.web)
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence)
   (json epsilon.json)
   (request epsilon.http.request)
   (response epsilon.http.response)
   (server epsilon.http.server))
  (:export
   main))

(in-package anagram)

;;; Simple anagram logic using character shuffling

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

;;; Web handlers using epsilon.web

(web:defhandler home-handler (req)
  (web:html 
   "<html>
<head>
  <title>Anagram Service</title>
  <style>
    body { font-family: Arial, sans-serif; max-width: 600px; margin: 50px auto; padding: 20px; }
    .form-group { margin: 20px 0; }
    input[type=text] { width: 300px; padding: 10px; }
    button { padding: 10px 20px; background: #007cba; color: white; border: none; cursor: pointer; }
    #result { margin-top: 20px; padding: 15px; background: #f0f0f0; border-radius: 5px; }
  </style>
</head>
<body>
  <h1>Anagram Service</h1>
  <p>Enter some text to generate an anagram:</p>
  
  <div class='form-group'>
    <input type='text' id='text-input' placeholder='Enter text here...'>
    <button onclick='generateAnagram()'>Generate Anagram</button>
  </div>
  
  <div id='result'></div>
  
  <script>
    function generateAnagram() {
      const text = document.getElementById('text-input').value;
      if (!text.trim()) {
        document.getElementById('result').innerHTML = '<p style=\"color: red;\">Please enter some text.</p>';
        return;
      }
      
      fetch('/api/anagram', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ text: text })
      })
      .then(response => response.json())
      .then(data => {
        if (data.error) {
          document.getElementById('result').innerHTML = '<p style=\"color: red;\">Error: ' + data.error + '</p>';
        } else {
          document.getElementById('result').innerHTML = 
            '<h3>Result:</h3>' +
            '<p><strong>Original:</strong> ' + data.original + '</p>' +
            '<p><strong>Anagram:</strong> ' + data.anagram + '</p>';
        }
      })
      .catch(error => {
        document.getElementById('result').innerHTML = '<p style=\"color: red;\">Error: ' + error + '</p>';
      });
    }
    
    // Allow Enter key to submit
    document.getElementById('text-input').addEventListener('keypress', function(e) {
      if (e.key === 'Enter') {
        generateAnagram();
      }
    });
  </script>
</body>
</html>"))

(web:defhandler health-handler (req)
  (web:json (map:make-map "status" "healthy")))

(web:defhandler anagram-handler (req)
  (web:with-json-body (data req)
    (let ((text (map:get data "text")))
      (if (and text (> (length (string-trim " " text)) 0))
          (let ((anagram (compute-anagram text)))
            (web:json (map:make-map "original" text
                                    "anagram" anagram)))
          (web:bad-request "No text provided")))))

;;; Route definition

(web:defroutes *routes*
  (:get "/" #'home-handler)
  (:get "/health" #'health-handler)
  (:post "/api/anagram" #'anagram-handler))

;;; Main entry point

(defun main ()
  "Main entry point for the anagram service"
  (format t "~&Starting Anagram Service...~%")
  
  ;; Parse command line arguments for port
  (let ((port (parse-integer 
               (or (sb-ext:posix-getenv "PORT") "8080")
               :junk-allowed t)))
    (when (null port)
      (setf port 8080))
    
    (format t "~&Starting service on port ~A~%" port)
    
    ;; Create handler with routes and middleware
    (let ((app (web:wrap-middleware 
                (web:handle-routes *routes*)
                web:logging-middleware
                web:json-errors-middleware)))
      
      ;; Start the server
      (handler-case
          (server:start-server app :port port :address "0.0.0.0")
        (sb-sys:interactive-interrupt ()
          (format t "~%Received interrupt signal~%")
          (server:stop-server port))
        (error (e)
          (format t "~%Error occurred: ~A~%" e)
          (server:stop-server port))))))
