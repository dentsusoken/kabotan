(in-package :kabotan)

(defparameter *app* nil)

(defun setup-api (api)
  ;;  (setf (ningle:route app "/") "Welcome to ningle!")
  (setf (ningle:route api "/health")
        (lambda (params)
          (declare (ignore params))
          (let ((json (jonathan:to-json '((:status . "ok")) :from :alist)))
            `(200
              (:content-type "application/json; charset=utf-8")
              (,json))))))

(defun serve-index (env)
  "Serve the main index.html page.
   
   The Lack session middleware automatically handles session cookies,
   so we don't need to manually set them here."
  (declare (ignore env))
  (let ((html (uiop:read-file-string "public/index.html"
                                     :external-format :utf-8)))
    `(200
      (:content-type "text/html; charset=utf-8")
      (,html))))

(defun main ()
  (let ((api (make-instance 'ningle:app)))
    (setup-api api)
    (setup-halloween-api api)
    (setf *app*
          (lack:builder
           ;; Enable session middleware with cookie-based storage
           :session
           ;; Custom error handler to suppress 404 stack traces
           (lambda (app)
             (lambda (env)
               (handler-case
                   (funcall app env)
                 (error (condition)
                   ;; Only log non-404 errors
                   (let ((path (getf env :path-info)))
                     (unless (or (search "favicon" path)
                                (search ".map" path))
                       (format *error-output* "[ERROR] ~A: ~A~%" path condition)))
                   '(500 (:content-type "text/plain; charset=utf-8")
                     ("Internal Server Error"))))))
           (:static :path "/static/" :root #p"./public/")
           (:mount "/api" api)
           :accesslog
           (lambda (env)
             (let ((p (getf env :path-info)))
               (cond
                 ((or (string= p "/") (string= p "")) (serve-index env))
                 (t '(404 (:content-type "text/plain; charset=utf-8")
                      ("not found"))))))
           ))
    (clack:clackup *app* :address "0.0.0.0" :port 5000 :server :hunchentoot)
    (loop (sleep 1))))
