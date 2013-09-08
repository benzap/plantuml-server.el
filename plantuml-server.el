;; Copyright (C) 2013

;; Author: Benjamin Zaporzan  <benzaporzan@gmail.com>
;; Keywords: uml, export, plantuml

(defconst plantuml-url "http://www.plantuml.com:80/plantuml"
  "URL to the plantuml main page")

(defconst plantuml-url-form (concat plantuml-url "/form")
  "URL to the plantuml server form")

(defconst plantuml-url-svg (concat plantuml-url "/svg")
  "URL to send the key for an svg copy of the result")

(defconst plantuml-url-ascii (concat plantuml-url "/txt")
  "URL to send the key for an ascii copy of the result")

(defun plantuml-strip-http-headers (httpbuffer)
  "Strip headers from HTTP reply."
  (save-excursion 
    (set-buffer httpbuffer)
    (goto-char (point-min))
    (let ((endpt (search-forward "

")))
      (delete-region (point-min) endpt)
					; we're still left with an extra newline
      (goto-char (point-max))
      (delete-backward-char 1)
      (insert-string " "))))

(defun plantuml-url-http-post (text callback)
      "Send TEXT to the plantuml server, upon completing the
request, perform the provided CALLBACK"
      (let ((url plantuml-url-form)
	    (url-request-method "POST")
            (url-request-extra-headers
             '(("Content-Type" . "application/x-www-form-urlencoded")))
            (url-request-data (concat (url-hexify-string "text")
				      "="
				      (url-hexify-string text))))
	
        (url-retrieve url callback)))

(defun plantuml-url-http-post-synchronously (text)
      "Send TEXT to the plantuml server, synchronously"
      (let ((url plantuml-url-form)
	    (url-request-method "POST")
            (url-request-extra-headers
             '(("Content-Type" . "application/x-www-form-urlencoded")))
            (url-request-data (concat (url-hexify-string "text")
				      "="
				      (url-hexify-string text))))
	
        (url-retrieve-synchronously url)))

(defun plantuml-eval-to-other-window (&optional begin end)
  (interactive "r")
  "Evaluates the region and returns a plantuml ascii diagram in a
  new buffer in the other window. It also places a link to the
  SVG equivalent page"
  (let* ((current-string (buffer-substring begin end))
	(ascii-string (plantuml-get-ascii-diagram current-string)))
    (if (get-buffer "*PlantUML-Ascii*")
	(kill-buffer "*PlantUML-Ascii*"))
    (switch-to-buffer-other-window "*PlantUML-Ascii*")
    (set-buffer "*PlantUML-Ascii*")
    (insert ascii-string)
    (read-only-mode)
    (other-window 0)))

(defun plantuml-eval-to-browser (&optional begin end)
  (interactive "r")
  "Evaluates the region and goes to the URL displaying an SVG of
  the current region"
  (let* ((current-string (buffer-substring begin end))
	 (svg-url (plantuml-get-svg-url current-string)))
    (browse-url svg-url)))

(defun plantuml-get-ascii-url (string)
  "Gets the ascii diagram produced by the given STRING. STRING
should be a valid plantuml text representation of a diagram"
  (let (ascii-url
	(html-buffer (plantuml-url-http-post-synchronously string))
	urls)
    (save-excursion
      (set-buffer html-buffer)
      (setq urls (plantuml-regex-get-urls html-buffer))
      (setq ascii-url (find-if 
		       (lambda (arg) (string-match plantuml-url-ascii arg)) 
		       urls))      
      )ascii-url))

(defun plantuml-get-svg-url (string)
  "Gets the url diagram produced by the given STRING. STRING
should be a valid plantuml text representation of a diagram"
  (let (svg-url
	(html-buffer (plantuml-url-http-post-synchronously string))
	urls)
    (save-excursion
      (set-buffer html-buffer)
      (setq urls (plantuml-regex-get-urls html-buffer))
      (setq svg-url (find-if 
		       (lambda (arg) (string-match plantuml-url-svg arg)) 
		       urls))      
      )svg-url))

(defun plantuml-get-ascii-diagram (string)
  "Evaluates the string, and returns a ascii representation of
  the result"
  (let ((ascii-url (plantuml-get-ascii-url string))
	ascii-string)
    (save-excursion
      (set-buffer (url-retrieve-synchronously ascii-url))
      (plantuml-strip-http-headers (current-buffer))
      (setq ascii-string (buffer-string)))))

(defun plantuml-get-svg-diagram (string)
  "Evaluates the string, and returns a svg representation of
  the result"
  (let ((svg-url (plantuml-get-svg-url string))
	svg-string)
    (save-excursion
      (set-buffer (url-retrieve-synchronously svg-url))
      (plantuml-strip-http-headers (current-buffer))
      (setq svg-string (buffer-string)))))   

(defun plantuml-regex-get-urls (buffer)
  "From the provided buffer, get all of the URLs that have the
  structure \"http:// + plantuml-url\""
  (let ((urls (list))
	(regex-url (concat "\"\\(" plantuml-url ".+\\)\"")))
    (save-excursion
      (set-buffer buffer)
      (while (re-search-forward regex-url nil t)
	 (add-to-list 'urls (match-string 1)))
       )urls))

(provide 'plantuml-server)
