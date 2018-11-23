(defpackage #:imap2rss/main
  (:use #:cl)
  (:import-from #:trivial-imap)
  (:import-from #:xml-emitter)
  (:import-from #:ubiquitous)
  (:import-from #:cl-arrows
                #:-<>
                #:->)
  (:import-from #:html2text
                #:html2text)
  (:import-from #:plump)
  (:import-from #:html2text-link-revealer)
  (:export
   #:main))
(in-package imap2rss/main)


(defmethod html2text:get-node-tag :around ((node t))
  "Нам это нужно, чтобы рендерить ссылки не в Markdown стиле, а для Org-Mode."
  (let ((original-result (call-next-method)))
    (if (eql original-result :a)
        :a-to-org-mode
        original-result)))


(html2text:def-tag-serializer (:a-to-org-mode)
  (let* ((url (or (plump:attribute html2text:node "href")
                  ""))
         (processed-url (funcall html2text:*href-processor* url)))
    (html2text:write "[[" processed-url "][")
    (call-next-method)
    (html2text:write "]]")
    (values)))


(defun process-favorite-tweet (text)
  "Оставляет только текст, который был процитирован с помощью >
   потому что после него идёт реклама IFTT."
  (let* ((lines (cl-strings:split text #\Newline))
         (lines (loop for line in lines
                      for is-cite = (cl-strings:starts-with line ">")
                      for new-line = (-> (cl-ppcre:regex-replace-all "^>[ ]*" line "")
                                         (cl-strings:replace-all " [[https://ifttt.com/login][![](https://assets.ifttt.com/images/channels/2/icons/on_color_regular.png)"
                                                                 ""))
                      when is-cite
                        collect new-line)))
    
    (cl-strings:join lines
                     :separator (coerce '(#\Newline) 'string))))


(defun get-text (email)
  (let* ((subject (trivial-imap:get-subject email))
         (html (trivial-imap:get-html email))
         (text (if html
                   (html2text html)
                   (trivial-imap:get-text email))))
    
    (when (cl-strings:starts-with subject "Favorite tweet by")
      (setf text (process-favorite-tweet text)))
    
    (string-trim '(#\Newline #\Space) text)))


(defun write-feed-items (feed-filename emails)
  (with-open-file (s feed-filename
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :external-format :utf-8)
    (xml-emitter:with-rss2 (s :encoding "UTF-8")
      (xml-emitter:rss-channel-header "Some title"
                                      "http://some-url.com")
      (link-revealer:with-turned-on ()
        (loop for email in emails
              for text = (get-text email)
              do (xml-emitter:rss-item
                  (format nil "TODO ~A" (trivial-imap:get-subject email))
                  :pubdate (trivial-imap:get-timestamp email)
                  :guid (trivial-imap:get-uid email)
                  :description text))))))


(defun limit-items (items limit)
  (subseq items
          0
          (min limit
               (length items))))


(defun get-seen-emails ()
  (let* ((emails (ubiquitous:value :seen-emails))
         (first-uid (when emails
                      (trivial-imap:get-uid (first emails))))
         (last-uid (when emails
                     (trivial-imap:get-uid (alexandria:last-elt emails)))))
    (log:info "Restoring last seen"
              first-uid
              last-uid
              ubiquitous:*storage-pathname*
              ubiquitous:*storage*)
    emails))


(defun save-last-seen (emails)
  (let* ((first-uid (when emails
                      (trivial-imap:get-uid (first emails))))
         (last-uid (when emails
                     (trivial-imap:get-uid (alexandria:last-elt emails)))))
    (log:info "Saving last seen"
              first-uid
              last-uid))
  
  (setf (ubiquitous:value :seen-emails)
        emails))


(defun read-new-emails (host user password
                        &key
                          (folder "Inbox")
                          (timeout 15)
                          (ssl t)
                          (starttls t)
                          (limit 10)
                          since)
  (check-type since (or trivial-imap:email
                        null))
  (let* ((uid (when since
                (trivial-imap:get-uid since)))
         (emails (trivial-imap:fetch-messages host
                                              user
                                              password
                                              :folder folder
                                              :timeout timeout
                                              :ssl ssl
                                              :starttls starttls
                                              :limit limit
                                              :since-uid uid)))
    (nreverse emails)))


(defun save-urls-cache ()
  (let* ((cache (function-cache:find-function-cache-for-name 'link-revealer:get-final-url))
         (hash (function-cache:cached-results cache)))
    (setf (ubiquitous:value :cached-urls)
          hash)
    (values)))


(defun restore-urls-cache ()
  (let* ((hash (ubiquitous:value :cached-urls))
         (cache (function-cache:find-function-cache-for-name 'link-revealer:get-final-url)))
    (when hash
      (setf (function-cache:cached-results cache)
            hash))
    (values)))


(defun main (host user password
             &key
               (feed-filename "feed.xml")
               (folder "Inbox")
               (timeout 15)
               (ssl t)
               (starttls t)
               (limit 100))
  (ubiquitous:restore 'imap2rss)
  (restore-urls-cache)
  
  (let* ((seen-emails (get-seen-emails))
         (last-seen-email (first seen-emails))
         (new-emails (read-new-emails host
                                      user
                                      password
                                      :timeout timeout
                                      :ssl ssl
                                      :folder folder
                                      :limit limit
                                      :starttls starttls
                                      :since last-seen-email))
         (emails (limit-items (append new-emails
                                      seen-emails)
                              limit)))
    (write-feed-items feed-filename emails)
    (save-last-seen emails)
    (save-urls-cache)

    (values emails)))
