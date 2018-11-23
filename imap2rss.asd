(defsystem imap2rss
  :class :package-inferred-system
  :depends-on ("imap2rss/main")
  ;; :defsystem-depends-on (:deploy)
  ;; :build-operation "deploy-op"
  :build-operation "program-op"
  :build-pathname "imap2rss"
  :entry-point "imap2rss/main::test-main")
