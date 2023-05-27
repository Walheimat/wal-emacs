((nil . ((project-vc-name . "Walheimat")
         (find-sibling-rules . (("test/\\([^/]+\\)-test.el" "build/\\1.el")
                                ("test/\\([^/]+\\)-test.el" "lib/\\1.org")
                                ("build/\\([^/]+\\).el" "test/\\1-test.el")
                                ("build/\\([^/]+\\).el" "lib/\\1.org")
                                ("lib/\\([^/]+\\).org" "test/\\1-test.el")
                                ("lib/\\([^/]+\\).org" "build/\\1.el")
                                ("tools/\\([^/]+\\).el" "test/\\1-test.el")
                                ("test/\\([^/]+\\)-test.el" "tools/\\1.el")))
         (consult-buffer-filter . ("\\` " "\\`\\*" "\\`magit"))
         (wal-project-build-default-cmd . "make build")
         (wal-project-test-default-cmd . "make test")
         (wal-project-install-default-cmd . "make install")
         (wal-project-lint-default-cmd . "make pacify")))
 (markdown-mode . ((eval . (auto-fill-mode t)))))
