((nil . ((project-vc-name . "Walheimat")
         (find-sibling-rules . (("test/\\([^/]+\\)-test.el" "build/\\1.el")
                                ("test/\\([^/]+\\)-test.el" "lib/\\1.org")
                                ("build/\\([^/]+\\).el" "test/\\1-test.el")
                                ("build/\\([^/]+\\).el" "lib/\\1.org")
                                ("lib/\\([^/]+\\).org" "test/\\1-test.el")
                                ("lib/\\([^/]+\\).org" "build/\\1.el")
                                ("tools/\\([^/]+\\).el" "test/\\1-test.el")
                                ("test/\\([^/]+\\)-test.el" "tools/\\1.el")))
         (wal-project-build-default-cmd . "make tangle")
         (wal-project-install-default-cmd . "make local")
         (wal-project-execute-default-cmd . ("make pacify"
                                             "make cold-boot"
                                             "make update-version"
                                             "git submodule update --remote"))))
 (markdown-mode . ((eval . (auto-fill-mode t)))))
