(ert-deftest projectile-test-string-suffix-p ()
  (should (projectile-string-suffix-p "test.txt" "txt"))
  (should (not (projectile-string-suffix-p "test.txt" "rb"))))

(ert-deftest projectile-test-uniquify-file ()
  (should (equal (projectile-uniquify-file "ala/bala/portokala") "bala/portokala")))

(ert-deftest projectile-test-project-get-name ()
  (should (equal (projectile-get-project-name) "project")))

(ert-deftest projectile-test-prepend-project-name ()
  (should (equal (projectile-prepend-project-name "Test") "[project] Test")))

(ert-deftest projectile-test-trim-no-whitespace ()
  (should (equal (projectile-trim "foo") "foo")))

(ert-deftest projectile-test-trim-whitespace-before ()
  (should (equal (projectile-trim " foo") "foo"))
  (should (equal (projectile-trim "\tfoo") "foo"))
  (should (equal (projectile-trim "\nfoo") "foo")))

(ert-deftest projectile-test-trim-whitespace-after ()
  (should (equal (projectile-trim "foo ") "foo"))
  (should (equal (projectile-trim "foo\t") "foo"))
  (should (equal (projectile-trim "foo\n") "foo")))

(ert-deftest projectile-test-trim-whitespace-both ()
  (should (equal (projectile-trim " foo ") "foo"))
  (should (equal (projectile-trim "\tfoo\t") "foo"))
  (should (equal (projectile-trim "\nfoo\n") "foo")))

(ert-deftest projectile-test-expand-root ()
  (should (equal (projectile-expand-root "foo") "/path/to/project/foo/")) 
  (should (equal (projectile-expand-root "foo/bar") "/path/to/project/foo/bar/")) 
  (should (equal (projectile-expand-root "./foo/bar") "/path/to/project/foo/bar/")))

