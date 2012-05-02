(ert-deftest projectile-test-string-suffix-p ()
  (should (projectile-string-suffix-p "test.txt" "txt"))
  (should (not (projectile-string-suffix-p "test.txt" "rb"))))

(ert-deftest projectile-test-uniquify-file ()
  (should (equal (uniquify-file "ala/bala/portokala") "bala/portokala")))

(ert-deftest projectile-test-project-get-name ()
  (should (equal (projectile-get-project-name) "projectile")))

(ert-deftest projectile-test-prepend-project-name ()
  (should (equal (projectile-prepend-project-name "Test") "[projectile] Test")))
