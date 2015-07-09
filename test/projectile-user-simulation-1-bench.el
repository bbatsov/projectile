;; (require 'projectile)
(require 'ert)
(require 'noflet)
(require 'test-helper)
(require 'bench-helper)
(require 'benchmark)
(require 'dash)
(require 'tramp)


(ert-deftest 00-connect-tramp ()
  (with-benchmarks
   (let ((default-directory bench-projectile-basedir))
     (should (file-exists-p (bp))))))

(projectile-global-mode 1)
(setq tramp-verbose 2)

(ert-deftest projectile-sim1-01-dired ()
  (with-benchmarks
   (save-window-excursion
     (dired (bp 'linux)))))

(ert-deftest projectile-sim1-02-find-file-makefile ()
  (with-benchmarks
   (save-window-excursion
     (find-file (bp 'linux "Makefile")))))

(ert-deftest projectile-sim1-03-dired-sound ()
  (with-benchmarks
   (save-window-excursion
     (dired (bp 'linux "sound")))))

(ert-deftest projectile-sim1-04-projectile-list-project-files ()
  (with-benchmarks
   (save-window-excursion
     (let ((default-directory (bp 'linux "sound")) )
       (projectile-current-project-files)))))

(ert-deftest projectile-sim1-05-find-file-snd/fw/fw/pcm.c ()
  (with-benchmarks
   (save-window-excursion
     (find-file (bp 'linux "sound/firewire/fireworks/fireworks_pcm.c")))))

(ert-deftest projectile-sim1-06-dired-sound/fw ()
  (with-benchmarks
   (save-window-excursion
     (dired (bp 'linux "sound/firewire/")))))

(ert-deftest projectile-sim1-07-dired-sound ()
  (with-benchmarks
   (save-window-excursion
     (dired (bp 'linux "sound")))))
