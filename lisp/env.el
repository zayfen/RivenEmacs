(defconst root-dir
  (abbreviate-file-name
   (file-name-directory
    (directory-file-name
     (file-name-directory (file-truename load-file-name))))))

(defconst lisp-dir (concat root-dir "lisp/"))
(defconst local-dir (concat root-dir "local/"))
(defconst cache-dir (concat local-dir "cache/"))
(defconst repo-dir (concat root-dir "elpa/"))

(defconst os/linux (and (memq system-type '(gnu gnu/linux)) t))
(defconst os/bsd (and (memq system-type '(darwin berkeley-unix gnu/kfreebsd)) t))
(defconst os/win (and (memq system-type '(cygwin windows-nt ms-dos)) t))
(defconst os/mac (eq system-type 'darwin))
