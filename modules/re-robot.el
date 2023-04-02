;;; re-ros.el --- Robot Operating System -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(defgroup rivenemacs-robot nil
  "RivenEmacs robotics stuff."
  :group 'rivenemacs)

(defcustom +ros-mcap-command (or (executable-find "mcap") (executable-find "mcap-cli"))
  "ROS 2 MCAP command."
  :group 'rivenemacs-robot)

(defcustom +ros-rosbag-command (executable-find "rosbag")
  "ROS 1 \"rosbag\" command."
  :group 'rivenemacs-robot)

(defcustom +ros-ros2-command (executable-find "ros2")
  "ROS 2 \"ros2\" command."
  :group 'rivenemacs-robot)

(dolist (ext-mode '(("\\.rviz\\'"   . conf-unix-mode)
                    ("\\.urdf\\'"   . xml-mode)
                    ("\\.xacro\\'"  . xml-mode)
                    ("\\.launch\\'" . xml-mode)
                    ("\\.msg\\'"    . gdb-script-mode)
                    ("\\.srv\\'"    . gdb-script-mode)
                    ("\\.action\\'" . gdb-script-mode)))
  (add-to-list 'auto-mode-alist ext-mode))

(+deferred-when! (or +ros-mcap-command +ros-rosbag-command +ros-ros2-command)
  ;; A mode to display info from ROS bag files (via MCAP)
  (define-derived-mode rosbag-info-mode conf-colon-mode "ROS bag"
    "Major mode for viewing ROS/ROS2 bag files."
    :interactive nil
    (buffer-disable-undo)
    (set-buffer-modified-p nil)
    (setq-local buffer-read-only t
                truncate-lines t))

  (defun rosbag-info-mode-open-file (file)
    "Browse the contents of an ROS bag (v1, SQLite, or MCAP) file."
    (interactive "fROS/ROS2/MCAP bag file name: ")
    (let ((bag-format (file-name-extension file)))
      (if (not (member bag-format '("bag" "db3" "mcap")))
          (user-error "File \"%s\" doesn't seem to be a ROS/ROS2 bag file."
                      (file-name-nondirectory file))
        (let ((buffer-read-only nil)
              (buff (get-buffer-create
                     (format "*ROS (%s) %s*" (upcase bag-format) (file-name-nondirectory file)))))
          (pop-to-buffer buff)
          (pcase bag-format
            ("bag"
             (call-process +ros-rosbag-command
                           nil buff nil "info" (expand-file-name file)))
            ("db3"
             (call-process +ros-ros2-command
                           nil buff nil "bag" "info" (expand-file-name file)))
            ("mcap"
             (call-process +ros-mcap-command
                           nil buff nil "info" (expand-file-name file)))
            (rosbag-info-mode)))))))

;; Needed by ros.el
(use-package kv
  :straight t)

(use-package string-inflection
  :straight t)

(use-package with-shell-interpreter
  :straight t)

(when (< emacs-major-version 29)
  (use-package docker-tramp
    :straight t))

;; ROS package
(use-package ros
  :straight (:host github :repo "DerBeutlin/ros.el")
  :init
  (+map! :infix "o"
    "r"  '(nil :wk "ros")
    "rr" '(+hydra-ros-main/body :wk "Hydra")
    "rs" '(ros-set-workspace :wk "Set workspace")
    "rp" '(ros-go-to-package :wk "Go to package")
    "rC" '(ros-cache-clean :wk "Clean cache"))
  :config
  (defhydra +hydra-ros-main (:color blue :hint nil :foreign-keys warn)
    "
[ROS]                                                  [_q_] quit
  ├──────────────────────────────────────────────────────────────────────╮
  │  [_c_] Compile    [_t_] Test       [_w_] Set workspace   [_p_] Packages      │
  │  [_m_] Messages   [_s_] Services   [_a_] Actions         [_x_] Clean cache   │
  ╰──────────────────────────────────────────────────────────────────────╯
"
    ("c" ros-colcon-build-transient)
    ("t" ros-colcon-test-transient)
    ("w" ros-set-workspace)
    ("p" hydra-ros-packages/body)
    ("m" hydra-ros-messages/body)
    ("s" hydra-ros-srvs/body)
    ("a" hydra-ros-actions/body)
    ("x" ros-cache-clean)
    ("q" nil :color blue)))

(use-package robot-mode
  :straight t)


(provide 're-robot)
