;; -*- coding: utf-8; lexical-binding: t -*-
;;; init-project.el --- project managerment config

(require 'project)

(defalias 'project-switch-project-ex #'project-switch-project)

;; (use-package otpp
;;   :vc (:url "https://github.com/abougouffa/one-tab-per-project")
;;   :after project
;;   :init
;;   ;; If you like to define some aliases for better user experience
;;   (defalias 'one-tab-per-project-mode 'otpp-mode)
;;   (defalias 'one-tab-per-project-override-mode 'otpp-override-mode)
;;   ;; Enable `otpp-mode` globally
;;   (otpp-mode 1)
;;   ;; If you want to advice the commands in `otpp-override-commands`
;;   ;; to be run in the current's tab (so, current project's) root directory
;;   (otpp-override-mode 1))

(provide 'init-project)
