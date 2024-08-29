;; -*- coding: utf-8; lexical-binding: t -*-

;; config rss feed reader


(use-package elfeed
  :vc (:fetcher github :repo skeeto/elfeed)
  :custom
  (elfeed-feeds '("https://www.mdpi.com/rss/journal/algorithms"
                  "https://web.dev/static/blog/feed.xml"
                  "https://tympanus.net/codrops/feed/"
                  "https://planet.emacslife.com/atom.xml"
                  "https://hackernoon.com/feed"
                  "https://blog.rust-lang.org/feed.xml"
                  "https://latesthackingnews.com/feed/"
                  "https://www.welivesecurity.com/en/rss/feed/"
                  "https://stackoverflow.com/feeds/tag/algorithm"
                  "https://insights.sei.cmu.edu/blog/feeds/topic/software-architecture/atom/"
                  "http://feeds.dzone.com/performance"
                  "http://feeds.dzone.com/big-data"
                  "http://feeds.dzone.com/devops"
                  "http://feeds.dzone.com/publications")))

(use-package elfeed-goodies
  :vc (:fetcher github :repo jeetelongname/elfeed-goodies)
  :config
  (elfeed-goodies/setup))



(provide 'init-feed)
