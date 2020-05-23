(ns split-text.config)

(def host "192.168.1.109")
(def port "5984")
(def user "admin")
(def passwd "admin")
(def db "texts")



(def server "http://192.168.1.109:5984")
(def directory "c:\\Users\\MartinRoberts\\private_projects\\split-text\\resources\\diglot-html\\")
(def main-directory "C:\\Users\\MartinRoberts\\Dropbox (Personal)\\texts\\files\\11May2020\\html")
(def md-directory "C:\\Users\\MartinRoberts\\Dropbox (Personal)\\texts\\files\\11May2020\\html")
(def modified-book "2019- A2A-11 Epistles only James from modified")
(def book-name "Jamesfrommodified")
(def filename (str directory book-name ".htm"))
(def mod-filename (str main-directory modified-book  ".htm"))

;;; Config for transformation

(def name-highlight  "<span style=\"color:blue;\">$2</span>")

