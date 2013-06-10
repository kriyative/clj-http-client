(ns clj-http-client.test.core
  (:use clojure.test)
  (:require
   [clj-http-client.core :as http]))

(deftest codec
  (= "%2Ftest%3Fname%3D%22abc%22%26var%3D123"
     (http/url-encode "/test?name=\"abc\"&var=123"))

  (= "/test?name=\"abc\"&var=123"
     (http/url-decode "%2Ftest%3Fname%3D%22abc%22%26var%3D123")))


(deftest encode-params
  (is (= "var=123&name=abc%2Bxyz"
         (http/encode-params {"name" "abc+xyz" "var" 123}))))

(deftest head
  (is (= 200
         (-> (http/head "http://www.google.com")
           (:status))))

  (is (thrown? java.net.SocketTimeoutException
               (http/head "http://www.google.com" {:timeout 10}))))

(deftest test-methods
  (is (= 200
         (-> (http/get "http://www.google.com")
           (:status))))
  (is (= 200
         (-> (http/post "http://duckduckgo.com" {} "q=clojure+rocks")
           :status))))
