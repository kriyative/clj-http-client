(ns clj-http-client.core
  (:refer-clojure :exclude [get])
  (:require
   [clojure.string :as str]
   [clojure.java.io :as jio])
  (:import
   java.io.ByteArrayOutputStream
   java.io.File
   java.io.InputStream
   java.io.OutputStream
   java.io.OutputStreamWriter
   java.net.URL
   java.net.URLEncoder
   java.net.URLDecoder
   java.net.URLConnection
   java.net.HttpURLConnection))

(defn url-encode [value]
  (let [^String value (if (keyword? value) (name value) (str value))]
    (URLEncoder/encode value)))

(defn url-decode [^String value]
  (URLDecoder/decode value))

(defn encode-params [params]
  (str/join "&"
            (map (fn [[k v]]
                   (str (url-encode k) "=" (url-encode v)))
                 params)))

(defn- get-response-headers [^URLConnection conn]
  (loop [i 0
         headers {}]
    (let [key (.getHeaderFieldKey conn i)
          val (.getHeaderField conn i)]
      (cond
        (and (nil? key) (nil? val)) headers
        (nil? key) (recur (inc i) headers)
        :else (recur (inc i) (assoc headers key val))))))

(defn ^HttpURLConnection make-http-connection
  ([url]
     (make-http-connection url nil))
  ([url {:keys [method timeout headers]}]
     (let [^URL url (jio/as-url url)
           ^HttpURLConnection conn (.openConnection url)
           method (or method "GET")
           timeout (or timeout 10000)]
       (doto conn
         (.setDoOutput true)
         (.setRequestMethod method)
         (.setConnectTimeout timeout)
         (.setReadTimeout timeout))
       (doseq [[k v] headers]
         (.setRequestProperty conn k v))
       conn)))

(defn head [url & [opts]]
  (let [opts (or opts {})
        conn (make-http-connection url (assoc opts :method "HEAD"))]
    {:status (.getResponseCode conn)
     :headers (get-response-headers conn)}))

(defn get [url & [opts]]
  (let [opts (or opts {})
        conn (make-http-connection url (assoc opts :method "GET"))]
    (with-open [in (.getInputStream conn)
                ostr (ByteArrayOutputStream. 1024)]
      (jio/copy in ostr)
      {:status (.getResponseCode conn)
       :headers (get-response-headers conn)
       :body (.toString ostr)})))

(defn post [url & [opts body]]
  (let [opts (or opts {})
        conn (make-http-connection url (assoc opts :method "POST"))
        ^String body (if (map? body) (encode-params body) (str (or body "")))]
    (with-open [out (.getOutputStream conn)
                writer ^Writer (OutputStreamWriter. out)]
      (.write writer body)
      (.flush writer)
      (with-open [in (.getInputStream conn)
                  ostr (ByteArrayOutputStream. 1024)]
        (jio/copy in ostr)
        {:status (.getResponseCode conn)
         :headers (get-response-headers conn)
         :body (.toString ostr)}))))
