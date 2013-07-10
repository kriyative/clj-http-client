(ns clj-http-client.core
  (:refer-clojure :exclude [get])
  (:require
   [clojure.string :as str]
   [clojure.java.io :as jio])
  (:import
   (java.io ByteArrayOutputStream File InputStream OutputStream OutputStreamWriter
            IOException)
   (java.net URL URLEncoder URLDecoder URLConnection HttpURLConnection)))

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

(defn http-do-method [method url & [opts]]
  (let [opts (or opts {})
        conn (make-http-connection url (assoc opts :method method))]
    {:status (.getResponseCode conn)
     :headers (get-response-headers conn)
     :body (try
             (slurp (.getInputStream conn))
             (catch IOException ex
               (slurp (.getErrorStream conn))))}))

(defn get [url & [opts]]
  (http-do-method "GET" url opts))

(defn write-request-body [^URLConnection conn ^String body]
  (with-open [out (.getOutputStream conn)
              writer ^Writer (OutputStreamWriter. out)]
    (.write writer body)
    (.flush writer)))

(defn http-do-method-body [method url & [opts body]]
  (let [opts (or opts {})
        conn (make-http-connection url (assoc opts :method method))
        ^String body (if (map? body) (encode-params body) (str (or body "")))]
    (write-request-body conn body)
    {:status (.getResponseCode conn)
     :headers (get-response-headers conn)
     :body (try
             (slurp (.getInputStream conn))
             (catch IOException ex
               (slurp (.getErrorStream conn))))}))

(defn post [url & [opts body]]
  (http-do-method-body "POST" url opts body))

(defn delete [url & [opts]]
  (http-do-method "DELETE" url opts))

(defn put [url & [opts body]]
  (http-do-method-body "PUT" url opts body))
