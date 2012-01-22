(ns clj-ipaddr
  (:use [clojure.string :only (join split)]))

(defn- to-ip [ip-int]
  (join "." (map (fn [shift]
                    (str 
                      (bit-and (bit-shift-right ip-int shift) 255)))
                 (range 24 -1 -8))))
(defn- to-int [ip]
  (reduce + 0 (map
                (fn [num shift] 
                  (bit-shift-left (Integer/parseInt num) (* 8 shift) ))
                (split ip #"\.") (iterate dec 3))))
  
(defn ip-range 
  "Return a lazy seq off all possible IP adresses in the provided subnet"
  [ip mask]
  (let [ip-int (to-int ip)
        mask-int (to-int mask)
        first (bit-and ip-int mask-int)
        range-size (ffirst 
                     ( drop-while #(not (bit-test mask-int (second %))) 
                                  (iterate (fn [[size i]] [(* 2 size) (inc i)]) [1 0])))
        ]
    (take range-size (map to-ip (iterate inc first)))))
