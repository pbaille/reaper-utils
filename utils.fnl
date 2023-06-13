
;; ------------------------------------------------------------
;; misc

(fn reload [p]
  (tset package.loaded p nil)
  (require p))

(fn fold [x f xs]
  (accumulate [x x _ e (ipairs xs)] (f x e)))


;; ------------------------------------------------------------
(local path {})

(fn path.pwd []
  (: (io.popen "pwd") :read))

(set path.home "/Users/pierrebaille")
(set path.user (.. path.home "/Code/Lua"))

(fn path.relative [subpath]
  (.. (path.pwd) "/" subpath))


;; ------------------------------------------------------------
(local file {})

(fn file.slurp [path]
  (match (io.open path)
    f (let [content (f:read :*all)]
        (f:close)
        content)
    (nil err-msg) (print "Could not open file:" err-msg)))

(lambda file.spit [path content]
  (match (io.open path :w)
    f (do (f:write content)
          (f:close))
    (nil err-msg) (print "Could not open file:" err-msg)))



;; ------------------------------------------------------------
(local tbl {})

(fn tbl.matcher [m]
  (case (type m)
    :function m
    :table (fn [t]
             (accumulate [ok true
                          km vm (pairs m)]
               (and ok
                    (case (. t km)
                      vt (case (type vm)
                           :function (vm vt)
                           _ (= vm vt))))))))

(fn tbl.getter
  [at]
  (case (type at)
    :string (fn [this] (?. this at))
    :table (fn [this] (accumulate [this this _ k (ipairs at)]
                        (?. this k)))))

(fn tbl.match [t m]
  ((tbl.matcher m) t))

(fn tbl.get [t p]
  ((tbl.getter p) t))

(fn tbl.upd-at [t k u]
  (case (type u)
    :function (tset t k (u (. t k)))
    _ (tset t k u))
  t)

(fn tbl.upd [t u]
  (case (type u)
    :table (each [k f (pairs u)]
             (tbl.upd-at t k f))
    :function (u t))
  t)

(fn tbl.merge [a b]
  (each [k v (pairs b)]
    (tset a k v))
  a)

(fn tbl.put [t k v]
  (tset t k v)
  t)

(fn tbl.rem [t k]
  (tset t k nil)
  t)

;; ------------------------------------------------------------
(local seq {})

(fn seq.first [s]
  (. s 1))

(fn seq.last [s]
  (. s (length s)))

(fn seq.index-of [s v]
  (var idx nil)
  (each [i x (ipairs s) &until idx]
    (if (= x v)
        (set idx i)))
  idx)

(fn seq.append [s x]
  (table.insert s x)
  s)

(fn seq.concat [s xs]
  (each [_ x (ipairs xs)]
    (seq.append s x))
  s)

(fn seq.keep [s f]
  (icollect [_ x (ipairs s)] (f x)))

(fn seq.filter [s f]
  (seq.keep s (fn [x] (if (f x) x))))

(fn seq.remove [s f]
  (seq.keep s (fn [x] (if (not (f x)) x))))

(fn seq.find [s f]
  (var found nil)
  (each [_ x (ipairs s) &until found]
    (if (f x)
        (set found x)))
  found)

(fn seq.sort [s key-fn compare-fn]
  (if (not key-fn) (table.sort s)
      (not compare-fn) (seq.sort-by s key-fn)
      (table.sort s (fn [a b] (compare-fn (key-fn a) (key-fn b)))))
  s)

(fn seq.sort-with [s f]
  (table.sort s f)
  s)

(fn seq.sort-by [s key-fn]
  (seq.sort-with s (fn [a b] (< (key-fn a) (key-fn b))))
  s)

(fn seq.reverse-sort-by [s key-fn]
  (seq.sort-with s (fn [a b] (> (key-fn a) (key-fn b))))
  s)

;; ------------------------------------------------------------
(local hof {})

(set hof.not #(not $))

(fn hof.k [x] (fn [_] x))
(fn hof.inc [x] (+ 1 x))
(fn hof.dec [x] (- 1 x))
(fn hof.adder [x] (fn [y] (+ x y)))
(fn hof.gt [x] (fn [y] (> y x)))
(fn hof.lt [x] (fn [y] (< y x)))
(fn hof.gte [x] (fn [y] (>= y x)))
(fn hof.lte [x] (fn [y] (<= y x)))


;; ------------------------------------------------------------
:tries

(macro scratch [...])

(scratch [:tries
          (relative-slurp "xp/first.fnl")
          (relative-spit "test.txt" "hello")
          (relative-spit "test.txt" )
          (relative-slurp "test.txt")]

         [:seq-tries
          (let [s [1 2 3 -4 8]]
            (seq.keep s (fn [x] (if (> x 0) (+ 1 x)))))
          (let [s [1 2 3 -4 8]]
            (fold 0 (fn [ret x] (if (> x 0) (+ ret x) ret)) s))
          (seq.first [1 2 3 4])
          (seq.last [1 2 3 4])
          (seq.append [1 2 3] 4)
          (seq.concat [1 2 3] [4 5 6])
          (seq.index-of [1 3 2 5 4 6 2] 2)
          (seq.sort-by [1 2 3 2 4 3] (fn [a] a))
          (seq.sort [1 2 3 4 3 2 5])
          (seq.sort [1 2 3 4 3 2 5] (fn [a] (* -1 a)))
          (seq.sort [1 2 3 4 3 2 5] (fn [a] (* -1 a)) (fn [a b] (> a b)))
          (seq.sort-with [1 2 3 2 4 3] (fn [a b] (> a b)))]

         [:tbl-tries
          (local m (table-matcher {:a 1
                                   :b (fn [x] (= :boolean (type x)))}))

          (tbl.upd {:a 1} {:a (hof.adder 3)})
          (tbl.upd {:a 1 :b 2}
                   {:b 67})

          (tbl.merge {:a 1 :b 2}
                     {:b 67 :c 90})

          (m {:a 1 :b true :x 4})
          (m {:a 1 :b 5})

          ((tbl.getter [:a :b]) {:x {:b 3}})
          ((tbl.getter [:a :b]) {:a {:b 3}})])



;; ------------------------------------------------------------
:export


{: path
 : file
 : tbl
 : hof
 : reascript
 : reload
 : fold
 : seq}
