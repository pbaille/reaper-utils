


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
(local table {})

(fn table.matcher [m]
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

(fn table.match [t m]
  ((table.matcher m) t))

(fn table.upd-at [t k u]
  (case (type u)
    :function (tset t k (u (. t k)))
    _ (tset t k u))
  t)

(fn table.upd [t u]
  (case (type u)
    :table (each [k f (pairs u)]
             (table.upd-at t k f))
    :function (u t))
  t)

(fn table.merge [a b]
  (each [k v (pairs b)]
    (tset a k v))
  a)

(fn table.put [t k v]
  (tset t k v)
  t)

(fn table.rem [t k]
  (tset t k)
  t)


;; ------------------------------------------------------------
;; bread and butter

(fn fold [x f xs]
  (accumulate [x x _ e (ipairs xs)] (f x e)))

(local seq {})

(fn seq.filter [s f]
  (keep s (fn [x] (if (f x) x))))

(fn seq.remove [s f]
  (keep s (fn [x] (if (not (f x)) x))))

(fn seq.keep [s f]
  (icollect [_ x (ipairs s)] (f x)))

(fn seq.sort-by [s f]
  (table.sort s f)
  s)

(fn seq.append [s x]
  (table.insert s x)
  s)

(fn seq.concat [s xs]
  (each [_ x (ipairs xs)]
    (seq.append s x))
  s)

(comment :seq-tries
         (let [s [1 2 3 -4 8]]
           (seq.keep s (fn [x] (if (> x 0) (+ 1 x)))))
         (let [s [1 2 3 -4 8]]
           (fold 0 (fn [ret x] (if (> x 0) (+ ret x) ret)) s)))

;; ------------------------------------------------------------
(local hof {})

(set hof.not #(not $))

(fn hof.getter
  [at]
  (case (type at)
    :string (fn [this] (?. this at))
    :table (fn [this] (accumulate [this this _ k (ipairs at)]
                        (?. this k)))))

(comment
 ((hof.getter [:a :b]) {:x {:b 3}})
 ((hof.getter [:a :b]) {:a {:b 3}}))

(fn hof.k [x] (fn [_] x))
(fn hof.inc [x] (+ 1 x))
(fn hof.dec [x] (- 1 x))
(fn hof.adder [x] (fn [y] (+ x y)))
(fn hof.gt [x] (fn [y] (> y x)))
(fn hof.lt [x] (fn [y] (< y x)))
(fn hof.gte [x] (fn [y] (>= y x)))
(fn hof.lte [x] (fn [y] (<= y x)))

;; ------------------------------------------------------------
(local reascript {})

(fn reascript.emit-action [f]
  (file.spit (.. path.user "/reascripts/pb_" f ".fnl")
             (.. "(local ru (require :ruteal)) " "(ru." f ")")))



;; ------------------------------------------------------------
:tries

(fn reload [p]
  (tset package.loaded p nil)
  (require p))

(comment :tries
         (relative-slurp "xp/first.fnl")
         (relative-spit "test.txt" "hello")
         (relative-spit "test.txt" )
         (relative-slurp "test.txt")

         (emit-reascript :channel-up-at-cursor)

         (local m (table-matcher {:a 1
                                  :b (fn [x] (= :boolean (type x)))}))

         (table.upd {:a 1} {:a (hof.adder 3)})
         (table.upd {:a 1 :b 2}
                    {:b 67})

         (table.merge {:a 1 :b 2}
                      {:b 67 :c 90})

         (m {:a 1 :b true :x 4})
         (m {:a 1 :b 5}))



;; ------------------------------------------------------------
:export


{: path
 : file
 : table
 : hof
 : reascript
 : reload
 : fold
 : seq}
