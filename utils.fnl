


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


;; ------------------------------------------------------------
(local hof {})

(set hof.not #(not $))

(fn hof.getter
  [at]
  (case (type at)
    :string (fn [this] (?. this at))
    :table (fn [this] (accumulate [this this _ k (ipairs at)]
                        (?. this k)))))

((hof.getter [:a :b]) {:x {:b 3}})
((hof.getter [:a :b]) {:a {:b 3}})

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

         (m {:a 1 :b true :x 4})
         (m {:a 1 :b 5}))



;; ------------------------------------------------------------
:export


{: path
 : file
 : table
 : hof
 : reascript
 : reload}
