(fn id [a] a)

(id 3)

(macro my-max [x y]
  `(let [x# ,x y# ,y]
     (if (< x# y#) y# x#)))

(my-max 3 4)

(fn dog [name]
  {: name
   :bark (fn [d] (.. "woaf! my name is "  d.name ", woaf!"))})

(let [steve (dog "steve")]
  (steve:bark))

(let [x {:a {:b 2}
         :c 4
         :nest {:getval (fn [this {: x : y}] (+ this.w this.v x y))
                :v 2
                :w 3}}]
  (+ x.a.b
     x.c
     (x.nest:getval {:x 1 :y 7})))

(case {:x 4 :y 2}
   {&as x : y} [x y])

(let [c (coroutine.create (fn [x] (+ x (coroutine.yield))))]
  (coroutine.resume c 1)
  (let [(_ x) (coroutine.resume c 4)]
    x))

(let [t [1 2 3 4 5]]
  (table.sort t (fn [a b] (> a b)))
  t)

(% 45 12)

(icollect [i v (ipairs [1 2 3])]
  i)

(local t {:a 1})

(fn assoc [t k v]
  (local old-v (. t k))
  (set old-v v)
  t)

(tset t "a" 3)

(icollect [i o (ipairs [{:a true} {:a false}])]
  (if o.a i))

(match (io.open "/Users/pierrebaille/Code/Lua/xp/first.fnl")
  ;; when io.open succeeds, it will return a file, but if it fails it will
  ;; return nil and an err-msg string describing why
  f (do (print (f:read :*all))
        (f:close))
  (nil err-msg) (print "Could not open file:" err-msg))

(match (io.open "../first.fnl")
  ;; when io.open succeeds, it will return a file, but if it fails it will
  ;; return nil and an err-msg string describing why
  f (do (print (f:read :*all))
        (f:close))
  (nil err-msg) (print "Could not open file:" err-msg))

(local current-dir (: (io.popen "pwd") :read))
(fn relative-path [subpath]
  (.. current-dir "/" subpath))

(fn relative-slurp [subpath]
  (match (io.open (relative-path subpath))
    ;; when io.open succeeds, it will return a file, but if it fails it will
    ;; return nil and an err-msg string describing why
    f (let [content (f:read :*all)]
        (f:close)
        content)
    (nil err-msg) (print "Could not open file:" err-msg)))

(relative-slurp "xp/first.fnl")
