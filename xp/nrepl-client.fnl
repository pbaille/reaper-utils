(local socket (require :socket))
(local bencode (require :bencode))
(var session nil)

(fn send [conn msg]
  (tset msg :session session)
  (conn:send (bencode.encode msg)))

(fn receive [conn]
  (let [(data _ part) (conn:receive "*a")]
    (when (or data (and part (not= part "")))
      (bencode.decode (or data part)))))

(fn connect [port]
  (let [conn (assert (socket.connect "localhost" port))]
    (conn:settimeout 0.01)
    (send conn {:op :clone})
    (set session (. (receive conn) :new-session))
    conn))

(fn ask [conn code]
  (receive conn)
  (send conn code)
  (. (receive conn) :value))


(comment

 (local fen (require :fennel))
 (local json (require :dkjson))

 (do :get-noon-score

     (local conn (connect 59276))

     (macro clj [...]
       (let [fennel (require :fennel)]
         `(ask conn {:op :eval :code ,(fennel.view (list 'do ...))})))

     (macro noon [...]
       `(let [s# (clj (in-ns 'noon.score)
                      (score->reaper-notes (mk ,...)))
              (s#) (s#:gsub "," "")]
          (fen.eval s#)))

     (comment :tries

              (receive conn)

              (ask conn {:op :eval :code "(do (map inc (range 10)) (+ 1 2))"})

              (let [s (clj (in-ns 'noon.score)
                           (score->reaper-notes (mk)))
                    (s) (s:gsub "," "")]
                (fen.eval s))

              (macrodebug (noon (lin d1 d2)))

              (noon (lin d1 d2))))

 (do :write-score-to-reaper

     (local reaper-socket (socket.udp))

     (reaper-socket:setpeername "127.0.0.1" 9999)

     (macro reaper> [...]
       (let [fen (require :fennel)
             code (fen.view `(do ,...))]
         `(let [(compiled#) (fen.compile-string ,code)]
            (reaper-socket:send (bencode.encode {:code ,code
                                                 :compiled compiled#})))))

     ;; this is not working, fen.eval not available
     (macro nean [...]
       (let [fen (require :fennel)
             score (fen.eval (fen.view `(noon ,...)))]
         `(reaper>
           (global ru (require :ruteal))
           (let [take# (ru.take.get-active)
                 score# ,(fen.view score)]
             (each [i# n# (ipairs score#)]
               (tset n# :take r#)
               (ru.note.insert n#))))))

     (fn nean-fn [score]
       (let [code (.. "(let [r (require :ruteal) take (r.take.get-active) score "
                      (fen.view score) "]"
                      "(each [i n (ipairs score)] (tset n :take take) (r.note.insert n)))")
             (compiled) (fen.compile-string code)]
         (reaper-socket:send (bencode.encode {:code code
                                              :compiled compiled}))))



     (comment :tries

              (let [(compiled) (fen.compile-string "(+ 1 3 8)")]
                (reaper-socket:send (bencode.encode {:code "yo"
                                                     :compiled compiled})))

              (reaper> (+ 1 2 3))
              (reaper> (global ru (require :ruteal))
                       (ru.take.get-active))

              (let [code (fen.view (noon (lin d1 d2)))
                    (compiled) (fen.compile-string code)]
                (reaper-socket:send (bencode.encode {:code code
                                                     :compiled compiled})))

              (nean (cat d1 d2))

              (nean-fn (noon (cat d1 d2)))))





 )
