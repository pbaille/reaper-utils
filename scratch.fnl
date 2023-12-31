(local fen (require :fennel))
(local {: file
        : path}
       (require :utils))

(path.pwd)
(macro send-reaper [code]
  `(file.spit "/Users/pierrebaille/Library/Application Support/REAPER/Scripts/PB/DYN.lua"
             (fen.compile-string ,(tostring code))))

(macrodebug (send-reaper (print "hello from emacs")))

(set state.a 0)
(set state.quit true)
(print (+ state.a 1))
(set loop nil)
(print :io)
(print state.a)
(reaper.GetResourcePath)
(global iop 2)
(set _G.testglob 3)

(+ 1 2)

(do :insert-a-note
    (global ru (require :ruteal))
    (ru.cursor.position (ru.take.get-active))
    (let [at (ru.cursor.position (ru.take.get-active))]
      (ru.note.insert {:channel 1
                       :start-position at
                       :end-position (+ at 1000)
                       :muted false
                       :selected false
                       :velocity 55
                       :pitch 43
                       :take (ru.take.get-active)})))

(do :insert-a-noon-score
    ())
(do package.loaded)
(set _G.fen (require :fennel))
(set _G.pp (require :pprint))
(pp.format ru)

(local pp (require :pprint))
(pp.setup {:show_all true})
(pp fen)

(ru.misc.pp table)
(+ "a" 1)
(require :pouet)

(local socket (require :socket))
(socket.connect "127.0.0.1:9004")
(: (socket.udp) :sendto "hello" "127.0.0.1" 9004)

(let [x {:a 34}]
  (tset x :b 34)
  x)
