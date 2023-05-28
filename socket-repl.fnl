(require :reascript-extended-paths)
(local sok (require :socket))
(local bencode (require :bencode))
(local {&as ru
        :misc {: log}} (require :ruteal))
(local r reaper)

(local udp (assert (sok.udp)))
(assert (udp:setsockname "127.0.0.1" 9999))

(fn on-error [err]
  (log (.. "error:\n\n" err)))

(fn main []
  (udp:settimeout 0.0001)
  (local m (udp:receive))
  (if m
      (do (let [{: code : compiled} (bencode.decode m)
                (f err) (load compiled)
                (status ret err) (xpcall f on-error)]
            (log (.. "__________\n\n>> " code "\n"))
            (if status
                (log ret)))))
  (reaper.defer main))

(main)
