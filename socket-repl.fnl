(require :reascript-extended-paths)
(local sok (require :socket))
(local bencode (require :bencode))
(local json (require :dkjson))

(global u (require :utils))
(global ru (require :ruteal))

(local {:misc {: log}} ru)

(local r reaper)

(local udp (assert (sok.udp)))
(assert (udp:setsockname "127.0.0.1" 9999))
(local udp-out (assert (sok.udp)))
(assert (udp-out:setpeername "127.0.0.1" 9997))

(fn on-error [err]
  (log (.. "error:\n\n" err))
  (udp-out:send (json.encode {:error err} {})))

(fn main []
  (udp:settimeout 0.0001)
  (local m (udp:receive))
  (if m
      (do (let [{: code : compiled} (bencode.decode m)
                (f err) (load compiled)
                (status ret err) (xpcall f on-error)]
            (log (.. "__________\n\n>> " code "\n"))
            (if status
                (do (log ret)
                    (xpcall (fn [] (udp-out:send (json.encode ret {})))
                            on-error))))))
  (reaper.defer main))

(main)
