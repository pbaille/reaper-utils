(require :reascript-extended-paths)
(local sok (require :socket))
(local bencode (require :bencode))
(local json (require :dkjson))

(global u (require :utils))
(global ru (require :ruteal))

(local {:misc {: log}} ru)

(local r reaper)

(local udp (assert (sok.udp)))
(udp:setoption "reuseaddr" true)
(assert (udp:setsockname "127.0.0.1" 9999))
(local udp-out (assert (sok.udp)))
(assert (udp-out:setpeername "127.0.0.1" 9997))

(fn log-as-error [err]
  (log (.. "error:\n\n" (or err "nil"))))

(fn send-back-as-error [e]
  (udp-out:send (json.encode {:error e} {})))

(fn main []
  (udp:settimeout 0.0001)
  (local m (udp:receive))
  (if m
      (let [{: code : compiled : no-return} (bencode.decode m)
            (f err) (load compiled)
            (success ret) (pcall f)]
        (log (.. "__________\n\n>> " code "\n"))
        (if success
            (do (log ret)
                (or no-return
                    (xpcall (fn [] (udp-out:send (json.encode ret {})))
                            send-back-as-error)))
            (do (log-as-error ret)
                (or no-return
                    (send-back-as-error ret))))))
  (reaper.defer main))

(main)
