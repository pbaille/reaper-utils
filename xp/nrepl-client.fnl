(local socket (require :socket))
(local bencode (require :bencode))
(local fen (require :fennel))
(local json (require :dkjson))

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
  (send conn code)
  (. (receive conn) :value))

(local conn (connect 51822))

(ask conn {:op :eval :code "(map inc (range 10))"})
(json.decode (json.encode {:a 1 :b [1 2 3]}))
