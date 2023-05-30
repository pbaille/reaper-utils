(local {&as utils
        :table tbl
        : hof} (require :utils))

(local pp (require :pprint))

(local r reaper)

;; ------------------------------------------------------------
(local misc {})

(fn misc.log [param]
  (reaper.ShowConsoleMsg (.. (tostring param) "\n")))

(fn misc.pp [param]
  (reaper.ShowConsoleMsg (pp.pformat param)))

(macro misc.undo-block [...]
  `(do (r.Undo_BeginBlock)
       ,...
       (r.Undo_EndBlock (: (select 2 (r.get_action_context)) :match
                             "([^\\/]+)%.%w+") (- 1))))

;; ------------------------------------------------------------
(local cursor {})

(fn cursor.position [take]
  (let [curs-pos (r.GetCursorPosition)]
    (reaper.MIDI_GetPPQPosFromProjTime take curs-pos)))

;; ------------------------------------------------------------
(local take {})


(fn take.get-active []
  (r.MIDIEditor_GetTake (r.MIDIEditor_GetActive)))

(fn take.event-count [take]
  (let [(_ notecnt _ _) (r.MIDI_CountEvts take)] notecnt))

(fn take.get-note [take idx]
  (let [(_ selected muted
           start-position end-position
           channel pitch velocity) (r.MIDI_GetNote take idx)]
    {: channel
     : end-position
     : muted
     : pitch
     : selected
     : start-position
     : velocity
     : take
     : idx}))

(fn take.sort [take]
  (r.MIDI_Sort take))

(fn take.mark-dirty [take]
  (r.MarkTrackItemsDirty (r.GetMediaItemTake_Track take)
                         (r.GetMediaItemTake_Item take)))

(fn take.notes [take]
  (let [cnt (event-count take)]
    (if (> cnt 0)
        (faccumulate [ret [] i 0 (- cnt 1)]
          (table.insert ret (take.get-note take i)))
        [])))

(fn take.select-notes [t matcher]
  (accumulate [ret [] i n (ipairs (take.notes t))]
    (if (tbl.match n matcher)
        (table.insert ret n)
        ret)))

(fn take.upd-notes [t u]
  (each [_ n (ipairs (take.notes t))]
    (note.upd n u))
  (take.sort))

(fn take.upd-selected-notes [t matcher u]
  (each [_ n (ipairs (take.select-notes t matcher))]
    (note.upd n u))
  (take.sort))

(fn take.upd-at-cursor [take f]
  (let [cp (cursor-position take)]
    (take.upd-selected-notes #(= $.start-position cp) f)))

;; ------------------------------------------------------------
(local note {})

(fn note.default []
  {:channel 1
   :pitch 60
   :velocity 80
   :start-position 0
   :end-position 960
   :muted false
   :selected true})

(fn note.mk [n]
  (tbl.merge (note.default) n))

(fn note.sync [{: channel
                : end-position
                : muted
                : pitch
                : selected
                : start-position
                : velocity
                : take
                : idx}]
  (r.MIDI_SetNote take idx
                  selected muted
                  start-position end-position
                  channel pitch velocity
                  true))

(fn note.insert [n]
  (let [{: channel
         : end-position
         : muted
         : pitch
         : selected
         : start-position
         : velocity
         : take}  (note.mk n)]
    (r.MIDI_InsertNote take
                       selected muted
                       start-position end-position
                       channel pitch velocity
                       true)))

(fn note.upd [n t]
  (tbl.upd n t)
  (note.sync n))

(fn note.at-cursor? [n]
  (= n.position (cursor-position n.take)))

(fn note.shift-position [n offset]
  (tbl.upd n {:start-position (hof.adder offset)
              :end-position (hof.adder offset)}))

(fn take.insert-note [t n]
  (let [{: channel
         : end-position
         : muted
         : pitch
         : selected
         : start-position
         : velocity} (note.mk n)
        idx (take.event-count t)]
    (r.MIDI_InsertNote t
                       selected muted
                       start-position end-position
                       channel pitch velocity
                       true)
    (take.get-note t idx)))

;; ------------------------------------------------------------

(comment

 (fn cycle-select-at-cursor []
   (let [cp (cursor-position (active-take))
         notes (filter-notes #(= $.start-position cp))
         note-count (length notes)]
     (table.sort notes (fn [{:pitch p1} {:pitch p2}] (> p1 p2)))
     (let [selected-idxs (icollect [i n (ipairs notes)]
                           (if n.selected i))]
       (each [_ n (ipairs notes)]
         (transform-note n {:selected (fn [_] false)}))
       (each [_ i (ipairs selected-idxs)]
         (transform-note (. notes (+ 1 (% i note-count)))
                         {:selected (fn [_] true)})))))

 (comment pouet)

 (fn channel-up-at-cursor []
   (transform-at-cursor {:channel #(+ 1 $)})))

{: take
 : note
 : misc
 : cursor}
