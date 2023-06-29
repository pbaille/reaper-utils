(local {&as u
        : tbl
        : seq
        : hof} (require :utils))

(local pp (require :pprint))

(local r reaper)

(local time {:signature {}})

(fn time.signature.get []
  (let [(bpm bpi) (reaper.GetProjectTimeSignature2 0)]
    {: bpm : bpi}))

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
(local midi-editor {:pitch-cursor {}})

(fn midi-editor.get-active []
  (r.MIDIEditor_GetActive))

(fn midi-editor.get-take [me]
  (r.MIDIEditor_GetTake me))

(fn midi-editor.pitch-cursor.get [me]
  (reaper.MIDIEditor_GetSetting_int me "active_note_row"))

(fn midi-editor.pitch-cursor.set [me i]
  (reaper.MIDIEditor_SetSetting_int me "active_note_row" i))

(fn midi-editor.pitch-cursor.update [me delta]
  (midi-editor.pitch-cursor.set me (+ delta (midi-editor.pitch-cursor.get me))))


;; ------------------------------------------------------------
(local note {})

(fn note.shift-position [n offset]
  (tbl.upd n {:start-position (hof.adder offset)
              :end-position (hof.adder offset)}))

(fn note.lte [a b]
  (if (= a.start-position b.start-position)
      (< a.pitch b.pitch)
      (< a.start-position b.start-position)))

(fn note.gte [a b]
  (if (= a.start-position b.start-position)
      (> a.pitch b.pitch)
      (> a.start-position b.start-position)))

;; ------------------------------------------------------------
(local take {:grid {}
             :time-selection {}
             :note-selection {}
             :cursor {}
             :focus {}
             :note {}
             :notes {}
             :ccs {}})

(fn take.get-active []
  (midi-editor.get-take (midi-editor.get-active)))

(fn take.get-ppq [t]
  (math.floor (r.MIDI_GetPPQPosFromProjQN t 1)))

(fn take.ppq->qpos [t x]
  (/ x (take.get-ppq t)))

(fn take.qpos->ppq [x]
  (* x (take.get-ppq t)))


(fn take.mark-dirty [t]
  (r.MarkTrackItemsDirty (r.GetMediaItemTake_Track t)
                         (r.GetMediaItemTake_Item t))
  :ok)

(fn take.sort [t]
  (r.MIDI_Sort t)
  :ok)

(fn take.disable-sort [t]
  (r.MIDI_DisableSort t)
  :ok)

;; note

(fn take.note.default [t]
  {:channel 1
   :pitch 60
   :velocity 80
   :start-position 0
   :end-position (take.get-ppq t)
   :muted false
   :selected true})

(fn take.note.to-absolute-position [t n]
  (let [{: position : duration} n
        start-pos (take.qpos->ppq t position)
        end-pos (+ start-pos (take.qpos->ppq t duration))]
    (tset n :position nil)
    (tset n :duration nil)
    (tset n :start-position start-pos)
    (tset n :end-position end-pos)
    n))

(fn take.note.mk [t n]
  (if (and n.position n.duration)
      (take.note.mk t (take.note.to-absolute-position t n))
      (tbl.merge (take.note.default t) n)))

;; time

(fn take.project-time->ppq [t x]
  (reaper.MIDI_GetPPQPosFromProjTime t x))

(fn take.ppq->project-time [t x]
  (reaper.MIDI_GetProjTimeFromPPQPos t x))

(fn take.project-time->qpos [t x]
  (take.ppq->qpos t (take.project-time->ppq t x)))

(fn take.qpos->project-time [t x]
  (take.ppq->project-time t (take.qpos->ppq t x)))

;; grid

(fn take.grid.get [t]
  (reaper.MIDI_GetGrid t))

(fn take.grid.get-ppq [t]
  (take.qpos->ppq t (reaper.MIDI_GetGrid t)))

(fn take.grid.set [t x]
  (let [sig (time.signature.get)]
    (reaper.SetMIDIEditorGrid 0 (/ x sig.bpi))))

;; time-selection

(fn take.time-selection.get [t]
  (let [(start end) (reaper.GetSet_LoopTimeRange false false 0 0 false)]
    (if (not (= start end))
        {:start (take.project-time->ppq t start)
         :end (take.project-time->ppq t end)})))

(fn take.time-selection.set [t start end]
  (let [start (take.ppq->project-time t start)
        end (take.ppq->project-time t end)]
    (reaper.GetSet_LoopTimeRange true false start end false)))

(fn take.time-selection.get-qpos [t]
  (let [{: start : end} (take.time-selection.get t)]
    {:start (take.ppq->qpos t start)
     :end (take.ppq->qpos t end)}))

(fn take.time-selection.set-qpos [t start end]
  (take.time-selection.set t (take.qpos->ppq t start) (take.qpos->ppq t end)))

(fn take.time-selection.update [t side delta]
  (let [cursor-pos (take.cursor.get t)
        sel (or (take.time-selection.get t)
                {:start cursor-pos :end cursor-pos})
        increment (take.qpos->ppq t (* delta (take.grid.get t)))]
    (case side
      :fw (take.time-selection.set t sel.start (+ sel.end increment))
      :bw (take.time-selection.set t (+ sel.start increment) sel.end)
      _ (take.time-selection.set t (+ sel.start increment) (+ sel.end increment)))
    :ok))

;; note-selection

(fn take.note-selection.get [t]
  (let [notes (take.notes.get t)
        selected-notes (seq.filter notes (fn [n] n.selected))
        candidates (if (= 0 (length selected-notes)) notes selected-notes)
        time-selection (take.time-selection.get t)]
    (case time-selection
      {:start start :end end} (seq.filter candidates (fn [n] (<= start n.start-position n.end-position end)))
      _ candidates)))

(fn take.note-selection.delete-all [t]
  (take.delete-notes t (take.note-selection.get t)))

;; cursor

(fn take.cursor.get [t]
  (let [curs-pos (r.GetCursorPosition)]
    (take.project-time->ppq t curs-pos)))

(fn take.cursor.set [t p]
  (reaper.SetEditCurPos (ru.take.ppq->project-time t p)
                        true false))

(fn take.cursor.get-qpos [t]
  (take.ppq->qpos t (take.cursor.get t)))

(fn take.cursor.set-qpos [t p]
  (take.cursor.set t (take.qpos->ppq t p)))

(fn take.cursor.ceil [t]
  (take.cursor.set t (reaper.BR_GetNextGridDivision (take.cursor.get t))))

(fn take.cursor.floor [t]
  (take.cursor.set t (reaper.BR_GetPrevGridDivision (take.cursor.get t))))

(fn take.cursor.round [t]
  (take.cursor.set t (reaper.BR_GetClosestGridDivision (take.cursor.get t))))

(fn take.cursor.update [t delta]
  (let [increment (take.qpos->ppq t (* delta (take.grid.get t)))]
    (take.cursor.set t (+ (take.cursor.get t) increment))))

;; focus

(fn take.focus.get [t]
  {:x (take.cursor.get t)
   :y (midi-editor.pitch-cursor.get (midi-editor.get-active))})

(fn take.focus.set [t upd]
  (let [{: x : y &as new-focus} (tbl.upd (take.focus.get t) upd)]
    (take.cursor.set t x)
    (midi-editor.pitch-cursor.set (midi-editor.get-active) y)
    new-focus))

(fn take.focus.next-note [t]
  (let [{: x : y} (take.focus.get t)
        candidates (take.notes.filter t (fn [n] (or (> n.start-position x)
                                                   (and (= n.start-position x)
                                                        (> n.pitch y)))))]
    (take.focus-note t
     (seq.first (seq.sort-with candidates note.lte)))))

(fn take.focus.previous-note [t]
  (let [{: x : y} (take.focus.get t)
        candidates (take.notes.filter t (fn [n] (or (< n.start-position x)
                                                   (and (= n.start-position x)
                                                        (< n.pitch y)))))]
    (take.focus-note t
     (seq.first (seq.sort-with candidates note.gte)))))

(fn take.focus.get-closest-note [t]
  (let [{: x : y} (take.focus.get t)
        notes (take.notes.get t)
        by-dist (-> notes
                    (seq.keep (fn [n] {:delta-x (math.abs (- n.start-position x))
                                       :delta-y (math.abs (- n.pitch y))
                                       :note n}))
                    (seq.sort-by (fn [{: delta-x : delta-y}] (+ delta-x delta-y))))]
    (?. (seq.first by-dist)
        :note)))

(fn take.focus.closest-note [t]
  (take.focus-note t (take.focus.get-closest-note t)))

(fn take.focus.cycle-at-cursor [t]
  (let [{: x : y} (take.focus.get t)
        candidates (take.notes.filter t (fn [n] (= n.start-position x)))
        n-choices (length candidates)
        current-idx (seq.index-of candidates y)]
    (if (not current-idx)
        (take.focus.set t {: x :y (seq.first candidates)})
        (> n-choices 1)
        (take.focus.set t {: x :y (. candidates
                                     (if (= current-idx n-choices) 1 (+ 1 current-idx)))}))))

;; note

(fn take.get-note [t idx]
  (let [(_ selected muted
           start-position end-position
           channel pitch velocity) (r.MIDI_GetNote t idx)]
    {: channel
     : end-position
     : muted
     : pitch
     : selected
     : start-position
     : velocity
     : idx}))

(fn take.focus-note [t n]
  (if n
      (take.focus.set t {:x n.start-position :y n.pitch})))

(fn take.focused-note [t]
  (let [{: x : y} (take.focus.get t)]
    (seq.find (take.notes.get t)
              (fn [n] (and (= x n.start-position)
                           (= y n.pitch))))))

(fn take.set-note [t {: channel
                      : end-position
                      : muted
                      : pitch
                      : selected
                      : start-position
                      : velocity
                      : idx}]
  (r.MIDI_SetNote t idx
                  selected muted
                  start-position end-position
                  channel pitch velocity
                  true))

(fn take.upd-note [t n u]
  (take.set-note t (tbl.upd n u)))

(fn take.delete-note [t idx]
  (r.MIDI_DeleteNote t idx))

(fn take.delete-notes [t xs]
  (let [idxs (let [idxs (seq.keep xs (fn [n] n.idx))]
               (seq.sort-with idxs (fn [a b] (> a b))))]
    (take.disable-sort t)
    (each [_ i (ipairs idxs)]
      (take.delete-note t i))
    (take.sort t)))

(fn take.insert-note [t n]
  (let [{: channel
         : end-position
         : muted
         : pitch
         : selected
         : start-position
         : velocity} (take.note.mk t n)
        idx (take.notes.count t)]
    (r.MIDI_InsertNote t
                       selected muted
                       start-position end-position
                       channel pitch velocity
                       true)
    (take.get-note t idx)))

(fn take.insert-notes [t xs]
  (take.disable-sort t)
  (each [_ n (ipairs xs)]
    (take.insert-note t n))
  (take.sort t))

;; notes

(fn take.notes.count [t]
  (let [(_ notecnt _ _) (r.MIDI_CountEvts t)] notecnt))

(fn take.notes.get [t]
  (let [cnt (take.notes.count t)]
    (if (> cnt 0)
        (faccumulate [ret [] i 0 (- cnt 1)]
          (seq.append ret (take.get-note t i)))
        [])))

(fn take.notes.clear [t]
  (let [cnt (take.notes.count t)]
    (if (> cnt 0)
        (for [i (- cnt 1) 0 -1]
          (take.delete-note t i)))))

(fn take.notes.upd [t u]
  (take.disable-sort t)
  (each [_ n (ipairs (take.notes.get t))]
    (take.upd-note t n u))
  (take.sort t))

(fn take.notes.filter [t matcher]
  (seq.filter (take.notes.get t)
              (tbl.matcher matcher)))

(fn take.notes.filtered-upd [t matcher u]
  (each [_ n (ipairs (take.notes.filter t matcher))]
    (take.upd-note t n u))
  (take.sort))

;; CCs

(fn take.ccs.count [t]
  (let [(_ _ cc-cnt _) (r.MIDI_CountEvts t)] cc-cnt))


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
 : midi-editor
 : time}
