(local {&as u
        :table tbl
        : seq
        : hof} (require :utils))

(local pp (require :pprint))

(local r reaper)

(local TICKS_PER_QUARTER_NOTE 960)

(local time {:signature {}})

(fn time.ppq->qpos [x]
  (/ x TICKS_PER_QUARTER_NOTE))

(fn time.qpos->ppq [x]
  (* x TICKS_PER_QUARTER_NOTE))

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

(tset midi-editor :pitch-cursor {})

(fn midi-editor.pitch-cursor.get [me]
  (reaper.MIDIEditor_GetSetting_int me "active_note_row"))

(fn midi-editor.pitch-cursor.set [me i]
  (reaper.MIDIEditor_SetSetting_int me "active_note_row" i))

(fn midi-editor.pitch-cursor.update [me delta]
  (midi-editor.pitch-cursor.set me (+ delta (midi-editor.pitch-cursor.get me))))


;; ------------------------------------------------------------
(local note {})

(fn note.default []
  {:channel 1
   :pitch 60
   :velocity 80
   :start-position 0
   :end-position TICKS_PER_QUARTER_NOTE
   :muted false
   :selected true})

(fn note.to-absolute-position [n]
  (let [{: position : duration} n
        start-pos (time.qpos->ppq position)
        end-pos (+ start-pos (time.qpos->ppq duration))]
    (tset n :position nil)
    (tset n :duration nil)
    (tset n :start-position start-pos)
    (tset n :end-position end-pos)
    n))

(fn note.mk [n]
  (if (and n.position n.duration)
      (note.mk (note.to-absolute-position n))
      (tbl.merge (note.default) n)))

(fn note.shift-position [n offset]
  (tbl.upd n {:start-position (hof.adder offset)
              :end-position (hof.adder offset)}))

;; ------------------------------------------------------------
(local take {:grid {}
             :time-selection {}
             :note-selection {}
             :cursor {}
             :focus {}
             :notes {}
             :ccs {}})

(fn take.get-active []
  (midi-editor.get-take (midi-editor.get-active)))

(fn take.mark-dirty [t]
  (r.MarkTrackItemsDirty (r.GetMediaItemTake_Track t)
                         (r.GetMediaItemTake_Item t))
  :ok)

(fn take.sort [t]
  (r.MIDI_Sort t)
  :ok)

;; time

(fn take.project-time->ppq [t x]
  (reaper.MIDI_GetPPQPosFromProjTime t x))

(fn take.ppq->project-time [t x]
  (reaper.MIDI_GetProjTimeFromPPQPos t x))

(fn take.project-time->qpos [t x]
  (time.ppq->qpos (take.project-time->ppq t x)))

(fn take.qpos->project-time [t x]
  (take.ppq->project-time t (time.qpos->ppq x)))

;; grid

(fn take.grid.get [t]
  (reaper.MIDI_GetGrid t))

(fn take.grid.get-ppq [t]
  (time.qpos->ppq (reaper.MIDI_GetGrid t)))

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
    {:start (time.ppq->qpos start)
     :end (time.ppq->qpos end)}))

(fn take.time-selection.set-qpos [t start end]
  (take.time-selection.set t (time.qpos->ppq start) (time.qpos->ppq end)))

(fn take.time-selection.update [t side delta]
  (let [sel (take.time-selection.get t)
        increment (time.qpos->ppq (* delta (take.grid.get t)))]
    (case side
      :fw (take.time-selection.set t sel.start (+ sel.end increment))
      :bw (take.time-selection.set t (+ sel.start increment) sel.end)
      _ (take.time-selection.set t (+ sel.start increment) (+ sel.end increment)))
    :ok))

;; note-selection

(fn take.note-selection.get [t]
  (let [notes (seq.keep (take.notes t) (fn [n] (tbl.rem n :take)))
        selected-notes (seq.filter notes (fn [n] n.selected))
        candidates (if (= 0 (length selected-notes)) notes selected-notes)
        time-selection (take.time-selection.get t)]
    (case time-selection
      {:start start :end end} (seq.filter candidates (fn [n] (<= start n.start-position n.end-position end)))
      _ candidates)))

(fn take.note-selection.delete-all [t]
  (let [notes (take.note-selection.get t)
        idxs (let [idxs (seq.keep notes (fn [n] n.idx))]
               (seq.sort-by idxs (fn [a b] (> a b))))]
    (each [_ i (ipairs idxs)]
      (take.delete-note t i))))

;; cursor

(fn take.cursor.get [t]
  (let [curs-pos (r.GetCursorPosition)]
    (take.project-time->ppq t curs-pos)))

(fn take.cursor.set [t p]
  (reaper.SetEditCurPos (ru.take.ppq->project-time t p)
                        true false))

(fn take.cursor.get-qpos [t]
  (time.ppq->qpos (take.cursor.get t)))

(fn take.cursor.set-qpos [t p]
  (take.cursor.set t (time.qpos->ppq p)))

(fn take.cursor.ceil [t]
  (take.cursor.set t (reaper.BR_GetNextGridDivision (take.cursor.get t))))

(fn take.cursor.floor [t]
  (take.cursor.set t (reaper.BR_GetPrevGridDivision (take.cursor.get t))))

(fn take.cursor.round [t]
  (take.cursor.set t (reaper.BR_GetClosestGridDivision (take.cursor.get t))))

(fn take.cursor.update [t delta]
  (let [increment (time.qpos->ppq (* delta (take.grid.get t)))]
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
  (take.focus.set t {:x n.start-position :y n.pitch}))

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

(fn take.insert-note [t n]
  (let [{: channel
         : end-position
         : muted
         : pitch
         : selected
         : start-position
         : velocity} (note.mk n)
        idx (take.note-count t)]
    (r.MIDI_InsertNote t
                       selected muted
                       start-position end-position
                       channel pitch velocity
                       true)
    (take.get-note t idx)))

(fn take.insert-notes [t xs]
  (each [_ n (ipairs xs)]
    (take.insert-note t n))
  (take.sort))

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
  (each [_ n (ipairs (take.notes.get t))]
    (take.upd-note t n u))
  (take.sort))

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
