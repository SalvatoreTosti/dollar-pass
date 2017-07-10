(ns dollar-pass.core
  (:gen-class))

(defn rand-nth-keys
  [coll]
  (rand-nth (keys coll)))

(defn rand-nth-values
  [coll]
  (coll (rand-nth-keys coll)))

(defn non-zero-dec
  [curr-key mp]
  (if (not (pos? (curr-key mp)))
    mp
    (update-in mp [curr-key] dec)))

(defn key-step
  [curr-key mp]
  (let [rand-key (rand-nth-keys mp)]
    (if (= rand-key curr-key)
      (key-step curr-key mp)
      (-> (non-zero-dec curr-key mp)
          (update-in [rand-key] inc)))))

(defn single-step-rec
  [key-list mp]
  (if (empty? key-list) mp
      (->> (key-step (first key-list) mp)
           (single-step-rec (rest key-list)))))

(defn single-step
  [mp]
  (single-step-rec (keys mp) mp))

(defn step
  [number coll]
  (loop [n number cl coll]
  (if
    (not (pos? n)) cl
    (recur (dec n) (single-step cl)))))

(defn starting-players
  [number dollars]
  (zipmap (map keyword
               (map str (range 1 number)))
          (take number
                (repeat dollars))))

(defn play
  [player-count starting-amount steps]
  (->>
   (starting-players player-count starting-amount)
   (step steps )))

;(play 50 45 5000)
