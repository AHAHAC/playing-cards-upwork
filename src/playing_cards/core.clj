(ns playing-cards.core
  (:require [clojure.set :as set]
            [clojure-csv.core :as csv]
            [clojure.java.io :as io]))

(with-open [file (io/reader "picked-cards.txt")]
  (-> file
      (slurp)
      (csv/parse-csv)))

(defn unique-random-numbers [n]
  (let [a-set (set (take n (repeatedly #(rand-int n))))]
    ;(print a-set)
    (concat a-set (set/difference (set (take n (range)))
                                  a-set))))

(def suit-long-names {:SP "SPADES" 
                      :HR "HEARTS" 
                      :DI "DIAMONDS" 
                      :CL "CLUBS"})

(defn get-suit-long-name [suit]
  (get suit-long-names suit))

(defn get-suit-short-name [suit]
  (name suit))

(defn card-name [card]
  (case card
    0 "ACE"
    10 "JACK"
    11 "QUEEN"
    12 "KING"
    (str card)))

(defn suit-set [suit]
  (map #(list % suit) (take 13 (range))))

(defn show-card [s]
  (str (card-name (first s)) " of " (get-suit-long-name (second s)))
  )

(defn show-deck [d]
  (into '() (map show-card d)))

;* Create a deck of cards.
(def deck-of-cards
  (mapcat #(suit-set %) (keys suit-long-names)))

(show-deck deck-of-cards)

;* Shuffle the cards.
(show-deck (shuffle deck-of-cards))

(defn pick-cards [n deck]
  (into '() (map #(nth deck %) (take n (unique-random-numbers (count deck))))))

;* Pick 10 cards at random.
(def pick-10 (pick-cards 10 deck-of-cards))
;* Show selected and remaining cards in the deck.
(println "Selected:")
(println (clojure.string/join ", " (show-deck pick-10)))

(defn rest-of-deck [sets deck]
  (concat (set/difference (set deck) (set (concat sets))))
  )

(println "Rest of deck:")
(println (clojure.string/join ", " (show-deck (rest-of-deck pick-10 deck-of-cards) )))
;* Repeat a couple of times

(take 2 (repeatedly #(concat (set/difference (set deck-of-cards) (set (pick-cards 10)))) ))

(defn picked-n-rest [n deck times]
  (if (= times 1)
    (let [picked (pick-cards n deck)]
      {:picked (list picked) :rest (concat (set/difference (set deck) (set picked)))})
    (let [picked (pick-cards n deck)
          sets (picked-n-rest n (rest-of-deck picked deck) (dec times))]
      (print picked)
      {:picked (cons (doall picked) (:picked sets))
       :rest (:rest sets)}
    )
    )
  )

(spit "picked-cards.txt" "" :append false)

(map #(spit "picked-cards.txt" 
            (str 
             (clojure.string/join "," (map (fn [a] (str (first a) " " (name (second a)))) %))
             "\n") :append true) (:picked (doall (picked-n-rest 10 deck-of-cards 3))) )



