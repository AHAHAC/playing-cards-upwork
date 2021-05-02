(ns playing-cards.core
  (:require [clojure.set :as set]
            [clojure-csv.core :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            ))

(defn unique-random-numbers [n]
  (let [a-set (set (take n (repeatedly #(rand-int n))))]
    (concat a-set (set/difference (set (take n (range n)))
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
    1 "ACE"
    11 "JACK"
    12 "QUEEN"
    13 "KING"
    (str card)))

(defn suit-set 
  "Takes suit and returns complete set of cards for the given suit"
  [suit]
  (map #(vector % suit) (take 13 (range 1 14))))

(defn show-card [s]
  (str (card-name (first s)) " of " (get-suit-long-name (second s)))
  )

(defn show-deck [d]
  (into [] (map show-card d)))

;-------------------------------------
;*Read the saved results (if exists) from your CSV file.
;*Display previous results.
;-------------------------------------
(println "Read from file:")

(defn parse-card-string [s]
  (into [] (map (fn [c] (let [card (string/split c #" ")]
                        (list (Integer/parseInt (first card)) (keyword (second card)))))
              (string/split s #",")))
  )

(let [cards-from-file (into [] (map #(hash-map :picked (parse-card-string (first %))
                                       :rest (parse-card-string (second %)))
                            (with-open [file (io/reader "picked-cards.txt")]
                              (-> file
                                  (slurp)
                                  (csv/parse-csv)))))]
  (map
   #(pprint/pprint (assoc % :picked (show-deck (:picked %))
                                  :rest (show-deck (:rest %))))
   cards-from-file
   )
  )

;-------------------------------------
;* Create a deck of cards.
;-------------------------------------
(def deck-of-cards
  (mapcat #(suit-set %) (keys suit-long-names)))

(show-deck deck-of-cards)

deck-of-cards

;-------------------------------------
;* Shuffle the cards.
;-------------------------------------
(show-deck (shuffle deck-of-cards))

(defn pick-cards [n deck]
  (into [] 
        (map #(nth deck %) 
             (take n (unique-random-numbers (count deck)))
             )
        )
  )

;-------------------------------------
;* Pick 10 cards at random.
;-------------------------------------
(def pick-10 (pick-cards 10 deck-of-cards))
;-------------------------------------
;* Show selected and remaining cards in the deck.
;-------------------------------------
(println "Selected:")
(println (string/join ", " (show-deck pick-10)))

(defn rest-of-deck [sets deck]
  (concat (set/difference (set deck) (set (concat sets))))
  )

(println "Remaining cards in the deck:")
(println (string/join ", " (show-deck (rest-of-deck pick-10 deck-of-cards) )))

;-------------------------------------
;* Repeat a couple of times
;-------------------------------------
(take 2 (repeatedly #(concat (set/difference (set deck-of-cards) (set (pick-cards 10 deck-of-cards))))))

(defn picked-n-rest [n deck times]
  (if (= times 1)
    (let [picked (pick-cards n deck)]
      {:picked (list picked) :rest (into [] (concat (set/difference (set deck) (set picked))))})
    (let [picked (pick-cards n deck)
          sets (into [] (picked-n-rest n (rest-of-deck picked deck) (dec times)))]
      ;(print picked)
      {:picked (cons picked (:picked sets))
       :rest (:rest sets)}
    )
    )
  )

(defn get-deck-string [deck]
  (string/join "," (map (fn [a] (str (first a) " " (name (second a)))) deck))
  )

;-------------------------------------
;* Dump the results to a database of your choice (in-memory welcome)
;-------------------------------------
(println "Dump the results to a database of your choice (in-memory welcome)")
(let [decks (doall (take 5 (repeatedly  #(picked-n-rest 10 deck-of-cards 1))))
      db-ops (into [] (let [my-datasource (jdbc/get-datasource {:dbtype "sqlite" :dbname ":memory:"
                                          ;"playng-cards.db"
                                                                })]
                        (with-open [connection (jdbc/get-connection my-datasource)]
                          (jdbc/execute-one! connection ["create table if not exists cards(linenr integer primary key, picked text, rest text)"])
                           ; insert all cards: picked and rest of deck
                          (into '() (map #(jdbc/execute-one! connection ["insert into cards(picked, rest) values(?,?)" (get-deck-string (first (:picked %)))  (get-deck-string (:rest %))])
                                         decks))
                           ; let's read a check if what's inserted is the same as generated
                          (map (fn [a b] (if (and (= (:picked a) (get-deck-string (first (:picked b))))
                                                  (= (:rest a) (get-deck-string  (:rest b))))
                                           true
                                           false))
                               (jdbc/execute! connection ["select * from cards"] {:builder-fn rs/as-unqualified-lower-maps})
                               decks))))]
  (print (str "I'll do 5 sets. Results are: " db-ops))
  ;-------------------------------------
  ;* Also dump the same results to a CSV file.
  ;-------------------------------------
  (spit "picked-cards.txt" "" :append false)
  (map #(spit "picked-cards.txt"
              (str
               "\""
               (get-deck-string (first (:picked %)))
               "\","
               "\""
               (get-deck-string (:rest %))
               "\""
               "\n")
              :append true)
       decks))






