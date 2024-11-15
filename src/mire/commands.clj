(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

;; Opposite direction for each direction

(def direction_opposite {
               "north" "south",
               "south" "north",
               "east" "west",
               "west" "east"})

;; Command functions

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (str (:desc @player/*current-room*)
       "\nExits: " (keys @(:exits @player/*current-room*)) "\n"
       (str/join "\n" (map #(str "There is " % " here.\n")
                           @(:items @player/*current-room*)))
       (str/join "\n" (map #(str "Person " % " is here.\n") 
                           (disj @(:inhabitants @player/*current-room*) player/*name*)))))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [direction]
  (dosync
   (let [target-name ((:exits @player/*current-room*) (keyword direction))
         target (@rooms/rooms target-name)]
     (if target
       (do
         (move-between-refs player/*name*
                            (:inhabitants @player/*current-room*)
                            (:inhabitants target))
         (doseq [inhabitant (disj @(:inhabitants @player/*current-room*) player/*name*)]
          (binding [*out* (player/streams inhabitant)]
           (println (str player/*name* " exits room through " direction))
           (println player/prompt)))
         (doseq [inhabitant (disj @(:inhabitants target) player/*name*)]
          (binding [*out* (player/streams inhabitant)]
           (println (str player/*name* " enters room through " (direction_opposite direction)))
           (println player/prompt)))
         (ref-set player/*current-room* target)
         (look))
       "You can't go that way."))))

(defn grab
  "Pick something up."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do (move-between-refs (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
         (str "You picked up the " thing "."))
     (str "There isn't any " thing " here."))))

(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do (move-between-refs (keyword thing)
                            player/*inventory*
                            (:items @player/*current-room*))
         (str "You dropped the " thing "."))
     (str "You're not carrying a " thing "."))))

(defn give
  "Give something from your inventory to someone else."
  [thing_seq person]
  (dosync
  (let [thing (first thing_seq)]
   (if (player/carrying? thing)
     (let [person_name (str/join " " person)]
      (if (some #{person_name} (disj @(:inhabitants @player/*current-room*) player/*name*))
       (do
         (move-between-refs (keyword thing)
                            player/*inventory*
                            (rooms/players_inventories person_name))
         (binding [*out* (player/streams person_name)]
          (println (str player/*name* " gives you: " thing))
          (println player/prompt))
         (str "You gave the " thing " to " player/*name* "."))
       (str person_name " is not here")))

     (str "You're not carrying a " thing ".")))))

(defn inventory
  "See what you've got."
  []
  (str "You are carrying:\n"
       (str/join "\n" (seq @player/*inventory*))))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (@player/*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals @rooms/rooms)))]
      (str item " is in " (:name room))
      (str item " is not in any room."))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                             player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (println (str player/*name* " says: " message))
        (println player/prompt)))
    (str "You said " message)))

(defn whisper
  "Say something out quiet so only one person in the room can hear."
  [words person]
  (let [message (str/join " " words)]
    (let [person_name (str/join " " person)]
     (if (some #{person_name} (disj @(:inhabitants @player/*current-room*) player/*name*))
      (do 
       (binding [*out* (player/streams person_name)]
        (println (str player/*name* " whispers: " message))
        (println player/prompt))
       (str "You said " message " to " person_name))
      (str person_name " is not here")))))

(defn help
  "Show available commands and what they do."
  []
  (str/join "\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

;; Command data

(def commands {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab
               "discard" discard
               "inventory" inventory
               "detect" detect
               "look" look
               "say" say
               "help" help})

(def commands_with_person 
              {"whisper" whisper,
               "give" give})

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (if (some #{"to"} args) 
          (let [[command_args command_person] (split-at (.indexOf args "to") args)]
           (apply (commands_with_person command) command_args (vector (remove #{"to"} command_person)))) 
          (apply (commands command) args)))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "You can't do that!")))
