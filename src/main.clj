(ns main
    (:require [clojure.string :as str]
              [clojure.set :as set]))

(defn flines
    [fname]
    (str/split (slurp fname) #"\n"))

(defn as-ints
    [fname]
    (map #(Integer/parseInt %) (flines fname)))

(defn try-find-match
    [n others]
    (loop [[x & r] others]
        (if (not x)
            nil
            (if (= 2020 (+ x n))
                x
                (recur r)))))

(defn day1-1
    [fname]
    (loop [[x & rs] (as-ints fname)]
        (let [m (try-find-match x rs)]
            (if m
                (* m x)
                (recur rs)))))

;aka how unidiomatic can we make things
(defn day1-2
    [fname]
    (let [nums (as-ints fname)
          triples (for [ai (range (- (count nums) 2))
                        bi (range (inc ai) (dec (count nums)))
                        ci (range (inc bi) (count nums))
                        :let [res [(nth nums ai) (nth nums bi) (nth nums ci)]]]
                    res)]
        (loop [[[a b c] & others] triples]
            (if (= 2020 (+ a b c ))
                (do (println a b c) (* a b c))
                (recur others)))))

(defn day2-1
    [fname]
    (loop [[line & rs] (map #(str/split % #" ") (flines fname))
            num-valid 0]
        (if (not line)
            num-valid
            (let [[valid-range letter-with-colon password] line
                   [min-count max-count] (map #(Integer/parseInt %) (str/split valid-range #"-"))
                   letter (first letter-with-colon)
                   occurences (count (filter #(= % letter) password))
                   valid? (and (>= occurences min-count) (<= occurences max-count))]
             (recur rs (if valid? (inc num-valid) num-valid))))))

(defn xor
    [a b]
    (if a
        (not b)
        b))

(defn day2-2
    [fname]
    (loop [[line & rs] (map #(str/split % #" ") (flines fname))
            num-valid 0]
        (if (not line)
            num-valid
            (let [[valid-range letter-with-colon password] line
                   positions (map #(Integer/parseInt %) (str/split valid-range #"-"))
                   [first-letter second-letter] (map #(get password (dec %)) positions)
                   letter (first letter-with-colon)
                   valid? (xor (= first-letter letter) (= second-letter letter))]
             (recur rs (if valid? (inc num-valid) num-valid))))))

(defn is-tree?
    [[x y] map]
    (= (nth (nth map y) x) \#))

(defn traverse-map
    [map slope]
    (let [width (count (first map))
          height (count map)]
          (loop [point [(first slope) (second slope)]
                 trees 0]
            (if (>= (second point) height)
                trees
                (recur 
                    [(mod (+ (first slope) (first point)) width) (+ (second slope) (second point))]
                    (if (is-tree? point map) (inc trees) trees))))))

(defn day3-1
    [fname]
    (traverse-map (flines fname) [3 1]))

(defn day3-2
    [fname]
    (let [terrain (flines fname)
          slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]]
          (reduce * 1 (map (partial traverse-map terrain) slopes))))

(defn year-in-range?
    [s min-year max-year]
    (let [m (re-find #"^\d\d\d\d$" s)]
        (if m
            (let [year (Integer/parseInt s)]
                (and (>= year min-year) (<= year max-year))))))

(defn valid-hgt?
    [s]
    (let [m (re-find #"(\d+)(cm|in)$" s)]
        (if m
            (let [num (Integer/parseInt (second m))
                  unit (last m)]
                (cond (= "cm" unit)
                      (and (>= num 150) (<= num 193))
                      (= "in" unit)
                      (and (>= num 59) (<= num 76)))))))

(def field-requirements
    { "byr" #(year-in-range? % 1920 2002)
      "iyr" #(year-in-range? % 2010 2020)
      "eyr" #(year-in-range? % 2020 2030)
      "hgt" valid-hgt?
      "hcl" #(re-find #"^#[0-9a-f]{6}$" %)
      "ecl" #(contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %)
      "pid" #(re-find #"^[0-9]{9}$" %)})

(defn includes-required?
    [field-map]
    (every? field-map (keys field-requirements)))

(defn parse-day4
    [fname]
    (let [records (str/split (slurp fname) #"\n\n")
          cleaned (map (comp #(str/split % #"\s") 
                             #(str/replace % #"\n" " "))
                      records)
          field-splitter #(str/split % #":")
          field-pairs (map #(map field-splitter %) cleaned)
          fields (map flatten field-pairs)]
          (map (partial apply hash-map) fields)))

(defn day4-1
    [fname]
    (count (filter includes-required? (parse-day4 fname))))

(defn get-field-validator
    [field]
    (or (get field-requirements field)
        (fn [s] true)))

(defn record-valid?
    [fields]
    (every? (fn [[k v]] ((get-field-validator k) v)) fields))

(defn day4-2
    [fname]
    (let [fields-included (filter includes-required? (parse-day4 fname))
          valid-records (filter record-valid? fields-included)]
          (count valid-records)))

(def top-half)
(def bot-half)
(def instrs
    { \F 'bot-half
      \B 'top-half
      \L 'bot-half
      \R 'top-half })

(defn b-search-str
    [pass-s min max]
    (loop [[instr-str & other-instrs] pass-s min min max max]
        (if (nil? instr-str)
            (if (= min max)
                min
                (throw (Exception. (str "min and max should have been the same but were " min " and " max))))
            (let [instr (get instrs instr-str)
                  new-min (if (= 'bot-half instr) min (int (Math/ceil (/ (+ min max) 2))))
                  new-max (if (= 'top-half instr) max (int (Math/floor (/ (+ min max) 2))))]
                  (recur other-instrs new-min new-max)))))

(defn seat-id
    [pass-s]
    (let [row-s (take 7 pass-s)
          row (b-search-str row-s 0 127)
          col-s (drop 7 pass-s)
          col (b-search-str col-s 0 7)]
          (+ col (* row 8))))

(defn day5-1
    [fname]
    (apply max (map seat-id (flines fname))))

(defn day5-2
    [fname]
    (loop [[x & xs] (sort (map seat-id (flines fname)))]
        (if (not (= (first xs) (+ x 1)))
            (+ x 1)
            (recur xs))))

(defn answered
    [group]
    (map set group))

(defn day6
    [fname set-op]
    (let [group-strs (str/split (slurp fname) #"\n\n")
          groups (map #(str/split % #"\n") group-strs)
          group-sets (map #(map set %) groups)
          totals (map #(apply set-op %) group-sets)]
          (apply + (map count totals))))

(defn day6-1
    [fname]
    (day6 fname set/union))

(defn day6-2
    [fname]
    (day6 fname set/intersection))

(defn contained-info
    [bag-str]
    (let [re-res (re-find #"([0-9]+) ([a-z]+ [a-z]+) bag" bag-str)]
        (and 
            re-res 
            [(nth re-res 2) 
             (Integer/parseInt (second re-res))])))

(defn day7-bag-content
    [content]
    (let [contained-strs (str/split content #"," )
          contained-pairs (map contained-info contained-strs)
          not-nil (filter #(not (nil? %)) contained-pairs)]
          (into {} not-nil)))
        
(defn day7-parse-line
    [line]
    (let [[bag content] (str/split line #" bags contain ")
          content-map (day7-bag-content content)]
          [bag content-map]))

(defn day7-parse
    [lines]
    (into {} (map day7-parse-line lines)))

(defn str-in-vec
    [s v]
    (some (partial = s) v))

(defn colour-reachable
    [target cur-type contain-map]
    (if (nil? cur-type)
        false
        (let [types-contained (get contain-map cur-type)]
            (if (str-in-vec target types-contained)
                true
                (some #(colour-reachable target % contain-map) types-contained)))))

(defn day7-1
    [fname]
    (let [full-map (day7-parse (flines fname))
          map-unpacker (fn [[k v]] [k (keys v)])
          contain-map (into {} (map map-unpacker full-map))
          search-from-col #(colour-reachable "shiny gold" % contain-map)]
        (count (filter search-from-col (keys contain-map)))))

(defn bags-contained
    ([col contain-map] (bags-contained col contain-map 1))
    ([col contain-map of-this]
    (let [contained (get contain-map col)]
        (if (empty? contained)
            of-this
            (+ of-this (* of-this (apply + (map
                (fn
                 [[next-col num]]
                 (bags-contained next-col contain-map num))
                contained))))))))

(defn day7-2
    [fname]
    (let [contain-map (day7-parse (flines fname))]
        (- (bags-contained "shiny gold" contain-map) 1)))

(defn run
    [opts & args]
    (println (day7-2 "inputs/day7")))
