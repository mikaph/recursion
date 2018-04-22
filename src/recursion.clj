(ns recursion)

(defn
  product
  [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))


(defn
  singleton?
  [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))


(defn
  my-last
  [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))


(defn
  max-element
  [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))


(defn
  seq-max
  [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))


(defn
  longest-sequence
  [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))



(defn
  my-filter
  [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))


(defn
  sequence-contains?
  [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))


(defn
  my-take-while
  [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ())))


(defn
  my-drop-while
  [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      (into '() (reverse a-seq)))))



(defn
  seq=
  [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (or (empty? a-seq) (empty? b-seq))
      false
      (if (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
        false))))

(defn
  my-map
  [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))


(defn
  power
  [n k]
  (if (zero? k)
    1
    (if (zero? n)
      0
      (* n (power n (- k 1))))))

(defn
  fib
  [n]
  (if (zero? n)
    0
    (if (= 1 n)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))

(defn
  my-repeat
  [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn
  my-range
  [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn
  tails
  [a-seq]
  (let [r-seq (repeat (count a-seq) a-seq)
        amount (inc (count a-seq))]
    (cons '() (map drop (range amount) r-seq))))

(defn
  inits
  [a-seq]
  (let [r-seq (repeat (count a-seq) (reverse a-seq))
        amount (inc (count a-seq))]
    (map reverse (cons '() (map drop (range amount) r-seq)))))

(defn
  rotations
  [a-seq]
  (let [a-list (reverse (into '() a-seq))
        rotate-list (fn [x] (concat (rest x) (list (first x))))]
  (cond
    (empty? a-seq) (list a-list)
    (not (seq? (first a-seq))) (rotations (list a-list))
    (= (first a-seq) (rotate-list (last a-seq))) a-seq
    :else (rotations (concat a-seq (list (rotate-list (last a-seq))))))))


(defn
  my-frequencies-helper
  [freqs a-seq]
  (let [key-vec (vector (first a-seq))]
  (cond
    (empty? a-seq) freqs
    (contains? freqs (first a-seq)) (my-frequencies-helper (update-in freqs key-vec inc) (rest a-seq))
    :else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn
  my-frequencies
  [a-seq]
  (my-frequencies-helper {} a-seq))

(defn
  un-frequencies
  [a-map]
  (let [first-key (first (first a-map))
        first-count (second (first a-map))]
    (if (empty? a-map)
      ()
      (concat (repeat first-count first-key) (un-frequencies (rest a-map))))))


(defn
  my-take
  [n coll]
  (if (or (empty? coll) (zero? n))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))


(defn
  my-drop
  [n coll]
  (cond
    (empty? coll) ()
    (zero? n) (cons (first coll) (my-drop n (rest coll)))
    :else (my-drop (dec n) (rest coll))))



(defn
  halve
  [a-seq]
  (let [half-count (int (/ (count a-seq) 2))]
  (vector (my-take half-count a-seq) (my-drop half-count a-seq))))


(defn
  seq-merge
  [a-seq b-seq]
  (cond
    (and (empty? a-seq)
         (empty? b-seq))
      ()
    (empty? a-seq) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    (empty? b-seq) ((cons (first a-seq) (seq-merge (rest a-seq) b-seq)))
    (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge (rest b-seq) a-seq))))


(defn
  merge-sort
  [a-seq]
  (let [split-seq (halve a-seq)]
  (if (< (count a-seq) 2)
    a-seq
    (seq-merge (merge-sort (first split-seq)) (merge-sort (second split-seq))))))


(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

