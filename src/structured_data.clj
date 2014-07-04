(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(do-a-thing 2)



(defn spiff [v]
  (cond
   (< (count v) 3) nil
   :else (+ (get v 0) (get v 2))))

(spiff [1 2 3])



(defn cutify [v]
  (conj v "<3"))

(cutify [1 2])



(defn spiff-destructuring [v]
  (let [[first second third] v]
    (+ first third)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(width [[1 3] [4 5]])

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))


(defn title-length [book]
  (let [title (:title book)]
    (count title)))


(defn author-count [book]
  (let [authors (:authors book)]
    (count authors)))

(defn multiple-authors? [book]
  (cond
   (> (author-count book) 1) true
   :else false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))


(defn alive? [author]
  (cond
  (:death-year author) false
  :else true))


(defn element-lengths [collection]
  (map count collection))

(element-lengths [[1 2] [1]])

(get [1 2] 3)

(defn second-elements [collection]
  (let [second (fn [coll] (get coll 1))]
    (map second collection)))

(second-elements [[1 2 3] [1 2]])

(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (or
   (apply >= a-seq)
   (apply <= a-seq)))

(monotonic? [3 1 0 4])

(defn stars [n]
  (apply str (repeat n \*)))

(stars 4)

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(toggle #{:a :b :c} :d) ;=> #{:a :c :b :d}
(toggle #{:a :b :c} :a) ;=> #{:c :b}

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq))))

(contains-duplicates? [1 2 5 5])

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))


(defn has-author? [book author]
  (let [new-book (old-book->new-book book)
        authors (:authors new-book)]
    (contains? authors author)
    ))


(defn authors [books]
  (let [new-books (map old-book->new-book books)
        all-authors (map :authors new-books)]
    (apply clojure.set/union all-authors)))

       ;=> #{china, friedman, felleisen}

(defn all-author-names [books]
  (set (map :name (authors books))))



(map (fn [x] (+ x 1)) #{1 2 3})

(str "a "  "b " 1 nil)

(defn timeline [author]
  (let [birth-year (str (:birth-year author))
        death-year (str (:death-year author))]
    (cond
     (clojure.string/blank? birth-year) ""
     :else (str " (" birth-year " - " death-year ")")))
  )

(defn author->string [author]
  (let [name (:name author)]
    (str name (timeline author))))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (let [name (:title book)
        authors-string (authors->string (:authors book))]
    (str name ", written by " authors-string)))


(defn books->string [books]
  (let [num-books (count books)]
    (cond
     (== num-books 0) "No books."
     :else (str num-books
                " book" (if (> num-books 1) "s" nil) ". "
                (apply str (interpose ". " (map book->string books)))
                "."))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author))
          books))


(defn author-by-name [name authors]
  (let [author (filter (fn [author] (= name (:name author))) authors)]
    (cond
     (empty? author) nil
     :else (get (into-array author) 0))))


(defn living-authors [authors]
  (filter alive? authors))


(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
    false
    true))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


; %________%
