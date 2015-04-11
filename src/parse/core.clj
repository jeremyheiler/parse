(ns parse.core
  (:require [clojure.core :as c])
  (:refer-clojure :exclude [char concat derive empty or repeat seq]))

(defprotocol Grammar
  (derive
    [g c])
  (nullable?
    [g]))

(def empty
  "A singleton representing the empty set."
  (with-meta
    (reify Grammar
      (derive
        [g c]
        (throw (Exception. "cannot drive the empty set")))
      (nullable?
        [g]
        false))
    {:type ::empty}))

(defmethod print-method ::empty
  [g w]
  (.write w "empty"))

(def epsilon
  "A singleton representing the set containing nil."
  (with-meta
    (reify Grammar
      (derive
        [g c]
        empty)
      (nullable?
        [g]
        true))
    {:type ::epsilon}))

(defmethod print-method ::epsilon
  [g w]
  (.write w "epsilon"))

(deftype Char [x]
  Grammar
  (derive
    [g c]
    (if (= c x) epsilon empty))
  (nullable?
    [g]
    false))

(defmethod print-method Char
  [g w]
  (.write w (str "(char \\" (.x g) ")")))

(declare union concat)

(deftype Union [p q]
  Grammar
  (derive
    [g c]
    (cond
      (and (= empty p) (= empty q)) empty
      (= empty p) (derive q c)
      (= empty q) (derive p c)
      :else (concat (derive p c) (derive q c))))
  (nullable?
    [g]
    ;; todo: check for a fixpoint
    (c/or (nullable? p) (nullable? q))))

(defmethod print-method Union
  [g w]
  (.write w "(union ")
  (print-method (.p g) w)
  (.write w " ")
  (print-method (.q g) w)
  (.write w ")"))

(deftype Concat [p q]
  Grammar
  (derive
    [g c]
    (if (nullable? p)
      (union (concat (derive p c) q) (derive q c))
      (concat (derive p c) q)))
  (nullable?
    [g]
    (and (nullable? p) (nullable? q))))

(defmethod print-method Concat
  [g w]
  (.write w "(concat ")
  (print-method (.p g) w)
  (.write w " ")
  (print-method (.q g) w)
  (.write w ")"))

;; kleene star
(deftype Repeat [p]
  Grammar
  (derive
    [g c]
    (and (derive p c) g))
  (nullable?
    [g]
    (nullable? p)))

(defn char
  [x]
  (->Char x))

(defn union
  [p q]
  (->Union p q))

(defn concat
  [p q]
  (->Concat p q))

;; i don't think this works...
(defn seq
  [& gs]
  (reduce concat gs))

(defn repeat
  [p]
  (->Repeat p))

(defn string
  [s]
  (apply seq (map char s)))

(defn parse
  [g str]
  (reduce derive g (seq str)))

(comment

  (derive (concat
           (char \z)
           (union (char \a) (char \b)))
          \z)

  (derive (concat
           (concat
            (char \z)
            (union (char \a)
                   (char \b)))
           (char \c))
          \z)
  
  (parse (concat
          (concat
           (char \z)
           (union (char \a)
                  (char \b)))
          (char \c))
         "z")
  
  (parse (concat
          (concat
           (char \z)
           (union (char \a)
                  (char \b)))
          (char \c))
         "az")

  (parse (concat
          (concat
           (char \z)
           (union (char \a)
                  (char \b)))
          (char \c))
         "zac")

  )
