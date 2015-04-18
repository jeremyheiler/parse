(ns parse.core
  (:require [clojure.core :as c]
            [clojure.set :as set])
  (:refer-clojure :exclude [char empty empty? repeat seq]))

(defmacro defix
  [name lang bottom & body]
  (let [cache (atom {})
        changed? (atom :error-changed)
        running? (atom false)
        visited (atom :error-visited)]
    `(defn ~name
       [~lang]
       (loop [l# ~lang]
         (let [cached?# (contains? @cache l)
               cached# (get @cache l bottom)
               run?# @running?]
           (cond

             (and cached?# (not run?#))
             ;;=>
             cached#

             (and run? (contains? ~@visited l))
             ;;=>
             (if cached?# cached# ~bottom)

             run?#
             ;;=>
             (do
               (swap! ~visited assoc l# true)
               (let [new-val# (do ~@body)]
                 (when-not (= new-val# cached#)
                   (reset! ~changed? true)
                   (swap! ~cache assoc l# new-val#))
                 new-val#))

             (and (not cached?#) (not run?#))
             ;;=>
             (let [v# (atom ~bottom)]
               (reset! ~changed? true)
               (reset! ~running? true)
               (reset! ~visited {})
               (while ~@changed?
                 (reset! ~changed? false)
                 (reset! ~visited {})
                 (reset! v# (~name ~lang)))
               @v#)
             
             ))))))

;;;;;;;;;;

(declare alt seq)

(defprotocol Parser
  (nullable?
    [l])
  (match-derive
    [l c])
  (parse-derive
    [l c])
  (parse-null
    [l])
  (empty?
    [l])
  (null?
    [l])
  (compact
    [l]))

(def empty
  "A singleton representing the empty set."
  (with-meta
    (reify Parser
      (nullable?
        [l]
        false)
      (match-derive
        [l c]
        empty)
      (parse-derive
        [l c]
        empty)
      (parse-null
        [l]
        #{})
      (empty?
        [l]
        true)
      (null?
        [l]
        false)
      (compact
        [l]
        l))
    {:type ::empty}))

(def eps
  "A singleton representing the set containing null."
  (with-meta
    (reify Parser
      (nullable?
        [l]
        true)
      (match-derive
        [l c]
        empty)
      (parse-derive
        [l c]
        ;; TODO?
        empty)
      (parse-null
        [l]
        ;; TODO?
        true)
      (empty?
        [l]
        false)
      (null?
        [l]
        true)
      (compact
        [l]
        l))
    {:type ::eps}))

(deftype StatefulEps [parse-trees]
  Parser
  (nullable?
    [l]
    false) ;; TODO is this right?
  ;; TODO does match-derive need to be implemented?
  (parse-derive
    ;; TODO
    [l c]
    empty)
  (parse-null
    [l]
    parse-trees)
  (empty?
    [l]
    false)
  (null?
    [l]
    true)
  (compact
    [l]
    l)
  Object
  (equals
    [this that]
    (if (instance? (class this) that)
      (= parse-trees (.parse_trees that))
      false)))

(defn eps*
  [parse-trees]
  (->StatefulEps parse-trees))

(deftype Null [p]
  Parser
  (parse-derive
    [l c]
    empty)
  (parse-null
    [l]
    (parse-null p)))

(deftype Char [x]
  Parser
  (nullable?
    [l]
    false)
  (match-derive
    [l c]
    (if (= c x)
      eps
      empty))
  (parse-derive
    [l c]
    (if (= c x)
      (eps* (hash-set c))
      empty))
  (parse-null
    [l]
    #{})
  (empty?
    [l]
    false)
  (null?
    [l]
    false)
  (compact
    [l]
    l))

(deftype Union [p q]
  Parser
  (nullable?
    [l]
    (or (nullable? @p) (nullable? @q)))
  (match-derive
    [l c]
    (Union. (delay (match-derive @p c)) (delay (match-derive @q c))))
  (parse-derive
    [l c]
    (Union. (delay (parse-derive @p c)) (delay (parse-derive @q c))))
  (parse-null
    [l]
    (set/union (parse-null @p) (parse-null @q)))
  (empty?
    [l]
    (and (empty? p) (empty? q)))
  (null?
    [l]
    (and (null? p) (null? q))) ;; or?
  (compact
    [l]
    (cond
      (and (not (empty? p)) (empty? q)) p
      (and (empty? p) (not (empty? p))) q
      :else (alt (compact p) (compact q)))))

(deftype Concat [p q]
  Parser
  (nullable?
    [l]
    (and (nullable? @p) (nullable? @q)))
  (match-derive
    [l c]
    (if (nullable? @p)
      (Union. (delay (match-derive @q c)) (delay (Concat. (delay (match-derive @p c)) q)))
      (Concat. (delay (match-derive @p c)) q)))
  (parse-derive
    [l c]
    (Union. (delay (Concat. (delay (parse-derive @p c)) q))
            (delay (Concat. (delay (->Null @p)) (delay (parse-derive @q c))))))
  (parse-null
    [l]
    (set (for [pt (parse-null @p) qt (parse-null @q)]
           [pt qt])))
  (empty?
    [l]
    (or (empty? p) (empty? q)))
  (null?
    [l]
    (and (null? p) (null? q)))
  (compact
    [l]
    (comment (cond
               (and (not (null? p)) (parse-null (and )))

               
               (and (not (null? p)) (null? q)) (red (compact p) (fn [w] [w t]))
               (and (null? p) (not (null? p))) q
               :else (seq (compact p) (compact q))))
    
    (if (or (nullable? p) (nullable? q))
      empty
      l)))

(deftype Reduce [p f]
  Parser
  ;; TODO implement nullable?
  ;; TODO implement match-derive?
  (parse-derive
    [l c]
    (parse-derive l c))
  (parse-null
    [l]
    (set (map f (parse-null p))))
  (compact
    [l]
    ))

(deftype Repeat [p]
  Parser
  (nullable?
    [l]
    true)
  (match-derive
    [l c]
    (seq (match-derive p c) l))
  (parse-derive
    [l c]
    (seq (parse-derive p c) l))
  (parse-null
    [l]
    #{nil}))

;;;;;;;;;;

(defn char
  [x]
  (->Char x))

(defmacro alt
  ([]
   empty)
  ([p]
   p)
  ([p q]
   `(->Union (delay ~p) (delay ~q)))
  ([p q & r]
   `(->Union (delay ~p) (delay (alt ~q ~@r)))))

(defmacro seq
  ([]
   eps)
  ([p]
   p)
  ([p q]
   `(->Concat (delay ~p) (delay ~q)))
  ([p q & r]
   `(->Concat (delay ~p) (delay (seq ~q ~@r)))))

(defmacro red
  [l f]
  `(->Reduce (delay ~l) ~f))

(defn redf
  [l f]
  (->Reduce l f))

(defn repeat
  [p]
  (->Repeat p))

(defmacro string
  [s]
  `(seq
    ~@(for[c s]
        `(char ~c))))

;;;;;;;;;;

(defn parse-loop
  [d l str]
  (loop [p l [c & w] str]
    (if c
      (recur (d p c) w)
      p)))

(defn matches?
  [l str]
  (nullable? (parse-loop match-derive l str)))

(defn parse
  [l str]
  (parse-null (parse-loop parse-derive l str)))

;;;;;;;;;;

(defmethod print-method ::empty
  [g w]
  (.write w "empty"))

(defmethod print-method ::eps
  [g w]
  (.write w "eps"))

(defmethod print-method StatefulEps
  [g w]
  (.write w "(eps* ")
  (print-method (.parse_trees g) w)
  (.write w ")"))

(defmethod print-method Char
  [g w]
  (.write w (str "(char \\" (.x g) ")")))

(defmethod print-method Union
  [g w]
  (.write w "(alt ")
  (print-method (.p g) w)
  (.write w " ")
  (print-method (.q g) w)
  (.write w ")"))

(defmethod print-method Concat
  [g w]
  (.write w "(seq ")
  (print-method (.p g) w)
  (.write w " ")
  (print-method (.q g) w)
  (.write w ")"))

(defmethod print-method Repeat
  [g w]
  (.write w "(repeat ")
  (print-method (.p g) w)
  (.write w ")"))

;;;;;;;;;;

(comment

  ;; TODO: how to impl recursive definitions?
  (def l
    (alt (seq l (alt (char \a) (char \b))) eps))

  (match-derive l \a)

  (match-derive (seq
                 (char \z)
                 (alt (char \a) (char \b)))
                \z)

  (match-derive (seq
                 (seq
                  (char \z)
                  (alt (char \a) (char \b)))
                 (char \c))
                \z)
  
  (parse (seq
          (seq
           (char \z)
           (alt (char \a) (char \b)))
          (char \c))
         "z")

  (parse (seq
          (seq
           (char \z)
           (alt (char \a) (char \b)))
          (char \c))
         "a")
  
  (parse (seq
          (seq
           (char \z)
           (alt (char \a) (char \b)))
          (char \c))
         "ax")

  (parse (seq
          (seq
           (char \z)
           (alt (char \a) (char \b)))
          (char \c))
         "za")

  (matches? (seq
             (seq
              (char \z)
              (alt (char \a) (char \b)))
             (char \c))
            "za")

  (matches? (seq
             (seq
              (char \z)
              (alt (char \a) (char \b)))
             (char \c))
            "zac")

  )
