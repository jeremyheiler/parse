(ns parse.core-test
  (:require [clojure.test :refer :all]
            [parse.core :as p]))

(deftest test-parse-char
  (is (= #{\a} (p/parse (p/char \a) "a")))
  (is (= #{} (p/parse (p/char \a) "b"))))

(deftest test-parse-derive-char
  (is (= (p/eps* #{\a}) (p/parse-derive (p/char \a) \a)))
  (is (= p/empty (p/parse-derive (p/char \a) \b))))

(deftest test-nullable?-char
  (is (false? (p/nullable? (p/char \a)))))

(deftest test-parse-null-char
  (is (= #{} (p/parse-null (p/char \a)))))
