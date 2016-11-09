(ns koans.24-macros
  (:require [koan-engine.core :refer :all]))

(defmacro hello [x]
  (str "Hello, " x))

(defmacro infix [form]
  (list (second form) (first form) (nth form 2)))

(defmacro infix-better [form]
  `(~(second form) ; Note the syntax-quote (`) and unquote (~) characters!
    ~(first form)
    ~(nth form 2)))

; https://github.com/jarofghosts/clojure-koans/blob/master/17_macros.clj
(defmacro r-infix [form]
  (cond (not (seq? form)) form
        (= 1 (count form)) `(r-infix ~(first form))
        :else
        (let [operator (second form)
              first-arg (first form)
              others ((comp rest rest) form)]
          `(~operator
            (r-infix ~first-arg)
            (r-infix ~others)))))

(meditations
  "Macros are like functions created at compile time"
  (= "Hello, Macros!" (hello "Macros!"))

  "I can haz infix?"
  (= (+ 9 1) (infix (9 + 1)))

  "Remember, these are nothing but code transformations"
  (= (list (second '(9 + 1)) (first '(9 + 1)) (nth '(9 + 1) 2)) (macroexpand '(infix (9 + 1))))

  "You can do better than that - hand crafting FTW!"
  (= '(* 10 2) (macroexpand '(infix-better (10 * 2))))

  "Things don't always work as you would like them to... "
  (= '(+ 10 (2 * 3)) (macroexpand '(infix-better ( 10 + (2 * 3)))))

  "Really, you don't understand recursion until you understand recursion"
  (= 36 (r-infix (10 + (2 * 3) + (4 * 5)))))
