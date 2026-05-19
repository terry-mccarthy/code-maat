;;; Copyright (C) 2013 Adam Tornhill
;;;
;;; Distributed under the GNU General Public License v3.0,
;;; see http://www.gnu.org/licenses/gpl.html

(ns code-maat.analysis.cohesion-test
  (:require [code-maat.analysis.cohesion :as cohesion]
            [incanter.core :as incanter])
  (:use clojure.test))

;;; Scenarios:
;;;
;;; rev 1: entities A and B change together  -> mixed
;;; rev 2: entity A alone                    -> pure for A
;;; rev 3: entities A and B change together  -> mixed
;;; rev 4: entity B alone                    -> pure for B
;;;
;;; Component A: 3 revisions (1,2,3), 1 internal (2) => cohesion 33%
;;; Component B: 3 revisions (1,3,4), 1 internal (4) => cohesion 33%

(def ^:const mixed-and-pure
  (incanter/to-dataset
   [{:entity "A" :rev 1}
    {:entity "B" :rev 1}
    {:entity "A" :rev 2}
    {:entity "A" :rev 3}
    {:entity "B" :rev 3}
    {:entity "B" :rev 4}]))

;;; rev 1: A, B, C  -> mixed
;;; rev 2: A only   -> pure A
;;; rev 3: B only   -> pure B
;;;
;;; A: 2 revisions, 1 internal => 50%
;;; B: 2 revisions, 1 internal => 50%
;;; C: 1 revision,  0 internal =>  0%

(def ^:const three-components
  (incanter/to-dataset
   [{:entity "A" :rev 1}
    {:entity "B" :rev 1}
    {:entity "C" :rev 1}
    {:entity "A" :rev 2}
    {:entity "B" :rev 3}]))

;;; A only in all revisions => 100% cohesion

(def ^:const perfectly-cohesive
  (incanter/to-dataset
   [{:entity "A" :rev 1}
    {:entity "A" :rev 2}
    {:entity "A" :rev 3}]))

(def ^:private default-options {})

(deftest calculates-cohesion-for-mixed-and-pure-revisions
  (let [result (incanter/to-list (cohesion/by-cohesion mixed-and-pure default-options))]
    ;; sorted ascending by cohesion (least cohesive first)
    ;; both at 33%, but order between equal-cohesion items may vary
    (is (= (count result) 2))
    (let [row-map (into {} (map (fn [[entity revs internal pct]] [entity pct]) result))]
      (is (= (row-map "A") 33))
      (is (= (row-map "B") 33)))))

(deftest calculates-cohesion-for-three-components
  (let [result (incanter/to-list (cohesion/by-cohesion three-components default-options))
        row-map (into {} (map (fn [[entity revs internal pct]] [entity pct]) result))]
    (is (= (row-map "C") 0))
    (is (= (row-map "A") 50))
    (is (= (row-map "B") 50))))

(deftest reports-internal-revision-counts
  (let [result (incanter/to-list (cohesion/by-cohesion three-components default-options))
        row-map (into {} (map (fn [[entity revs internal pct]] [entity {:revisions revs :internal internal}]) result))]
    (is (= (get-in row-map ["A" :revisions]) 2))
    (is (= (get-in row-map ["A" :internal]) 1))
    (is (= (get-in row-map ["C" :revisions]) 1))
    (is (= (get-in row-map ["C" :internal]) 0))))

(deftest gives-full-cohesion-when-component-never-mixes
  (let [result (incanter/to-list (cohesion/by-cohesion perfectly-cohesive default-options))]
    (is (= (count result) 1))
    (let [[entity revisions internal cohesion] (first result)]
      (is (= entity "A"))
      (is (= revisions 3))
      (is (= internal 3))
      (is (= cohesion 100)))))

(deftest least-cohesive-component-appears-first
  (let [result (incanter/to-list (cohesion/by-cohesion three-components default-options))
        first-cohesion (last (first result))]
    (is (= first-cohesion 0))))
