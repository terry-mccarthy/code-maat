;;; Copyright (C) 2013 Adam Tornhill
;;;
;;; Distributed under the GNU General Public License v3.0,
;;; see http://www.gnu.org/licenses/gpl.html

(ns code-maat.analysis.cohesion
  (:require [code-maat.analysis.coupling-algos :as c]
            [code-maat.dataset.dataset :as ds]
            [code-maat.analysis.math :as m]
            [incanter.core :as incanter]))

;;; Cohesion analysis: measures how often a component's changes are
;;; self-contained, i.e. revisions that only touch files within the
;;; same component.
;;;
;;; High cohesion = component revisions rarely mix with other components.
;;; Low cohesion = component is frequently changed together with other components.
;;;
;;; Intended for use with the --group option to define architectural boundaries.
;;;
;;; Input: an Incanter dataset with (at least) the following columns:
;;; :entity :rev
;;;
;;; Output: an Incanter dataset with the following columns:
;;; :entity :revisions :internal :cohesion
;;; where
;;;   :entity    => the component name
;;;   :revisions => total number of revisions touching this component
;;;   :internal  => revisions where only this component was changed
;;;   :cohesion  => internal / revisions as a percentage (0-100)

(defn- unique-entities-per-revision
  "Returns a seq of sets, each set holding the distinct components
   touched within one revision."
  [ds]
  (->>
   (c/as-entities-by-revision ds)
   (map c/entities-in-rev)
   (map set)))

(defn- update-stats-for-revision
  [stats entity-set]
  (let [internal? (= 1 (count entity-set))]
    (reduce (fn [acc entity]
              (-> acc
                  (update-in [entity :revisions] (fnil inc 0))
                  (update-in [entity :internal] (fnil + 0) (if internal? 1 0))))
            stats
            entity-set)))

(defn- cohesion-stats
  "Returns a map of entity -> {:revisions n :internal n}."
  [entity-sets]
  (reduce update-stats-for-revision {} entity-sets))

(defn by-cohesion
  "Calculates cohesion for each architectural component.
   Returns results sorted ascending by cohesion (least cohesive first)
   so the most problematic components appear at the top.
   Best used with the --group option."
  ([ds options]
   (by-cohesion ds options :asc))
  ([ds options order-fn]
   (->>
    (unique-entities-per-revision ds)
    cohesion-stats
    (map (fn [[entity {:keys [revisions internal]}]]
           {:entity    entity
            :revisions revisions
            :internal  internal
            :cohesion  (int (m/as-percentage (/ internal revisions)))}))
    (ds/-dataset [:entity :revisions :internal :cohesion])
    (incanter/$order [:cohesion :revisions] order-fn))))
