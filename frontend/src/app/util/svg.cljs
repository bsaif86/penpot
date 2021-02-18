;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020-2021 UXBOX Labs SL

(ns app.util.svg
  (:require
   [app.common.uuid :as uuid]
   [app.common.data :as cd]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [cuerdas.core :as str]))

(defonce replace-regex #"[^#]*#([^)\s]+).*")

(defn clean-attrs
  "Transforms attributes to their react equivalent"
  [attrs]
  (letfn [(transform-key [key]
            (-> (name key)
                (str/replace ":" "-")
                (str/camel)
                (keyword)))

          (format-styles [style-str]
            (->> (str/split style-str ";")
                 (map str/trim)
                 (map #(str/split % ":"))
                 (group-by first)
                 (map (fn [[key val]]
                        (vector
                         (transform-key key)
                         (second (first val)))))
                 (into {})))

          (map-fn [[key val]]
            (cond
              (= key :class) [:className val]
              (and (= key :style) (string? val)) [key (format-styles val)]
              :else (vector (transform-key key) val)))]

    (->> attrs
         (map map-fn)
         (into {}))))

(defn update-attr-ids
  "Replaces the ids inside a property"
  [attrs replace-fn]
  (letfn [(update-ids [key val]
            (cond
              (map? val)
              (cd/mapm update-ids val)

              (= key :id)
              (replace-fn val)

              :else
              (let [[_ from-id] (re-matches replace-regex val)]
                (if from-id
                  (str/replace val from-id (replace-fn from-id))
                  val))))]
    (cd/mapm update-ids attrs)))

(defn replace-attrs-ids
  "Replaces the ids inside a property"
  [attrs ids-mapping]
  (if (and ids-mapping (not (empty? ids-mapping)))
    (letfn [(replace-ids [key val]
              (cond
                (map? val)
                (cd/mapm replace-ids val)

                (and (= key :id) (contains? ids-mapping val))
                (get ids-mapping val)

                :else
                (let [[_ from-id] (re-matches replace-regex val)]
                      (if (and from-id (contains? ids-mapping from-id))
                        (str/replace val from-id (get ids-mapping from-id))
                        val))))]
      (cd/mapm replace-ids attrs))

    ;; Ids-mapping is null
    attrs))

(defn generate-id-mapping [content]
  (letfn [(visit-node [result node]
            (let [element-id (get-in node [:attrs :id])
                  result (cond-> result
                           element-id (assoc element-id (str (uuid/next))))]
              (reduce visit-node result (:content node))))]
    (visit-node {} content)))

(defn extract-defs [{:keys [tag content] :as node}]
  
  (if-not (map? node)
    [{} node]
    (letfn [(def-tag? [{:keys [tag]}] (= tag :defs))

            (assoc-node [result node]
              (assoc result (-> node :attrs :id) node))

            (node-data [node]
              (->> (:content node) (reduce assoc-node {})))]

      (let [current-def (->> content
                             (filterv def-tag?)
                             (map node-data)
                             (reduce merge))
            result      (->> content
                             (filter (comp not def-tag?))
                             (map extract-defs))

            current-def (->> result (map first) (reduce merge current-def))
            content     (->> result (mapv second))]

        [current-def (assoc node :content content)]))))

(defn find-attr-references [attrs]
  (->> attrs
       (filter (fn [[_ attr-value]]
                 (re-matches replace-regex attr-value)))
       (mapv (fn [[attr-key attr-value]]
               (let [[_ id] (re-matches replace-regex attr-value)]
                 id)))))

(defn find-node-references [node]
  (let [current (->> (find-attr-references (:attrs node)) (into #{}))
        children (->> (:content node) (map find-node-references) (flatten) (into #{}))]
    (-> (cd/concat current children)
        (vec))))

(defn find-def-references [defs references]
  (loop [result (into #{} references)
         checked? #{}
         to-check (first references)
         pending (rest references)]

    (cond
      (nil? to-check)
      result
      
      (checked? to-check)
      (recur result
             checked?
             (first pending)
             (rest pending))

      :else
      (let [node (get defs to-check)
            new-refs (find-node-references node)]
        (recur (cd/concat result new-refs)
               (conj checked? to-check)
               (first pending)
               (rest pending))))))

(defn svg-transform-matrix [shape]
  (if (:root-attrs shape)
    (let [{svg-width :width svg-height :height} (:root-attrs shape)
          {:keys [x y width height]} (:selrect shape)]
      (gmt/multiply
       (gmt/matrix)
       (gsh/transform-matrix shape)
       (gmt/translate-matrix (gpt/point x y))
       (gmt/scale-matrix (gpt/point (/ width svg-width) (/ height svg-height)))))

    ;; :else
    (gmt/matrix)))

