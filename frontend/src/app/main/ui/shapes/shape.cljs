;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.main.ui.shapes.shape
  (:require
   [app.common.geom.shapes :as geom]
   [app.common.uuid :as uuid]
   [app.main.ui.context :as muc]
   [app.main.ui.shapes.filters :as filters]
   [app.main.ui.shapes.gradients :as grad]
   [app.util.object :as obj]
   [cuerdas.core :as str]
   [rumext.alpha :as mf]
   [app.util.svg :as usvg]
   ))

(mf/defc svg-node [{:keys [node replace-id]}]
  (cond
    (string? node) node

    :else
    (let [{:keys [tag attrs content]} node
          attrs (-> attrs
                    (usvg/update-attr-ids replace-id)
                    (usvg/clean-attrs))]
      ;; TODO: Clean attrs so they have react format
      [:> (name tag) (clj->js attrs)
       (for [node content] [:& svg-node {:node node
                                         :replace-id replace-id}])])))

(mf/defc svg-defs [{:keys [shape render-id]}]
  (when-let [svg-defs (:svg-defs shape)]
    (let [replace-id (fn [id] (if (contains? svg-defs id)
                                (str render-id "-" id)
                                id))] (for [svg-def (vals svg-defs)]
              [:& svg-node {:node svg-def
                            :replace-id replace-id}]))))

(mf/defc shape-container
  {::mf/wrap-props false}
  [props]
  (let [shape     (obj/get props "shape")
        children  (obj/get props "children")
        render-id (mf/use-memo #(str (uuid/next)))
        filter-id (str "filter_" render-id)
        styles    (cond-> (obj/new)
                    (:blocked shape) (obj/set! "pointerEvents" "none"))

        {:keys [x y width height type]} shape
        frame? (= :frame type)
        group-props (-> (obj/clone props)
                        (obj/without ["shape" "children"])
                        (obj/set! "id" (str "shape-" (:id shape)))
                        (obj/set! "filter" (filters/filter-str filter-id shape))
                        (obj/set! "style" styles)

                        (cond-> frame?
                          (-> (obj/set! "x" x)
                              (obj/set! "y" y)
                              (obj/set! "width" width)
                              (obj/set! "height" height)
                              (obj/set! "xmlnsXlink" "http://www.w3.org/1999/xlink")
                              (obj/set! "xmlns" "http://www.w3.org/2000/svg"))))

        wrapper-tag (if frame? "svg" "g")]
    [:& (mf/provider muc/render-ctx) {:value render-id}
     [:> wrapper-tag group-props
      [:defs
       [:& svg-defs        {:shape shape :render-id render-id}]
       [:& filters/filters {:shape shape :filter-id filter-id}]
       [:& grad/gradient   {:shape shape :attr :fill-color-gradient}]
       [:& grad/gradient   {:shape shape :attr :stroke-color-gradient}]]
      children]]))
