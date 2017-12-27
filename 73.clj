(letfn [(get-value [state xy]
          (get (:game state) (get-position state xy)))

        (get-position [state xy]
          (let [[x y] xy]
            (if (and (< -1 x (:max-x state)) (< -1 y (:max-y state)))
              (+ x (* y (:max-x state)))
              -1)))

        (compute-position [state index]
          (let [nx (mod index (:max-x state))
                ny (quot index (:max-x state))]
            [nx ny]))

        (sum-line-dir [state position dir sum]
          (let [player (get-value state position)
                new-pos (map + position dir)
                new-pos-value (get-value state new-pos)]
            (if (not= player new-pos-value)
              sum
              (recur state new-pos dir (inc sum)))))

        (sum-line [state directions position]
          (let [[left right] directions
                sum (inc (+ (sum-line-dir state position left 0)
                            (sum-line-dir state position right 0)))]
            (if (<= (:win-count state) sum)
              (let [val (get-value state position)]
                (if (not= val :e)
                  val
                  nil
                  )
                )
              nil
              )))

        (someone-won? [state]
          (first (drop-while nil? (flatten (map (fn [position]
                         (list (sum-line state [[0 1] [0 -1]] position)
                           (sum-line state [[1 0] [-1 0]] position)
                           (sum-line state [[-1 -1] [1 1]] position)
                           (sum-line state [[-1 1] [1 -1]] position)))
                   (map #(compute-position state %) (range (* (:max-x state) (:max-y state)))))))))
        ]

  (fn [board] (someone-won? {:game (into [] (flatten (reverse board))) :max-x 3 :max-y 3 :win-count 3})))
