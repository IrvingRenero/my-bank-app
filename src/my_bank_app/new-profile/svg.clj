(ns new-profile.svg)


(def order-details
  {:name "Mitchard Blimmons"
   :email "mitchard.blimmonsgmail.com"})

(def order-details-validations
  {:name
   ["Please enter a name" not-empty]
   :email
   ["Please enter an email address" not-empty
    "Your email address doesn't look like an email address"
     #(or (empty? %) (re-seq #"@"%))]})


(defn error-messages-for
  "Return a seq of error messages"
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))


(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value (get to-validate fieldname)
                  error-messages (error-messages-for value validation-check-groups)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))


(def my-error-name (validate order-details order-details-validations))



(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))


#_(macroexpand
    '(if-valid order-details order-details-validations my-error-name
               (println :success)
               (println :failure my-error-name)))
#_(let*
    [my-error-name (validate order-details order-details-validations)]
    (if (clojure.core/empty? my-error-name)
      (println :success)
      (println :failure my-error-name)))


(if-valid order-details order-details-validations my-error-name
          (println :success)
          (println :failure my-error-name))


(defn add-new-profile
  []
  (println "ingresa u nombre, da enter; ingresa tu email da enter")
  (let [new-profile-data  (seq [(read-line) (read-line)])
        map-newprofile {:name (first new-profile-data) :email (second new-profile-data)}
        my-error-name (validate map-newprofile order-details-validations)]
    (if-valid map-newprofile order-details-validations my-error-name
              (println :success)
              (println :failure my-error-name))))


(defn -main
  []
  (add-new-profile))