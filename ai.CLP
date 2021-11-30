(deftemplate av
  (slot attribute)
  (slot value))

(deftemplate question
   (slot attribute)
   (slot text))

(deftemplate dt
    (slot t)
    (multislot x)
    (slot y)
    (slot z)
    (slot v)
)

(deftemplate fa
    (slot a)
    (multislot b)
    (slot c)
    (slot d)
)

(deftemplate ans
    (slot i)
    (slot j)
)

(deftemplate result
    (slot text)
)

(defmethod ask-question (?question)
    (printout t ?question)
    (bind ?answer (read))
    (while (and (not (floatp ?answer)) (not (integerp ?answer))) do
        (printout t ?question)
        (bind ?answer (read)))
    ?answer)

(defrule question-number
    (question (attribute ?attribute)
             (text ?text))
    (not (av (attribute ?attribute)))
    =>
    (bind ?value (ask-question ?text))
    (assert (av (attribute ?attribute)
            (value ?value)))
    
)

(defmethod check ((?v INTEGER FLOAT) 
                  (?op SYMBOL (eq ?op <))
                  (?upper INTEGER FLOAT))
   (< ?v ?upper))

(defmethod check ((?v INTEGER FLOAT) 
                  (?op SYMBOL (eq ?op >))
                  (?lower INTEGER FLOAT))
   (> ?v ?lower))

(defmethod check ((?v INTEGER FLOAT) 
                  (?op SYMBOL (eq ?op <=))
                  (?upper INTEGER FLOAT))
   (<= ?v ?upper))

(defmethod check ((?v INTEGER FLOAT) 
                  (?op SYMBOL (eq ?op >=))
                  (?lower INTEGER FLOAT))
   (>= ?v ?lower))

(defmethod check ((?v INTEGER FLOAT) 
                  (?op SYMBOL (eq ?op =))
                  (?eq INTEGER FLOAT))
   (eq ?v ?eq))

(defrule decision
    (dt (t ?attribute) (x $?x) (y ?attribute2) (z ?text) (v ?c))
    ?i <- (av (attribute counter) (value ?c2))
    ?j <- (av (attribute ?attribute) (value ?v))
    ?k <- (question (attribute ?attribute) (text ?tex2))
    (test (check ?v (expand$ ?x)))
    (test (check ?c2 = ?c))
    =>
    (retract ?i)
    (retract ?j)
    (retract ?k)
    (assert (question (attribute ?attribute2)
                        (text ?text)))
    (assert (av (attribute counter) (value (+ ?c 1))))
)


(defrule final-decision
    (fa (a ?attribute) (b $?b) (c ?final) (d ?c))
    ?i <- (av (attribute counter) (value ?c2))
    ?j <- (av (attribute ?attribute) (value ?v))
    ?k <- (question (attribute ?attribute) (text ?tex2))
    (test (check ?v (expand$ ?b)))
    (test (check ?c2 = ?c))
    =>
    (retract ?i)
    (retract ?j)
    (retract ?k)
    (assert (final ?final))
    (assert (av (attribute counter) (value (+ ?c 1))))
)

(defrule output-console
    (result (text ?text))
    (final ?final)
    (ans (i ?i) (j ?j))
    (test (check ?final = ?i))
    =>
    (format t ?text ?j)
)

(deffacts questions
    (question (attribute mean_concave_points)
               (text "mean_concave_points? "))
)

(deffacts counters
    (av (attribute counter) (value 1))
)

(deffacts result
    (result (text "%nHasil Prediksi = %s%n"))
)

(deffacts decision_tree
    (dt (t mean_concave_points) (x >  0.05) (y worst_perimeter) (z "worst_perimeter? ") (v 1))
    (dt (t mean_concave_points) (x <= 0.05) (y worst_radius) (z "worst_radius? ") (v 1))
    (dt (t worst_perimeter) (x <= 114.45) (y worst_texture) (z "worst_texture? ") (v 2))
    (dt (t worst_radius) (x > 16.83) (y mean_texture) (z "mean_texture? ") (v 2))
    (dt (t worst_radius) (x <= 16.83) (y radius_error) (z "radius_error? ") (v 2))
    (dt (t worst_texture) (x > 25.65) (y perimeter_error) (z "perimeter_error? ") (v 3))
    (dt (t worst_texture) (x <= 25.65) (y worst_concave_points) (z "worst_concave_points? ") (v 3))
    (dt (t mean_texture) (x > 16.19) (y concave_points_error) (z "concave_points_error? ") (v 3))
    (dt (t radius_error) (x > 0.63) (y mean_smoothness) (z "mean_smoothness? ") (v 3))
    (dt (t radius_error) (x <= 0.63) (y worst_texture) (z "worst_texture? ") (v 3))
    (dt (t perimeter_error) (x <= 1.56) (y mean_radius) (z "mean_radius? ") (v 4))
    (dt (t worst_texture) (x > 30.15) (y worst_area) (z "worst_area? ") (v 4))
    (dt (t worst_area) (x > 641.60) (y mean_radius) (z "mean_radius? ") (v 5))
    (dt (t mean_radius) (x <= 13.45) (y mean_texture) (z "mean_texture? ") (v 6))
    
)

(deffacts final-answer
    (fa (a worst_perimeter) (b > 114.45) (c 0) (d 2))
    (fa (a mean_texture) (b <= 16.19) (c 1) (d 3))
    (fa (a concave_points_error) (b <= 0.01) (c 0) (d 4))
    (fa (a concave_points_error) (b > 0.01) (c 1) (d 4))
    (fa (a perimeter_error) (b > 1.56) (c 0) (d 4))
    (fa (a mean_radius) (b <= 13.34) (c 0) (d 5))
    (fa (a mean_radius) (b > 13.34) (c 1) (d 5))
    (fa (a worst_concave_points) (b > 0.17) (c 0) (d 4))
    (fa (a worst_concave_points) (b <= 0.17) (c 1) (d 4))
    (fa (a mean_smoothness) (b <= 0.09) (c 1) (d 4))
    (fa (a mean_smoothness) (b > 0.09) (c 0) (d 4))
    (fa (a worst_texture) (b <= 30.15) (c 1) (d 4))
    (fa (a worst_area) (b <= 641.60) (c 1) (d 5))
    (fa (a mean_radius) (b > 13.45) (c 1) (d 6))
    (fa (a mean_texture) (b <= 28.79) (c 0) (d 7))
    (fa (a mean_texture) (b > 28.79) (c 1) (d 7))
)

(deffacts formatting-answer
    (ans (i 0) (j "Tidak terkena penyakit kanker payudara"))
    (ans (i 1) (j "Terkena penyakit kanker payudara"))
)