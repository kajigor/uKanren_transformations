(load "mk/test-check.scm")
(load "evalo-standard.scm")


(define toN (lambda (n) (if (= n 0) 'z `(s ,(toN (- n 1))))))

(time
  (test 'unify_test
  (run 1 (q)
    (evalo
      `(letrec ([eq_nat (lambda (a b)
                    (match a
                      [`z      (match b [`z #t] [`(s ,y) #f])]
                      [`(s ,x) (match b [`z #f] [`(s ,y) (eq_nat x y)])]
                    )
                )])


       (letrec ([get_term (lambda (v s)
                    (match s
                      [`() 'none]
                      [`(,x . ,xs)
                        (match v
                          [`z x]
                          [`(s ,n) (get_term n xs)]
                      )]
                    )
       )])

       (letrec ([check_uni (lambda (s t1 t2)

          (letrec ([forall2 (lambda (s l1 l2)
            (match l1
              [`()         (match l2 [`()         #t])]
              [`(,x . ,xs) (match l2 [`(,y . ,ys) (and (check_uni s x y) (forall2 s xs ys))])]
            )
          )])

         (match (list t1 t2)
           [`((constr ,n1 ,a1) (constr ,n2 ,a2)) (and (eq_nat n1 n2) (forall2 s a1 a2))]
           [`((var ,v1)        (constr ,n2 ,a2)) (match (get_term v1 s)
                                                    [`none #f]
                                                    [`(some ,t) (check_uni s t t2)])]
           [`((constr ,n1 ,a1) (var ,v2)       ) (match (get_term v2 s)
                                                    [`none #f]
                                                    [`(some ,t) (check_uni s t1 t)])]
           [`((var ,v1)        (var ,v2)       ) (match (get_term v1 s)
                                                    [`(some ,t) (check_uni s t1 t)]
                                                    [`none (match (get_term v2 s)
                                                              [`(some ,t) #f]
                                                              [`none (eq_nat v1 v2)])])]
         )
       ))])

       (check_uni ',q '(constr z ((var z)                                                      (constr (s z) ((var (s (s z))) (constr (s (s (s z))) ()))) (var z)    ))
                      '(constr z ((constr (s z) ((constr (s (s z)) ()) (var (s (s (s z))))))   (var (s z))                                                (var (s z))))
       )
       )))
      #t))

    ""))
