(load "store.scm")
(load "interp.scm")
the-store!
(initialize-store!)
the-store!
(newref! (num-val 7))
the-store!
(newref! (num-val 6))
the-store!
(newref! (num-val 9))
the-store!
(deref (ref-val 2))
the-store!
(deref (ref-val 3))
the-store!
(deref (ref-val 4))
the-store!
(increment_count! (ref-val 1))
the-store!
(increment_count! (ref-val 4))
the-store!
(decrement_count! (ref-val 0))
the-store!
(decrement_count! (ref-val 4))
the-store!
free-cells!
(newref! (num-val 8))
the-store!
(remove-cell! (ref-val 0))
the-store!
free-cells!
(remove-cell! (ref-val 3))
the-store!
free-cells!
(remove-cell! (ref-val 4))
the-store!
free-cells!
(newref! (num-val 10))
the-store!
(setref! (ref-val 2) (num-val 20))
the-store!
(setref! (ref-val 3) (num-val 20))
the-store!
(setref! (ref-val 4) (num-val 20))
the-store!