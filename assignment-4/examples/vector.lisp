(let ((v (vector 5 5))) (let ((out (vector 2 v))) (do (vector-set out 0 (vector-set v 0 50)) out)))
