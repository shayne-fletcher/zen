(*Matthias Gaunard*)

let rotate_left n l =
    let rec rotate_left_impl n l l2 =
        match l with
                [] -> l2
            |   hd::tail -> if n == 0 then l@l2
                            else (rotate_left_impl (n-1) tail (l2@[hd]))
    in
    rotate_left_impl (if n < 0 then ((List.length l)+n) else n) l []
;;

let rotate_right n l = rotate_left (-n) l;;

(*Harvey Stein*)

;;; Slow, easy version:
(defun rotate-left-one (lst)
  (append (cdr lst) (list (car lst))))

(defun rotate-right-one (lst)
  (append (last lst) (butlast lst)))

(defun rotate (n lst)
  (cond ((>= n 1)
         (dotimes (i n) (setq lst (rotate-right-one lst))))
        ((<= n -1)
         (dotimes (i (- n)) (setq lst (rotate-left-one lst)))))
  lst)

(defun rotate-right (n lst)
  (rotate n lst))

(defun rotate-left (n lst)
  (rotate (- n) lst))


;;; Mostly faster version (slower for n=1).
(defun rotate-fast2 (n lst)
  (if (= n 0)
      lst
    (let* ((l (length lst))
           (n (mod n l)))
      (cond ((>= n 1)
             (append (last lst n) (butlast lst n)))
            (t lst)))))

;;; Avoid rescanning list by consing up beginning
(defun rotate-fast3 (n lst)
  (if (= n 0)
      lst
    (let* ((l (length lst))
           (n (mod n l)
      (cond ((>= n 1)
             (rot n () lst))
            (t lst)))))))

(defun rot (n s e)
  (cond ((= n 0) (append e (reverse s)))
        (t (rot (- n 1) (cons (car e) s) (cdr e)))))
