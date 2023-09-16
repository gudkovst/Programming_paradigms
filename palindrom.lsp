(defun rev (lst) (if (null lst) lst (append (rev (cdr lst)) (list (car lst))))); reverse

(defun isPal (lst) (if (equal lst (rev lst)) T nil)); check palindrom

(defun findPal (lst &optional (res ())) (if (= (length lst) 2) (if (isPal lst) (append res (list lst))) 
															   (if (isPal lst) (append res (list lst) (findPal (cdr lst) res) (findPal (cdr (rev lst)) res))
															                   (append res (findPal (cdr lst) res) (findPal (cdr (rev lst)) res))))); find palindrom-part with repetere

(defun in (lst el) (if (null lst) nil (if (equal el (car lst)) T (in (cdr lst) el)))); check el in lst

(defun remDup (lst &optional (res ())) (if (null lst) res (if (in res (car lst)) (remDup (cdr lst) res) (remDup (cdr lst) (append res (list (car lst))))))); remove duplicate

(defun findPalSet (lst) (remDup (findPal lst))); find palindrom-part without repetere

(defun countPal (lst &optional (c 0)) (if (null lst) c (if (atom (car lst)) (countPal (cdr lst) c) (if (isPal (car lst)) (countPal (cdr lst) (+ c 1)) (countPal (cdr lst) c))))); count linear palindrom-part

(defun minPart (lst &optional (m (car lst))) (if (null lst) m (if (< (length (car lst)) (length m)) (minPart (cdr lst) (car lst)) (minPart (cdr lst) m)))); find part of min length

(defun andList (lst &optional (res T)) (if (null lst) res (andList (cdr lst) (and res (car lst))))); and logic list

(defun findPart (lst match &optional (index 0)) (cond ((< (length lst) (length match)) -1) 
	                                                  ((andList (mapcar #'equal lst match)) index) 
	                                                  (T (findPart (cdr lst) match (+ index 1))))); get index begin match in lst (or -1 else)

(defun del (lst index &optional (res ())) (cond ((< (length lst) index) lst) 
                                                ((= index 0) (append res (cdr lst)))
                                                (T (del (cdr lst) (- index 1) (append res (list (car lst))))))); del lst[index]

(defun getDelIndex (lst) (if (< (- (/ (length lst) 2) (findPart lst (minPart (findPalSet lst)))) (length (minPart (findPalSet lst))))
                         (findPart lst (minPart (findPalSet lst))) (+ (findPart lst (minPart (findPalSet lst))) (length (minPart (findPalSet lst)))))); define optimal index for deleting element

(defun delPalPart (lst) (if (null (findPalSet lst)) lst (delPalPart (del lst (getDelIndex lst))))); delete min count atoms for absense palindrom-part