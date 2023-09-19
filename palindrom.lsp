(defun rev (lst) (if (null lst) lst (append (rev (cdr lst)) (list (car lst))))); reverse

(defun isPal (lst) (if (equal lst (rev lst)) T nil)); check palindrom

(defun findPal (lst) (if (= (length lst) 2) (if (isPal lst) (list lst)) 
		     (if (isPal lst) (append (list lst) (findPal (cdr lst)) (findPal (cdr (rev lst))))
		     (append (findPal (cdr lst)) (findPal (cdr (rev lst))))))); find palindrom-part with repetere

(defun in (lst el) (if (null lst) nil (if (equal el (car lst)) T (in (cdr lst) el)))); check el in lst

(defun remDup (lst) (if (null lst) () (if (in (cdr lst) (car lst)) (remDup (cdr lst)) (append (remDup (cdr lst)) (list (car lst)))))); remove duplicate

(defun findPalSet (lst) (remDup (findPal lst))); find palindrom-part without repetere

(defun countPal (lst) (if (null lst) 0 (if (atom (car lst)) (countPal (cdr lst)) (if (isPal (car lst)) (+ (countPal (cdr lst)) 1) (countPal (cdr lst)))))); count linear palindrom-part

(defun minSublist (lst) (if (null (cdr lst)) (car lst) (if (< (length (car lst)) (length (minSublist (cdr lst)))) (car lst) (minSublist (cdr lst))))); find sublist of min length (all elements in lst - list)

(defun andList (lst) (if (null lst) T (and (car lst) (andList (cdr lst))))); and logic list

(defun findPart (lst match) (cond ((< (length lst) (length match)) -1) 
	                              ((andList (mapcar #'equal lst match)) 0) 
	                              (T (+ (findPart (cdr lst) match) 1)))); get index begin match in lst (or -1 else)

(defun del (lst index) (cond ((< (length lst) index) lst) 
                       ((= index 0) (cdr lst))
                       (T (append (list (car lst)) (del (cdr lst) (- index 1)))))); del lst[index]

(defun getDelIndex (lst) (if (< (- (/ (length lst) 2) (findPart lst (minSublist (findPalSet lst)))) (length (minSublist (findPalSet lst))))
                         (findPart lst (minSublist (findPalSet lst))) (+ (findPart lst (minSublist (findPalSet lst))) (length (minSublist (findPalSet lst)))))); define optimal index for deleting element

(defun delPalPart (lst) (if (null (findPalSet lst)) lst (delPalPart (del lst (getDelIndex lst))))); delete min count atoms for absense palindrom-part

(defun revWithSublist (lst) (if (null lst) lst (append (revWithSublist (cdr lst)) (if (atom (car lst)) (list (car lst)) (list (revWithSublist (car lst))))))); reverse with all sublists

(defun isPalWithSublist (lst) (if (equal lst (revWithSublist lst)) T nil)); check unlinear palindrom
