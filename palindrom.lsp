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

(defun delPalPart (lst) (cond ((null (findPalSet lst)) lst)
							  ((> (length (delPalPart (del lst (findPart lst (minSublist (findPalSet lst)))))) (length (delPalPart (del lst (- (+ (findPart lst (minSublist (findPalSet lst))) (length (minSublist (findPalSet lst)))) 1))))) 
							  		(delPalPart (del lst (findPart lst (minSublist (findPalSet lst))))))
							  (T (delPalPart (del lst (- (+ (findPart lst (minSublist (findPalSet lst))) (length (minSublist (findPalSet lst)))) 1)))))); delete min count atoms for absense palindrom-part (brute force solution)

(defun delPalPartHeuristic (lst) (if (null (findPalSet lst)) lst (delPalPartHeuristic (del lst (getDelIndex lst))))); delete min count atoms for absense palindrom-part (heuristic solution)

(defun revWithSublist (lst) (if (null lst) lst (append (revWithSublist (cdr lst)) (if (atom (car lst)) (list (car lst)) (list (revWithSublist (car lst))))))); reverse with all sublists

(defun isPalWithSublist (lst) (if (equal lst (revWithSublist lst)) T nil)); check unlinear palindrom

(defun isLinearList (lst) (if (null lst) T (if (atom (car lst)) (isLinearList (cdr lst)) nil))); check what lst is linear

(defun delLinearSubpal (lst) (cond ((null lst) ())
								   ((atom (car lst)) (append (list (car lst)) (delLinearSubpal (cdr lst))))
								   ((not (isLinearList (car lst))) (append (list (delLinearSubpal (car lst))) (delLinearSubpal (cdr lst))))
								   ((isPal (car lst)) (delLinearSubpal (cdr lst)))
								   (T (append (list (car lst)) (delAllSubpal (cdr lst)))))); delete linear sublist-palindrom

(defun delAllSubpal (lst) (cond ((null lst) ())
								((atom (car lst)) (append (list (car lst)) (delAllSubpal (cdr lst))))
								((isPalWithSublist (car lst)) (delAllSubpal (cdr lst)))
								(T (append (list (delAllSubpal (car lst))) (delAllSubpal (cdr lst)))))); delete all sublist-palindrom

(defun eqBorder (lst) (equal (car lst) (car (rev lst)))); check (lst[first] == lst[last])

(defun delLast (lst) (rev (cdr (rev lst)))); delete last element

(defun cutBorder (lst) (delLast (cdr lst))); delete first and last elements

(defun reductionToPal (lst) (cond ((isPal lst) lst)
								  ((eqBorder lst) (append (list (car lst)) (reductionToPal (cutBorder lst)) (list (car lst))))
								  ((> (length (reductionToPal (cdr lst))) (length (reductionToPal (delLast lst)))) (reductionToPal (cdr lst)))
								  (T (reductionToPal (delLast lst))))); minimal reduction lst to palindrom