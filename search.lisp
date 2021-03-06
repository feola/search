(require 'split-sequence)
(require 'alexandria)

(defmacro read-file (pathname)
  `(progn
     (print ,pathname)
     (alexandria:read-file-into-string ,pathname)))

(defun recur-files (path)
  ;; получаем список файлов и поддиректорий
  (let ((files (directory (merge-pathnames path "*.*")))
        (dirs  (directory (merge-pathnames path "*"))))
    (setf files (remove-if #'(lambda (x)
                               (find x dirs :test #'(lambda (a b)
                                                      (string= (format nil "~A" a)
                                                               (format nil "~A" b)))))
                           files))
    ;; для каждой поддиректории:
    (loop :for dir :in dirs :do
       ;; рекурсивно вызываем себя
       (setf files (append files (recur-files dir))))
    ;; возвращаем результат
    files))

;; (defun get-all-pages (path)
;;   (let ((all-files (recur-files path)))
;;     (loop :for file :in all-files :collect
;;        (list (format nil "~A" file)
;;              (read-file file)))))

;; (defun get-search-words (search-query)
;;   (split-sequence:split-sequence #\Space search-query))

(defun search-word-in-page (word page)
  (search word page :test #'string=))

;; (defun search-in-pages (search-query pages)
;;   ;; разбить поисковый запрос на слова
;;   (let ((results nil)
;;         (words (get-search-words search-query)))
;;     (loop :for word :in words :do
;;        ;; для каждого слова: взять все страницы
;;        (loop :for (file page) :in pages :do
;;           ;; для каждой страницы: искать слово
;;           (if (search-word-in-page word page)
;;               ;; если слово найдено - добавить в результаты
;;               (push file results))))
;;     (remove-duplicates results)))

;; (search-in-pages "факультет информационных технологий" (get-all-pages (path "content/")))

(defun relevance (pattern str &optional (start 0) (res 0))
  ;; Релевантность - это число, сопоставленное строке
  ;; (2 . "предположим, для примера, что вы пишете программу для...")
  ;; Инициализируем переменную возвращаемого результата :RES
  ;; Ищем первое вхождение слова :POS
  ;; Если нашли - инкрементируем X, и рекурсивно вызываем себя для оставшейся подстроки
  (let ((pos (search pattern str :start2 start)))
    (if pos
        (progn
          (incf res)
          (relevance pattern str (+ (length pattern) pos) res))
        (cons res str))))

(defun sort-strings (strings pattern)
  (mapcar #'cdr
          (sort (mapcar #'(lambda (x)
                            (relevance pattern x))
                        strings)
                #'(lambda (a b)
                    (> (car a) (car b))))))

;; tests

(defparameter *test*
  '("предположим, для примера, что вы про пишете программу для подсчета релевантости"
    "для того чтобы ее правильно написать"
    "программа номер один и программа номер два"))

(defparameter *query-string* "для про")

;; (defparameter *search-words* (get-search-words *query-string*))

;; (sort (mapcar #'(lambda (x)
;;                   (cons
;;                    (reduce #'+
;;                            (mapcar #'(lambda (y)
;;                                        (car (relevance y x)))
;;                                    *search-words*))
;;                    x))
;;               *test*)
;;       #'(lambda (a b)
;;           (> (car a) (car b))))


(defun word-list (file)
  "Преобразовать файл (страницу) в список слов"
  (mapcar
   #'(lambda (x)
       (string-downcase
        (string-trim '(#\Space #\Tab #\Newline) x)))
   (remove-if
    #'(lambda (x)
        (or (equal x "")))
    (split-sequence:split-sequence-if
     #'(lambda (x)
         (or (equal x #\Space)
             (equal x #\Newline)
             (equal x #\()
             (equal x #\))
             (equal x #\[)
             (equal x #\])
             (equal x #\{)
             (equal x #\})
             (equal x #\<)
             (equal x #\>)
             (equal x #\,)
             (equal x #\;)
             (equal x #\.)
             (equal x #\:)
             (equal x #\!)
             (equal x #\/)
             (equal x #\\)
             ))
     (read-file file)))))


(defun word-hash-tf (file)
  "Определить TF"
  ;; создать хеш-таблицу, и добавить в неё слова и частоту их встречаемости
  ;; определить количество слов в файле и преобразовать хеш таблицу так, чтобы
  ;; каждому слову соответствовало его значение tf
  (let ((result (make-hash-table :test #'equal)))
    (mapcar #'(lambda (x)
                (multiple-value-bind (val present)
                    (gethash x result)
                  (if (null present)
                      (setf (gethash x result) 1)
                      (setf (gethash x result) (incf val)))))
            (word-list file))
    (let ((cnt (hash-table-count result)))
      (maphash #'(lambda (k v)
                   (setf (gethash k result)
                         (float (/ v cnt))))
               result)
      result)))

;; (maphash #'(lambda (k v) (format t "~a => ~a~%" k v))
;;          (word-hash-tf "habr/post171335.txt"))

(defun files-with-word (word path)
  "Определить количество файлов, в которых встречается слово"
  (let ((res 0))
    (mapcar #'(lambda (x)
                (if
                 (search-word-in-page word
                                      (format nil "~{~A~^ ~}"
                                              (word-list  x)))
                 (setf res (+ 1 res))))
            (recur-files path))
    res))

;; (let ((x (format nil "~{~A~^ ~}"
;;                  (word-list "habr/post171335.txt"))))
;;   (print (subseq x (- (search "ход" x) 0))))

;; (let ((x "ход")
;;       (y "ход"))
;;   (if (and
;;        (equal 0 (search x y))
;;        (equal (length x) (length y)))
;;       t
;;       nil))

;; (files-with-word "ваша" "habr/")

;; (files-with-word "л" (mapcar #'(lambda (x) ;; список точечных пар (файл . хэш-таблица)
;;                                       (cons x (word-hash-tf x)))
;;                                   (recur-files "habr/")))


(defun word-hash-idf (file path)
  "Определить idf"
  ;; создать хеш-таблицу со списком слов с значением idf
  ;; чтобы найти idf, необходимо разделить общее количество файлов
  ;; на количество файлов, в которых встречается слово
  (flet ((files-with-word (word list-of-hash)
           (let ((res 0))
             (mapcar #'(lambda (x)
                         (multiple-value-bind (val present)
                             (gethash word (cdr x))
                           (when present
                             (incf res))))
                     list-of-hash)
             res)))
    (let* ((files (recur-files path))
           (tfs (mapcar #'(lambda (x) ;; список точечных пар (файл . хэш-таблица)
                            (cons x (word-hash-tf x)))
                        files)))
      ;; проходим по списку точечных пар
      (mapcar #'(lambda (x)
                  (let ((file (car x))
                        (hash (cdr x))
                        (res  (make-hash-table :test #'equal))) ;; результирующая хэш-таблица IDF для этого файла
                    (maphash #'(lambda (word tf)
                                 (setf (gethash word res)
                                       (float
                                        (log (/ (length files)
                                           (files-with-word word tfs))))))
                             hash)
                    (cons file res)))
              tfs))))


(mapcar #'(lambda (x)
            (let ((file (car x))
                  (hash (cdr x)))
              (format t "~%~%:::::::::::::::~A~%" file)
              (maphash #'(lambda (k v)
                           (format t "~a => ~a~%" k v))
                       hash)))
        (word-hash-idf #P"/home/feolan/search/habr/post171335.txt" "habr/"))

;; BM25
;; количественная оценка релевантности файла в отношении поискового запроса

(defconstant *k1* 2)
(defconstant *b* 0.75)
"Задаются свободные коэффициенты"


(defmacro get-hash-val-or-zero-if-not-exists (val form &body body)
  (let ((present (gensym "PRESENT")))
    `(multiple-value-bind (,val ,present)
         ,form
       (unless ,present (setf ,val 0))
       ,@body)))

;; (macroexpand-1
;;  '(get-hash-val-or-zero-if-not-exists
;;    idf
;;    (gethash word (cdr x))
;;    (+ 1 2)))

(defun bm (word file path)
  "Определить bm25"
  ;; Берём мапкар от результата вызова функции (word-hash-idf file path) -
  ;; это список точечных пар (файл.хеш-таблица-идф)
  ;; нам надо получить список точечных пар (файл.бм)
  (mapcar #'(lambda (x)
              ;; Итак, с помощью мапкар применяем к исходным точечным парам функцию
              ;; - берём точечную пару, и соединяем её car (название файла) с величиной bm
              (get-hash-val-or-zero-if-not-exists idf (gethash word (cdr x))
                (get-hash-val-or-zero-if-not-exists tf (gethash word (word-hash-tf file))
                  (cons
                   (car x)
                   (/
                    ;; Числитель:
                    ;; - ищем слово в хеш таблице идф - получаем идф
                    ;; - ищем слово в хеш таблице тф - получаем тф
                    ;; - к к1 прибавляем 1
                    ;; - все три величины перемножаем
                    (* idf tf (+ *k1* 1))
                    ;; Знаменатель:
                    ;; - ищем слово в хеш таблице тф - получаем тф
                    ;; - к тф прибавляем произведение к1 на большую скобку
                    ;; - в этой скобке складываем (1-b) и произведение b на дробь
                    ;; - дробь - это деление количества слов в документе на среднюю длину
                    ;; документа в папке
                    ;; количество слов в документе - это (length (word-list y))
                    ;; средняя длина документа - это сумма длин всех документов разделить на
                    ;; общее количество документов в папке
                    (+ tf
                       (* *k1*
                          (+
                           (- 1 *b*)
                           (* *b*
                              (/ (length (word-list file))
                                 (/
                                  (apply #'+
                                         (mapcar #'(lambda (y) (length (word-list y)))
                                                 (recur-files path)))
                                  (length (recur-files path)))))))))))))
          (word-hash-idf file path)))

;; Комментарии для rigidus:
;; 2.
;; 3. Величина бм определяется по формуле, поэтому дальше идёт подстановка
;; значений в формулу:

(bm "код" "habr/post171335.txt" "habr/")

(setf a (gethash "код" (word-hash-tf "habr/post171335.txt")))

(setf b (gethash "код" (cdr (car (word-hash-idf "habr/post171335.txt" "habr/")))))

(* a b)


(maphash #'(lambda (k v)
             (print (list k v)))
         (cdar (word-hash-idf "habr/post171335.txt" "habr/")))
