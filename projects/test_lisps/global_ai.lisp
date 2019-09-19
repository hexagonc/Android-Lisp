  
  (setq _TYPE_STRING_SERIALIZABLE "STRING-SERIALIZABLE")

  (setq MAX_THREAD_PRIORITY 10)
  (setq MIN_THREAD_PRIORITY 0)

  (defmacro run-with-priority (priority expr)
      `(progn
          (setq +*prior*+ (get-thread-priority))
          (set-thread-priority ,priority)
          (finally
            ,expr
            (set-thread-priority +*prior*+)
            )
        )
    )

  (defun do-method (lambda-obj method ...)

      (apply (eval (list 'with
                          lambda-obj
                          (list '#'
                                method))) 
             ...))

  (defun is-object-p (v)
      (and (lambda-p v)
         (with v (#' "has-type"))
           (do-method v "has-type"  "OBJECT")
           v))

  (defun is-string-serializable-p (v)
      (and (lambda-p v)
         (with v (#' "has-type"))
           (do-method v "has-type" _TYPE_STRING_SERIALIZABLE)
           v))

  (defun copy-object (obj) 
           
      (if (is-object-p obj)
          (eval (parse (serialize obj 1)))))

  (defun copy-map (map overide-map)
    (make-string-hashtable (mapcar key
                                   (get-hash-keys map)
                                   (list key
                                         (if (and overide-map (contains-hash-key overide-map key))
                                             (gethash overide-map key)
                                             (gethash map key))))))


  (defun create-base-object (speech-form)
    (do-method (with* (funcall (lambda () this))
                (multiple-bind (type  outer-this type-map speech-form id score speech-function-name)
                               (list "OBJECT" 
                                     this 
                                     (make-string-hashtable (list (list "OBJECT" 1))) 
                                     F
                                     (unique-id)
                                     1
                                     F))
                (defun init (speech)
                    (set speech-form speech )
                    outer-this)

                (defun set-score (s)
                    (set score s))

                (defun get-score ()
                    score)
                
                (defun has-value-p ()
                    1)
                (defun get-type ()
                    type)
                    
                (defun get-id ()
                    id)

                (defun set-id (i)
                    (set id i)
                    outer-this)

                (defun has-type (type-name)
                    (gethash type-map
                             type-name))

                (defun add-type (type-name)
                    (set type type-name)
                    (defhash type-map
                             type-name
                             1))

                (defun remove-type (type-name)
                    (remhash type-map
                             type-name))

                (defun get-speech ()
                    speech-form))
        "init"
        speech-form))


  (defun get-types (obj)
      (and (is-object-p obj)
           (with obj
                 type-map)))


  (defun is-object-p (v)
      (and (lambda-p v)
           (with v (#' "has-type"))
           (do-method v "has-type"  "OBJECT")
           
           v))

  (defun get-speech (v)
      (and (is-object-p v)
           (do-method v "get-speech")))


  (defun create-numeric-value (value subtype)
      (setq super 
            (create-base-object (concat (string value) 
                                        (if subtype
                                            (concat " " (lower subtype))
                                            ""))))
      (do-method super "add-type" "NUMBER")
      (with* super
              (setq value F)
              (setq subtype F)
              (defun get-subtype ()
                      subtype)
              (defun init (v s)
                  (set subtype s)
                  (set value v))


              (defun get-value ()
                  value)
              )
      (do-method super "init" value subtype)
      super)


  (defun is-number-p (v)
      (and (is-object-p v)
           (do-method v "has-type"  "NUMBER")
           v))

  (defun simple-tokenize (string)
    (mapcar x (split string " ") (lower x)))


  (defun join-tokens (token-list)
      (join token-list
            " "))

  (defun average (list)
    (/ (apply "+" list)
       (length list)))

  (defun dec (x)
      (- x 1))

  (defun inc (x)
      (+ x 1))

  (defun non-zero (value)
    (and value
         (number-p value)
         (> value 0)))

  (defun non-empty (list-string-hashtable)
      (and list-string-hashtable
           (> (length list-string-hashtable) 0)
           list-string-hashtable))

  (defun non-empty-list (list)
      (and list
           (> (length list) 0)
           list))


  (defun empty-list (list)
      (or (not list)
          (= 0 (length list))))

  (defun singleton-list (list)
      (and list
           (= (length list) 1)
           (if (list-p list)
               (first list)
               (gethash list
                        (first (get-hash-keys list))))))

  (defun empty (list-or-hash)
      (= 0 (length list-or-hash)))

  (defun last-list-index (list)
    (if (non-empty-list list)
        (dec (length list))))

  (defmacro append-data (var-name data)
    `(set ,var-name (append-item ,var-name ,data)))

  (defmacro get-next-value (list-var-name index)
    `(and (setq *temp*
                (last-list-index ,list-var-name))
          (< ,index *temp*)
          (nth ,list-var-name
               (inc ,index))))

  (defun umod (value mod)
      (or (<= 0
              (setq v
                    (mod value mod)))
          (+ v
              mod)))

  (defun the (map)
    (and (non-empty map)
         (= (length map) 1)
         (gethash map (first (get-hash-keys map)))))

  (defun flatten (list-of-lists)
      (setq out
            ())

      (for inner-list
           list-of-lists
           out
           (set out
                (append out
                        inner-list))))


  (defmacro random-evaluate (arg-list)
      (random-select arg-list))


  (defun incr-list-map (map key value limit)
      (if (setq prior 
                (gethash map key))
          (defhash map 
                   key
                   (if limit
                       (progn
                          (setq n (append-item prior value))
                          (subseq n (- (length n) 
                                       (min (length n) 
                                            limit))))
                       (append-item prior value)))
          (defhash map
                   key
                   (list value)))
      map)

  (defmacro incr-list-index (list index value)
    `(set ,list 
          (set-nth ,list 
                   ,index 
                   (+ ,value 
                      (nth ,list ,index)))))

  (defun decr-map (map name amount min-weight)
    (setq min-weight
        (or min-weight
            0))
    (setq amount
        (or amount
            1))
    (defhash map
             name
            (max min-weight 
                 (- (or (gethash map name) 0) amount))))

  (defun incr-map (map name amount max-weight)
    
    (setq amount
        (or amount
            1))
    (if max-weight
        (defhash map 
                 name
                 (min max-weight
                      (+ (or (gethash map name) 0) 
                         amount)))
        (defhash map 
                 name
                 (+ (or (gethash map name) 0) 
                    amount))))
    

  (defun incr-int-set-map (map key int-value)
      (if (setq prior 
                (gethash map key))
          (incr-map prior 
                    int-value)
          (defhash map
                   key
                   (make-int-hashtable (list (list int-value 1)))))
      map)

  (defun incr-string-set-map (map key str-value)
      (if (setq prior 
                (gethash map key))
          (incr-map prior 
                    str-value)
          (defhash map
                   key
                   (make-string-hashtable (list (list str-value 1)))))
      map)



(defun round-to-nearest-integer (deci center-up-p)
  (setq down
        (integer deci))
  (setq up
        (inc down))

  (setq dup (- up deci))

  (setq ddown (- deci down))

  (if (< dup ddown)
      up
      (if (< ddown dup)
          down
          (if center-up-p
              up
              down))))

(defun round-to-decimal-places (num places)
    (setq multiplier
          (pow 10 places))
    (/ (integer (* num 
                   multiplier))
       multiplier))

(defun insert-into-ascending-sorted-set (token set)
    (setq out ())

    (setq set (or set ()))

    (for (set-token i)
         set
         (append set token)
         (if (= 0 (string-compare token set-token))
             (return set)
             (if (< (string-compare token set-token) 0)
                 (return (append out
                                 token
                                 (subseq set i)))
                 (set out
                      (append out set-token))))))

(defmacro append-exp (var exp)
  (list 'set var 
         (list 'append-item
              var
              exp)))

(defmacro timed-execute (exp)
  `(progn
    (setq start (time))
    (setq result
          ,exp)
    (setq execution-time
          (- (time) start))
    (list execution-time result)))



; Use this to catch exceptions.  The [...] arguments are the exception handlers which 
; handle the exception data, which will be bound to the local variable e
; If you just want the exception message then use this pattern:
; (catch (* F 12) (println (first e))) 
; where (first e) is the exception message
  (defmacro catch (expr ...)
    `(signal-block ("**+**")
        (("**+**") (try ,expr))
        (("RUNTIME_ERROR" e) ,@...)))


(defun right-list (list amount include-meta-p)
  (setq len (length list))
  (if include-meta-p
      (mapcar i
              (make-range (max 0 (- len amount))
                          (- len 1))
              (list (nth list i) i))
      (subseq list
              (max 0 
                   (- len amount))
              len)))

(defun left-list (list amount include-meta-p)
  (setq len (length list))
  (if include-meta-p
      (mapcar i
              (make-range 0 (- (min len amount) 1))
              (list (nth list i) i))
      (subseq list
              0
              (min len amount))))

; Get a list iterator
(defun get-list-iterator (list end-of-list-value)

      (lambda ()
        
        (if (non-empty-list list)
            (progn
              (setq item
                    (first list))
              (set list 
                (rest list))
            
              item)
            end-of-list-value)))

(defun get-list-iterator-with-index (list end-of-list-value)
      (setq i 0)
      (lambda ()
        
        (if (non-empty-list list)
            (progn
              (setq item
                    (first list))
              (set list 
                (rest list))
            
              (finally 
                (list item i)
                (set i (+ 1 i))))
            end-of-list-value)))

(defun get-datetime-string (date)
    (defun format-d (num)
        (setq len (length (setq s (concat "0" (string num)))))
        (substring s (- len 2)))

    (setq date (or date (time)))

    (setq date-spec
         (get-datetime-parts date))

    (setq temporal-suffix
          (concat (gethash date-spec "YEAR")
                  (format-d (gethash date-spec "MONTH"))
                  (format-d (gethash date-spec "DAY_OF_MONTH"))
                  "-"
                  (format-d (gethash date-spec "HOUR_OF_DAY"))
                  ":"
                  (format-d (gethash date-spec "MINUTE"))
                  ":"
                  (format-d (gethash date-spec "SECOND"))
                  ":"
                  (gethash date-spec "MILLISECONDS")
                  )))

(defun sort-strings-ascending (string-list secondary-sort-length-ascending-p)
  (setq split-value-map
        (make-string-hashtable))

  (setq comparator
        (lambda (x y)
          (multiple-bind (x-len y-len x-split y-split i)
                         (list (length x) 
                               (length y)
                               (or (gethash split-value-map x)
                                   (defhash split-value-map x (mapcar c (split x "") (char-to-code (lower c)))))
                               (or (gethash split-value-map y)
                                   (defhash split-value-map y (mapcar c (split y "") (char-to-code (lower c)))))
                               0))

          (multiple-bind (limit length-comp)
                         (if (< x-len y-len)
                             (list x-len -1)
                             (if (= x-len y-len)
                                 (list x-len 0)
                                 (list y-len 1))))
                

          (while (and (< i limit)
                      (= (nth x-split i)
                         (nth y-split i))) 
            (setq i (+ 1 i)))

          (if (= i limit)
              (if secondary-sort-length-ascending-p
                  length-comp
                  (- length-comp))
              (if (< (nth x-split i)
                     (nth y-split i))
                  -1
                  1))))
  (sort string-list comparator))


  (defun sort-strings-descending (string-list secondary-sort-length-ascending-p)
    (setq split-value-map
          (make-string-hashtable))

    (setq comparator
          (lambda (x y)
            (multiple-bind (x-len y-len x-split y-split i)
                           (list (length x) 
                                 (length y)
                                 (or (gethash split-value-map x)
                                     (defhash split-value-map x (mapcar c (split x "") (char-to-code (lower c)))))
                                 (or (gethash split-value-map y)
                                     (defhash split-value-map y (mapcar c (split y "") (char-to-code (lower c)))))
                                 0))

            (multiple-bind (limit length-comp)
                           (if (< x-len y-len)
                               (list x-len -1)
                               (if (= x-len y-len)
                                   (list x-len 0)
                                   (list y-len 1))))
                  

            (while (and (< i limit)
                        (= (nth x-split i)
                           (nth y-split i))) 
              (setq i (+ 1 i)))

            (if (= i limit)
                (if secondary-sort-length-ascending-p
                    length-comp
                    (- length-comp))
                (if (< (nth x-split i)
                       (nth y-split i))
                    1
                    -1))))
    (sort string-list comparator))

(defun wait (milli)
    (setq stop (+ (time) milli))
    (unless (> (time) stop)
      milli)
    milli)

(defun empty-string (str)
    (or (not str)
        (= (length str) 0)))

  ; |-o-| |-o-| |-o-| |-o-| |-o-| |-o-| |-o-| |-o-| |-o-| |-o-| 
  ; Vector related helper functions 
  ; ___________________________________________________________

  (defun vequals (v1 v2)
      (and (list-p v1) (list-p v2) (= (length v1) (length v2)) (all (x i) v1 (= (nth v2 i) x))))

  (defun simple-discretize (raw-number num-steps min-value max-value)
      (setq range 
            (- max-value min-value))

      (setq step-size
            (/ range
               num-steps))

      (setq discretized
            (integer (/ (- raw-number min-value)
                        step-size))))

  (defun unsigned-vectorize (raw-number num-steps min-value max-value)
      (fast-num-to-tally-vector (simple-discretize raw-number num-steps min-value max-value)
                                num-steps))

  
  (defun signed-vectorize (raw-number num-steps abs-width)
      (setq unsigned-discretized
            (simple-discretize (abs raw-number)
                               num-steps
                               0
                               abs-width))
      (fast-num-to-tally-vector (* unsigned-discretized
                             (if (>= raw-number 0) 1 -1))
                                num-steps
                                1))

(defun get-unsigned-tally-vector-difference (vec1 vec2)
  (fast-num-to-tally-vector (- (fast-tally-vector-to-num vec1)
                               (fast-tally-vector-to-num vec2))
                            (length vec1)
                            1))

  (defun map-vector-value (vmap vector-key value)
      (for xi
           vector-key
           (defhash vmap -1 value)
           (set vmap
                (or (gethash vmap (integer xi))
                    (defhash vmap (integer xi) (make-int-hashtable))))))

  (defun get-vector-value (vmap vector-key)
      (for xi
           vector-key
           (gethash vmap -1)
           (if (not (set vmap
                         (gethash vmap (integer xi))))
               (return F))))

  (defun incr-vector-map (map key value limit)
      (if (setq prior 
                (get-vector-value map key))
          (map-vector-value map 
                            key
                            (if limit
                                (progn
                                   (setq n (append-item prior value))
                                   (subseq n (- (length n) 
                                                (min (length n) 
                                                     limit))))
                                (append-item prior value)))
          (map-vector-value map
                            key
                            (list value)))
      map)

(defun get-all-vector-entry-list (vmap on-item-handler)
    (setq entry-list ())

    (setq node-list (list vmap))
    (setq path-list (list ()))

    (setq new-node-list ())
    (setq new-path-list ())
    (while (non-empty-list node-list)
        (multiple-set (new-node-list new-path-list)
                      (list () ()))
        (for (node i)
             node-list
             (multiple-set (node-list path-list)
                           (list new-node-list new-path-list))
             (if (setq value (gethash node -1))
                 (progn
                    (set entry-list
                         (append-item entry-list (list (nth path-list i) value)))
                    (if on-item-handler
                        (funcall vector-key (nth path-list i) value)))
                 (for x
                      (get-hash-keys node)
                      F
                      (progn
                          (set new-node-list
                               (append-item new-node-list
                                            (gethash node x)))
                          (set new-path-list
                               (append-item new-path-list (append (mapcar c (nth path-list i) c) x))))))))
    entry-list)


; -~)--  -~)-- -~)-- -~)-- -~)-- -~)-- -~)-- -~)-- -~)-- -~)-- -~)-- 
; Robot Constants
; -~)--  -~)-- -~)-- -~)-- -~)-- -~)-- -~)-- -~)-- -~)-- -~)-- -~)-- 



