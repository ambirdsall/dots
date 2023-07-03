(fn file-exists [name]
  (let [f (io.open name :r)]
    (and (~= f nil) (io.close f))))

(fn identity [x] x)

(macro view!
  [form] (view form))

(macro assert!
  [x msg]
  "Similar to assert but displays the asserted form on failure. Mimics
   behavior in other lisp languages."
  (if (= msg nil)
    `(assert ,x
       (.. "AssertionError: " ,(view x)))
    `(assert ,x
       (.. "AssertionError: " ,msg "\n" ,(view x)))))

{: file-exists
 : identity}
