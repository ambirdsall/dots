(fn file-exists [name]
  (let [f (io.open name :r)]
    (and (~= f nil) (io.close f))))

(fn identity [x] x)

{: file-exists
 : identity}
