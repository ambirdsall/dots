(fn apply [config]
  (doto config
    (tset :unix_domains
          [{ :name "airbyte" }
           { :name "conf" }])))

{: apply}
