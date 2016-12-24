# timsg.alioth-benchmarks

```clojure
(require 'timsg.alioth-benchmarks.spectral-norm)
(in-ns 'timsg.alioth-benchmarks.spectral-norm)
(import [timsg.alioth_benchmarks SpectralNorm])

(time
  (SpectralNorm/Main (make-array String 0)))

;; ~1075-1095

(time (-main))

;; ~526 
```
