;;; -*- lexical-binding: t -*-
;; Native compilation settings
;; Display the architecture using:
;;   gcc -march=native -Q --help=target | grep march
;;
(setq my-cpu-architecture "znver4")

;; `native-comp-compiler-options' specifies flags passed directly to the C
;; compiler (for example, GCC) when compiling the Lisp-to-C output
;; produced by the native compilation process. These flags affect code
;; generation, optimization, and debugging information.
(setq native-comp-compiler-options `("-O2"
                                     ,(format "-mtune=%s" my-cpu-architecture)
                                     ,(format "-march=%s" my-cpu-architecture)
                                     "-g0"
                                     "-fno-omit-frame-pointer"
                                     "-fno-finite-math-only"))
