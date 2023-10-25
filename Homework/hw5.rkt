#lang racket

(provide hours
         gate gate? gate-type gate-inputs gate-output
         ckt ckt? ckt-inputs ckt-outputs ckt-gates
         good-gate? good-circuit?
         all-wires find-gate
         ha-ckt fa-ckt
         next-value
         next-config
         stable? all-stable-configs output-values init-config
         simulate
         final-config
         add-ckt
         dff-ckt
         timing-ckt
)


; Please do not edit lines above this one

;**********************************************************
; CS 201b HW #5, due 11:59 pm Wednesday, November 1st
; using the submit command.
;**********************************************************
; Name: Shane Lee
; Email address: shane.lee@yale.edu
;**********************************************************

; Computer science topics: gates and circuits

; Unless the problem specifies otherwise:
; * You may solve the problem using any method 
; and any Racket constructs, except mutators (set! and its relatives.)
; EXCEPT you may use hash mutators, e.g., hash-set!
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;**********************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 3)

;**********************************************************

; Wires and gates.

; A wire is identified by a Racket symbol, for example 'x, 'y0, or 'next.
; Strings (eg, "x" or "next") are not permitted wire names.

; A gate is a struct with three fields: 
; 1) a symbol indicating the type of gate, one of:
;       'not, 'and, 'or, 'xor, 'nand, 'nor
; 2) a list of the wire identifiers of the inputs
; 3) the output wire identifier

(struct gate (type inputs output) #:transparent)

; Examples of gates:

(define gate1 (gate 'and '(x y) 'z))
(define gate2 (gate 'or '(x v) 'v))
(define gate3 (gate 'not '(s) 't))
(define gate4 (gate 'nand '(x x) 'z))

;**********************************************************

; Circuits.

; A circuit is a struct with three fields
; (1) a list of input wire identifiers
; (2) a list of output wire identifiers
; (3) a list of gates

(struct ckt (inputs outputs gates) #:transparent)

;**********************************************************
; Examples of circuits

; Here is a circuit to compare the values of its two inputs
; and output 1 if they are equal, 0 if they are unequal.
; This computes one-bit compare for equality, and implements
; the sum-of-products representation.  This is a combinational
; circuit (no cycle of wires and gates.)

(define eq1-ckt
  (ckt
   '(x y)
   '(z)
   (list
    (gate 'not '(x) 'cx)
    (gate 'not '(y) 'cy)
    (gate 'and '(x y) 't1)
    (gate 'and '(cx cy) 't2)
    (gate 'or '(t1 t2) 'z))))
 
; This is interpreted as follows:
; the inputs of the circuit are the wires x and y,
; the outputs of the circuit consist of just the wire z,
; there are five gates specified as follows:
; wire cx is the output of a NOT gate with input x,
; wire cy is the output of a NOT gate with input y,
; wire t1 is the output of an AND gate with inputs x and y,
; wire t2 is the output of an AND gate with inputs cx and cy,
; wire z is the output of an OR gate with inputs t1 and t2.

; Here is another implementation of comparing two bits for equality.
; This uses the implementation as the NOT of (x XOR y).
; This is also a combinational circuit.
; The inputs and output of this circuit are named as in eq1-ckt.

(define eq2-ckt
  (ckt
   '(x y)
   '(z)
   (list
    (gate 'xor '(x y) 'w)
    (gate 'not '(w) 'z))))

; Here is a two-bit selector whose Boolean expressions are as follows.

; z_1 = x_1 * s' + y_1 * s
; z_0 = x_0 * s' + y_0 * s

; For this circuit, z_1 and z_0 are
; equal to x_1 and x_0 if s = 0, and
; z_1 and z_0 are equal to y_1 and y_0
; if s = 1.

; This is also a combinational circuit.

(define sel-ckt
  (ckt
   '(x1 x0 y1 y0 s)
   '(z1 z0)
   (list
    (gate 'not '(s) 'sc)
    (gate 'and '(x1 sc) 'u1)
    (gate 'and '(y1 s) 'v1)
    (gate 'or '(u1 v1) 'z1)
    (gate 'and '(x0 sc) 'u0)
    (gate 'and '(y0 s) 'v0)
    (gate 'or '(u0 v0) 'z0))))

; This is a NAND latch, used to store one bit.
; It is a sequential (not combinational) circuit,
; because it has a loop from a wire to itself through
; other wires and gates.

(define latch-ckt
  (ckt
   '(x y)
   '(q u)
   (list
    (gate 'nand '(x u) 'q)
    (gate 'nand '(y q) 'u))))

; The following is also a sequential circuit, with
; an OR gate one of whose inputs is its output.
; (The "Garden of Eden" circuit.)

(define seq-or-ckt
  (ckt
   '(x)
   '(z)
   (list
    (gate 'or '(x z) 'z))))

; The next is also a sequential circuit.
; It could serve as a clock.
; Note that this circuit has *no* inputs, but
; does have an output.

(define clock-ckt
  (ckt
   '()
   '(z)
   (list
    (gate 'not '(z) 'z))))

;**********************************************************
; ** problem 1 ** (9 points)
; Write two procedures:

; (good-gate? value)
; (good-circuit? value)

; (good-gate? value) takes an arbitrary value
; and returns #t if it is a well-formed gate,
; and #f otherwise.
; To be a well-formed gate, the value must be
; a gate struct whose three fields satisfy
; (1) the type field is one of the gate symbols:
;     'not, 'and, 'or, 'xor, 'nand, 'nor,
; (2) the inputs field is a list of wire identifiers
; (3) the output field is a single wire identifier

; In addition, the number of inputs should be correct for each gate type.
; A gate of type 'not has 1 input, while
; gates of types 'and, 'or, 'xor, 'nand, 'nor have 2 inputs.

; (good-circuit? value) takes an arbitrary value and
; returns #t if value is a well-formed circuit, 
; and returns #f otherwise.

; To be a well-formed circuit, it must be a ckt struct
; and its inputs field must be a list of wires,
; its outputs field must be a list of wires, and
; its gates field must be a list of gates that
; are well-formed according to the good-gate? procedure.

; In addition, the circuit must satisfy the conditions:
; (1) no input of the circuit is the output of a gate,
; (2) every input of a gate is either 
; an input of the circuit or the output of a gate,
; (3) no wire is the output of two or more gates,
; (4) every output of the circuit is either an input
; of the circuit or the output of a gate.

; Examples
; (good-gate? gate1) => #t
; (.. and similarly for gate2, gate3, gate4)
; (good-gate? (gate 'not 'x 'y)) => #f
; (good-gate? (gate 'nor '("x" "y") "z")) => #f
; (good-gate? (gate 'and '(1 2) 3)) => #f
; (good-gate? (gate 'equal '(x y) 'z)) => #f
; (good-gate? (gate 'or '(w x y) 'z)) => #f

; (good-circuit? sel-ckt) => #t
; (.. and similarly for eq1-ckt, eq2-ckt, latch-ckt, seq-or-ckt, clock-ckt)
; (good-circuit? 'hi) => #f
; (good-circuit? (ckt '() '() '())) => #t
; (good-circuit? (ckt '(x y) '(z) (list (gate 'and '(x y) 'x) (gate 'or '(x y) 'z)))) => #f
; (good-circuit? (ckt '(x y) '(z) (list (gate 'nor '(x y) 'z) (gate 'nand '(x y) 'z)))) => #f
; (good-circuit? (ckt '(x y) '(u z) (list (gate 'or '(x y) 'z)))) => #f
;**********************************************************


(define (good-gate? value)
  (cond
    [(not (gate? value))
     #f]
    [(not (ormap (lambda (potential-gate-type)
                   (equal? (gate-type value) potential-gate-type))
                 (list 'not 'and 'or 'xor 'nand 'nor)))
     #f]
    [(not (list? (gate-inputs value)))
     #f]
    [(not (symbol? (gate-output value)))
     #f]
    [(not (andmap (lambda (x)
                    (symbol? x))
                  (gate-inputs value)))
     #f]
    [(and (equal? (gate-type value) 'not) (equal? (length (gate-inputs value)) 1))
     #t]
    [(equal? (length (gate-inputs value)) 2)
     #t]
    [else
     #f]))

(define (good-circuit? value)
  (cond
    [(not (ckt? value))
     #f]
    [(not (list? (ckt-inputs value)))
     #f]
    [(not (list? (ckt-outputs value)))
     #f]
    [(not (list? (ckt-gates value)))
     #f]
    [(not (andmap (lambda (x)
                    (symbol? x))
                  (ckt-inputs value)))
     #f]
    [(not (andmap (lambda (x)
                    (symbol? x))
                  (ckt-outputs value)))
     #f]
    [(not (andmap (lambda (x)
                    (good-gate? x))
                  (ckt-gates value)))
     #f]
    [(ormap (lambda (gate1)
              (ormap (lambda (x)
                       (equal? x (gate-output gate1)))
                     (ckt-inputs value)))
            (ckt-gates value))
     #f]
    [(not (andmap (lambda (gate1)
                    (andmap (lambda (gateinput)
                              (or (ormap (lambda (x)
                                           (equal? x gateinput))
                                         (ckt-inputs value))
                                  (ormap (lambda (gate2)
                                           (equal? (gate-output gate2) gateinput))
                                         (ckt-gates value))))
                            (gate-inputs gate1)))
                  (ckt-gates value)))
     #f]
    [(ormap (lambda (gate1)
              (ormap (lambda (gate2)
                       (and (not (equal? gate1 gate2))
                            (equal? (gate-output gate1) (gate-output gate2))))
                     (ckt-gates value)))
            (ckt-gates value))
     #f]
    [(andmap (lambda (output1)
               (or (ormap (lambda (input1)
                            (equal? input1 output1))
                          (ckt-inputs value))
                   (ormap (lambda (gate1)
                            (equal? (gate-output gate1) output1))
                          (ckt-gates value))))
             (ckt-outputs value))
     #t]
    [else
     #f]))

;**********************************************************
; ** problem 2 ** (10 points)
; Write two procedures.

; (all-wires circuit) to return the list of all the wire names that appear
;      in the circuit, as circuit inputs, circuit outputs, gate
;      inputs or gate outputs, in that order, with duplicates removed.
; (find-gate wire circuit) to return the gate in the circuit with the given
;      output wire, or #f if there is no such gate.

; You may assume that circuit is a well-formed circuit; in particular,
; a wire is the output of *at most one* gate.

; Examples:

; (all-wires eq1-ckt) => '(x y z cx cy t1 t2)
; (all-wires sel-ckt) => '(x1 x0 y1 y0 s z1 z0 sc u1 v1 u0 v0)
; (find-gate 't2 eq1-ckt) => (gate 'and '(cx cy) 't2)
; (find-gate 'w eq2-ckt) => (gate 'xor '(x y) 'w)
; (find-gate 'y sel-ckt) => #f
;**********************************************************


(define (all-wires circuit) 
  (remove-duplicates (append (ckt-inputs circuit)
                             (append (ckt-outputs circuit)
                                     (append (foldr (lambda (gate1 result)
                                                      (append (gate-inputs gate1) result))
                                                    '()
                                                    (ckt-gates circuit))
                                             (foldr (lambda (gate1 result)
                                                      (cons (gate-output gate1) result))
                                                    '()
                                                    (ckt-gates circuit)))))))

(define (find-gate wire circuit)
  (cond
    [(empty? (ckt-gates circuit))
     #f]
    [(equal? wire (gate-output (first (ckt-gates circuit))))
     (first (ckt-gates circuit))]
    [else
     (find-gate wire (ckt (ckt-inputs circuit) (ckt-outputs circuit) (rest (ckt-gates circuit))))]))

;**********************************************************
; ** problem 3 ** (10 points)
; Define circuits for a half-adder and a full-adder in the representation described above.

; Your half-adder should be called ha-ckt
; and should have input wires: x and y and output wires: z and co, 
; where z is the exclusive or of x and y, and co is 1 if both x and y are 1.

; Your full-adder should be called fa-ckt 
; and should have input wires: x, y, and ci and output wires: z and co,
; where the value of z is 1 if the sum of x, y, and ci is odd,
; and the value of co is 1 if and only if at least two of x, y, and ci are 1.

; The order and names of the circuit input and output wires should be as specified above,
; but the number and names of internal wires (wires that are neither circuit inputs 
; nor circuit outputs) are up to you.

; Examples
; (good-circuit? ha-ckt) => #t
; (good-circuit? fa-ckt) => #t
; (ckt-inputs ha-ckt) => '(x y)
; (ckt-outputs ha-ckt) => '(z co)
; (ckt-inputs fa-ckt) => '(x y ci)
; (ckt-outputs fa-ckt) => '(z co)

; (output-values ha-ckt (final-config ha-ckt (init-config ha-ckt '(1 1)))) => (0 1)
; (output-values fa-ckt (final-config fa-ckt (init-config fa-ckt '(1 1 1)))) => (1 1)

; For the last two tests, your procedures output-values, final-config, init-config must be working.
;**********************************************************

(define ha-ckt
  (ckt '(x y)
       '(z co)
       (list
        (gate 'xor '(x y) 'z)
        (gate 'and '(x y) 'co))))

(define fa-ckt
  (ckt '(x y ci)
       '(z co)
       (list
        (gate 'xor '(x y) 'xxory)
        (gate 'xor '(xxory ci) 'z)
        (gate 'or '(x y) 'xy)
        (gate 'or '(x ci) 'xci)
        (gate 'or '(y ci) 'yci)
        (gate 'and '(xy xci) 'xy+xci)
        (gate 'and '(xy+xci yci) 'co))))

;**********************************************************

; A configuration of a circuit is a hash table giving a value (0 or 1)
; for each wire in the circuit.  A table is a list key-value pairs,
; such that the key is the symbol for the wire and the value is either
; 0 or 1.


; Examples

; Two configurations of the wires of the eq1-ckt Note that the order
; of wires in the configuration is that returned by (all-wires
; eq1-ckt).

(define eq1-config1x
  (make-hash '((x . 0) (y . 1) (z  . 0) (cx  . 0)
	       (cy  . 0) (t1  . 0) (t2  . 0))))

(define eq1-config2x
  (make-hash '((x . 0) (y . 0) (z  . 0) (cx  . 1)
	       (cy  . 1) (t1  . 0) (t2  . 0))))

; Two configurations of the wires of the sel-ckt

(define sel-config1x
  (make-hash '((x1 . 0) (x0 . 1) (y1 . 1) (y0 . 0) (s . 1) 
	(z1 . 0) (z0 . 0) (sc . 0) (u1 . 0) (v1 . 0)
	(u0 . 0) (v0 . 0))))

(define sel-config2x 
  (make-hash '((x1 . 1) (x0 . 1) (y1 . 0) (y0 . 0) (s . 0) 
	(z1 . 0) (z0 . 0) (sc . 1) (u1 . 0) (v1 . 0)
	(u0 . 0) (v0 . 0))))


; Two configurations of the wires of the latch-ckt

(define latch-config1x 
  (make-hash '((x . 0) (y . 0) (q . 0) (u . 0) )))

(define latch-config2x 
  (make-hash '((x . 0) (y . 1) (q . 1) (u . 0) )))

;**********************************************************
; ** problem 4 ** (10 points)
; Write a procedure 

;(next-value wire circuit config)

; that returns the value on the given wire of the given circuit,
; *after one gate delay* starting with the given configuration config
; of the circuit.

; You may assume that
; (1) circuit is a well-formed circuit, according to the specifications in problem 1,
; (2) the given wire is one of the wires of circuit, and
; (3) the given configuration config specifies a value for every wire in the circuit.

; If the given wire is an input wire of the circuit,
; its next value is just its value in the configuration config.

; If the given wire is the output wire of a gate, its next value is
; obtained by finding the gate of circuit for which it is the output
; wire, looking up the values of the input wires of the gate in the
; configuration config, and applying the appropriate function of the
; gate to the input values.

; Note that this doesn't compute the *eventual* value (if any) of the wire, 
; just the *next* value of the wire, after one gate delay.

; You may want to write an auxiliary procedure to look up the value of a wire in a configuration.

; Examples
 ;(next-value 'cx eq1-ckt eq1-config1x) => 1
; (next-value 't2 eq1-ckt eq1-config1x) => 0
; (next-value 'z eq1-ckt eq1-config2x) => 0
; (next-value 'x0 sel-ckt sel-config1x) => 1
; (next-value 'v1 sel-ckt sel-config1x) => 1
; (next-value 'v0 sel-ckt sel-config2x) => 0
;**********************************************************

; returns the value of a wire in a config, returns #f if the wire is not in the config
; (wire-value 'x eq1-config1) => 0
(define (wire-value wire config)
  (hash-ref config wire #f))

(define (next-value wire circuit config)
  (cond
    [(equal? #f (find-gate wire circuit))
     (wire-value wire config)]
    [else
     (define gate1 (find-gate wire circuit))
     (cond
       [(equal? (gate-type gate1) 'not)
        (remainder (+ (wire-value (first (gate-inputs gate1)) config) 1) 2)]
       [(equal? (gate-type gate1) 'and)
        (if (equal? (+ (wire-value (first (gate-inputs gate1)) config) (wire-value (last (gate-inputs gate1)) config)) 2)
            1
            0)]
       [(equal? (gate-type gate1) 'or)
        (if (equal? (+ (wire-value (first (gate-inputs gate1)) config) (wire-value (last (gate-inputs gate1)) config)) 0)
            0
            1)]
       [(equal? (gate-type gate1) 'xor)
        (if (equal? (+ (wire-value (first (gate-inputs gate1)) config) (wire-value (last (gate-inputs gate1)) config)) 1)
            1
            0)]
       [(equal? (gate-type gate1) 'nand)
        (if (equal? (+ (wire-value (first (gate-inputs gate1)) config) (wire-value (last (gate-inputs gate1)) config)) 2)
            0
            1)]
       [(equal? (gate-type gate1) 'nor)
        (if (equal? (+ (wire-value (first (gate-inputs gate1)) config) (wire-value (last (gate-inputs gate1)) config)) 0)
            1
            0)])]))

;**********************************************************
; ** problem 5 ** (10 points)
; Write a procedure 

; (next-config circuit config)

; that takes a circuit and a current configuration config and returns
; the "next" configuration of the circuit, after *one gate delay* has
; elapsed.

; In the "next" configuration of the circuit the value of each wire is
; the result of applying the next-value procedure to the wire, circuit
; and the configuration config.  Note that only the values of wires in
; config are used for inputs, not the new values.

; Thus, values on the input wires do not change, and each wire that is
; the output of a gate has the value determined by its gate function
; applied the values of its input wires in the configuration config.

; This is a rather simplified model of the time-varying behavior of
; wires and gates.

; Examples
;> (next-config eq1-ckt eq1-config1x)
; '#hash((cx . 1) (cy . 0) (t1 . 0) (t2 . 0) (x . 0) (y . 1) (z . 0))

;> (next-config eq1-ckt eq1-config2x)
; '#hash((cx . 1) (cy . 1) (t1 . 0) (t2 . 1) (x . 0) (y . 0) (z . 0))

;> (next-config sel-ckt sel-config1x)
; '#hash((s . 1) (sc . 0) (u0 . 0) (u1 . 0) (v0 . 0) (v1 . 1)
;        (x0 . 1) (x1 . 0) (y0 . 0) (y1 . 1) (z0 . 0) (z1 . 0))

;> (next-config sel-ckt (next-config sel-ckt sel-config1x))
; '#hash((s . 1) (sc . 0) (u0 . 0) (u1 . 0)  (v0 . 0) (v1 . 1)
;        (x0 . 1) (x1 . 0) (y0 . 0) (y1 . 1) (z0 . 0) (z1 . 1))

;> (next-config latch-ckt latch-config1x)
; '#hash((q . 1) (u . 1) (x . 0) (y . 0))

;> (next-config latch-ckt latch-config2x) 
; '#hash((q . 1) (u . 0) (x . 0) (y . 1))

;**********************************************************

(define (next-config circuit config)
  (make-hash (map (lambda (wire)
                    (cons wire (next-value wire circuit config)))
                  (all-wires circuit))))

;**********************************************************
; ** problem 6 ** (10 points)
; Write four procedures

; (stable? circuit config)
; (all-stable-configs circuit)
; (output-values circuit config)
; (init-config circuit input-values)

; (stable? circuit config)
; returns #t if the next configuration of the circuit after the
; configuration config is the same as config, ie, this configuration
; is stable for the circuit.

; (all-stable-configs circuit)
; returns a list of all the stable configurations of the circuit.  The
; wires in the configurations should be listed in the same order as
; (all-wires circuit), and the values in the configurations list
; should be in increasing order, considered as binary numbers.

; (output-values circuit config)
; returns a list giving the Boolean values of each of the output wires
; of the circuit in the configuration config.  The order is the same
; as the list of output wires of the circuit.

; (init-config circuit input-values)
; takes a circuit and a list input-values of Boolean values which has
; the same length as the number of inputs of the circuit and returns a
; configuration in which the circuit input wires have the values
; specified (in order) by the list inputs, and all other wires have
; the value 0.

; Examples

; (stable? eq1-ckt (make-hash '((x . 0) (y . 0) (z . 1) (cx . 1) 
;    (cy . 1) (t1 . 0) (t2 . 1)))) => #t

; (stable? eq1-ckt (make-hash '((x . 0) (y . 0) (z . 0)
;    (cx . 1) (cy . 0) (t1 . 1) (t2 . 0)))) => #f

;> (all-stable-configs eq2-ckt)
;'(#hash((w . 0) (x . 0) (y . 0) (z . 1))
;  #hash((w . 1) (x . 0) (y . 1) (z . 0))
;  #hash((w . 1) (x . 1) (y . 0) (z . 0))
;  #hash((w . 0) (x . 1) (y . 1) (z . 1)))

;> (all-stable-configs seq-or-ckt)
; '(#hash((x . 0) (z . 0)) #hash((x . 0) (z . 1)) #hash((x . 1) (z . 1)))

; (output-values eq1-ckt eq1-config2x) => '(0)
; (output-values latch-ckt latch-config2x) => '(1 0)

; (init-config eq1-ckt '(1 0)) => 
; '#hash((cx . 0) (cy . 0) (t1 . 0) (t2 . 0) (x . 1) (y . 0) (z . 0))

; (init-config clock-ckt '()) => '#hash((z . 0))

;**********************************************************

(define (stable? circuit config)
  (if (equal? config (next-config circuit config))
      #t
      #f))

; all-combs from HW4
(define (all-combs n)
  (cond
    [(equal? n 0)
     '(())]
    [else
     (append (map (lambda (lst) (cons 0 lst)) (all-combs (- n 1)))
             (map (lambda (lst) (cons 1 lst)) (all-combs (- n 1))))]))

; returns a config with assigned values according to a combination of 1s and 2s,
; given a list of wires and a combination
; (create-config '(x y) '(0 1)) => '#hash((x . 0) (y . 1))
(define (create-config wires comb)
  (make-hash (map (lambda (wire val)
                    (cons wire val))
                  wires
                  comb)))

(define (all-stable-configs circuit)
  (define all-wires1 (all-wires circuit))
  (foldr (lambda (comb result)
           (define config1 (create-config all-wires1 comb))
           (if (stable? circuit config1)
               (cons config1 result)
               result))
         '()
         (all-combs (length all-wires1))))

(define (output-values circuit config)
  (map (lambda (wire)
         (next-value wire circuit config))
       (ckt-outputs circuit)))

(define (init-config circuit input-values)
  (create-config (all-wires circuit)
                 (append input-values (build-list (- (length (all-wires circuit)) (length input-values))
                                                    (lambda (x) 0)))))

; *********************************************************
; ** problem 7 ** (10 points)
; Write a procedure 

; (simulate circuit config n)

; which simulates the given circuit from the given configuration by
; repeatedly calling next-config until either the configuration
; reached is stable, or next-config has been called n times, whichever
; occurs first.

; Examples
;> (simulate clock-ckt (make-hash '((z . 0))) 4)
; '(#hash((z . 0)) #hash((z . 1)) #hash((z . 0)) #hash((z . 1)) #hash((z . 0)))

;> (simulate eq1-ckt eq1-config1x 5) =>
; '(#hash((cx . 0) (cy . 0) (t1 . 0) (t2 . 0) (x . 0) (y . 1) (z . 0))
;   #hash((cx . 1) (cy . 0) (t1 . 0) (t2 . 0) (x . 0) (y . 1) (z . 0)))

;> (simulate sel-ckt sel-config1x 5) =>
;'(#hash((s . 1) (sc . 0) (u0 . 0) (u1 . 0) (v0 . 0) (v1 . 0)
;    (x0 . 1)  (x1 . 0) (y0 . 0) (y1 . 1) (z0 . 0) (z1 . 0))
;  #hash((s . 1) (sc . 0) (u0 . 0) (u1 . 0) (v0 . 0) (v1 . 1)
;    (x0 . 1) (x1 . 0) (y0 . 0) (y1 . 1) (z0 . 0) (z1 . 0))
;  #hash((s . 1) (sc . 0) (u0 . 0) (u1 . 0) (v0 . 0) (v1 . 1)
;    (x0 . 1) (x1 . 0) (y0 . 0) (y1 . 1) (z0 . 0) (z1 . 1)))

;> (simulate latch-ckt latch-config2x 3) =>
; '(#hash((q . 1) (u . 0) (x . 0) (y . 1)))

;> (simulate eq2-ckt (init-config eq2-ckt '(0 1)) 5) =>
;'(#hash((w . 0) (x . 0) (y . 1) (z . 0))
;  #hash((w . 1) (x . 0) (y . 1) (z . 1))
;  #hash((w . 1) (x . 0) (y . 1) (z . 0)))

;**********************************************************

(define (simulate circuit config n)
  (cond
    [(equal? n 0)
     (list config)]
    [(stable? circuit config)
     (list config)]
    [else
     (cons config (simulate circuit (next-config circuit config) (- n 1)))]))

;**********************************************************
; ** problem 8 ** (10 points)
; Write a procedure

; (final-config circuit config)

; that takes a circuit and a configuration config for the circuit.  If
; the circuit would eventually reach a stable configuration from
; config, then (final-config circuit config) returns the stable
; configuration of the circuit that would be reached.

; Otherwise, (final-config circuit config) returns the symbol 'none.

; Examples

; (final-config clock-ckt (make-hash '((z . 0)))) => 'none

; (final-config eq1-ckt
;	      (make-hash '((x . 1) (y . 1) (z . 0) (cx . 0)
;			   (cy . 0) (t1 . 0) (t2 . 0)))) =>
; (make-hash '((x . 1) (y . 1) (z . 1) (cx . 0)
;    	     (cy . 0) (t1 . 1) (t2 . 0))))

; (final-config sel-ckt 
;	      (make-hash '((x1 . 0) (x0 . 0) (y1 . 1) (y0 . 0)
;			   (s . 0) (z1 . 1) (z0 . 1) (sc . 0)
;			   (u1 . 1) (v1 . 1) (u0 . 0) (v0 . 1)))) =>
;(make-hash '((x1 . 0) (x0 . 0) (y1 . 1) (y0 . 0)
;	     (s . 0) (z1 . 0) (z0 . 0) (sc . 1)
;	     (u1 . 0) (v1 . 0) (u0 . 0) (v0 . 0)))
      
; (final-config latch-ckt (make-hash '((x . 1) (y . 1) (q . 0) (u . 0)))) =>
;       'none

; (final-config latch-ckt (make-hash '((x . 1) (y . 1) (q . 0) (u . 1)))) =>
; (make-hash '((x . 1) (y . 1) (q . 0) (u . 1))))


;**********************************************************

(define (final-config circuit config)
  (define simulation (simulate circuit config (expt 2 (length (all-wires circuit)))))
  (cond
    [(< (length simulation) (expt 2 (length (all-wires circuit))))
     (last simulation)]
    [else
     'none]))

;**********************************************************
; ** problem 9 ** (10 points)
; Define a 4-bit ripple-carry adder circuit as described in lecture
; using the circuit representation developed above.

; Please name it: add-ckt

; Its inputs are x3, x2, x1, x0, y3, y2, y1, y0  (in order)
; Its outputs are z4, z3, z2, z1, z0 (in order)
; What it computes is the sum of the two 4-bit binary numbers
; represented by the x's and the y's.
; For example, if the inputs are 
; x3 = 1, x2 = 0, x1 = 0, x0 = 1    (representing 9 in binary)
; y3 = 1, y2 = 1, y1 = 0, y0 = 1    (representing 13 in binary)
; then the output should be
; z4 = 1, z3 = 0, z2 = 1, z1 = 1, z0 = 0 (representing 22 in binary)

; Examples:
; (good-circuit? add-ckt) => #t
; (ckt-inputs add-ckt) => '(x3 x2 x1 x0 y3 y2 y1 y0)
; (ckt-outputs add-ckt) => '(z4 z3 z2 z1 z0)
; (output-values add-ckt (final-config add-ckt (init-config add-ckt '(1 0 0 1 1 1 0 1)))) => '(1 0 1 1 0)
; (output-values add-ckt (final-config add-ckt (init-config add-ckt '(0 1 1 1 0 1 1 0)))) => '(0 1 1 0 1)

; For the last two tests, your procedures output-values, final-config,
; init-config must be working.

; You may construct the circuit entirely by hand,
; or you may choose to write procedures to construct your circuit.
;**********************************************************

; creates a half-adder circuit given two inputs and two outputs
(define (ha-ckt-maker inputs outputs)
  (define a (first inputs))
  (define b (second inputs))
  (define c (first outputs))
  (define d (second outputs))
  (ckt (list a b)
       (list c d)
       (list
        (gate 'xor (list a b) c)
        (gate 'and (list a b) d))))

; creates a full-adder circuit given three inputs, two outputs, and five arbitrary wire symbols
(define (fa-ckt-maker inputs outputs arbitrary-wires)
  (define a (first inputs))
  (define b (second inputs))
  (define c (third inputs))
  (define d (first outputs))
  (define e (second outputs))
  (define f (first arbitrary-wires))
  (define g (second arbitrary-wires))
  (define h (third arbitrary-wires))
  (define i (fourth arbitrary-wires))
  (define j (fifth arbitrary-wires))
  (ckt (list a b c)
       (list d e)
       (list
        (gate 'xor (list a b) f)
        (gate 'xor (list f c) d)
        (gate 'or (list a b) g)
        (gate 'or (list a c) h)
        (gate 'or (list b c) i)
        (gate 'and (list g h) j)
        (gate 'and (list j i) e))))

(define add-ckt
  (ckt '(x3 x2 x1 x0 y3 y2 y1 y0)
       '(z4 z3 z2 z1 z0)
       (append
        (ckt-gates (ha-ckt-maker '(x0 y0) '(z0 c1)))
        (append
         (ckt-gates (fa-ckt-maker '(x1 y1 c1) '(z1 c2) '(A1 B1 C1 D1 E1)))
         (append
          (ckt-gates (fa-ckt-maker '(x2 y2 c2) '(z2 c3) '(A2 B2 C2 D2 E2)))
          (append
           (ckt-gates (fa-ckt-maker '(x3 y3 c3) '(z3 z4) '(A3 B3 C3 D3 E3)))))))))

;**********************************************************
; ** problem 10 ** (5 points)

; Define a D-flipflop as described in lecture, using the given
; representation of circuits.  Please name it: dff-ckt.

; It has inputs:  s, d (in order) and outputs q, qc (in order)

; Examples
; (good-circuit? dff-ckt) => #t
; (ckt-inputs dff-ckt) => '(s d)
; (ckt-outputs dff-ckt) => '(q qc)
;> (output-values dff-ckt (final-config dff-ckt (init-config dff-ckt '(1 0))))
;'(0 1)
;> (output-values dff-ckt (final-config dff-ckt (init-config dff-ckt '(1 1))))
;'(1 0)
;**********************************************************

(define dff-ckt
  (ckt
   '(s d)
   '(q qc)
   (list
    (gate 'not '(d) 'nd)
    (gate 'nand '(s d) 'x)
    (gate 'nand '(s nd) 'y)
    (gate 'nand '(q y) 'qc)
    (gate 'nand '(qc x) 'q))))

;**********************************************************
; ** problem 11 ** (5 points)

; Design a circuit using the given representation that has no inputs
; and one output 't.  When the circuit is started in the initial (all
; zero) configuration, after a few configurations, the output is 1 for
; one step, then 0 for 4 steps, then 1 for one step, then 0 for 4
; steps, then 1 for one step, and so on.

; Please name your circuit timing-ckt.

; Examples
; (good-circuit? timing-ckt) => #t
; (ckt-inputs timing-ckt) => '()
; (ckt-outputs timing-ckt) => '(t)
;> (map (lambda (config) (output-values timing-ckt config)) (simulate timing-ckt (init-config timing-ckt '()) 20))
;'((0) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1))
;**********************************************************

(define timing-ckt
  (ckt
   '()
   '(t)
   (list
    (gate 'not '(a) 'b)
    (gate 'or '(b b) 'c)
    (gate 'or '(c c) 'd)
    (gate 'or '(d d) 'e)
    (gate 'or '(e e) 'a)
    (gate 'or '(f b) 'f)
    (gate 'xor  '(a e) 'g)
    (gate 'and '(g f) 't))))

;**********************************************************
; ** problem 12 ** (5 points)

; Design a circuit using the given representation that has no inputs
; and one output 't.  When the circuit is started in the initial (all
; zero) configuration, after a few configurations, the output is 1 for
; one step, then 0 forever after.

; Please name your circuit one-one-ckt.

; Examples
; (good-circuit? one-one-ckt) => #t
; (ckt-inputs one-one-ckt) => '()
; (ckt-outputs one-one-ckt) => '(t)
;> (map (lambda (config) (output-values one-one-ckt config)) (simulate one-one-ckt (init-config one-one-ckt '()) 20))
;'((0) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1))
;**********************************************************

(define one-one-ckt
  (ckt
   '()
   '(t)
   (list
    (gate 'or '(a a) 'a)
    (gate 'not '(a) 'b)
    (gate 'or '(b b) 'c)
    (gate 'xor '(b c) 't))))

; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

(define (testold name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    '***OK***
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((OK (if (procedure? expected)
		    	(expected got)
			(equal? got expected)))
		(prefix (if OK
			    '***OK***
			    '***X***)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))

(test 'hours hours (lambda (x) (> x 0)))



(test 'good-gate? (good-gate? gate1) #t)
(test 'good-gate? (good-gate? gate2) #t)
(test 'good-gate? (good-gate? gate3) #t)
(test 'good-gate? (good-gate? gate4) #t)

(test 'good-gate? (good-gate? (gate 'not 'x 'y)) #f)
(test 'good-gate? (good-gate? (gate 'nor '("x" "y") "z")) #f)
(test 'good-gate? (good-gate? (gate 'and '(1 2) 3)) #f)
(test 'good-gate? (good-gate? (gate 'equal '(x y) 'z)) #f)
(test 'good-gate? (good-gate? (gate 'or '(w x y) 'z)) #f)

(test 'good-circuit? (good-circuit? sel-ckt) #t)
(test 'good-circuit? (good-circuit? eq1-ckt) #t)
(test 'good-circuit? (good-circuit? eq2-ckt) #t)
(test 'good-circuit? (good-circuit? latch-ckt) #t)
(test 'good-circuit? (good-circuit? seq-or-ckt) #t)
(test 'good-circuit? (good-circuit? clock-ckt) #t)

(test 'good-circuit? (good-circuit? 'hi) #f)
(test 'good-circuit? (good-circuit? (ckt '() '() '())) #t)
(test 'good-circuit? (good-circuit? (ckt '(x y) '(z) (list (gate 'and '(x y) 'x) (gate 'or '(x y) 'z)))) #f)
(test 'good-circuit? (good-circuit? (ckt '(x y) '(z) (list (gate 'nor '(x y) 'z) (gate 'nand '(x y) 'z)))) #f)
(test 'good-circuit? (good-circuit? (ckt '(x y) '(u z) (list (gate 'or '(x y) 'z)))) #f)


(test 'all-wires (all-wires eq1-ckt) '(x y z cx cy t1 t2))
(test 'all-wires (all-wires sel-ckt) '(x1 x0 y1 y0 s z1 z0 sc u1 v1 u0 v0))
(test 'find-gate (find-gate 't2 eq1-ckt) (gate 'and '(cx cy) 't2))
(test 'find-gate (find-gate 'w eq2-ckt) (gate 'xor '(x y) 'w))
(test 'find-gate (find-gate 'y sel-ckt) #f)


(test 'ha-ckt (good-circuit? ha-ckt) #t)
(test 'fa-ckt (good-circuit? fa-ckt) #t)
(test 'ha-ckt (ckt-inputs ha-ckt) '(x y))
(test 'ha-ckt (ckt-outputs ha-ckt) '(z co))
(test 'fa-ckt (ckt-inputs fa-ckt) '(x y ci))
(test 'fa-ckt (ckt-outputs fa-ckt) '(z co))

(test 'ha-ckt (output-values ha-ckt 
			     (final-config ha-ckt 
					   (init-config ha-ckt '(1 1)))) 
      '(0 1))
(test 'fa-ckt (output-values fa-ckt (final-config fa-ckt (init-config fa-ckt '(1 1 1)))) '(1 1))


(test 'next-value (next-value 'cx eq1-ckt eq1-config1x) 1)
(test 'next-value (next-value 't2 eq1-ckt eq1-config1x) 0)
(test 'next-value (next-value 'z eq1-ckt eq1-config2x) 0)
(test 'next-value (next-value 'x0 sel-ckt sel-config1x) 1)
(test 'next-value (next-value 'v1 sel-ckt sel-config1x) 1)
(test 'next-value (next-value 'v0 sel-ckt sel-config2x) 0)


(test 'next-config (next-config eq1-ckt eq1-config1x)
      (make-hash '((x . 0) (y . 1) (z . 0) (cx . 1) (cy . 0) (t1
. 0) (t2 . 0))))

(test 'next-config (next-config eq1-ckt eq1-config2x)
      (make-hash '((x . 0) (y . 0) (z . 0) (cx . 1) (cy . 1) (t1 . 0) (t2 . 1))))

(test 'next-config (next-config sel-ckt sel-config1x)
      (make-hash '((x1 . 0) (x0 . 1) (y1 . 1) (y0 . 0) (s . 1) (z1 . 0) (z0 . 0)
	    (sc . 0) (u1 . 0) (v1 . 1) (u0 . 0) (v0 . 0))))

(test 'next-config (next-config sel-ckt (next-config sel-ckt sel-config1x))
      (make-hash '((x1 . 0) (x0 . 1) (y1 . 1) (y0 . 0) (s . 1) (z1 . 1) (z0 . 0)
	    (sc . 0) (u1 . 0) (v1 . 1) (u0 . 0) (v0 . 0))))

(test 'next-config (next-config latch-ckt latch-config1x)
      (make-hash '((x . 0) (y . 0) (q . 1) (u . 1))))


(test 'next-config (next-config latch-ckt latch-config2x)
      (make-hash '((x . 0) (y . 1) (q . 1) (u . 0))))



(test 'stable? (stable? eq1-ckt (make-hash '((x . 0) (y . 0) (z . 1)
(cx . 1) (cy . 1) (t1 . 0) (t2 . 1)))) #t)

(test 'stable? (stable? eq1-ckt (make-hash '((x . 0) (y . 0) (z . 0)
(cx . 1) (cy . 0) (t1 . 1) (t2 . 0)))) #f)

(test 'all-stable-configs (all-stable-configs eq2-ckt)
      (list
       (make-hash '((x . 0) (y . 0) (z . 1) (w . 0)))
       (make-hash '((x . 0) (y . 1) (z . 0) (w . 1)))
       (make-hash '((x . 1) (y . 0) (z . 0) (w . 1)))
       (make-hash '((x . 1) (y . 1) (z . 1) (w . 0)))))

(test 'all-stable-configs (all-stable-configs seq-or-ckt)
      (list
       (make-hash '((x . 0) (z . 0)))
       (make-hash '((x . 0) (z . 1)))
       (make-hash '((x . 1) (z . 1)))))

(test 'output-values (output-values eq1-ckt eq1-config2x) '(0))
(test 'output-values (output-values latch-ckt latch-config2x) '(1 0))

(test 'init-config (init-config eq1-ckt '(1 0))
      (make-hash '((x . 1) (y . 0) (z . 0) (cx . 0) (cy . 0) (t1 . 0) (t2 . 0))))

(test 'init-config (init-config clock-ckt '()) (make-hash '((z . 0))))

(test 'simulate (simulate clock-ckt (make-hash '((z . 0))) 4)
      (list (make-hash '((z . 0)))
	    (make-hash '((z . 1)))
	    (make-hash '((z . 0)))
	    (make-hash '((z . 1)))
	    (make-hash '((z . 0)))))

(test 'simulate (simulate eq1-ckt eq1-config1x 5)
      (list
       (make-hash '((x . 0) (y . 1) (z . 0) (cx . 0) (cy . 0) (t1 . 0) (t2 . 0)))
       (make-hash '((x . 0) (y . 1) (z . 0) (cx . 1) (cy . 0) (t1 . 0) (t2 . 0)))))

(test 'simulate (simulate sel-ckt sel-config1x 5)
      (list
       (make-hash '((x1 . 0) (x0 . 1) (y1 . 1) (y0 . 0) (s . 1) (z1 . 0)
		    (z0 . 0) (sc . 0) (u1 . 0) (v1 . 0) (u0 . 0) (v0 . 0)))
       (make-hash '((x1 . 0) (x0 . 1) (y1 . 1) (y0 . 0) (s . 1) (z1 . 0)
		    (z0 . 0) (sc . 0) (u1 . 0) (v1 . 1) (u0 . 0) (v0 . 0)))
       (make-hash '((x1 . 0) (x0 . 1) (y1 . 1) (y0 . 0) (s . 1) (z1 . 1)
		    (z0 . 0) (sc . 0) (u1 . 0) (v1 . 1) (u0 . 0) (v0 . 0)))))

(test 'simulate (simulate latch-ckt latch-config2x 3)
      (list
       (make-hash '((x . 0) (y . 1) (q . 1) (u . 0)))))

(test 'simulate (simulate eq2-ckt (init-config eq2-ckt '(0 1)) 5)
      (list
       (make-hash '((x . 0) (y . 1) (z . 0) (w . 0)))
       (make-hash '((x . 0) (y . 1) (z . 1) (w . 1)))
       (make-hash '((x . 0) (y . 1) (z . 0) (w . 1)))))

(test 'final-config (final-config clock-ckt (make-hash '((z . 0))))
      'none)

(test 'final-config (final-config eq1-ckt
				  (make-hash '((x . 1) (y . 1) (z . 0) (cx . 0)
					       (cy . 0) (t1 . 0) (t2 . 0))))
      (make-hash '((x . 1) (y . 1) (z . 1) (cx . 0)
		   (cy . 0) (t1 . 1) (t2 . 0))))

(test 'final-config (final-config
		     sel-ckt 
		     (make-hash '((x1 . 0) (x0 . 0) (y1 . 1) (y0 . 0)
				  (s . 0) (z1 . 1) (z0 . 1) (sc . 0)
				  (u1 . 1) (v1 . 1) (u0 . 0) (v0 . 1))))
      (make-hash '((x1 . 0) (x0 . 0) (y1 . 1) (y0 . 0)
		   (s . 0) (z1 . 0) (z0 . 0) (sc . 1)
		   (u1 . 0) (v1 . 0) (u0 . 0) (v0 . 0))))
      
(test 'final-config (final-config latch-ckt
				  (make-hash '((x . 1) (y . 1) (q . 0) (u . 0))))
      'none)

(test 'final-config (final-config
		     latch-ckt
		     (make-hash '((x . 1) (y . 1) (q . 0) (u . 1))))
      (make-hash '((x . 1) (y . 1) (q . 0) (u . 1))))


(test 'add-ckt (good-circuit? add-ckt) #t)
(test 'add-ckt (ckt-inputs add-ckt) '(x3 x2 x1 x0 y3 y2 y1 y0))
(test 'add-ckt (ckt-outputs add-ckt) '(z4 z3 z2 z1 z0))
(test 'add-ckt (output-values add-ckt (final-config add-ckt (init-config add-ckt '(1 0 0 1 1 1 0 1)))) '(1 0 1 1 0))
(test 'add-ckt (output-values add-ckt (final-config add-ckt (init-config add-ckt '(0 1 1 1 0 1 1 0)))) '(0 1 1 0 1))


(test 'dff-ckt (good-circuit? dff-ckt) #t)
(test 'dff-ckt (ckt-inputs dff-ckt) '(s d))
(test 'dff-ckt (ckt-outputs dff-ckt) '(q qc))
(test 'dff-ckt (output-values dff-ckt (final-config dff-ckt (init-config dff-ckt '(1 0)))) '(0 1))
(test 'dff-ckt (output-values dff-ckt (final-config dff-ckt (init-config dff-ckt '(1 1)))) '(1 0))

 
(test 'timing-ckt (good-circuit? timing-ckt)  #t)
(test 'timing-ckt (ckt-inputs timing-ckt)  '())
(test 'timing-ckt (ckt-outputs timing-ckt)  '(t))
(test 'timing-ckt (map (lambda (config) (output-values timing-ckt config)) (simulate timing-ckt (init-config timing-ckt '()) 20))
      '((0) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1)))

(test 'one-one-ckt (good-circuit? one-one-ckt)  #t)
(test 'one-one-ckt (ckt-inputs one-one-ckt)  '())
(test 'one-one-ckt (ckt-outputs one-one-ckt)  '(t))
(test 'one-one-ckt (map (lambda (config) (output-values one-one-ckt config)) (simulate one-one-ckt (init-config one-one-ckt '()) 20))
      '((0) (0) (1) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0)))



;**************  end of hw # 5  ************************************
