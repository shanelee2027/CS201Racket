#lang racket

(provide hours
         ins ins-c-state ins-c-symbol ins-n-state ins-n-symbol ins-dir
         tm-reverse
         i-match? i-lookup
         conf conf-state conf-ltape conf-symbol conf-rtape
         halted? change-state write-symbol
	 normalize
         shift-head-left shift-head-right
         next-config
	 tm-convert
	 tm-sort
	 )

; Please do not modify the lines above this comment.
; ********************************************************
; CS 201 HW #3  DUE Wednesday, 10/4/2023 at 11:59 pm
;                via the submit system on the Zoo
; ****************************************************************
; Name: Shane Lee
; Email address: shane.lee@yale.edu
; ****************************************************************

; Unless the problem specifies otherwise:
; (a) You may solve the problem using any method and any Racket constructs 
; (*except* mutators, that is, set! and its relatives.)
; (b) You may write auxiliary procedure(s) in addition to the one(s) specified in the problem.  
; Please include a comment for each one specifying what it does and giving one or more examples of it.
; (c) Please make your code as clear and readable as possible.

; The topics of this assignment are:
; a simulator for Turing machines and writing Turing machine programs.

; ****************************************************************
; ** problem 0 ** (1 easy point)

; Modify the following definition to reflect the number of hours you
; spent on this assignment.

(define hours 3)

; ********************************************************
; ** problem 00 ** (1 fairly easy point)

; Below is a UNIX transcript with one command replaced by XXXX

(define transcript "
bash-4.4$ ls
bash-4.4$ date 
Tue Jan 29 17:08:02 EST 2019
bash-4.4$ xxxx
bash-4.4$ cat file
Tue Jan 29 17:08:11 EST 2019
bash-4.4$ ls -l
total 0
-rw-rw-r-- 1 sbs5 sbs5 29 Jan 29 17:08 file
")

; define xxxx below to be the correct UNIX command.

(define xxxx "date >>file")

; ****************************************************************
; Turing machines were described in the lectures; see also the lecture
; notes on the course web page.  Here is a top-level procedure to
; simulate a Turing machine starting from a given configuration until
; either it halts or it has executed n steps.  The procedure returns
; the list of the successive configurations of the computation,
; starting with the initial one.  The length of the list of
; configurations is one more than the number of steps taken by the
; machine.

(define (simulate mach config n) 
  (cond
    ((<= n 0) (list config))
    ((halted? mach config) (list config))
    (else
     (cons config
           (simulate 
            mach (next-config mach config) (- n 1))))))

; mach is a representation of a Turing machine
; config is a representation of a configuration of the machine
; n is the maximum number of steps to simulate

; The procedures halted? and next-config will be written by you in the
; problems below; you will then have a complete Turing machine
; simulator.

; The test solutions at the bottom specify a number of steps, n, that
; the simulation should run before stopping.  Your code may in fact
; need more steps to solve the given problem.  The auto-grade program
; will account for this, allowing roughly twice as many steps for your
; code to run.  Still, there is a limit.  We have yet to solve the
; halting problem.

;; (simulate-lite tm config n) is like simulate, but does not return
;; the intermediate states - just the final tape contents.  Thus, we
;; can use simulate-lite in the public tests without revealing the
;; Turing machine instructions.

(define (simulate-lite tm config n) 
  (cond
    ((<= n 0) 'timeout)
    ((halted? tm config) (list (conf-ltape config)
    			       (conf-symbol config)
			       (conf-rtape config)))
    (else
     (simulate-lite
      tm (next-config tm config) (- n 1)))))

; ****************************************************************
; Turing machine representation.

; A Turing machine is represented as a list of instructions, 
; where each instruction is a 5-tuple, represented as a struct
; defined as follows:

(struct ins (c-state c-symbol n-state n-symbol dir) #:transparent) 

; The fields represent the following components of an instruction:
; current state, current symbol, new state, new symbol, and move direction

; The current state and new state are Racket symbols, the current
; symbol and new symbol are Racket symbols or non-negative integers
; and the move direction must be either the symbol 'L or the symbol
; 'R, representing a move to the left or right, respectively.

; Example

(define i1 (ins 'q1 0 'q3 1 'L))

; creates an instruction with current state 'q1, current symbol 0, new
; state 'q3, new symbol 1, and move direction 'L, and names it i1.

; Because we've made ins "transparent", its field values
; will be printed out.
; > i1
; (ins 'q1 0 'q3 1 'L)

; We can access the components of i1 via the structure selectors:
; (ins-c-state i1) => 'q1
; (ins-c-symbol i1) => 0
; (ins-n-state i1) => 'q3
; (ins-n-symbol i1) => 1
; (ins-dir i1) => 'L

; Example (from lecture):

; a Turing machine that when started in state 'q1 on the leftmost of a
; string of 0's and 1's changes all the 0's to 1's and all the 1's to
; 0's and then returns the head to the leftmost symbol and halts.

; (next-config tm1 (conf 'q1 '() 0 '(0 1))) => (conf 'q1 '(1) 0 '(1))

(define tm1 
  (list
   (ins 'q1 0 'q1 1 'R)
   (ins 'q1 1 'q1 0 'R)
   (ins 'q1 'b 'q2 'b 'L)
   (ins 'q2 0 'q2 0 'L)
   (ins 'q2 1 'q2 1 'L)
   (ins 'q2 'b 'q3 'b 'R)))

; a Turing machine that copies its tape to itself.  See lecture notes.

(define tmcopy
  (list                        
   (ins 'q1 0 'q1 0 'R)        
   (ins 'q1 1 'q1 1 'R)        
   (ins 'q1 'b 'q2 'c 'L)      
   (ins 'q2 0 'q2 0 'L)        
   (ins 'q2 1 'q2 1 'L)        
   (ins 'q2 'b 'q3 'b 'R)      
   (ins 'q3 0 'q4 'd 'R)       
   (ins 'q3 1 'q5 'e 'R)       
   (ins 'q3 'c 'q7 'c 'L) 
   (ins 'q4 0 'q4 0 'R) 
   (ins 'q4 1 'q4 1 'R) 
   (ins 'q4 'c 'q4 'c 'R) 
   (ins 'q4 'b 'q6 0 'L) 
   (ins 'q5 0 'q5 0 'R) 
   (ins 'q5 1 'q5 1 'R) 
   (ins 'q5 'c 'q5 'c 'R) 
   (ins 'q5 'b 'q6 1 'L) 
   (ins 'q6 0 'q6 0 'L) 
   (ins 'q6 1 'q6 1 'L) 
   (ins 'q6 'c 'q6 'c 'L) 
   (ins 'q6 'd 'q3 0 'R) 
   (ins 'q6 'e 'q3 1 'R) 
   (ins 'q7 0 'q7 0 'L) 
   (ins 'q7 1 'q7 1 'L) 
   (ins 'q7 'b 'q8 'b 'R)
   ))


; ****************************************************************
; ** problem 1 (15 points)
; Define (in the format just given) a Turing machine named

; tm-reverse

; that takes an input string of 0's and 1's and produces an output
; string equal to the reverse of the input string.  When the machine
; halts, the head should be scanning the leftmost symbol of the
; output.

; That is, when started in state q1 with the head on the leftmost of a
; string of 0's and 1's, it halts with the head on the leftmost of a
; string of 0's and 1's, and the output string is obtained from the
; input string by reversing it.

; Your machine *may* use additional tape symbols but the output should
; contain no symbols other than 0, 1 and blank.  When the machine
; halts, symbols other than the output should be blank.

; Examples of the behavior of tm-reverse
; 1            =>  1
; 110          =>  011
; 0001         =>  1000
; 101011       =>  110101

; (test 'tm-reverse (simulate-lite tm-reverse (conf 'q1 '() 1 '()) 20) '(() 1 ()))
; (test 'tm-reverse (simulate-lite tm-reverse (conf 'q1 '() 1 '(1 0)) 200) '(() 0 (1 1)))
; (test 'tm-reverse (simulate-lite tm-reverse (conf 'q1 '() 0 '(0 0 1)) 200) '(() 1 (0 0 0)))
; (test 'tm-reverse (simulate-lite tm-reverse (conf 'q1 '() 1 '(0 1 0 1 1)) 200) '(() 1 (1 0 1 0 1)))


; (It may help to review ideas from the machine to make a copy of its input,
; described in lectures and in the online lecture notes.)

; The initial state of your machine should be q1 -- other states may
; be named with Racket symbols of your choice.

; IMPORTANT: please describe how your Turing machine works.  You'll be
; able to run it once you get the procedures for the simulator
; working.

; ****************************************************************

; q1 sets the left-most symbol to 'a or 'c and changes the state to q2 (0) or q3 (1) depending on whether it was originally 0 or 1, then moves to the right
; q2 (0) and q3 (1) move to the right until they find a blank, which is when they move one left and switch to q4 (0) or q5 (1)
; q4 (0) and q5 (1) set the symbol to 'd (0) or 'e (1), then depending on what the symbol originally was, moves left with either state q6 (0) or q7 (1)
; q6 and q7 move left until they encounter 'a or 'c, set it to either 0 (q6) or 1 (q7), then move right with q1, starting everything again
; all throughout there are checks to see if the process is done, the checks being that a letter has been found before any numbers while scanning in one of the various states
; if everything's done, q8 moves to the left correctly setting any leftover 'as to 0, 'cs to 1, 'ds to 0, and 'es to 1

(define tm-reverse
  (list                        
   (ins 'q1 0 'q2 'a 'R)
   (ins 'q1 1 'q3 'c 'R)
   (ins 'q1 'd 'q8 0 'L)
   (ins 'q1 'e 'q8 1 'L)
   (ins 'q1 'a 'q8 0 'L)
   (ins 'q1 'c 'q8 1 'L)
   (ins 'q2 0 'q2 0 'R)
   (ins 'q2 1 'q2 1 'R)
   (ins 'q2 'b 'q4 'b 'L)
   (ins 'q2 'd 'q4 0 'L)
   (ins 'q2 'e 'q4 1 'L)
   (ins 'q3 0 'q3 0 'R)
   (ins 'q3 1 'q3 1 'R)
   (ins 'q3 'b 'q5 'b 'L)
   (ins 'q3 'd 'q5 0 'L)
   (ins 'q3 'e 'q5 1 'L)
   (ins 'q4 0 'q6 'd 'L)
   (ins 'q4 1 'q7 'd 'L)
   (ins 'q4 'a 'q8 0 'L)
   (ins 'q4 'c 'q8 1 'L)
   (ins 'q5 0 'q6 'e 'L)
   (ins 'q5 1 'q7 'e 'L)
   (ins 'q5 'a 'q8 0 'L)
   (ins 'q5 'c 'q8 1 'L)
   (ins 'q6 0 'q6 0 'L)
   (ins 'q6 1 'q6 1 'L)
   (ins 'q6 'a 'q1 0 'R)
   (ins 'q6 'c 'q1 0 'R)
   (ins 'q7 0 'q7 0 'L)
   (ins 'q7 1 'q7 1 'L)
   (ins 'q7 'a 'q1 1 'R)
   (ins 'q7 'c 'q1 1 'R)
   (ins 'q8 0 'q8 0 'L)
   (ins 'q8 1 'q8 1 'L)
   (ins 'q8 'd 'q8 0 'L)
   (ins 'q8 'e 'q8 1 'L)
   (ins 'q8 'b 'q9 'b 'R)
   ))

; ****************************************************************
; ** problem 2 (10 points)

; Write the following two procedures.  Remember to use the instruction
; selectors: ins-c-state, ins-c-symbol, ins-n-state, ins-n-symbol,
; ins-dir

; (i-match? state symbol inst)

; returns #t if state and symbol are equal to the state and symbol of
; instruction inst otherwise returns #f

; (i-lookup state symbol mach)

; returns #f if no instruction of Turing machine mach has state and
; symbol equal to state and symbol otherwise returns the instruction
; in mach that matches.  You may assume that at most one instruction
; will match.

; The latter point is based on the requirement that Turing machines be
; deterministic, that is, there is only one way to execute a given
; program for a given input.  The alternative is non-determinism,
; as mentioned in class.

; For this assignment, when writing Turing machine programs (problems
; 1, 7, and 8) be certain that no two instructions have the same
; c-symbol and c-state (with differing n-state, n-symbol, or dir).

; Examples
; (i-match? 'q1 'b (ins 'q1 'b 'q3 'b 'L)) => #t
; (i-match? 'q1  0  (ins 'q1 1 'q4 1 'L)) => #f
; (i-match? 'q2 1 (ins 'q2 1 'q2 1 'L)) => #t
; (equal? (i-lookup 'q1 1 tm1) (ins 'q1 1 'q1 0 'R)) => #t
; (equal? (i-lookup 'q2 'b tm1) (ins 'q2 'b 'q3 'b 'R)) => #t
; (i-lookup 'q3 1 tm1) => #f
; ****************************************************************

(define (i-match? state symbol inst)
  (if (and (equal? state (ins-c-state inst)) (equal? symbol (ins-c-symbol inst)))
      #t
      #f))

(define (i-lookup state symbol mach)
  (define result (memf (lambda (instruction) (i-match? state symbol instruction)) mach))
  (if (list? result)
      (car result)
      #f))

; ****************************************************************
; Representation of a Turing machine configuration.
; We represent a Turing machine configuration using the following structure:

(struct conf (state ltape symbol rtape) #:transparent)

; where state is the current state of the machine,
; ltape is a list of symbols to the left of the currently scanned symbol,
; symbol is the currently scanned symbol,
; rtape is a list of symbols to the right of the currently scanned symbol.

; We reserve the symbol 'b for the blank.

; For example, we define the following two configurations:

(define config1 (conf 'q3 '(0 0) 1 '(1)))
(define config2 (conf 'q6 '(1 b) 0 '(b b)))

; Note that the selectors are
; conf-state, conf-ltape, conf-symbol, conf-rtape

; config1 represents the Turing machine configuration

;   --------------------------
;   .. | 0 | 0 | 1 | 1 |  | ..
;   --------------------------
;                ^
;                q3

; in which the non-blank symbols on the tape are 0011,
; and the machine is in state q3 with the read/write head
; scanning the leftmost 1.

; config2 represents the Turing machine configuration

;   ------------------------------
;   .. |   | 1 |  | 0 |   |   | ..
;   ------------------------------
;                   ^
;                   q6

; in which the symbols 1, blank, 0, are on the tape, surrounded
; by blanks, and the machine is in state q6 with the read/write
; head scanning the 0.

; A configuration is *normalized* if neither the first symbol of
; ltape nor the last symbol of rtape is the symbol 'b.
; Of the two configurations above, config1 is normalized, 
; but config2 is not (the last element of its rtape list is 'b.)

; Note that tape squares not explicitly represented are
; assumed to contain blanks.  A normalized configuration
; to represent the machine in state q1 with all tape squares
; blank is thus (conf 'q1 '() 'b '())).

; ****************************************************************
; ** problem 3 (9 points)
; Write the following three procedures.

; (halted? mach config)
; returns #t if the Turing machine mach is halted in machine configuration config 
; (ie, no instruction of the machine matches the current state and symbol 
; in configuration config) and returns #f otherwise.

; (change-state new-state config)
; takes a configuration config and returns a configuration
; in which the state of the machine is changed to new-state.

; (write-symbol new-symbol config) takes a configuration config and
; returns a configuration in which the symbol scanned by 
; the read/write head has been replaced by new-symbol.

; Examples
; (halted? tm1 (conf 'q1 '(1 1 0) 'b '())) => #f
; (halted? (list (ins 'q1 'b 'q2 'b 'R)) (conf 'q2 '() 'b '())) => #t
; (change-state 'q2 (conf 'q1 '(0) 1 '())) => (conf 'q2 '(0) 1 '())
; (change-state 'q13 (conf 'q4 '(0 1 1) 'b '())) => (conf 'q13 '(0 1 1) 'b '())
; (write-symbol 1 (conf 'q5 '(0) 0 '(1 1))) => (conf 'q5 '(0) 1 '(1 1))
; (write-symbol 'c (conf 'q2 '(0 0 1) 1 '(1 1))) => (conf 'q2 '(0 0 1) 'c '(1 1))
; (write-symbol 'b (conf 'q3 '(1) 0 '())) => (conf 'q3 '(1) 'b '())
; ****************************************************************

(define (halted? mach config)
  (not (ormap (lambda (instruction) (and (equal? (conf-state config) (ins-c-state instruction)) (equal? (conf-symbol config)(ins-c-symbol instruction)))) mach)))


(define (change-state new-state config)
  (conf new-state (conf-ltape config) (conf-symbol config) (conf-rtape config)))

(define (write-symbol new-symbol config)
  (conf (conf-state config) (conf-ltape config) new-symbol (conf-rtape config)))

; ****************************************************************
; ** problem 4 ** (10 points)
; Write one procedure

; (normalize config)
; takes a Turing machine configuration config and returns an equivalent 
; *normalized* configuration. That is, the same Turing machine configuration is
; represented by the input configuration and the output configuration, 
; and the output configuration does not have a 'b as the first element 
; of its ltape list or the last element of its rtape list.

; Examples
; (normalize config1) => (conf 'q3 '(0 0) 1 '(1))
; (normalize config2) => (conf 'q6 '(1 b) 0 '())
; (normalize (conf 'q3 '(b 0) 'b '(1 1 0 b b))) => (conf 'q3 '(0) 'b '(1 1 0))
; (normalize (conf 'q6 '(b 0 b 0) 1 '(0 b 0 b))) => (conf 'q6 '(0 b 0) 1 '(0 b 0))
; (normalize (conf 'q4 '(b b) 'b '(b b b))) => (conf 'q4 '() 'b '())
; ****************************************************************

(define (normalize config)
  (define (normalize-ltape ltape)
    (define result (memf (lambda (elem) (not (equal? 'b elem))) ltape))
    (if (equal? #f result)
        '()
        result))
  (conf (conf-state config) (normalize-ltape (conf-ltape config)) (conf-symbol config) (reverse (normalize-ltape (reverse (conf-rtape config))))))
                                                                                                                  

; ****************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (shift-head-left config)
; takes a normalized configuration config and returns a normalized configuration 
; in which the position of the read/write head has been moved one tape square 
; to the left.

; (shift-head-right config)
; takes a normalized configuration config and returns a normalized configuration 
; in which the position of the read/write head has been moved one tape square 
; to the right.

; Examples
; (shift-head-left (conf 'q5 '() 'b '())) => (conf 'q5 '() 'b '())
; (shift-head-left (conf 'q6 '(0 0) 1 '(1 1))) => (conf 'q6 '(0) 0 '(1 1 1))
; (shift-head-left (conf 'q7 '() 0 '(1 1 0))) => (conf 'q7 '() 'b '(0 1 1 0))
; (shift-head-right (conf 'q2 '() 'b '())) => (conf 'q2 '() 'b '())
; (shift-head-right (conf 'q9 '() 0 '(1 1 1))) => (conf 'q9 '(0) 1 '(1 1))
; (shift-head-right (conf 'q8 '(1 0 1 1) 'b '())) => (conf 'q8 '(1 0 1 1 b) 'b '())
; ****************************************************************

; last element of a list is built into Racket (last lst)

; all but last element of a list -- uses Racket's drop-right

(define (shift-head-left config)
  (if (null? (conf-ltape config))
      (normalize (conf (conf-state config) '() 'b (cons (conf-symbol config) (conf-rtape config))))
      (normalize (conf (conf-state config) (drop-right (conf-ltape config) 1) (last (conf-ltape config)) (cons (conf-symbol config) (conf-rtape config))))))

(define (shift-head-right config)
  (if (null? (conf-rtape config))
      (normalize (conf (conf-state config) (append (conf-ltape config) (list (conf-symbol config))) 'b '()))
      (normalize (conf (conf-state config) (append (conf-ltape config) (list (conf-symbol config))) (car (conf-rtape config)) (cdr (conf-rtape config))))))

; ****************************************************************
; ** problem 6 ** (15 points)
; Write a procedure 

; (next-config mach config)
; takes a Turing machine mach and a normalized configuration config
; and returns the normalized next configuration 
; for the Turing machine mach in the configuration config.
; If there is no applicable instruction, the configuration
; returned should be just the input configuration.

; Hint: get your procedures halted?, i-lookup, change-state,
; write-symbol, shift-head-left, shift-head-right working and combine
; them appropriately.

; Examples
; (next-config tm1 (conf 'q1 '() 0 '(0 1))) => (conf 'q1 '(1) 0 '(1))
; (next-config tm1 (conf 'q1 '(1) 0 '(1))) => (conf 'q1 '(1 1) 1 '())
; (next-config tm1 (conf 'q1 '(1 1 0) 'b '())) => (conf 'q2 '(1 1) 0 '())
; (next-config tm1 (conf 'q2 '() 'b '(1 1 0))) => (conf 'q3 '() 1 '(1 0))
; (next-config tm1 (conf 'q3 '() 1 '(1 0))) => (conf 'q3 '() 1 '(1 0))
; ****************************************************************
(define (next-config mach config)
  (define next-instruction (i-lookup (conf-state config) (conf-symbol config) mach))
  (cond
    [(equal? next-instruction #f)
     config]
    [(equal? (ins-dir next-instruction) 'L)
     (change-state (ins-n-state next-instruction) (shift-head-left (write-symbol (ins-n-symbol next-instruction) config)))]
    [else
     (change-state (ins-n-state next-instruction) (shift-head-right (write-symbol (ins-n-symbol next-instruction) config)))]))

; ****************************************************************
; If your procedures are working, then you should
; be able to run the following example, which
; shows the successive normalized configurations 
; of Turing machine tm1 when run from the given configuration.

;> (simulate tm1 (conf 'q1 '() 1 '(1 0 1 0)) 20)
;(list
; (conf 'q1 '() 1 '(1 0 1 0))
; (conf 'q1 '(0) 1 '(0 1 0))
; (conf 'q1 '(0 0) 0 '(1 0))
; (conf 'q1 '(0 0 1) 1 '(0))
; (conf 'q1 '(0 0 1 0) 0 '())
; (conf 'q1 '(0 0 1 0 1) 'b '())
; (conf 'q2 '(0 0 1 0) 1 '())
; (conf 'q2 '(0 0 1) 0 '(1))
; (conf 'q2 '(0 0) 1 '(0 1))
; (conf 'q2 '(0) 0 '(1 0 1))
; (conf 'q2 '() 0 '(0 1 0 1))
; (conf 'q2 '() 'b '(0 0 1 0 1))
; (conf 'q3 '() 0 '(0 1 0 1)))

; ****************************************************************
; ** problem 7 ** (15 points)
; Define (in the given representation) a Turing machine named

; tm-convert

; that takes as input a positive integer n represented in binary
; and produces as output a string of n x's.  When the machine
; halts, the read/write head should be positioned over the
; leftmost x in the output string.  The start state should be named
; q1 -- other states may be named by any other Racket symbols.

; You *may* use additional tape symbols.  When the machine halts,
; there should be just n x's, surrounded by blanks, on the tape.

; IMPORTANT: Give a clear overview description of how your Turing machine works.

; NOTE: you can still do this problem if your simulator is not working, 
; assuming you understand Turing machines and the representation of them 
; defined above.

; Examples of the behavior of tm-convert
; 1            => x
; 11           => xxx
; 110          => xxxxxx
; 1111         => xxxxxxxxxxxxxxx

; (test 'tm-convert (simulate-lite tm-convert (conf 'q1 '() 1 '()) 20) '(() x ()))
; (test 'tm-convert (simulate-lite tm-convert (conf 'q1 '() 1 '(1)) 200) '(() x (x x)))
; (test 'tm-convert (simulate-lite tm-convert (conf 'q1 '() 1 '(1 0)) 200) '(() x (x x x x x)))
; (test 'tm-convert (simulate-lite tm-convert (conf 'q1 '() 1 '(1 1 1)) 400) '(() x (x x x x x x x x x x x x x x)))

; Here are input configurations if you want to simulate your tm-convert on
; these inputs.

(define conv1 (conf 'q1 '() 1 '()))
(define conv6 (conf 'q1 '() 1 '(1 0)))
(define conv15 (conf 'q1 '() 1 '(1 1 1)))
(define conv64 (conf 'q1 '() 1 '(0 0 0 0 0 0)))

; ****************************************************************

; q1 goes to the right-most digit by turning left after the first blank, then starts q2
; q2 looks at the digits right-to-left until the first 1 appears, changes the 1 to a 0, then turns right and starts q3
; q3 turns every digit (which should all be 0s) into 1s, then places an 'x where the first blank is, then turns left and starts q2
; by doing this, we subtract the number by 1, and add an x. for example, 10100 goes down to 10011
; we end when q2 can't find any more 1s (aka q2 finds a blank). q2 turns right and starts q4
; q4 deletes the numbers, leaving with only xs

(define tm-convert
  (list
   (ins 'q1 0 'q1 0 'R)
   (ins 'q1 1 'q1 1 'R)
   (ins 'q1 'b 'q2 'b 'L)
   (ins 'q2 0 'q2 0 'L)
   (ins 'q2 'x 'q2 'x 'L)
   (ins 'q2 1 'q3 0 'R)
   (ins 'q2 'b 'q4 'b 'R)
   (ins 'q3 0 'q3 1 'R)
   (ins 'q3 'x 'q3 'x 'R)
   (ins 'q3 'b 'q2 'x 'L)
   (ins 'q4 0 'q4 'b 'R)
   ))

; ****************************************************************
; ** problem 8 ** (15 points)
; Define (in the given representation) a Turing machine named

; tm-sort

; that takes as input a non-empty string of 0's and 1's
; and produces as output a string of 0's and 1's equal to the input
; string rearranged to have all the 0's before all the 1's.
; When the machine halts, the read/write head should be positioned over the
; leftmost 0 or 1 in the output string.  The start state should be named
; q1 -- other states may be named by any other Racket symbols.

; You *may* use additional tape symbols.  When the machine halts,
; the only non-blank symbols on the tape should be the output string.

; IMPORTANT: Give a clear overview description of how your Turing machine works.

; NOTE: you can still do this problem if your simulator is not working, 
; assuming you understand Turing machines and the representation of them 
; defined above.

; Examples of the behavior of tm-sort
; 0          => 0  
; 1          => 1
; 00         => 00
; 110        => 011
; 1011011    => 0011111

; (test 'tm-sort (simulate-lite tm-sort (conf 'q1 '() 0 '()) 20) '(() 0 ()))
; (test 'tm-sort (simulate-lite tm-sort (conf 'q1 '() 1 '()) 20) '(() 1 ()))
; (test 'tm-sort (simulate-lite tm-sort (conf 'q1 '() 0 '(0)) 200) '(() 0 (0)))
; (test 'tm-sort (simulate-lite tm-sort (conf 'q1 '() 1 '(1 0)) 200) '(() 0 (1 1)))
; (test 'tm-sort (simulate-lite tm-sort (conf 'q1 '() 1 '(0 1 1 0 1 1)) 200) '(() 0 (0 1 1 1 1 1)))


; Here are some input configurations if you want to simulate your tm-sort on
; these inputs.

(define sort0 (conf 'q1 '() 0 '()))
(define sort1 (conf 'q1 '() 1 '()))
(define sort00 (conf 'q1 '() 0 '(0)))
(define sort110 (conf 'q1 '() 1 '(1 0)))
(define sort-long (conf 'q1 '() 1 '(0 1 1 0 1 1)))
; ****************************************************************

; q1 goes right until it finds a 1, then keeps going right with state q2
; q2 goes right until it finds a 0, sets it to 1, then turns left with state q3
; q3 goes left until it finds a 0 or blank, then turns right with state q4.
; q4 sets the current digit, which should be a 1, to 0, then goes right and back to state q1

(define tm-sort
  (list
   (ins 'q1 0 'q1 0 'R)
   (ins 'q1 1 'q2 1 'R)
   (ins 'q1 'b 'q5 'b 'L)
   ; add end state if q1 does not find 1s
   (ins 'q2 1 'q2 1 'R)
   (ins 'q2 0 'q3 1 'L)
   (ins 'q2 'b 'q5 'b 'L)
   ; add end state if q2 does not find 0s
   (ins 'q3 1 'q3 1 'L)
   (ins 'q3 0 'q4 0 'R)
   (ins 'q3 'b 'q4 'b 'R)
   (ins 'q4 1 'q1 0 'R)
   (ins 'q5 0 'q5 0 'L)
   (ins 'q5 1 'q5 1 'L)
   (ins 'q5 'b 'q6 'b 'R)
   ))

; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)

(define (testold name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((OK (if (procedure? expected)
		    	(expected got)
			(equal? got expected)))
		(prefix (if OK
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))


(test 'hours hours (lambda (x) (> x 0)))

(test 'i-match? (i-match? 'q1 'b (ins 'q1 'b 'q3 'b 'L)) #t)
(test 'i-match? (i-match? 'q1  0  (ins 'q1 1 'q4 1 'L)) #f)
(test 'i-match? (i-match? 'q2 1 (ins 'q2 1 'q2 1 'L)) #t)
(test 'i-lookup (i-lookup 'q1 1 tm1) (ins 'q1 1 'q1 0 'R))
(test 'i-lookup (i-lookup 'q2 'b tm1) (ins 'q2 'b 'q3 'b 'R))
(test 'i-lookup (i-lookup 'q3 1 tm1) #f)

(test 'halted? (halted? tm1 (conf 'q1 '(1 1 0) 'b '())) #f)
(test 'halted? (halted? (list (ins 'q1 'b 'q2 'b 'R)) (conf 'q2 '() 'b '())) #t)
(test 'change-state (change-state 'q2 (conf 'q1 '(0) 1 '())) (conf 'q2 '(0) 1 '()))
(test 'change-state (change-state 'q13 (conf 'q4 '(0 1 1) 'b '())) (conf 'q13 '(0 1 1) 'b '()))
(test 'write-symbol (write-symbol 1 (conf 'q5 '(0) 0 '(1 1))) (conf 'q5 '(0) 1 '(1 1)))
(test 'write-symbol (write-symbol 'c (conf 'q2 '(0 0 1) 1 '(1 1))) (conf 'q2 '(0 0 1) 'c '(1 1)))
(test 'write-symbol (write-symbol 'b (conf 'q3 '(1) 0 '())) (conf 'q3 '(1) 'b '()))

(test 'normalize (normalize config1) (conf 'q3 '(0 0) 1 '(1)))
(test 'normalize (normalize config2) (conf 'q6 '(1 b) 0 '()))
(test 'normalize (normalize (conf 'q3 '(b 0) 'b '(1 1 0 b b))) (conf 'q3 '(0) 'b '(1 1 0)))
(test 'normalize (normalize (conf 'q6 '(b 0 b 0) 1 '(0 b 0 b))) (conf 'q6 '(0 b 0) 1 '(0 b 0)))
(test 'normalize (normalize (conf 'q4 '(b b) 'b '(b b b))) (conf 'q4 '() 'b '()))


(test 'shift-head-left (shift-head-left (conf 'q5 '() 'b '())) (conf 'q5 '() 'b '()))
(test 'shift-head-left (shift-head-left (conf 'q6 '(0 0) 1 '(1 1))) (conf 'q6 '(0) 0 '(1 1 1)))
(test 'shift-head-left (shift-head-left (conf 'q7 '() 0 '(1 1 0))) (conf 'q7 '() 'b '(0 1 1 0)))
(test 'shift-head-right (shift-head-right (conf 'q2 '() 'b '())) (conf 'q2 '() 'b '()))
(test 'shift-head-right (shift-head-right (conf 'q9 '() 0 '(1 1 1))) (conf 'q9 '(0) 1 '(1 1)))
(test 'shift-head-right (shift-head-right (conf 'q8 '(1 0 1 1) 'b '())) (conf 'q8 '(1 0 1 1 b) 'b '()))


(test 'next-config (next-config tm1 (conf 'q1 '() 0 '(0 1))) (conf 'q1 '(1) 0 '(1)))
(test 'next-config (next-config tm1 (conf 'q1 '(1) 0 '(1))) (conf 'q1 '(1 1) 1 '()))
(test 'next-config (next-config tm1 (conf 'q1 '(1 1 0) 'b '())) (conf 'q2 '(1 1) 0 '()))
(test 'next-config (next-config tm1 (conf 'q2 '() 'b '(1 1 0))) (conf 'q3 '() 1 '(1 0)))
(test 'next-config (next-config tm1 (conf 'q3 '() 1 '(1 0))) (conf 'q3 '() 1 '(1 0)))

(test 'tm-reverse (simulate-lite tm-reverse (conf 'q1 '() 1 '()) 20) '(() 1 ()))
(test 'tm-reverse (simulate-lite tm-reverse (conf 'q1 '() 1 '(1 0)) 200) '(() 0 (1 1)))
(test 'tm-reverse (simulate-lite tm-reverse (conf 'q1 '() 0 '(0 0 1)) 200) '(() 1 (0 0 0)))
(test 'tm-reverse (simulate-lite tm-reverse (conf 'q1 '() 1 '(0 1 0 1 1)) 200) '(() 1 (1 0 1 0 1)))

(test 'tm-convert (simulate-lite tm-convert (conf 'q1 '() 1 '()) 20) '(() x ()))
(test 'tm-convert (simulate-lite tm-convert (conf 'q1 '() 1 '(1)) 200) '(() x (x x)))
(test 'tm-convert (simulate-lite tm-convert (conf 'q1 '() 1 '(1 0)) 200) '(() x (x x x x x)))
(test 'tm-convert (simulate-lite tm-convert (conf 'q1 '() 1 '(1 1 1)) 400) '(() x (x x x x x x x x x x x x x x)))

(test 'tm-sort (simulate-lite tm-sort (conf 'q1 '() 0 '()) 20) '(() 0 ()))
(test 'tm-sort (simulate-lite tm-sort (conf 'q1 '() 1 '()) 20) '(() 1 ()))
(test 'tm-sort (simulate-lite tm-sort (conf 'q1 '() 0 '(0)) 200) '(() 0 (0)))
(test 'tm-sort (simulate-lite tm-sort (conf 'q1 '() 1 '(1 0)) 200) '(() 0 (1 1)))
(test 'tm-sort (simulate-lite tm-sort (conf 'q1 '() 1 '(0 1 1 0 1 1)) 200) '(() 0 (0 1 1 1 1 1)))


; *************** end of hw3.rkt *********************************



