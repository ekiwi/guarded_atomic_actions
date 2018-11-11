; http://www.clifford.at/yosys/cmd_write_smt2.html
; we need QF_UFBV for this poof
(set-logic QF_UFBV)

; insert the auto-generated code here
%%

; declare two state variables s1 and s2
(declare-fun s1 () test_s)
(declare-fun s2 () test_s)

; state s2 is the successor of state s1
(assert (ref_t s1 s2))

; is there such a model?
(check-sat)
