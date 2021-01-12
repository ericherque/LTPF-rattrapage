open Parseur

module P = Parseur

module State :
  sig
    type state
    exception NotFound
    
    val init : state -> state
    (*val charToExp : char -> P.exp
    val expToChar : P.exp -> char*)
    val read : char -> state -> int
    val change : char -> int -> state -> state
    val execAffect : P.instr -> state -> state
    val printState : state
  end
