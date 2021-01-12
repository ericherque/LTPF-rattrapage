open State
open Parseur

module P = Parseur
module S = State

module Config :
sig
  type config
  val eval : P.exp -> S.state -> bool
  val faire_un_pas : P.instr -> S.state -> config
  val printConfig : config
end
