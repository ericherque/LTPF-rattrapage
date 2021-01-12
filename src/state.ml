open Parseur

module P = Parseur


module State =

  struct
    (* typedef de ce que c'est qu'un état *)
    (* Un état c'est: une variable (char), une valeur (int) et un état suivant (state)   *)
    type state = | State of (char * int * state)
                 | End

    (* Lire la valeur d'une variable *)
    (* On peut raise une exception lorsqu'on ne trouve pas ce qu'on cherche*)
    exception NotFound
    

    (* Initialisation *)
    (* Ici on initialise toutes les variables à 0 récursivement:
       s.value = 0 et on fait pareil pour les prochains jusqu'à End *)
    let rec init (s: state) : (state)  =
      match s with
      |End -> s (* ici on fait rien *)
      |State(var, value, next) -> State(var, 0, (init(next))) (* appliquer init(s)
                                                                 à tous les next         *)

   
    (* On veut la var entrée en paramètre soit égale à la var dans le state dans 
       lequel on se trouve, si c'est le cas on renvoit value. Pour cela on procède par
       récursion pour parcourir les états (logique)                                      *)
    let rec read (var: char) (s: state) : (int) =
      match s with
      |End -> raise NotFound (* Si on arrive ici c'est qu'on l'a pas trouvé *)
      |State(statevar, value, next) -> if (var = statevar) then value 
                                       else (read var next) 

    (* Même principe qu'avant sauf qu'on donne la variable qu'on souhaite changer ET
       la nouvelle valeur                                                                *)
    let rec change (var: char) (value: int) (s: state) : (state) =
      match s with
      |End -> State(var, value, End)
      |State(statevar, statevalue, next) ->  if (var = statevar) then State(statevar, value, next)
                                             else State(statevar, statevalue, (change var value next))

    (* on exécute une instruction d'affectation i dans l'état s *)
    let execAffect (i : P.instr) (s: state) : (state) =
      match i with
      |P.Assign(Var(variable), valu) -> (match valu with
                                         |Var(a) -> (change variable (read a s) s) (*Chercher la valeur de la variable puis l'affecter*)
                                         |Cst(a) -> (change variable a s) (*Affecte directement la valeur puisque c'est un constante*)
                                        )
      |_ -> raise NotFound
    
    (* affcihe les valeurs des états donc les valeurs de tous les variables*)
    let rec printState (s : state) =
      match s with
      |End -> print_string "\n"
      |State(c,i,s) -> (print_char c) ; (print_string " = ") ; (print_int i) ; (print_string "\n") ; (printState s)
          
   (*
    let charToExp (var: char) : (P.exp) =
      match var with
      |'a' -> P.Var(A)
      |'b' -> P.Var(B)
      |'c' -> P.Var(C)
      |'d' -> P.Var(D)
      |_ -> raise NotFound
     

    let expToChar (var: P.exp) : (char) =
      match var with
      |P.Var(A) -> 'a'
      |P.Var(B) -> 'b'
      |P.Var(C) -> 'c'
      |P.Var(D) -> 'd'
      |_ -> raise NotFound
     
    
    (* Executation d'affectation : ca fonctionne peut être ? *)
    let  execAffect (i : P.instr) (s: state) : (state) =
      match i with
      |P.Assign(var, valu) ->
        (match valu with
        |P.Var(a) -> (match a with
                     |A -> (change 'a' (read (expToChar var) s) s)
                     |B -> (change 'b' (read (expToChar var) s) s)
                     |C -> (change 'c' (read (expToChar var) s) s)
                     |D -> (change 'c' (read (expToChar var) s) s))
        |P.Cst(a) -> (match a with
                      |Un -> (change (expToChar var) 1 s)
                      |Zero -> (change (expToChar var) 0 s)
                     )
        )
      |_ -> raise NotFound
     *)

end

(*
module S = State

let s1 : S.state = S.State('a', 2, End)
let s1 = S.init s1
let _ = print_string "Test init"
let test_init  = S.printState s1 ; print_string "\n \n \n"


let s2 = S.State('a', 1, S.State('b', 2, S.State('c', 3, End)))
let _ = print_string "Test read\n"
let test_read = print_int (S.read 'c' s2) ; print_string "\n \n \n"

let _ = print_string "Test change\n"
let _ = print_string "s2 avant \n"
let _ = S.printState s2
let _ = print_string "s2 apres \n"
let test_change = S.printState (S.change 'c' 0 s2 )

let exp1 = "a:=1"
let test_exp1 = P.list_of_string exp1
let ranalist_exp1 = P.p_S test_exp1
let test_execAffect = S.execAffect 
 *)
