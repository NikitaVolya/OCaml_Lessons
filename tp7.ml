
let rec ralentir p =
  match p with
    [] -> []
  | x::r -> x::(x::(ralentir r));;

ralentir [6; 4; 2];;
ralentir [];;

let accelerer p =
  let rec accelerer_aux p a =
    match p with
      [] -> []
    | x::r ->
       if a
       then x::(accelerer_aux r false)
       else accelerer_aux r true
  in
  match p with
    [] -> []
  | [x] -> []
  | x::r -> x::(accelerer_aux r false);;

accelerer [0; 6; 6; 3; 4; 2;];;
accelerer [0; 6; 6; 3; 4];;
accelerer (ralentir [0; 6; 6; 3; 4]);;
ralentir (accelerer [0; 6; 6; 3; 4]);;

let une_sur_n n p =
  let rec une_sur_n_aux n p a =
    match p with
      [] -> []
    | x::r -> if a = 1
              then x::(une_sur_n_aux n r n)
              else une_sur_n_aux n r (a - 1)
  in
  if n < 0
  then failwith "Error: n doit etre positive"
  else une_sur_n_aux n p 1;;

une_sur_n 2 [0; 6; 6; 3; 4; 2];;
une_sur_n 3 [0; 6; 6; 3; 4; 2; 1; 0; 1];;
une_sur_n 4 [0; 6; 6; 3; 4; 2; 1; 0; 1];;
une_sur_n 5 [0; 6; 6; 3; 4; 2; 1; 0; 1];;

let inverser_2a2 p =
  let rec inverser_2a2_aux p =
    match p with
      [] -> []
    | [x] -> [x]
    | x::(y::r) -> y::(x::(inverser_2a2_aux r))
  in
  inverser_2a2_aux p;;

inverser_2a2 [0; 6; 6; 3; 4; 2; 3; 2];;
inverser_2a2 [0; 6; 6; 3; 4; 2; 3];;

let rec transcrire p =
  match p with
    [] -> []
  | x::r -> ((x + 2) mod 7)::(transcrire r);;

transcrire [0; 1; 2; 0; 3; 4; 0; 5; 6];;

let rec sommet p1 p2 =
  match (p1, p2) with
    ([], []) -> []
  | (x1::r1, []) -> x1::(sommet r1 [])
  | ([], x2::r2) -> x2::(sommet [] r2)
  | (x1::r1, x2::r2) -> ((x1 + x2) mod 7)::(sommet r1 r2);;

sommet [0; 1; 2; 3] [6; 1; 6; 0; 1; 2; 4; 5; 5];;


let mixage p1 p2 =
  let rec mixage_aux p1 p2 a =
    match (p1, p2) with
      ([], []) -> a
    | ([], _) | (_, []) -> failwith "Erreur mixage : nombre de notes different"
    | (x1::r1, x2::r2) -> mixage_aux r1 r2 (x2::(x1::a))
  in
  mixage_aux p1 p2 [];;

mixage [0; 2; 4; 6] [1; 3; 5; 7];;

let rec deux_mains p1 p2 =
  match (p1, p2) with
    ([], []) -> []
  | ([], _) | (_, []) -> failwith "Erreur deux_mains : nombre de notes different"
  | (x1::r1, x2::r2) -> (x1, x2)::(deux_mains r1 r2);;

deux_mains [6; 6; 0; 1; 1; 0] [4; 4; 5; 6; 5; 4];;

let rec appartient_couple (n1, n2) pc =
  match pc with
    [] -> false
  | (x1, x2)::r -> if n1 = x1 && n2 = x2
                   then true
                   else appartient_couple (n1, n2) r;;

appartient_couple (2, 5) [(1, 3); (2, 6); (2, 5)];;
appartient_couple (2, 5) [(2, 6); (5, 2)];;


let rec garder_aigues pc =
  match pc with
    [] -> []
  | (x1, x2)::r -> if x1 > x2
                   then x1::(garder_aigues r)
                   else x2::(garder_aigues r);;

garder_aigues [(6, 4); (6, 4); (0, 5); (1, 6); (1, 5); (0, 4)];;


let desunir pc =
  let rec desunir_aux pc (p1, p2) =
    match pc with
      [] -> (p1, p2)
    | (x1, x2)::r -> desunir_aux r (x1::p1, x2::p2)
  in
  desunir_aux pc ([], []);;

desunir [(6, 4); (6, 4); (0, 5); (1, 6); (1, 5); (0, 4)];;

let rec append n l = 
  match l with
    [] -> [n]
  | x::r -> x::(append n r);;
    

let desunir_2 pc =
  let rec desunir_2_aux pc (p1, p2) =
    match pc with
      [] -> (p1, p2)
    | (x1, x2)::r -> desunir_2_aux r (append x1 p1, append x2 p2)
  in
  desunir_2_aux pc ([], []);;

desunir_2 [(6, 4); (6, 4); (0, 5); (1, 6); (1, 5); (0, 4)];;