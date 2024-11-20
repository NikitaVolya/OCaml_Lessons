
let correspondance note =
  match note with
    "do" -> 0
  | "re" -> 1
  | "mi" -> 2
  | "fa" -> 3
  | "sol" -> 4
  | "la" -> 5
  | "si" -> 6
  | _ -> failwith "Erreur correspondance : note inconnue";;

correspondance "re";;
correspondance "si";;
(* correspondance "po";; *)

let rec map l f =
  match l with
    [] -> []
  | x::r -> (f x)::(map r f);;

let rec traduire l = map l correspondance;;

traduire ["do"; "re"; "mi"; "la"; "do"; "re"; "si"; "do"; "mi"; "fa"; "sol"];;

let rec tout_traduire l = map l traduire;;

tout_traduire [["si"; "si"; "do"; "re"; "re"; "do"; "si"; "la"];
["sol"; "sol"; "la"; "si"; "la"; "sol"; "sol"];
["do"; "sol"; "fa"; "mi"; "re"; "do"]];;

let rec tonalite l =
  match l with
    [x] -> x
  | x::r -> tonalite r
  | _ -> failwith "Erreur tonalite : liste vide";;

tonalite [0; 6; 6; 3; 4; 2; 3];;
tonalite [5];;
(* tonalite [];; *)

let plus_aigue l =
  let rec plus_aigue_aux l a =
    match l with
      [] -> a
    | x::r -> if a < x
              then plus_aigue_aux r x
              else plus_aigue_aux r a
  in
  match l with
    [] -> failwith "Erreur plus_aigue : liste vide"
  | x::r -> plus_aigue_aux r x;;

plus_aigue [4; 0; 0; 3; 2; 5; 1];;
(* plus_aigue [];; *)

let indice_plus_aigue l =
  let rec indice_plus_aigue_aux l best best_i i =
    match l with
      [] -> best_i
    | x::r -> if best < x
              then indice_plus_aigue_aux r x i (i + 1)
              else indice_plus_aigue_aux r best best_i (i + 1)
  in
  match l with
    [] -> failwith "Erreur (indice_)plus_aigue : liste vide"
  | x::r -> indice_plus_aigue_aux r x 0 1;;


indice_plus_aigue [1; 0; 2; 4; 2; 4; 2];;
(* indice_plus_aigue [];; *)

let plus_courte l =
  let rec plus_courte_aux l a a_s =
    match l with
      [] -> a
    | x::r ->
       let x_s = List.length x in
       if x_s < a_s
       then plus_courte_aux r x x_s
       else plus_courte_aux r a a_s
  in
  match l with
    [] -> failwith "Erreur plus_courte : liste vide"
  | x::r -> plus_courte_aux r x (List.length x);;

plus_courte [[6; 6; 0; 1; 0; 6; 5]; [4; 5; 6; 5; 4; 4]; [0; 4; 3; 1; 0]];;
plus_courte [[6; 6; 2]; [1; 1; 2; 5; 5]; []; [3; 2; 1]];;
(* plus_courte [];; *)

let gamme p =
  let rec gamme p a =
    match p with
      [] -> a = 7
    | x::r -> if x = a
              then gamme r (a + 1)
              else gamme r a
  in
  gamme p 0;;

gamme [5; 0; 1; 4; 2; 4; 3; 4; 4; 6; 5; 6; 0; 1];;
gamme [5; 0; 1; 4; 4; 3; 2; 4; 4; 6; 5; 6; 0; 1];;

let rec appartient e l =
  match l with
    [] -> false
  | x::r -> if e = x
            then true
            else appartient e r;;

let prefere p1 p2 =
  let appartient_en_p1 = appartient (correspondance "fa") p1 in
  let appartient_en_p2 = appartient (correspondance "fa") p2 in
  if appartient_en_p1 && appartient_en_p2
  then
    if List.length p1 mod 2 == 0 && List.length p2 mod 2 == 0
    then failwith "N'aime ni l'une ni l'autre"
    else
      if List.length p1 mod 2 == 1 && List.length p2 mod 2 == 1
      then failwith "N'aime ni l'une ni l'autre"
      else List.length p1 mod 2 == 1
  else
    if not appartient_en_p1 && not appartient_en_p2
    then failwith "Aime autant les deux"
    else not appartient_en_p1;;
           
prefere [6; 4; 2; 1; 0] [6; 5; 3; 4; 4; 2];;
prefere [6; 5; 3; 4; 4; 2] [6; 4; 2; 1; 0];;
prefere [0; 6; 6; 3; 4; 2; 3] [3; 4; 4; 4];;
prefere [3; 4; 4; 4] [0; 6; 6; 3; 4; 2; 3];;

(* prefere [3; 4; 4; 4] [0; 6; 3; 3; 5; 6];; *)

(* prefere [3; 4; 4; 4; 2] [0; 6; 3];; *)

(* prefere [0; 1; 0; 1; 0; 1] [0; 0];; *)
