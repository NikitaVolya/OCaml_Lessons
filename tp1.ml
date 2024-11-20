(* Variqbles et types simples *)
let a = 2 * 3 * 5 * 7 * 11;;
let b = a * a;;
let a = abs(75 - 125);;
let a = 42 in a * 100;;
let q = 51 / 7 and r = 51 mod 7;;
51. /. 7.;;
let perimetre =
  let pi = 3.1415 and r = 2. +. sqrt(6.) *. 1.5 in
  2. *. pi *. r;;
let pi_s = "-3.14";;
let pi_f = float_of_string(pi_s);;
let pi_i = int_of_float(pi_f);;
let s = "La chevalerie, c'est pas la ou on range les cheveux ?";;
let lg = String.length(s);;
let debut = s.[0];;
let fin = s.[lg - 1];;
(String.make 1 debut) ^ (String.make 1 fin);;

(* Premier fonctions *)
(* Des photocopies *)
let photocopies nb = if nb < 0 then "Erreur photocopies : valeur impossible"
                     else float_of_int nb *. 0.09;;

let photocopies2 nb =
  let board min_n max_n number = max min_n (min max_n number) in
  if nb < 0 then failwith "Erreur photocopies2 : valeur impossible"
  else
  float_of_int ( board 0 10 nb * 9 + board 0 20 (nb - 10) * 5 + max 0 (nb - 30) * 3) /. 100.;;

(* Les soldes *)
let prix_solde prix solde =
  if solde > 100. || solde < 0. then failwith "Erreur prix_solde : valeur impossible"
  else prix -. prix *. solde /. 100.;;
prix_solde 100. 20.;;
prix_solde 45.90 10.;;

(* Fonction de comparaison *)

let comparaison a b c d e =
  float_of_int a +. b > 10. || (String.length e - d != 0 && c <= 'd');;
comparaison 5 4. 'a' 2 "Adc";;