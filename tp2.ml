(* Droit de vote *)
let majorite country age =
  if country = "France" || country = "Inde"
  then (if age >= 18 then "Vous pouvez voter" else "Vous ne pouvez pas voter")
  else
    if (country = "Bresil" || country = "Autriche")
    then (if age >= 16 then "Vous pouvez voter" else "Vous ne pouvez pas voter")
    else
      if country = "Cameroun"
      then (if age >= 20 then "Vous pouvez voter" else "Vous ne pouvez pas voter")
      else failwith "Erreur majorite : pays inconnu";;

majorite "France" 15;;
majorite "Bresil" 17;;
(* majorite "Ukraine" 18;; Exception: Failure "Erreur majorite : pays inconnu" *) 

(* Ou exclusif *)
let ou_exclusif a b = (a && not b) || (not a && b);;
ou_exclusif false false;;
ou_exclusif false true;;
ou_exclusif true false;;
ou_exclusif true true;;

(* Casse *)
let bas_de_casse line =
  if String.length line = 0
  then failwith "Erreur bas_de_casse : chaine trop courte"
  else if line.[0] >= '0' && line.[0] <= '9'
  then failwith "Erreur bas_de_casse : le premier caractere n'est pas une lettre"
  else
    if String.length line = 1
    then (String.make 1 (Char.lowercase_ascii line.[0]))
    else (String.make 1 (Char.lowercase_ascii line.[0]))
         ^ String.sub line 1 (String.length line - 1);;

bas_de_casse "Bac";;
bas_de_casse "BAC";;
bas_de_casse "bac";;
(* bas_de_casse "6ac";; *)
(* bas_de_casse "";; *)

(* Plus petit entier *)
let min2 a b = if a < b then a else b;;
let max2 a b = if a > b then a else b;;

let plus_petit_entier a b c =
  let first = min2 a (min2 b c) and
        last = max2 a (max2 b c) in
  if (a > 9 || b > 9 || c > 9 || a < 0 || b < 0 || c < 0)
  then failwith "Erreur plus_petit_entier : on attend des chiffres"
  else if a > first && a < last then first * 100 + a * 10 + last
  else if b > first && b < last then first * 100 + b * 10 + last
  else first * 100 + c * 10 + last;;

plus_petit_entier 9 5 7;;
plus_petit_entier 9 7 5;;
plus_petit_entier 5 9 7;;
plus_petit_entier 7 9 5;;
plus_petit_entier 9 5 7;;

(* eu de fléchettes *)
let cible_losange x y =
  let generate_f i x = i -. x in 
  let y = abs_float y and x = abs_float x in
  if y < generate_f 2. x then 100
  else if y < generate_f 4. x then 75
  else if y < generate_f 6. x then 25
  else 0;;

cible_losange 1.5 1.6;;
cible_losange 1. 1.;;
cible_losange 1. 0.9;;

(* Moyen de transport *)
let moyen_transport nb_km tps_attente_tram =
  let velo_tepms = nb_km /. 19. *. 60. and
      train_temps = nb_km /. 20. *. 60. +. tps_attente_tram in
  if velo_tepms < train_temps
  then "velo"
  else "tram";;

moyen_transport 9.5 1.;;
moyen_transport 9.5 2.;;