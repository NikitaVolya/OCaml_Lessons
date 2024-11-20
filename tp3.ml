
let rec est_pair n =
  if n < 0
  then est_pair (abs n)
  else
    if n = 0
    then true
    else
      if n = 1
      then false
      else est_pair (n - 2);;

est_pair 5;;
est_pair (-10);;

let rec moyenne x y =
  if x < 0 || y < 0
  then moyenne (abs x) (abs y)
  else
    if x + 1 = y || x = y
    then x
    else
      if x > y
      then moyenne (x - 1) (y + 1)
      else moyenne (x + 1) (y - 1);;

moyenne 4 10;;
moyenne 17 12;;
moyenne 2 (-6);;

let rec somme_entiers_1 n =
  if n <= 0
  then failwith "Erreur somme_entiers : n doit etre superieur a 0"
  else
    if n = 1
    then 1
    else n + somme_entiers_1 (n - 1);;

let rec somme_entiers_2 n =
  let rec somme_entiers_rt n a =
    if n = 1
    then a + 1
    else somme_entiers_rt (n - 1) (a + n)
  in
  if n < 0
  then failwith "Erreur somme_entiers : n doit etre superieur a 0"
  else somme_entiers_rt n 0;;

somme_entiers_1 5;;
somme_entiers_2 5;;
(* somme_entiers (-3);; *)



let somme_entre p q =
  let rec somme_entre_tr p q a =
    if p > q
    then a
    else somme_entre_tr (p + 1) q (a + p)
  in
  if p < 0 || q < 0
  then failwith "Erreur somme_entre : p et q devent etre superieur a 0"
  else somme_entre_tr p q 0;;

somme_entre 1 10;;


let somme_div_entre n p q =
  let rec somme_div_entre_tr n p q a =
    if p >= q
    then a
    else
      if n mod p = 0
      then somme_div_entre_tr n (p + 1) q (a + p)
      else somme_div_entre_tr n (p + 1) q a
  in
  if p < 0 || q < 0
  then failwith "Erreur somme_div_entre : p et q devent etre superieur a 0"
  else somme_div_entre_tr n p q 0;;

somme_div_entre 12 9 11;;


let est_parfait n =
  if n < 0
  then failwith "Erreur est_parfait : n doit etre positif ou nul"
  else
    if n = 0
    then false
    else
      if n = somme_div_entre n 1 n
      then true
      else false;;

est_parfait 0;;