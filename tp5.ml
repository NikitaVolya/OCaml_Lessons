
let chiffre_of_char c =
  if c >= '0' && c <= '9'
  then int_of_char c - int_of_char '0'
  else failwith "Erreur chiffre_of_char : le caractere doit representer un chiffre";;


let somme_chiffres s =
  let rec somme_chiffres_rec s i a =
    if i = -1
    then a
    else
      if s.[i] >= '0' && s.[i] <= '9'
      then somme_chiffres_rec s (i - 1) (a + chiffre_of_char s.[i])
      else somme_chiffres_rec s (i - 1) a
  in
  somme_chiffres_rec s (String.length s - 1) 0;;

somme_chiffres "J'ai lu 87 pages de mon livre. Il en contient 245.";;
somme_chiffres "J'ai lu 87 pages de mon livre. Il en contient 205.";;


let produit_chiffres s =
  let rec produit_chiffres_rec s i a f =
    if i = -1
    then if not f
         then failwith "Erreur produit_chiffres : aucun chiffre trouve."
         else a
    else
      if s.[i] >= '0' && s.[i] <= '9'
      then produit_chiffres_rec s (i - 1) (a * chiffre_of_char s.[i]) true
      else produit_chiffres_rec s (i - 1) a f
  in
  produit_chiffres_rec s (String.length s - 1) 1 false;;

produit_chiffres "J'ai lu 87 pages de mon livre. Il en contient 245.";;
produit_chiffres "J'ai lu 87 pages de mon livre. Il en contient 205.";;
produit_chiffres "J'ai lu 11 pages de mon livre. Il en contient 111.";;
(* produit_chiffres "J'ai lu toutes les pages de mon livre.";; *)


let est_prefix prefix s =
  let rec est_prefix_rec prefix s i =
    if i >= String.length prefix
    then true
    else
      if i >= String.length s
      then false
      else
        if prefix.[i] <> s.[i]
        then false
        else est_prefix_rec prefix s (i + 1)
  in
  est_prefix_rec prefix s 0;;

est_prefix "abs" "abs";;


let est_suffixe suffixe s =
  let rec est_suffixe_rec suffixe s i =
    if i > String.length suffixe
    then true
    else
      if i > String.length s
      then false
      else
        if suffixe.[String.length suffixe - i] <> s.[String.length s - i]
        then false
        else est_suffixe_rec suffixe s (i + 1)
  in
  est_suffixe_rec suffixe s 1;;

est_suffixe "d" "acsd";;


let est_facteur facteur s =
  let rec est_facteur_rec facteur s =
    if String.length s < String.length facteur
    then false
    else
      if est_prefix facteur s
      then true
      else est_facteur_rec facteur (String.sub s 1 (String.length s - 1))
  in
  est_facteur_rec facteur s;;

est_facteur "facacac" "facacac";;


let est_sous_mot smt s =
  let rec est_sous_mot_rec smt s i j =
    if j = -1
    then true
    else
      if i = -1
      then false
    else
      if smt.[j] == s.[i]
      then est_sous_mot_rec smt s (i - 1) (j - 1)
      else est_sous_mot_rec smt s (i - 1) j
  in
  est_sous_mot_rec smt s (String.length s - 1) (String.length smt - 1);;

est_sous_mot "helloa" "oh! hel lo frend";;


let mots_meles s1 s2 =
  let rec mots_meles_rec s1 s2 i j a =
    if i = String.length s1
    then a ^ String.sub s2 j (String.length s2 - j)
    else
      if j = String.length s2
      then a ^ String.sub s1 i (String.length s1 - i)
      else mots_meles_rec s2 s1 j (i + 1)(a ^ String.make 1 s1.[i])
  in
  mots_meles_rec s1 s2 0 0 "";;

mots_meles "a" "tu";;