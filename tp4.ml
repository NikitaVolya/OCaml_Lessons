
let fib_1 n =
  let rec fib_rec n =
    if n = 0
    then 0
    else
      if n = 1
      then 1
      else fib_rec (n - 2) + fib_rec (n - 1)
  in
  if n < 0
  then failwith "Erreur fibonacci : n doit etre positif ou nul"
  else fib_rec n;;

let fibonacci_rt n =
  let rec fib_rt_rec i n a b =
    if i = n
    then a
    else fib_rt_rec (i + 1) n b (a + b)
  in
  if n < 0
  then failwith "Erreur fibonacci : n doit etre positif ou nul"
  else fib_rt_rec 0 n 0 1;;

fibonacci_rt 55;;

(* Exercice 2*)


let est_palindrome s =
  let rec palindrom_rt s i j =
    if i >= j
    then true
    else
      if s.[i] <> s.[j]
      then false
      else palindrom_rt s (i + 1) (j - 1)
  in
  palindrom_rt s 0 (String.length s - 1);;

est_palindrome "engagelejeuquejelegagne";;

let suppr_espaces_rs s =
  let rec suppr_rec s i =
    if i = -1
    then ""
    else
      if s.[i] = ' '
      then suppr_rec s (i - 1)
      else String.make 1 s.[i] ^ suppr_rec s (i - 1)
  in
  suppr_rec s (String.length s - 1);;

let suppr_espaces_tr s =
  let rec suppr_rec s i a =
    if i = -1
    then a
    else
      if s.[i] = ' '
      then suppr_rec s (i - 1) a
      else suppr_rec s (i - 1) ((String.make 1 s.[i])  ^ a)
  in
  suppr_rec s (String.length s - 1) "";;

suppr_espaces_rs "engage le jeu que je le gagne";;
suppr_espaces_tr "engage le jeu que je le gagne";;

let presque_palindrome s =
  est_palindrome (suppr_espaces_tr s);;

presque_palindrome "engage le jeu que je le gagne";;