(*                                                                   *)
(*                     Sous-programmes utilitaires                   *)
(*                                                                   *)

exception Pas_de_solution;;

let rec exponentiation i t=
  if i = 0 then 1
  else
    if (i mod 2) = 0
    then exponentiation (i/2) (t*t)
    else t* (exponentiation (i/2) (t*t));;

let exp_2 a =
  exponentiation a 2;;

let xor a b =
  not( not(a || b) || (a&&b));;


(*Applique une permutation donnée comme un vecteur d'indice à un vecteur*)

let permutation perm vecteur =
  let t = Array.length perm in
  let res = Array.make t vecteur.(0)  in
    for i = 0 to t-1 do
      res.(perm.(i))<- vecteur.(i)
    done;
    res ;;

let depermute perm vecteur =
  let t = Array.length perm in
    Array.init t (fun i -> vecteur.(perm.(i)));;

let defaut n =
  Array.init n (fun x -> x);;

(*Engendre aléatoirement des permutations de manière uniforme*)

let liste_moins liste n =
  let res = ref 0 in 
  let rec boucle l i =
    let a ::s = l in
      match i with
	|0-> res :=a ; s
	|k-> a :: boucle s (i-1)
  in (!res,boucle liste n);;  

let permutation_aleatoire n =
  Random.self_init();
  let res = Array.create n 0 in 
  let rec liste_def n =
    if n = 0 then [0]
    else n :: liste_def (n-1)
  in
  let rec boucle liste i =
    if i = -1 
    then ()
    else
      let (n,l)= liste_moins liste (Random.int (i+1)) in
	res.(i)<-n;
	boucle l (i-1)
  in boucle (liste_def (n-1)) (n-1);
    res;;

(* Construit la liste des positions des true dans un vecteur*)

let liste_position vecteur =
  let n =  Array.length vecteur in
  let rec boucle i =   
    if i = n then []
    else
      if vecteur.(i)
      then i:: boucle (i+1)
      else boucle (i+1)
  in boucle 0;;

let somme_vecteur colonne1 colonne2 =
  let t = Array.length colonne1 in
    Array.init t (fun i ->  xor colonne1.(i) colonne2.(i));;
  
let somme_liste liste =
  match liste with
    |[] -> failwith "liste vide"
    |a :: s ->  List.fold_left somme_vecteur (Array.copy a) s;;

let incremente i =
  i:= !i+1;;
  
let init i j =
  j := max !i !j;
  i:= 0;;

(*Encode et décode des vecteurs binaires comme des entiers dont ils sont la représentation en base 2*)

let encode v =
  let temp = ref 0 in
    for j=0 to (Array.length v) -1 do
      if v.(j) 
      then temp := !temp + (exp_2 j);
    done;
    !temp;;

let decode n k =
 let t = ref n in
 let res = Array.make k false in
   for j = 0 to k-1 do
     res.(j) <-  ((!t) mod 2) = 1;
     t := (!t)/2;
   done;
   res;;

(*                                                                   *)
(*               Algorithmes utilitaires sur les matrices            *)
(*                                                                   *)

let copy_matrice matrice =
  let m = Array.length matrice in
    Array.init m (fun i -> Array.copy matrice.(i));;
 
let echange_colonne matrice ordre i j=
 let vect_temp = matrice.(i) in
 let temp = ordre.(i) in
         matrice.(i) <- matrice.(j);
         matrice.(j) <- vect_temp;
         ordre.(i) <- ordre.(j);
         ordre.(j) <- temp;;

let echange_ligne matrice b i j =
 let temp = ref b.(i) in
   b.(i) <- b.(j);
   b.(j) <- !temp;
   for k=0 to (Array.length matrice) -1 do
     temp :=  matrice.(k).(i);
     matrice.(k).(i) <- matrice.(k).(j);
     matrice.(k).(j)<- !temp
   done;;


let sous_matrice matrice liste =
  let m = List.length liste in
  let n = Array.length matrice.(0) in
  let res = Array.create_matrix m n false in
  let rec boucle l i =
    match l with 
      |[]->()
      |a :: s -> (res.(i)<- (Array.copy matrice.(a)); boucle s (i+1))
  in 
    boucle liste 0;
  res;;

let transposee matrice =
 let m = Array.length matrice in
   if m=0 then [||]
   else
     let n = Array.length matrice.(0) in
       Array.init n (fun i-> Array.init m (fun j -> matrice.(j).(i)));;

let scan_matrice debut vecteur matrice =
  let m = Array.length matrice in
  let j = ref (-1) in
    for i = debut to m - 1 do
      if matrice.(i) = vecteur then j:=i
      else ()
    done;
    !j;;

let ajoute_colonne matrice colonne =
 let m = Array.length matrice in
   if m = 0 then [|colonne|]
   else
     Array.init (m+1) (fun i -> if i = m then colonne else matrice.(i));;      

let ajoute_ligne matrice vecteur=
 let m = Array.length matrice in
 let n = Array.length matrice.(0) in
   Array.init m (fun i -> Array.init (n+1) (fun j-> if j = n then vecteur.(i) else matrice.(i).(j)));;

(*Engendre des matrices et vecteurs aléatoires pour des besoins de test*)

let vecteur_aleatoire n =
 Random.self_init ();
Array.init n (fun i -> Random.bool());;

let matrice_aleatoire m n =
  Array.init m (fun i -> vecteur_aleatoire n);;  



(* La méthode qui sert à tout : le pivot de gauss sur F2, sans additionner de colonne. Maintient un tableau avec l'ordre des variables.
Si les lignes ne sont pas indépendantes, cela donne des lignes de false après application du pivot à la fin de la matrice, leur nombre étant donné par lignes_liees.
Les mdifications se font par effet de bord, on ne renvoit pas de nouvelles matrice à la fin *)
(*Gère les matrices rectangulaires dans les deux sens*)

let pivot_gauss matrice ordre vecteur =
 let m = Array.length matrice - 1   in
   if m = -1 
   then 0
   else
     begin
       let n = Array.length matrice.(0) - 1 in
       let lignes_liees = ref 0 in
       let i = ref 0 in
       let taille = ref 0 in
         while (!taille <= (n- (!lignes_liees))) do
           (*Cherche un vrai dans une ligne pour faire l'élimination avec la colonne correspondante *)
           while !i<> (m+1-(!taille)) && not matrice.(!taille+(!i)).(!taille) do
             i:=(!i)+1
           done;
           if !i = (m+1-(!taille)) 
	   then
             begin  
               (* Mets la ligne de false à la fin*)
               echange_ligne matrice vecteur (!taille) (n-(!lignes_liees));
               lignes_liees := (!lignes_liees) +1 ;
             end
               (* Met la colonne avec le vrai en première position *)
           else
             begin
               echange_colonne matrice ordre (!taille) (!taille +(!i));

               (*Elimine par combinaison linéaire*)
               for  j = 0 to n do
                 if matrice.(!taille).(j)  && j<>(!taille) 
		 then
                   begin
                     for k= !taille to m do
                       matrice.(k).(j)<- xor matrice.(k).(j) matrice.(k).(!taille)
                     done;
                     vecteur.(j) <- xor vecteur.(j)  vecteur.(!taille);
                   end
               done;
               taille:= !taille +1;
             end;
           i:= 0;
         done;
         !lignes_liees
     end;;

(*Une petite fonction qui grace au pivot de gauss décide si un système est de rang maximal*)

let rang_maximal matrice =
  let res = copy_matrice matrice in
  let m = Array.length matrice in
  let n = Array.length matrice.(0) in 
    if m >= n 
    then pivot_gauss res (defaut m) (Array.make n false)= 0
    else  pivot_gauss (transposee res) (defaut n)(Array.make m false) = 0
;;

(* Une fonction de prétraitement de matrice, qui permet de supprimer les lignes liées,
même dans les matrices rectangulaires.*)

let pretraite matrice b=
 let m = Array.length matrice in
 let ordre = (defaut m) in
 let lignes_liees = pivot_gauss  matrice ordre b in
 let n =  ((Array.length matrice.(0)) - lignes_liees) in
 let resultat_matrice = Array.make_matrix m n false in
 let resultat_vecteur = Array.make n false in
   for i = n  to  (n-1 + lignes_liees) do
     if b.(i) then raise Pas_de_solution
   done;
   for j = 0 to n - 1 do
     resultat_vecteur.(j) <- b.(j);
     for i = 0 to (m-1) do
       resultat_matrice.(i).(j) <- matrice.(i).(j)
     done;
   done;
   (ordre,(resultat_matrice,resultat_vecteur));;



(*                                                                   *)
(*                   Solutions de systèmes linéaires                 *)
(*                                                                   *)



(*Créé une solution à un système une fois que gauss est passé dessus*)

let k_solution nombre matrice b =
  let m = Array.length matrice in
  let n = Array.length matrice.(0) in
  let partie_B = decode nombre (m-n) in
  let partie_Id = Array.copy b in
    for j = 0 to n-1 do
      for k = n to m-1 do
        partie_Id.(j) <- xor partie_Id.(j) (matrice.(k).(j) && partie_B.(k-n))
      done;
    done;
    Array.append partie_Id partie_B;;

(* On met la matrice sous forme Id | B, la solution minimale correspond à mettre à false
toutes les variables en face de B*)

let solution_minimale matrice b =
 let (ordre,(resultat_matrice,resultat_vecteur)) = pretraite (copy_matrice matrice) (Array.copy b) in
   permutation ordre (k_solution 0 resultat_matrice resultat_vecteur);;

(* Un test en temps polynômial pour savoir si la solution est minimale *)

let est_minimale solution matrice =
  let n = Array.length matrice.(0) in
  let liste = liste_position solution in
    if  List.length liste > n then false
    else
      rang_maximal (sous_matrice matrice liste);;


(*Trouve toute les solutions minimales, en les testant toutes une à une pour voir si elles sont minimales. Se fait en temps exponentiel.*)

let brut_force matrice b =
  let (ordre,(resultat_matrice,resultat_vecteur))= pretraite (copy_matrice matrice) (Array.copy b) in
  let m = Array.length ordre in
  let n = Array.length resultat_vecteur in
    (* Enumère toutes les solutions possibles correspondant à la partie B de la matrice et les prolonge de manière unique en une solution du système*)
  let rec boucle i =
    match i with
      | -1 -> []
      | _ ->
	  let sol = k_solution i resultat_matrice resultat_vecteur in
	    if est_minimale sol resultat_matrice 
	    then  (permutation ordre sol):: (boucle (i-1))
	    else  boucle (i-1)
  in let resultat =  boucle ((exp_2 (m-n)) -1) in
    resultat
;;

(*Un algo qui énumère les ensembles indépendants et qui essaye de les compléter en un cycle,
c'est l'algo qui marche bien sur les matrices de Hamming*)

let rec oubli liste =
  match liste with
    |[] -> []
    |a::s -> snd a :: oubli s
;;

let solution_correcte matrice liste =
  let m = Array.length matrice in
  let index = scan_matrice (fst(List.hd liste) + 1) (somme_liste (oubli liste)) matrice in
  let solution = Array.make m false in 
  let rec construit_sol l =
    match l with
      |a :: []-> ()
      |(n,v) :: s -> solution.(n)<- true; construit_sol s
  in
    if index >= 0 then (construit_sol liste;solution.(index)<-true; (true, solution))
    else (false, [||]) ;;


(*Créé des ensembles de vecteurs indépendants et on vérifie que leur somme est un vecteur*)

let brut_force2 matrice b =
  let delai = ref 0 in
  let delai_max = ref 0 in
  let m = Array.length matrice in
  let n = Array.length matrice.(0) in
  let index = scan_matrice 0 b matrice in  
  let rec boucle i liste =
    let t = List.length liste in
      if t = 0 then []
      else if i = m then 
	let a :: s = liste in
	  (incremente delai; boucle ((fst a)+1) s)
      else let liste_vecteur = matrice.(i) :: (oubli liste) in
      let liste_numerotee = (i,matrice.(i))::liste in
	if  rang_maximal (Array.of_list liste_vecteur)
	then 
	  let (a,b) = solution_correcte matrice liste_numerotee in
	    if   t < n-1  
	    then 
	      if a  
	      then(init delai delai_max; b :: boucle  (i+1) liste_numerotee)
	      else (incremente delai; boucle (i+1) liste_numerotee)
	    else 
	      if a  
	      then (init delai delai_max; b :: boucle  (i+1) liste)
	      else  (incremente delai ; boucle (i+1) liste)
	else  (incremente delai ; boucle (i+1) liste)
  in if index >=0 
    then (!delai_max, (Array.init m (fun x -> x = index) ):: boucle 0 [(0,b)])
    else (!delai_max, boucle 0 [(0,b)]);;


(*                                                                   *)
(*                  Algorithme probabiliste d'énumération            *)
(*                                                                   *)

(*Un algo probabiliste, qui dépends de la qualité du tirage d'une solution,
plus il est uniforme, meilleur sera le résultat. Il tente de tirer param de fois une nouvelle solution
et si il n'y arrive pas il s'arrête*)
(*Utiliser set pour accélérer*)

let proba_solution tirage param matrice b =
  let rec boucle i liste =
    if i < param
    then 
      let sol = tirage matrice b in
	if List.mem sol liste 
	then boucle (i+1) liste
	else boucle 0 (sol :: liste)
    else liste
  in boucle 0 [];;

(*Tire une solution de manière uniforme et trouve une solution plus petite*)
(*le tirage 1 et 2 sont devenus faux, les corriger, problème de la fonction solution minimale
peut être que dans ce sous programme car je fais un effet de bord de merde!*)


let tirage1 matrice b =
  let (ordre,(resultat_matrice,resultat_vecteur))= pretraite (copy_matrice matrice) (Array.copy b) in
  let m = Array.length ordre in
  let n = Array.length resultat_vecteur in
  let res = k_solution (Random.int (exp_2 (m-n) -1)) resultat_matrice resultat_vecteur in
  let liste = liste_position res in
  let petite_matrice = sous_matrice resultat_matrice liste in
  let petite_sol = solution_minimale petite_matrice resultat_vecteur in
  let sol = Array.create m false in
  let rec boucle l i =
    match l with
      |[]-> ()
      |a :: s -> (sol.(a)<- petite_sol.(i) ; boucle s (i+1))
  in boucle liste 0;
    permutation ordre sol;;

(*Mélange les colonnes de la matrice pour avoir toutes les éliminations de Gauss possibles*) 

let tirage2 matrice b=
  let m = Array.length matrice in 
  let perm = permutation_aleatoire m in
  let sol =  solution_minimale (permutation perm matrice)  b in
   depermute perm sol;;


(*Tire une permutation, ce qui donne un ordre lexicographique, et on construit la plus petite solution pour cet ordre,
 argh c'est la même loi que la technique d'avant*)

let tirage3 matrice b =
  let m = Array.length matrice in  
  let ordre = permutation_aleatoire m in
  let solution  = Array.create m false in
  let def = Array.create (Array.length matrice.(0)) false in
  let rec boucle vect i =
    if vect = def then solution
    else
      if i = m then (solution.(ordre.(m-1))<-true; solution)
      else
	try ignore(pretraite (sous_matrice matrice (Array.to_list (Array.sub ordre i (m-i)))) (Array.copy vect)); 
	  boucle vect (i+1)
	with Pas_de_solution -> solution.(ordre.(i-1))<- true ; boucle (somme_vecteur vect matrice.(ordre.(i-1))) (i+1)
  in boucle (Array.copy b) 1 ;;

(*Algorithme d'énumération des solutions minimales d'une matrice de Hamming
à rendre bien récursif terminal pour éviter les stack overflow*)

(*Utilitaire pour afficher la liste des solutions de manière standard,
  qui en plus dit si la solution trouvée l'a déjà été avant*)

let rends_joli liste =
  let t= Array.length (List.hd liste) in
  let res = Array.create ((exp_2 t) -1)  false in
  let rec deplier l1 =
    match l1 with
      |a::[]-> ()
      |a::s-> (res.(encode a -1) <- true ; deplier s)
  in  let last = somme_liste liste in 
    deplier (last :: liste);
    if (encode last) > (encode (List.hd liste)) 
    then (true,res) else (false,res);;

(*Version nettoyée et vérifiée de l'énumération pour les matrices de hamming en énumérant les systèmes libres,
qu'on complète en circuit*)

let enum_hamming n b =
  let delai = ref 0 in
  let delai_max = ref 0 in 
  let rec boucle i liste =
    let t = List.length liste in
      if t = 0 
      then (snd(rends_joli [b])) :: []   (*Rajoute la solution à un vecteur*)
      else
	if i = (exp_2 n)
	then let a::s = liste in (incremente delai ; boucle ((encode a) +1) s)
	else let courant = (decode i n) :: liste in	  
	  if rang_maximal (Array.of_list courant) 
	  then (*si c'est de rang maximal, on a une solution minimale qu'il faut jeter quand elle n'est pas dans le bon ordre pour éviter les répétitions*)
	    let (c,d) = (rends_joli courant) in 	      
	      if t < (n-1) 
	      then  
		if c 
		then  (init delai delai_max; d :: boucle (i+1)  courant) 
		else (incremente delai ; boucle (i+1)  courant )
	      else
		if c 
		then (init delai delai_max; d :: boucle (i+1)  liste) 
		else  (incremente delai ; boucle (i+1)  liste) 
	  else  (incremente delai ;  boucle (i+1) liste)
  in  ( !delai_max, boucle 1 [b]);;


(*                                                                   *)
(*               Enumération IncremP grâce aux techniques            *)
(*                       de matroïdes                                *)
(*                                                                   *)


(*Teste si la liste ne contient pas une intersection de sol1 et sol2*)

let clos_intersection sol1 sol2 liste =
  let rec boucle s1 s2 l pivot =
    match l  with
      |[]-> true
      |sol3::s ->
	 let res = ref true in
	   for j = 0 to (Array.length s1) -1 do
	     if j = pivot
	     then  res := !res && (not sol3.(j))
	     else  
	       if s1.(j) || s2.(j)
	       then ()
	       else res := !res && (not sol3.(j))  
	   done; 
	   if !res then false else boucle s1 s2 s pivot
  in
  let n = Array.length sol1 in
  let i = ref 0 in
    while (!i<n) & not( sol1.(!i) &  sol2.(!i)) do
      i := !i + 1
    done;
    if !i = n then false
    else boucle sol1 sol2 liste !i
 ;;

(*Donne les solutions fondamentales d'une base quelconque ou juste la première*)

let solution_homogene matrice=
  let (ordre,(resultat_matrice,resultat_vecteur)) = pretraite (copy_matrice matrice) (Array.create (Array.length matrice.(0)) false) in
  let m = Array.length resultat_matrice in
  let n = Array.length resultat_matrice.(0) -1 in
  let solution = Array.make m false in
    solution.(n+1)<-true;
    for i = 0 to n do
      solution.(i)<-resultat_matrice.(n+1).(i)
    done;
    permutation ordre solution;;


let solutions_fondamentales matrice = 
  let (ordre,(resultat_matrice,resultat_vecteur)) = pretraite (copy_matrice matrice) (Array.create (Array.length matrice.(0)) false) in
  let m = Array.length resultat_matrice   in
  let n = Array.length resultat_matrice.(0) -1 in
  let rec boucle l k=
    if k = 0 
    then l
    else
      let solution = Array.make m false in
      solution.(n+k)<-true;
      for i = 0 to n do
	solution.(i)<-resultat_matrice.(n+k).(i)
      done;
      boucle ((permutation ordre solution)::l) (k-1)
  in boucle [] (m-n-1);;


(*Donne une solution dans l'intersection de sol1 et sol2,
index out of bond si on compare deux solutions égales*)

let new_intersection matrice sol1 sol2 =
  let l = liste_position (somme_vecteur sol1 sol2) in
  let sol = solution_homogene (sous_matrice matrice l)in
  let res = Array.create (Array.length sol1) false
  in let rec boucle liste i=
      match liste with
	|[]->()
	|a::s -> if sol.(i) 
	  then (res.(a) <- true;  boucle s (i+1))
	  else  boucle s (i+1)
  in boucle l 0;
    res
;;

(*Bug si on a deux solutions égales : circuit ne doit pas être dans les listes
et l1 et l2 sont disjointes.
Calcule le "produit" du circuit avec la liste l1 et le rajoute à l2.*)

let produit circuit l1 l2 matrice =
  let rec prod liste1 liste2 =
    match liste1 with
      |[]-> liste2
      |a::s ->     
	 if clos_intersection a circuit (l1@l2) 
	 then prod s ((new_intersection matrice circuit a)::liste2)
	 else  prod s liste2 
 in prod l1 l2;;

let produit_cycle liste matrice=
  let rec boucle l1 l2 =
    match l1 with
      |[]-> l2 
      |a::s -> boucle s (produit a s l2 matrice)
  in boucle liste [];;

let enum_niveau matrice =
  let rec boucle l i k=
    let new_liste = l@(produit_cycle l matrice) in
    let t = List.length new_liste in
      if t = k then (i,l)
      else boucle new_liste (i+1) t
  in boucle (solutions_fondamentales matrice) 0 0;; 

(*Ne maintiens pas des listes disjointes*)
let enum_homogene matrice =
  let rec boucle l1 l2 i=
    match l2 with 
      |[]-> l1
      |a :: s -> boucle (a::l1) (produit a l1 s matrice) (i+1)
  in 
  let l = solutions_fondamentales matrice in
    boucle l (produit_cycle l matrice) 0;;


(* Polyoutput pour enum affine: on énumère tous les cycles
et ceux contenant b correspondent à des solutions minimales*)
(*Peut être transformé en incremP, réfléchir si ca améliore la complexité globale, a priori non*)

let enum_improved matrice b =
  let m = Array.length matrice in
  let rec boucle l =
    match l with
      |[]-> []
      |a::s  -> if a.(m) then (Array.init m (fun i -> a.(i))) :: boucle s else boucle s
  in boucle (enum_homogene (ajoute_colonne matrice b))
;;

(*                                                                   *)
(*                             Statistiques                          *)
(*                                                                   *)


(*Un petit test pour trouver le nombre de solutions d'un système et leur écart temporel moyen*)
(*Permet de voir si une méthode est incremP ou delayP ou rien du tout*)

let statistique methode m n echantillon=
 let nombre_max = ref 0 in
 let nombre_min = ref (exp_2 (m-n)) in
 let nombre_moyen = ref 0 in
 let delai_max = ref 0 in
   for i = 0 to echantillon - 1 do
     try let (a,b) = (methode (matrice_aleatoire m n) (vecteur_aleatoire n)) in
     let t = List.length b in
       print_int t;
       print_string "\n";
       nombre_max := max (!nombre_max) t;
       nombre_min := min (!nombre_min) t;
       nombre_moyen := (!nombre_moyen) + t;
       delai_max := max (!delai_max) a;
     with Pas_de_solution -> ()
   done;
   (((!nombre_max,!nombre_min),(!nombre_moyen/echantillon)),!delai_max);;

(* Teste l'efficacité des différents tirages aléatoires*)

let test_proba m n param= 
  Random.self_init ();
  let matrice = matrice_aleatoire m n in
  let vecteur = vecteur_aleatoire n in 
  let nombre_sol1 = ref 0 in
  let nombre_sol2 = ref 0 in
  let nombre_sol3 = ref 0 in
    for i = 1 to 20 do
      nombre_sol1:= (!nombre_sol1) + List.length (proba_solution tirage1 param matrice vecteur);
      nombre_sol2:= (!nombre_sol2) + List.length (proba_solution tirage2 param matrice vecteur);
      nombre_sol3:= (!nombre_sol3) + List.length (proba_solution tirage3 param matrice vecteur);
    done;
   ((((!nombre_sol1)/20,(!nombre_sol2)/20),(!nombre_sol3)/20),List.length(brut_force matrice vecteur));;
