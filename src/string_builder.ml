    (* Constantes *)
    (**
        valid chars : 32 - 126 (95) 
        numbers : 48 - 57 (10)
        maj letters : 65 - 90 (26)
        min letters : 97 - 122 (26)
    *)
    (** la valeur minimale de char code *)
    let min_char_code = 97

    (** la valeur maximale de char code *)
    let max_char_code = 122

    (** la longueur maximale pour un random string *)
    let max_random_string_length = 128

    (** la profondeur des random strings pour calculer la valeur de perd *)
    let max_depth_random_string_for_gains = 12
    (* Constantes *)

    (* Q1 *)
    (* Définir le type string_builder *)
    type string_builder = 
    | Mot of string * int
    | Node of string_builder * string_builder

    (****** les fonctions à l'aide ******)
    (**
        @param sb : un string_builder
        @return : la longueur totale pour les string dans ce string_buidler
    *)
    let rec length sb = match sb with
    | Mot (s, l) -> l
    | Node (sb1, sb2) -> (length sb1) + (length sb2)

    (**
        @param sb : un string_builder
        @return : le nombre de mot total
    *)
    let rec nb_noeud sb = match sb with
    | Mot (s, l) -> 1
    | Node (sb1, sb2) -> (nb_noeud sb1) + (nb_noeud sb2)

    (**
        @param sb : un string_builder
        @return : la profondeur maximale d'un string_builder
    *)
    let max_depth sb =
        let rec dep sb (curr, max) = match sb with
        | Mot (s, l) -> 
            if curr > max then (curr, curr) else (curr, max)
        | Node (sb1, sb2) -> 
            let (c1, m1) = dep sb1 (curr+1, max) in 
            let (c2, m2) = dep sb2 (curr+1, max) in
            if m1 > m2 then (c1, m1) else (c2, m2) in 
        let (_, res) = dep sb (0, 0) in
        res
    (****** les fonctions à l'aide ******)

    (**
        @param chaine : une chaîne de caractères
        @return : le string_builder constitué d'une seule feuille correspondant
    *)
    let word chaine = Mot (chaine, String.length chaine)

    (**
        @param sb1 : premier string_builder
        @param sb2 : deuxième string_builder
        @return : nouveau string_builder résultant de leur concaténation
     *)
    let concat sb1 sb2 = Node(sb1, sb2)

    (* Q2 *)
    (** 
        @param sb : un string_builder
        @param i : un entier, indice
        @raise Invalid_argument index out of bound
            quand i < 0 ou i > longueur de string du string_builder - 1
        @return : un caractère de string du string_builder à la position i
    *)
    let rec char_at sb i = match sb with
    | Mot(s, l) -> String.get s i
    | Node (sb1, sb2) -> 
        let l = length sb1 in 
        if l > i then
            char_at sb1 i
        else
            char_at sb2 (i - l)

    (* Q3 *)
    (**
        @param sb : un string_builder
        @param i : un entier, indice de début (inclus)
        @param m : un entier, la longeur du sub_string
        @raise Invalid_argument
            quand i < 0 ou i > longueur de string du string_builder - 1
                ou m < 0 ou  i + m > longueur de string du string_builder
        @return : un sous string_builder de sb de i à i+m-1
     *)
    let rec sub_string sb i m = 
        if i < 0 then raise (Invalid_argument "Index out of bound") else 
        if m < 1 then raise (Invalid_argument "Length should be positive") else
        if (m + i) > (length sb) then raise (Invalid_argument "Length of substring out of bound") else
        match sb with
        | Mot (s, l) -> word (String.sub s i m)
        | Node (sb1, sb2) -> 
            let l = length sb1 in 
            if (i + m) <= l then
                sub_string sb1 i m
            else if i >= l then
                sub_string sb2 (i-l) m
            else
                concat (sub_string sb1 i (l-i)) (sub_string sb2 0 (m-l+i))

    (* Q4 *)
    (**
        @param sb : un string_builder
        @return : le cost de l'accès de string_builder
            cost = sum ((length m) * (depth m))
     *)
    let cost sb = 
        let rec c sb depth = match sb with
        | Mot (s, l) -> depth * l
        | Node (sb1, sb2) -> (c sb1 (depth+1)) + (c sb2 (depth+1)) in
        c sb 0
    
    (* Q5 *)
    (***** les fonctions pour random string *****)
    (** le nombre de caractère dans l'intervalle de char donnée*)
    let ascii_length = max_char_code - min_char_code + 1

    (**
        @param i : le nombre de caractères d'un string
        @return : un random string
    *)
    let rec get_random_str i = 
        if (i < 1) then raise (Invalid_argument "The length of string should be positive")
        else if (i = 1) then 
            (String.make 1 (Char.chr (min_char_code + Random.int ascii_length)))
        else
            (String.make 1 (Char.chr (min_char_code + Random.int ascii_length))) ^ (get_random_str (i-1))

    (**
        @param m : un mot
        @param sb : un string_builder
        @return : le string_builder en ajoutant aléatoirement m
    *)
    let rec add_node m sb = match sb with
        | Mot (s, l) -> 
            if Random.bool() then
                Node(Mot (s, l), m)
            else
                Node(m, Mot (s, l))
        | Node (sb1, sb2) -> 
            if Random.bool() then
                Node(add_node m sb1, sb2)
            else
                Node(sb1, add_node m sb2)
    (***** les fonctions pour random string *****)

    (**
        @param i : le profondeur
        @return : un string_builder de profondeur i
     *)
    let random_string i = 
        if i < 0 then raise (Invalid_argument "The depth shouldn't be negative") else
        let rec generate_random_string sb i = 
            if (max_depth sb) < i then
                generate_random_string (add_node (word (get_random_str ((Random.int max_random_string_length)+1))) sb) i
            else
                sb
            in 
        generate_random_string (word (get_random_str ((Random.int max_random_string_length)+1))) i

    (* Q6 *)
    (**
        @param sb : un string_builder
        @return : une liste de string qui représente dans l'ordre le string_builder donné
     *)
    let rec list_of_string sb = match sb with
    | Mot (s, l) -> [s]
    | Node (sb1, sb2) -> (list_of_string sb1) @ (list_of_string sb2)

    (* Q7 *)
    (* 
        first version
        @param sb : un string_builder
        @return : un string_builder équilibré

        let balance sb = 
        let lst = list_of_string sb in
        let absorb (acc, index) ele = 
            if (index mod 2) = 0 then
                (ele :: acc, index + 1)
            else
                match acc with
                | [] -> ([ele], index + 1)
                | h::t -> ((concat h ele)::t, index + 1) in
        let rec absorb2 sb = 
            if (List.length sb = 1) then
                sb
            else
                let (res, ind) = List.fold_left absorb ([], 0) sb in 
                absorb2 (List.rev(res)) in
        let lst_sb = List.map (fun x -> word x) lst in
        List.hd (absorb2 lst_sb) 
    *)
    
    (***** les fonctions balance *****)
    (**
        @param lst : une liste à découper
        @return : une paire de listes équilibrées
                === la différence de la longueur total de caractères deux listes est minimum
    *)
    let split lst = 
        let rec split0 lst1 lst2 len1 len2 = match lst2 with
        | [] -> raise (Invalid_argument "Invalid list")
        | Node (sb1, sb2) :: t -> raise (Invalid_argument "Not a mot list")
        | Mot (s, l)::t -> 
            if (len1 + l) > (len2 - l) then
                let last = len2 - len1 in 
                let curr = len1 - len2 + l + l in
                if (curr < last) then
                    (lst1@[Mot (s, l)], t)
                else
                    (lst1, lst2)
            else
                split0 (lst1@[Mot (s, l)]) t (len1 + l) (len2 - l) in
        split0 [] lst 0 (List.fold_left (fun acc x -> acc + length x) 0 lst)
    
    (**
        @param lst : une liste de mot à transférer en un arbre équilibré
        @return : un arbre équilibré
    *)
    let rec treefy lst = 
        let len = List.length lst in 
        if len < 1 then raise (Invalid_argument "Empty list") else
        if len = 1 then List.hd lst else
        if len = 2 then concat (List.hd lst) (List.nth lst 1) else
        let (lft, rght) = split lst in 
        concat (treefy lft) (treefy rght)
    (***** les fonctions balance *****)

    (**
        @param sb : un arbre peut-être non équilibré
        @return : un arbre équilibré
    *)
    let balance sb = 
        let lst = list_of_string sb in 
        treefy (List.map word lst)


    (* Q8 *)
    (***** les fonctions pour gains *****)
    (**
        @param dep : la profondeur
        @return : la différence de cost avant et après l'équilibrage d'un arbre
    *)
    let get_cost_diff dep = 
            let t = random_string dep in
            (cost t) - (cost (balance t))
    (***** les fonctions pour gains *****)
    
    (**
        @param dep : le depth pour tous les arbres générés
        @param n : nombre de arbres générés
        @return : cost : (min * max * moyenne * median)
     *)
    let gains dep n = 
        if dep > max_depth_random_string_for_gains then raise (Invalid_argument "The depth is too large") else
        let rec generate n acc = 
            if n < 1 then raise (Invalid_argument "The number of trees should be positive") else
            if n = 1 then (get_cost_diff dep):: acc else
            generate (n-1) ((get_cost_diff dep) :: acc) in
        let lst = List.sort (fun x y -> x - y) (generate n []) in
        let sum = List.fold_left (fun acc x -> acc + x) 0 lst in
        ((List.hd lst), (List.hd (List.rev lst)), (float_of_int sum /. float_of_int (List.length lst)), List.nth lst (List.length lst / 2))