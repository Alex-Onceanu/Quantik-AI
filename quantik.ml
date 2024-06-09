type piece =
| Sphere
| Cube
| Cylinder
| Cone 

type player = 
| White
| Black

let other (p : player) : player =
  match p with
  | White -> Black
  | Black -> White

type state = {
  white_pieces : (piece * int) list;
  black_pieces : (piece * int) list;
  board : ((piece * player) option) array array
}

let ($$) a b = (a || b) && not (a && b)

let couples : (int * int) list = 
  let l = [0;1;2;3] in
  List.concat_map (fun i' -> List.map (fun j' -> (i', j')) l) l

exception IllegalMove

let cadran (i' : int) (j' : int) : int =
  if i' < 0 || i' > 3 || j' < 0 || j' > 3 then raise IllegalMove
  else if i' <= 1 then (if j' <= 1 then 1 else 2)
  else (if j' <= 1 then 3 else 4)

let is_valid (s : state) ((x, pl) : piece * player) (i : int) (j : int) : bool =
  if i < 0 || i > 3 || j < 0 || j > 3 then raise IllegalMove
  else if Option.is_some s.board.(i).(j) then false else

  let neighbours = List.filter (fun (i', j') -> 
    (i, j) <> (i', j') && (i' = i || j' = j || cadran i j = cadran i' j')) couples in
  
  List.for_all (fun (i', j') -> 
    match s.board.(i').(j') with
    | None -> true
    | Some (x', pl') -> (x, other pl) <> (x', pl')) neighbours

let test_is_valid () : unit =
  let b = Array.make_matrix 4 4 None in 
  b.(0).(2) <- Some (Sphere, Black);
  b.(1).(2) <- Some (Cube, Black);
  b.(1).(3) <- Some (Cylinder, White);
  let s = {white_pieces = [(Sphere, 2); (Cube, 2); (Cylinder, 1)]; black_pieces = [(Sphere, 2)]; board = b} in
  assert (not (is_valid s (Sphere, White) 0 1));
  assert (is_valid s (Cylinder, White) 0 3);
  assert (not (is_valid s (Cube, White) 1 1));
  assert (is_valid s (Sphere, White) 1 1);
  assert (is_valid s (Cube, Black) 1 1);
  assert (not (is_valid s (Cube, White) 0 3));
  try (let _ = is_valid s (Cube, White) 0 2 in ()) with
  | IllegalMove -> ()
  | e -> raise e

let has_piece (s : state) (p : player) (x : piece) : bool =
  let l = match p with
          | White -> s.white_pieces
          | Black -> s.black_pieces in 
  List.exists (fun (y, n) -> y = x) l

let copy_matrix (a : 'a array array) : 'a array array = 
  Array.init (Array.length a) (fun i -> Array.copy a.(i))

(* On suppose que le coup est valide et que pl possède la pièce x *)
let play_piece (s : state) ((x, pl) : piece * player) (i : int) (j : int) : state =
  let s' = match pl with
  | White -> {  white_pieces = List.filter_map (fun (y, nb) -> if y = x then (if nb = 1 then None else Some (y, nb - 1)) else Some (y, nb)) s.white_pieces; 
                black_pieces = s.black_pieces; 
                board = copy_matrix s.board}
  | Black -> {  white_pieces = s.white_pieces; 
                black_pieces = List.filter_map (fun (y, nb) -> if y = x then (if nb = 1 then None else Some (y, nb - 1)) else Some (y, nb)) s.black_pieces;
                board = copy_matrix s.board} in 
  s'.board.(i).(j) <- Some (x, pl);
  s'

(* C'est au tour de pl de jouer *)
let next_states (pl : player) (s : state) : (state * piece * int * int) list =
  let l = match pl with
  | White -> s.white_pieces
  | Black -> s.black_pieces in 

  let possibilities : (piece * int * int) list = 
    List.concat_map (fun (x, nb) -> List.map (fun (i', j') -> (x, i', j')) couples) l in 
  
  List.filter_map (fun (x, i, j) -> 
    if is_valid s (x, pl) i j then Some (play_piece s (x, pl) i j, x, i, j) 
    else None) possibilities

(* On suppose que le dernier coup a été en (i,j) *)
let is_winning_move (s : state) (i : int) (j : int) : bool =
  let every_piece (l : (int * int) list) : bool =
    let sph = ref false in 
    let cub = ref false in 
    let cyl = ref false in
    let con = ref false in 
    List.iter (fun (i', j') ->
      match s.board.(i').(j') with
      | None -> ()
      | Some (Sphere, _) -> sph := true
      | Some (Cube, _) -> cub := true
      | Some (Cylinder, _) -> cyl := true
      | Some (Cone, _) -> con := true) l;
    !sph && !cub && !cyl && !con in

  let l1 = [(0, j); (1, j); (2, j); (3, j)] in 
  let l2 = [(i, 0); (i, 1); (i, 2); (i, 3)] in 
  let l3 = match cadran i j with
  | 1 -> [(0, 0); (0, 1); (1, 0); (1, 1)]
  | 2 -> [(0, 2); (0, 3); (1, 2); (1, 3)]
  | 3 -> [(2, 0); (2, 1); (3, 0); (3, 1)]
  | _ -> [(2, 2); (2, 3); (3, 2); (3, 3)] in 

  List.exists every_piece [l1; l2; l3]

(* pl est le joueur qui a joué le dernier coup (qu'on suppose légal et gagnant) *)
let leaf_heuristic (pl : player) (s : state) : float =
  let count_none (t : ('a option) array array) : int =
    let som = ref 0 in 
    for i = 0 to 3 do
      for j = 0 to 3 do
        if Option.is_none t.(i).(j) then incr som
        else ()
      done
    done;
    !som in
  let ans = 16 + count_none s.board |> float_of_int in
  match pl with
  | White -> ans
  | Black -> -.ans

(* On prend en argument le dernier coup joué, simule une partie aléatoire et renvoie son leaf_heuristic *)
let rec random_final_state_score (s : state) ((x, pl) : piece * player) (i : int) (j : int) : float =
  if is_winning_move s i j then leaf_heuristic pl s
  else 
    let nexts = next_states (other pl) s in
    let n = List.length nexts in
    if n <= 0 then 
      match pl with
      | White -> 16.0
      | Black -> -16.0
    else 
      let (s', x', i', j') = List.nth nexts (Random.int n) in
      random_final_state_score s' (x', other pl) i' j'

(* Renvoie un score : plus il est grand, plus White a de chances de gagner *)
(* On suppose que le dernier coup joué est (x, pl) en (i, j) et qu'il est pas gagnant *)
let node_heuristic (turn : int) (s : state) ((x, pl) : piece * player) (i : int) (j : int) : float =
  let nb_simulations = 8 * (turn + 1) in 
  let total_score = ref 0. in
  for o = 1 to nb_simulations do
    let score = random_final_state_score s (x, pl) i j in 
    total_score := score +. !total_score
  done;
  total_score := !total_score /. float_of_int nb_simulations;
  !total_score

(* On suppose que le dernier coup joué est (x, pl) en (i, j) *)
let heuristic (turn : int) (s : state) ((x, pl) : piece * player) (i : int) (j : int) : float =
  if is_winning_move s i j then leaf_heuristic pl s 
  else node_heuristic turn s (x, pl) i j

(* C'est au tour de pl de jouer, renvoie un coup et l'état du plateau après ce coup, un score de confiance, puis true ssi le jeu est terminé *)
let nigamax (turn : int) (pl_t : player) (s_t : state) : (state * piece * int * int) * float =
  let rec alphabeta (depth : int) (pl : player) (s : state) (alpha0 : float) (beta0 : float) : (state * piece * int * int) * float =
    (* Vaut soit heuristic soit un appel récursif de nigamax *)
    let evaluate (s' : state) ((x', pl') : piece * player) (i' : int) (j' : int) (alpha' : float) (beta' : float) : float =
      if is_winning_move s' i' j' then (if pl' = Black then -1. else 1.) *. leaf_heuristic pl' s'
      else if depth <= 0 then (if pl' = Black then -1. else 1.) *. heuristic turn s' (x', pl') i' j'
      else -.(alphabeta (depth - 1) (other pl') s' (-.beta') (-.alpha') |> snd) in
    
    let nexts = next_states pl s in
    
    let alpha = ref alpha0 in 
    let beta = ref beta0 in 
    let v = ref (-33.0) in 
    let arg = ref (s, Sphere, -1, -1) in
    let stop = ref false in
    List.iter (fun (s', x', i', j') -> 
      if !stop then () 
      else 
        begin
          let t = evaluate s' (x', pl) i' j' !alpha !beta in 
            (if t > !v then 
              begin
                v := t; 
                arg := (s', x', i', j'); 
                if t >= !beta then stop := true else ()
              end
            else ());
          alpha := max !alpha !v
        end) nexts;
    (!arg, !v) in 
    alphabeta (1 + (max 0 (min (8 - turn) (1 + turn / 2)))) pl_t s_t (-33.0) 33.0

(* C'est au tour de pl de jouer, renvoie un coup et l'état du plateau après ce coup (et une heuristique) *)
(* let rec minmax (depth : int) (pl : player) (s : state) : (state * piece * int * int) * float =
  (* Vaut soit heuristic soit un appel récursif de minmax *)
  let evaluate (s' : state) ((x', pl') : piece * player) (i' : int) (j' : int) : float =
    if is_winning_move s' i' j' then leaf_heuristic pl' s'
    else if depth <= 0 then heuristic s' (x', pl') i' j'
    else minmax (depth - 1) (other pl') s' |> snd in

  let nexts = next_states pl s in
  let next_scores = List.map (fun (s', x', i', j') -> ((s', x', i', j'), evaluate s' (x', pl) i' j')) nexts in 
  let (ne, opt) = match pl with 
    | White -> ((s, Sphere, -1, -1), -33.0), fun (y, n) (y', n') -> if n' > n then (y', n') else (y, n)
    | Black -> ((s, Sphere, -1, -1),  33.0), fun (y, n) (y', n') -> if n' > n then (y, n) else (y', n') in 
  List.fold_left opt ne next_scores *)


let board_elt_to_char (x : (piece * player) option) : char =
  let piece_to_string (p : piece) : string =
    match p with
    | Sphere -> "pP"
    | Cylinder -> "yY"
    | Cube -> "uU"
    | Cone -> "oO" in

  match x with
  | None -> '.'
  | Some (p, Black) -> (piece_to_string p).[0]
  | Some (p, _) -> (piece_to_string p).[1]
    
let print_state (s : state) : unit =
  let letters = [| 'a';'b';'c';'d' |] in
  print_endline "   1 2 3 4";
  Array.iteri (fun i t -> Printf.printf "%c  " letters.(i); Array.iter (fun x -> Printf.printf "%c " (board_elt_to_char x)) t; print_newline ()) s.board

let string_of_play (x : piece) (i : int) (j : int) : string =
  let sp = match x with
  | Sphere -> "SP"
  | Cylinder -> "CY"
  | Cube -> "CU"
  | Cone -> "CO" in 
  sp ^ [|"a";"b";"c";"d"|].(i) ^ string_of_int (j + 1)

let play_of_string (s : string) : (piece * int * int) option =
  if String.length s <> 4 then None
  else let sp = (String.make 1 s.[0]) ^ (String.make 1 s.[1]) in 
  let x = match sp with
  | "SP" -> Some Sphere
  | "CY" -> Some Cylinder
  | "CU" -> Some Cube
  | "CO" -> Some Cone
  | _ -> None in 
  try (
  let i = match s.[2] with
          | 'a' -> Some 0
          | 'b' -> Some 1
          | 'c' -> Some 2
          | 'd' -> Some 3
          | _ -> None in 
  let j = int_of_string (String.make 1 s.[3]) in Some (Option.get x, Option.get i, j - 1))
  with
  | _ -> None


let initial_state = {
  white_pieces = [(Sphere, 2);(Cube, 2);(Cylinder, 2);(Cone, 2)];
  black_pieces = [(Sphere, 2);(Cube, 2);(Cylinder, 2);(Cone, 2)];
  board = Array.make_matrix 4 4 None
}

(* Prend en argument le joueur joué par l'ordi, renvoie le gagnant *)
let play (p : player) : unit =
  let s = ref initial_state in 
  let winner = ref None in 
  let ended = ref false in 
  let turn = ref 1 in 
  while not !ended do 
    let continue_asking = ref true in 
    while !continue_asking do 
      print_endline "À votre tour de jouer. Exemple de syntaxe : \"SPc3\"";
      let ans = play_of_string (read_line ()) in 
      match ans with
      | None -> print_endline "Erreur de syntaxe. Merci de réessayer."
      | Some (x', i', j') -> try (
        if not (has_piece !s (other p) x') then
          print_endline "Vous n'avez pas cette pièce. Merci de réessayer."
        else if not (is_valid !s (x', other p) i' j') then 
          print_endline "Coup illégal. Merci de réessayer." 
        else begin
          continue_asking := false;
          s := play_piece !s (x', other p) i' j';
          ended := is_winning_move !s i' j';
          if !ended then winner := Some (other p) else ();
          print_endline "État actuel du plateau :";
          print_state !s;
        end) 
        with
        | IllegalMove -> print_endline "Coup en dehors du plateau. Merci de réessayer." 
        | e -> raise e
    done;
    if !ended then ()
    else 
      print_endline "Au tour de l'ordinateur";
      let ((s', x', i', j'), r) = nigamax !turn p !s in 
      print_state s';
      Printf.printf "A joué %s avec une confiance de %f\n" (string_of_play x' i' j') (100. *. r /. (32. -. 2.0 *. float_of_int !turn));
      s := s';
      ended := is_winning_move s' i' j';
      if !ended then winner := Some p else ();
      print_endline "___________________Tour suivant___________________";
      incr turn
  done;
  match !winner with
  | None -> print_endline "Match nul !"
  | Some w -> if w = p then print_endline "L'ordinateur a gagné !" else print_endline "L'humain a gagné !"

let () : unit =
  print_state initial_state;
  play Black;