let factorize n =
  let rec f n d acc =
    if d * d > n then
      n :: acc
    else if n mod d = 0 then
      f (n / d) d (d :: acc)
    else
      f n (d + 1) acc
  in
  let rec g = function
  | [] -> []
  | h :: t ->
    match g t with
    | [] -> [h, 1]
    | (i, j) :: t ->
      if h = i then (h, j + 1) :: t
      else (h, 1) :: (i, j) :: t
  in
  if n = 1 then
    []
  else
    g (f n 2 [])

let rec pow p n =
  if n = 0 then 1
  else p * pow p (n - 1)

let phi n =
  List.fold_left (fun x (p, n) -> x * pow p (n - 1) * (p - 1)) 1 (factorize n)

let rec f n =
  if n = 1 then Z.of_int 2
  else Z.add (Z.of_int (phi n)) (f (n - 1))

let low = Z.of_string_base 16 "1023456789abcdef"

let all_digits_once n =
  n >= low && begin
    let s = Z.format "%x" n in
    let a = Array.make 16 false in
    for i = 0 to 15 do
      match s.[i] with
      | '0' .. '9' -> a.(Char.code s.[i] - Char.code '0') <- true
      | 'a' .. 'f' -> a.(Char.code s.[i] - Char.code 'a') <- true
      | _ -> failwith "impossible"
    done;
    Array.for_all (fun b -> b) a
  end

let rec find_answer n v =
  if all_digits_once v then
    n
  else
    find_answer (n + 1) (Z.add v (Z.of_int (phi (n + 1))))

let () =
  (*
  let n = ref 1 in
  let v = ref (Z.of_int 2) in
  *)
  let n = ref 50331645 in
  let v = ref (Z.of_string_base 16 "2bc54fa7a2f53") in
  for e = 1 to 100 do
    let m = pow 2 e in
    for i = 1 to m do
      incr n;
      v := Z.add !v (Z.of_int (phi !n))
    done;
    Printf.printf "%d: %s\n%!" !n (Z.format "%x" !v)
  done
