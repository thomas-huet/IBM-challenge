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
    done;
    Array.for_all (fun b -> b) a
  end

let rec find_answer n v =
  if all_digits_once v then
    n
  else
    find_answer (n + 1) (Z.add v (Z.of_int (phi (n + 1))))

let () =
  let ans = find_answer 1 (Z.of_int 2) in
  Printf.printf "%d: %s\n" ans (Z.format "%x" (f ans))
