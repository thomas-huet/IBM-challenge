let sqrt n = int_of_float (sqrt (float n))

let is_square n =
  let r = sqrt n in
  n = r * r

let delta d =
  let rec delta i =
    let i2 = i * i in
    if is_square (i2 + d) then
      i2 :: delta (i + 1)
    else if d <= 2 * i then
      []
    else
      delta (i + 1)
  in
  delta 1

let choose n l =
  let rec rev acc = function
  |[] -> acc
  | h :: t -> rev (h :: acc) t
  in
  let rec choose acc pre tail tl n =
    if n = 0 then pre :: acc
    else if tl = n then rev pre tail :: acc
    else match tail with
      | [] -> invalid_arg "not enough elements"
      | h :: t ->
        choose (choose acc pre t (tl - 1) n) (h :: pre) t (tl - 1) (n - 1)
  in
  let tl = List.length l in
  if n > tl then []
  else choose [] [] l tl n

let print_result l =
  if List.length l > 3 then begin
    List.iter
      (fun (i, [a; b; c; d]) ->
        Printf.printf "%d + %d = %d\n%d + %d = %d\n%d + %d = %d\n%d + %d = %d\n"
          d i (d+i) c i (c+i) b i (b+i) a i (a+i))
      l;
    Printf.printf "\n%!"
  end

let main n =
  let h = Hashtbl.create 10000 in
  let add i [a; b; c; d] =
    let k = (c - d, b - d, a - d) in
    if Hashtbl.mem h k then
      print_result ((i, [a; b; c; d]) :: Hashtbl.find_all h k);
    Hashtbl.add h k (i, [a; b; c; d])
  in
  for i = 1 to n do
    let fours = choose 4 (delta i) in
    List.iter (add i) fours
  done
