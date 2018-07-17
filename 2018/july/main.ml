let sum (a, b, c) = a + b + c

let product (a, b, c) = a * b * c

let add (a, b, c) n = (a + n, b + n, c + n)

let sqrt n =
  let root = int_of_float (sqrt (float n)) in
  if n = root * root then Some root
  else None

let pair s p =
  match sqrt (s * s - 4 * p) with
  | None -> None
  | Some root ->
    if (s + root) mod 2 = 1 then None
    else Some ((s - root) / 2, (s + root) / 2)

let obscure t =
  let count s p =
    let rec count i =
      if i >= s then 0
      else if p mod i = 0 then
        match pair (s - i) (p / i) with
        | None -> count (i + 1)
        | Some (a, b) ->
          if a < i then 0
          else 1 + count (i + 1)
      else count (i + 1)
    in
    count 1
  in
  count (sum t) (product t) > 1

let rec valid n t = n = 0 || obscure t && valid (n - 1) (add t 1)

let print_obscure n =
  for i = 1 to n do
    for j = i to n do
      for k = j to n do
        if obscure (i, j, k) then Printf.printf "%d %d %d\n" i j k
        else ()
      done
    done
  done

let print_valid y n =
  for i = 1 to n do
    for j = i to n do
      for k = j to n do
        if valid y (i, j, k) then Printf.printf "%d %d %d\n" i j k
        else ()
      done
    done
  done

let () = print_valid 4 100
