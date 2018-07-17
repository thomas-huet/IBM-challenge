let sum (a, b, c) = a + b + c

let product (a, b, c) = a * b * c

let add (a, b, c) n = (a + n, b + n, c + n)

(* Hashtbl.find_opt is only available since ocaml 4.05 *)
let find_opt table key = try
  Some (Hashtbl.find table key)
with Not_found -> None

let memo_obscure n =
  let memo = Array.init n Hashtbl.create in
  for s = 3 to n do
    for i = 1 to s / 3 do
      for j = i to (s - i) / 2 do
        let k = s - i - j in
        match find_opt memo.(s - 1) (i * j * k) with
        | None -> Hashtbl.add memo.(s - 1) (i * j * k) false
        | Some false -> Hashtbl.replace memo.(s - 1) (i * j * k) true
        | Some true -> ()
      done
    done
  done;
  memo

let is_obscure memo t =
  find_opt memo.(sum t - 1) (product t) = Some true

let rec valid memo n t = n = 0 || is_obscure memo t && valid memo (n - 1) (add t 1)

let print_obscure n =
  let memo = memo_obscure n in
  for s = 3 to n do
    for i = 1 to s / 3 do
      for j = i to (s - i) / 2 do
        let k = s - i - j in
        if is_obscure memo (i, j, k) then Printf.printf "%d %d %d\n" i j k
        else ()
      done
    done
  done

let print_valid y n =
  let memo = memo_obscure (n + 3 * (y - 1)) in
  for s = 3 to n do
    for i = 1 to s / 3 do
      for j = i to (s - i) / 2 do
        let k = s - i - j in
        if valid memo y (i, j, k) then Printf.printf "%d %d %d\n" i j k
        else ()
      done
    done
  done

let () = print_valid 6 1600
