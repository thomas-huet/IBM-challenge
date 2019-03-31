let rec pow x n m =
  if n = 1 then
    x
  else
    let r = pow x (n / 2) m in
    if n mod 2 = 0 then
      (r * r) mod m
    else
      (r * r * x) mod m

let miller_rabin n a =
  let rec dec n =
    if n mod 2 = 1 then
      0, n
    else
      let r, d = dec (n / 2) in
      r + 1, d
  in
  let rec re x r =
    r > 0 && (x = n - 1 || re (x * x mod n) (r - 1))
  in
  n <= a || begin
    let r, d = dec (n - 1) in
    let x = pow a d n in
    x = 1 || re x r
  end

let p n =
  n > 1 &&
  miller_rabin n 2 &&
  miller_rabin n 3 &&
  miller_rabin n 7 &&
  miller_rabin n 61 &&
  miller_rabin n 24251

(*
let prime n =
  let rec p d =
    d >= n || (n mod d <> 0 && p (d + 1))
  in
  p 2
*)

exception Stuck of int * int * int

let grid ?(start = 1) ?(offset = 1) ?(give_up = 10_000_000) n =
  let g = Array.make_matrix n n 0 in
  let next =
    let i = ref start in
    fun () ->
      while not (p (!i * n + offset)) do
        incr i
      done;
      incr i;
      (!i - 1) * n + offset
  in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      g.(i).(j) <- next ();
      if i = n - 1 && j = n - 1 then begin
        let a = ref 0
        and b = ref 0
        and c = ref 0
        in
        for k = 0 to n - 2 do
          a := !a + g.(n - 1).(k);
          b := !b + g.(k).(n - 1);
          c := !c + g.(k).(k)
        done;
        while not (p ((!a + g.(i).(j)) / n)
                && p ((!b + g.(i).(j)) / n)
                && p ((!c + g.(i).(j)) / n)) do
          g.(i).(j) <- next ();
          if g.(i).(j) > give_up then
            raise (Stuck(!c, !b, !a))
        done
      end else if i = n - 1 && j = 0 then begin
        let a = ref 0
        and b = ref 0
        in
        for k = 0 to n - 2 do
          a := !a + g.(k).(0);
          b := !b + g.(k).(n - 1 - k)
        done;
        while not (p ((!a + g.(i).(j)) / n)
                && p ((!b + g.(i).(j)) / n)) do
          g.(i).(j) <- next ();
          if g.(i).(j) > give_up then
            raise (Stuck(!a, !b, 0))
        done
      end else if i = n - 1 then begin
        let a = ref 0 in
        for k = 0 to n - 2 do
          a := !a + g.(k).(j)
        done;
        while not (p ((!a + g.(i).(j)) / n)) do
          g.(i).(j) <- next ()
        done
      end else if j = n - 1 then begin
        let a = ref 0 in
        for k = 0 to n - 2 do
          a := !a + g.(i).(k)
        done;
        while not (p ((!a + g.(i).(j)) / n)) do
          g.(i).(j) <- next ()
        done
      end;
      (*Printf.printf "%3d " g.(i).(j)*)
    done;
    (*Printf.printf "\n"*)
  done;
  g

let build a b c =
  let x = (2 * a + b + c) / 4 in
  let s = 3 * x in
  [a; b; s - a - b; c; x; s - x - c; s - a - c; s - x - b; s - x - a]

let ultra a b c =
  (2 * a + b + c) mod 4 = 0 && begin
    let l = build a b c in
    let l = List.sort_uniq compare l in
    List.length l = 9 && List.for_all p l
  end

let search n =
  for a = 3 to n do
  for b = 3 to n do
  for c = b to n do
    if ultra a b c then
      Printf.printf "%d %d %d\n%!" a b c
  done
  done
  done
