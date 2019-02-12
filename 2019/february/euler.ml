let is_prime n =
  let rec p i =
    i * i > n || (n mod i <> 0 && p (i + 1))
  in
  p 2

let primes = Array.of_list (List.filter is_prime (List.init 1000000 (fun i -> i + 2)))

let phi n =
  let n = ref n in
  let ans = ref 1 in
  let i = ref 0 in
  while primes.(!i) * primes.(!i) <= !n do
    if !n mod primes.(!i) = 0 then begin
      ans := !ans * (primes.(!i) - 1);
      n := !n / primes.(!i);
      while !n mod primes.(!i) = 0 do
        ans := !ans * primes.(!i);
        n := !n / primes.(!i)
      done
    end;
    incr i
  done;
  if !n > 1 then
    (!n - 1) * !ans
  else
    !ans

let low = 0x1023456789abcdef

let all_digits_once n =
  n >= low && begin
    let s = Printf.sprintf "%x" n in
    let a = Array.make 16 false in
    for i = 0 to 15 do
      match s.[i] with
      | '0' .. '9' -> a.(Char.code s.[i] - Char.code '0') <- true
      | 'a' .. 'f' -> a.(Char.code s.[i] - Char.code 'a') <- true
      | _ -> failwith "impossible"
    done;
    Array.for_all (fun b -> b) a
  end

let () =
  (*
  let n = ref 1 in
  let v = ref 2 in
  *)
  let n = ref 425615482 in
  let v = ref 0xc39f19af441b9b in
  while true do
    incr n;
    v := !v + (phi !n);
    if all_digits_once !v then
      Printf.printf "\n=== %d: %x ===\n\n%!" !n !v
  done
