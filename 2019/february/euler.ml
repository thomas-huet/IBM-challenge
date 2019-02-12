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
  let n = ref 1 in
  let v = ref 2 in
  while true do
    incr n;
    v := !v + (phi !n);
    if all_digits_once !v then begin
      Printf.printf "== %d: %x ==\n%!" !n !v;
      exit 0
    end;
    if !n mod 1_000_000 = 0 then
      Printf.eprintf "\r%d: %x%!" !n !v
  done
