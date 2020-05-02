module T = Domainslib.Task
let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let mat_size = try int_of_string Sys.argv.(2) with _ -> 1200
let chunk_size = try int_of_string Sys.argv.(3) with _ -> 16

module SquareMatrix = struct

  let create_seq f : float array =
    let fa = Array.create_float (mat_size * mat_size) in
    for i = 0 to mat_size * mat_size - 1 do
      fa.(i) <- f (i / mat_size) (i mod mat_size)
    done;
    fa
  let create pool f : float array =
    let fa = Array.create_float (mat_size * mat_size) in
    T.parallel_for pool ~chunk_size:(mat_size * mat_size / num_domains) ~start:0
    ~finish:( mat_size * mat_size - 1) ~body:(fun i ->
      fa.(i) <- f (i / mat_size) (i mod mat_size));
    fa

  let get (m : float array) r c = m.(r * mat_size + c)
  let set (m : float array) r c v = m.(r * mat_size + c) <- v
  let copy_par pool a =
    let len = Array.length a in
    let res = Array.create_float len in
    T.parallel_for pool ~chunk_size:(len/num_domains) ~start:0 ~finish:(len - 1)
    ~body:(fun i -> res.(i) <- a.(i));
    res
end

open SquareMatrix

let lup pool (a0 : float array) =
  let a = copy_par pool a0 in
  for k = 0 to (mat_size - 2) do
  T.parallel_for pool ~chunk_size:chunk_size ~start:(k + 1) ~finish:(mat_size  -1)
  ~body:(fun row ->
    let factor = get a row k /. get a k k in
    for col = k + 1 to mat_size-1 do
      set a row col (get a row col -. factor *. (get a k col))
      done;
    set a row k factor )
  done ;
  a

let () =
  let pool = T.setup_pool ~num_domains:(num_domains - 1) in
  let a = create_seq (fun _ _ -> (Random.float 100.0) +. 1.0 ) in (* (Random.float 100.0)+.1.0*)
  let lu = lup pool a in
  let _l = create pool (fun i j -> if i > j then get lu i j else if i = j then 1.0 else 0.0) in
  let _u = create pool (fun i j -> if i <= j then get lu i j else 0.0) in
  T.teardown_pool pool
