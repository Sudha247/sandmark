module C = Domainslib.Chan

let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let size = try int_of_string Sys.argv.(2) with _ -> 1024
let chunk_size = try int_of_string Sys.argv.(3) with _ -> 16

let matrix_multiply z x y s e =
  (* let x0 = Array.length x  in *)
  let y0 = Array.length y in
  let y1 = if y0 = 0 then 0 else Array.length y.(0) in
  for i = s to e do
    for j = 0 to y1-1 do
      for k = 0 to y0-1 do
        Domain.Sync.poll();
        z.(i).(j) <- z.(i).(j) + x.(i).(k) * y.(k).(j)
      done
    done
  done


type message =
    Work of int * int
  | Quit

let rec create_work c start left =
  if left < chunk_size then begin
    (* Printf.printf "%d %d\n" start (start + left - 1); *)
    C.send c (Work (start, start + left - 1));
    for _i = 1 to num_domains do
(*       print_endline "Quit"; *)
      C.send c Quit
    done
  end else begin
    (* Printf.printf "%d %d\n" start (start + chunk_size - 1); *)
    C.send c (Work (start, start + chunk_size - 1));
    create_work c (start + chunk_size) (left - chunk_size)
  end

let rec worker z x y c =
  match C.recv c with
  | Work(s,e) ->
     matrix_multiply z x y s e; worker z x y c
  | Quit -> ()


let print_matrix mat =
  let x = Array.length mat in
  let y = Array.length mat.(0) in
  for i = 0 to x-1 do
    for j = 0 to y-1 do
      print_int mat.(i).(j); print_string "  "
    done;
    print_newline ();
  done

let () =
  let m1 = Array.init size (fun _ -> Array.init size (fun _ -> 10))
  and m2 = Array.init size (fun _ -> Array.init size (fun _ -> 10))
  and res = Array.init size (fun _ -> Array.make size 0) in
  (* let mat=aux [|[|1;2|];[|3;4|]|] [|[|-3;-8;3|];[|-2;1;4|]|] in *)
  let c = C.make (size/chunk_size + 1 + num_domains) in
  create_work c 0 (size);
  let domains = Array.init (num_domains - 1) (fun _ -> Domain.spawn(fun _ -> worker res m1 m2 c)) in
  worker res m1 m2 c;
  Array.iter Domain.join domains
  (* print_matrix res *)
