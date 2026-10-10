open Gameoflife
let generation = 100

let print_cells cells =
  List.sort compare cells
  |> List.iter (fun (x, y) -> Printf.printf "(%d, %d) " x y);
  print_newline ()

let () =
  Printf.printf "Generation %d: " generation;
  print_cells (determinefuturegeneration generation starting_map)