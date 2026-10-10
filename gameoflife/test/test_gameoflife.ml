open Gameoflife

let sorted = List.sort compare

let list1 = [(1, 1); (1, 2); (1, 3)]
let list2 = [(0, 2); (1, 2); (2, 2)]

let test_neighbors () =
  assert (List.length (neighbors (5, 5)) = 8);
  assert (List.length (neighbors (0, 0)) = 8)

let test_alive_neighbors () =
  assert (sorted (determine_alive_neighbors (5, 5) [(4, 4); (5, 6); (9, 9)])
          = [(4, 4); (5, 6)])

let test_determine_if_alive () =
  assert (determine_if_alive (1, 2) list1);
  assert (not (determine_if_alive (1, 1) list1))

let test_cell_survivors () =
  assert (cell_survivors [] = []);
  assert (cell_survivors [(3, 3)] = []);
  assert (sorted (cell_survivors list1) = list2)

let test_future_generation () =
  assert (determinefuturegeneration 0 starting_map = starting_map);
  assert (sorted (determinefuturegeneration 3 list1) = list2)

let test_cellset () =
  assert (CellSet.cardinal (CellSet.of_list [(1, 1); (1, 1); (2, 2)]) = 2);
  assert (CellSet.elements (CellSet.of_list [(2, 2); (0, 5); (0, 1)])
          = [(0, 1); (0, 5); (2, 2)])

let () =
  test_neighbors ();
  test_alive_neighbors ();
  test_determine_if_alive ();
  test_cell_survivors ();
  test_future_generation ();
  test_cellset ();
  print_endline "All tests passed!"