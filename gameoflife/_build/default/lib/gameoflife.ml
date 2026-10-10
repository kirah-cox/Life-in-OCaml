let starting_map = [(0, 0); (0, 1); (1, 2); (0, 2); (2, 1)]
let offset = [-1; 0; 1]

module Cell = struct
  type t = int * int
  let compare = Stdlib.compare
end

module CellSet = Set.Make (Cell)

let neighbors (x, y) = 
  let rec getyoffsets offsetlist xoffset =
    match offsetlist with
    | [] -> []
    | yoffset :: rest ->
      if xoffset = 0 && yoffset = 0 then getyoffsets rest xoffset
      else (x + xoffset, y + yoffset) :: getyoffsets rest xoffset
  in
  let rec getxoffsets offsetlist = 
    match offsetlist with
    | [] -> []
    | xoffset :: rest -> getyoffsets offset xoffset @ getxoffsets rest
  in
  getxoffsets offset

let determine_alive_neighbors (x, y) alivemap =
  let completeneighborlist = neighbors(x, y) in
  let rec getneighborcells neighborlist alivecell = 
    match neighborlist with
    | [] -> []
    | cell :: rest ->
        if cell = alivecell then cell :: getneighborcells rest alivecell
        else getneighborcells rest alivecell
  in
  let rec getalivecells alivelist = 
    match alivelist with
    | [] -> []
    | cell :: rest -> getneighborcells completeneighborlist cell @ getalivecells rest
  in
  getalivecells alivemap

let determine_if_alive (x, y) cellmap =
  let n = List.length (determine_alive_neighbors (x, y) cellmap) in
  let rec iscellalive cells = 
    match cells with
    | [] -> false
    | cell :: rest ->
      if (x, y) = cell then true
      else iscellalive rest
  in
  if (iscellalive cellmap) then n = 2 || n = 3
  else n = 3

let rec all_neighbors cells = 
  match cells with
  | [] -> []
  | cell :: rest -> neighbors cell @ all_neighbors rest

let cell_survivors alivemap =
  let full_cell_list = CellSet.elements (CellSet.of_list (alivemap @ all_neighbors alivemap))
  in
  List.filter (fun cell -> determine_if_alive cell alivemap) full_cell_list

let rec determinefuturegeneration n alivemap = 
  match n with
  | 0 -> alivemap
  | _ -> determinefuturegeneration (n - 1) (cell_survivors alivemap)