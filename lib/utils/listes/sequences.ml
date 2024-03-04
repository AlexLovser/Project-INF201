

let rec get_line_or_n_cases (
  m: int, 
  direction: int, 
  start_point: case, 
dim: dimension): case int =
  let i, j, k = start_point in 
  match m with 
  | 1 -> [start_point]
  | _ -> (if est_dans_etoile (start_point, dim) then [start_point] else [])
  @
  get_line_or_n_cases(m - 1, direction, case (i + direction, j, k), dim)



(** renvoit la liste des case inclus dans le triangle de bas *)
let rec remplir_triangle_bas (m: int, start_point: case): case list = 
  let i, j, k = start_point in 
  match (m-1) with
  | 0 -> []
  | _ -> 
    get_line_or_n_cases(m, -1, start_point, dim)
    @ 
    remplir_triangle_bas(m - 1, case (i + 1, j - 1, k)) 

;;


