module SaddlePoints

let findAll (predicate:'T -> bool) (list: 'T list): int list =
    list 
    |> List.mapi (fun idx elem -> (idx, elem))
    |> List.filter (fun (idx, elem) -> predicate elem)
    |> List.map (fun (idx, _elem) -> idx)

let rec product (list1: 'T list) (list2: 'U list): ('T * 'U) list =
    match list1 with
    | [] -> []
    | val1 :: rest1 ->
        let rowProduct = List.map ( fun elem -> (val1,elem) ) list2
        rowProduct @ product rest1 list2
        
let saddlePoints (matrix: int list list): (int*int) list =
    if matrix = [[]] then []
    else
        let rowMaxima = List.map List.max matrix
        let colMinima = 
            matrix 
            |> List.transpose
            |> List.map List.min
        let rowIndices = findAll ( fun elem -> List.contains elem colMinima ) rowMaxima
        let colIndices = findAll ( fun elem -> List.contains elem rowMaxima ) colMinima  
        product rowIndices colIndices