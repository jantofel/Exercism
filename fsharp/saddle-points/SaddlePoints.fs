module SaddlePoints

let filteri (predicate:'T -> bool) (list: 'T list): int list =
    let indexedList = List.mapi (fun idx elem -> (idx, elem)) list
    let filteredIndexedList = List.filter (fun (idx, elem) -> predicate elem) indexedList
    List.map (fun (idx, _elem) -> idx) filteredIndexedList

let rec product accumulator (list1: 'T list) (list2: 'U list): ('T * 'U) list =
    match list1 with
    | [] -> accumulator
    | val1 :: rest1 ->
        let x = List.map (fun elem -> (val1,elem)) list2
        product ( accumulator @ x ) rest1 list2

let saddlePoints (matrix: int list list): (int*int) list =
    if List.isEmpty matrix then []
    else
        let rowMaxima = List.map List.max matrix
        let colMinima = List.map List.min (List.transpose matrix)
        let rowIndices = filteri (fun elem -> List.contains elem colMinima ) rowMaxima
        let colIndices = filteri (fun elem -> List.contains elem rowMaxima ) colMinima  
        product [] rowIndices colIndices