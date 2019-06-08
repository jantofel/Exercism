module LargestSeriesProduct

let rec myFold accumulator accFn inputs =
    match inputs with
    | [] -> Some accumulator
    | None :: _ -> None
    | Some n :: rest -> myFold (accFn accumulator n) accFn rest

let toInt (n: char): int option =
    let res = int(n) - int('0')
    if 0 <= res && res <= 9 then Some res
    else None
    
let largestProduct (input: string) (seriesLength: int) : int option =
    if input.Length < seriesLength then None
    elif seriesLength < 0 then None
    elif input.Length = 0 || seriesLength = 0 then Some 1
    else 
        let product (start: int): int option =
            input.ToCharArray( start, seriesLength )
            |> List.ofArray
            |> List.map toInt
            |> myFold 1 (*)
        [0..(input.Length-seriesLength)]
        |> List.map product 
        |> myFold 0 max