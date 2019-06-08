module LargestSeriesProduct

open Microsoft.VisualBasic.CompilerServices

let rec myMax (maxSoFar:int) (numbers: int option list): int option =
    match numbers with
    | [] -> Some maxSoFar
    | None :: _ -> None
    | Some n :: rest -> myMax (max maxSoFar n) rest

let toInt (n: char): int option =
    let res = int(n) - int('0')
    if 0 <= res && res <= 9 then Some res
    else None

let rec myProduct (productSoFar:int) (numbers: int option list): int option =
    match numbers with
    | [] -> Some productSoFar
    | None :: _ -> None
    | Some n :: rest -> myProduct (productSoFar*n) rest

let largestProduct (input: string) (seriesLength: int) : int option =
    if input.Length < seriesLength then None
    elif seriesLength < 0 then None
    elif input.Length = 0 || seriesLength = 0 then Some 1
    else 
        let product (start: int): int option =
            input.ToCharArray( start, seriesLength )
            |> List.ofArray
            |> List.map toInt
            |> myProduct 1
        [0..(input.Length-seriesLength)]
        |> List.map product 
        |> myMax 0