module SumOfMultiples

let divisibleBy (numbers: int list) (number: int): bool =
    let divisible x = x<>0 && number%x = 0
    numbers
    |> List.map divisible
    |> List.fold (||) false

let sum (numbers: int list) (upperBound: int): int =
    [1..upperBound-1]
    |> List.filter ( divisibleBy numbers )
    |> List.sum