module SumOfMultiples

let sum (numbers: int list) (upperBound: int): int =
    let whichDivisible n = ( List.map (fun x -> x<>0 && n%x = 0 ) numbers )
    let allDivisible n = List.fold (||) false ( whichDivisible n )
    let y = List.filter allDivisible [1..upperBound-1]
    List.sum y