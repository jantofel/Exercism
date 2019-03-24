module Grains

let square (n: int): Result<uint64,string> = 
    match n with
    | i when i <= 0 || i > 64 -> Error "square must be between 1 and 64"
    | _ -> Ok ( pown (uint64 2) (n-1) )
    

let total: Result<uint64,string> = 
    let rec mySum sequence (accumulator: uint64): Result<uint64,string>=
        match sequence with
        | [] -> Ok accumulator
        | Error err :: _ -> Error err
        | Ok n:: rest -> mySum rest (n+accumulator)
    // Ideally, I'd want to use sumBy instead, but I don't know how
    mySum (List.map square [1..64]) (uint64 0)
