module Grains

let square (n: int): Result<uint64,string> = 
    match n with
    | n when n < 0 -> Error "Cannot be negative "
    | n when n >= 64 -> Error "Cannot be that big"
    | _ -> Ok ( pown (uint64 2) n )
    
let total: Result<uint64,string> = 
    let res = uint64 0
    let squares = List.map square [0..63]
    let x = square 5
    x.
    List.sumBy (fun x -> x.ResultValue) squares
