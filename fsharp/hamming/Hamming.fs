module Hamming

let distance (strand1: string) (strand2: string): int option = 
    if strand1.Length = strand2.Length then
        let isDifferent (c1,c2: char) = if c1 = c2 then 0 else 1
        Seq.zip strand1 strand2
        |> Seq.map isDifferent
        |> Seq.sum
        |> Some
    else
        None