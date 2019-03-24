module Hamming

open System.Linq

let distance (strand1: string) (strand2: string): int option = 
    if strand1.Length = strand2.Length then
        let zipped = Seq.zip strand1 strand2
        let cmp (c1,c2: char):int = 
            if c1 = c2 then
                0
            else
                1
        let differences = Seq.map cmp zipped
        let differenceCount: int = Seq.sum differences
        option.Some differenceCount
    else
        option.None