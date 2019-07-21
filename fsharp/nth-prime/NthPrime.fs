module NthPrime

let primer (state: int list): (int * int list) option =
    let isprime n =
        let rec check i =
            i > n/2 || (n % i <> 0 && check (i + 1))
        check 2
    for candidate in [ state.Head +1 .. ]

    if state.Length > 1000 then None
    else 
        Some( nextPrime, nextPrime::state ) 

let prime nth : int option = 
    if nth < 1 then None
    else
        //let primes = [|2;3|]
        let primes = seq { for n in 1..nth do if isprime n then yield n }        
        let px = Seq.unfold primer [2]
        Seq.iter (fun elem -> printf "%d " elem) seqFirst5MultiplesOf10
        primes.[nth]
        