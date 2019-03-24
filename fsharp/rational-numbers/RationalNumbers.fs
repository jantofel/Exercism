module RationalNumbers

type RationalNumber = {n:int; d:int}

let create numerator denominator = {n=numerator; d=denominator}

let rec greatestCommonDivisor a b =
    match (a,b) with
    | (_,0) -> a
    | _ -> greatestCommonDivisor b (a%b)

let rec reduce r = 
    let gcd = greatestCommonDivisor r.n r.d
    match (r.n, r.d) with
    | (0,_) -> {n=0; d=1}
    | (_,y) when y<0 -> reduce {n=(-r.n); d=(-r.d)}
    | _ -> 
        match gcd with
        | 1 | -1 -> r
        | _ -> reduce {n=r.n/gcd; d=r.d/gcd}

let add r1 r2 = 
    reduce {n=(r1.n*r2.d + r2.n*r1.d); d=r1.d*r2.d}

let sub r1 r2 = 
    reduce {n=(r1.n*r2.d - r2.n*r1.d); d=r1.d*r2.d}

let mul r1 r2 =
    reduce {n=r1.n*r2.n; d=r1.d*r2.d}

let div r1 r2 = 
    match r1.d * r2.n with
    | 0 -> {n=0;d=0} // can't divide by zero
    | _ -> reduce {n=r1.n*r2.d; d=r2.n*r1.d}

let abs r = 
    let abs' x =
        match x with
        | i when i < 0 -> -x
        | _ -> x
    reduce {n=abs' r.n; d=abs' r.d} 

let exprational n r = 
    reduce {n=(pown r.n n); d=(pown r.d n)}

let expreal r n =
    (float n) ** ( float r.n / float r.d )
