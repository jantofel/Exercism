module AllYourBase

let rec fromInput digits inputBase accumulator =
    match inputBase with 
    | i when i < 2 -> None
    | _ ->
        match digits with
        | [] -> Some accumulator
        | digit::ds ->
            match digit with 
            | i when i < 0 -> None
            | i when i >= inputBase -> None 
            | _ -> fromInput ds inputBase ( inputBase * accumulator + digit )

let rec toOutput outputBase maybeNumber accumulator =
    match (outputBase, maybeNumber) with
    | (_, None) -> None
    | (i, _) when i < 2 -> None
    | (_, Some number) -> 
        match number with
        | 0 -> 
            match accumulator with 
            | [] -> Some [0]
            | _ -> Some accumulator
        | _ -> toOutput outputBase (Some (number / outputBase)) ((number % outputBase) :: accumulator)

let rebase digits inputBase outputBase = toOutput outputBase ( fromInput digits inputBase 0 ) []