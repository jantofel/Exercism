module Bob

let (|Yelling|_|) (input:string) =
    if input.ToUpper() = input && input.ToLower() <> input then Some(input)
    else None

let (|Question|_|) (input:string) =
    if input.Trim().EndsWith('?') then Some(input)
    else None

let (|Nothing|_|) (input:string) =
    if input.Trim() = "" then Some(input)
    else None

let response (input: string): string =
    match input with
        | Question _ & Yelling _ -> "Calm down, I know what I'm doing!"
        | Nothing _              -> "Fine. Be that way!"
        | Question _             -> "Sure."
        | Yelling _              -> "Whoa, chill out!"
        | _                      -> "Whatever."