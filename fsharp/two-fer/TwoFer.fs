module TwoFer

let twoFer (input: string option): string =
    let otherParty = 
        match input with
        | None -> "you"
        | Some name -> name
    "One for " + otherParty + ", one for me."