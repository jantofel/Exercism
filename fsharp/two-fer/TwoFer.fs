module TwoFer

let twoFer (input: string option): string =
    let otherParty = Option.defaultValue "you" input
    sprintf "One for %s, one for me." otherParty