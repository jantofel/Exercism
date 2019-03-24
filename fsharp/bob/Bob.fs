module Bob

let response (input: string): string =
    let isQuestion = input.Trim().EndsWith('?')
    let isYelling = input.ToUpper() = input && input.ToLower() <> input
    let isNothing = input.Trim() = ""
    if isQuestion && isYelling then "Calm down, I know what I'm doing!"
    elif isNothing             then "Fine. Be that way!"
    elif isQuestion            then "Sure."
    elif isYelling             then "Whoa, chill out!"
    else                            "Whatever."