module RnaTranscription

let toRna (dna: string): string = 
    let transOne character = 
        match character with
        | 'A' -> 'U'
        | 'T' -> 'A'
        | 'C' -> 'G'
        | 'G' -> 'C'
        | _ -> '_'
    String.map transOne dna
    