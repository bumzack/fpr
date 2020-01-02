module Parser

open System

let safeEquals (it: string) (theOther: string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)



[<Literal>]
let HelpLabel = "Help"



let mapToCoord (x, y) =
    let c: Domain.Coord =
        { X = x
          Y = y }
    c


let (|TryAt|Help|ParseFailed|) (input: string) =
    //    let tryParseInt (arg: string) valueConstructor =
    //        let (worked, arg') = Int32.TryParse arg
    //        if worked then valueConstructor arg'
    //        else ParseFailed

    let tryParseCoord (arg: string) valueConstructor =
        if arg.Length <> 2 then
            ParseFailed
        else
            let x = arg.[0] // char with [0]
            let y_string = arg.[1..1] // string with []
            let (worked, y) = Int32.TryParse y_string

            // TODO: check if y is a character, not a number
            if worked then valueConstructor (x, y)
            else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with

    | [ verb ] when safeEquals verb HelpLabel -> Help
    | [ verb; arg ] when safeEquals verb (nameof Domain.TryAt) ->
        tryParseCoord arg (fun (x, y) -> TryAt (mapToCoord (x,y)))
    | _ -> ParseFailed
