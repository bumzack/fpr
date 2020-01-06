module Parser

open Domain
open System

let safeEquals (it: string) (theOther: string) = String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)


[<Literal>]
let HelpLabel = "Help"

let mapToCoord (x, y) =
    { X = x
      Y = y }

let (|Set|Try|ShowShips|Help|ParseFailed|) (input: string) =
    let tryParseCoord (arg: string) valueConstructor =
        if arg.Length <> 2 then
            ParseFailed
        else
            let x = arg.[0] // char with [0]
            let y_string = arg.[1..1] // string with []
            let (worked, y) = Int32.TryParse y_string

            // TODO: check if y is a character, not a number
            if worked then valueConstructor (x, y) else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with

    | [ verb ] when safeEquals verb HelpLabel -> Help
    | [ verb ] when safeEquals verb (nameof Domain.ShowShips) -> ShowShips
    | [ verb; arg ] when safeEquals verb (nameof Domain.Try) ->
        tryParseCoord arg (fun (x, y) -> Try(mapToCoord (x, y)))
    | [ verb; arg1 ] when safeEquals verb (nameof Domain.Set) ->
        tryParseCoord (arg1) (fun (x, y) -> Set(mapToCoord (x, y)))
    | _ -> ParseFailed
