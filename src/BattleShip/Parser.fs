module Parser

open Domain
open System
open Utils
let safeEquals (it: string) (theOther: string) = String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"


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

    let tryParseShip (position: string, len: string, direction: string) valueConstructor =
        if position.Length <> 2 || len.Length <> 1 || direction.Length <> 1 then
            ParseFailed
        else
            let x = position.[0] // not it is a char (with [0])
            let y_string = position.[1..1] // string with [.. range]
            let (worked, y) = Int32.TryParse y_string

            if not worked then
                ParseFailed
            else
                let coord = mapToCoord (x, y)
                let (worked, len) = Int32.TryParse len
                if not worked then
                    ParseFailed
                else
                    let direction = convertToDirection (direction)
                    if direction.IsSome then valueConstructor (coord, len, direction.Value)
                    else ParseFailed


    let parts = input.Split(' ') |> List.ofArray
    match parts with

    | [ verb ] when safeEquals verb HelpLabel -> Help
    | [ verb ] when safeEquals verb (nameof Domain.ShowShips) -> ShowShips
    | [ verb; arg ] when safeEquals verb (nameof Domain.Try) ->
        tryParseCoord arg (fun (x, y) -> Try(mapToCoord (x, y)))
    | [ verb; arg1; arg2; arg3 ] when safeEquals verb (nameof Domain.Set) ->
        tryParseShip (arg1, arg2, arg3) (fun (c, l, d) -> Set(mapToShip (c, l, d)))
    | _ -> ParseFailed
