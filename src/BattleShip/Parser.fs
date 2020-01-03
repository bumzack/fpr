module Parser

open Domain
open System

let safeEquals (it: string) (theOther: string) = String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let mapToCoord (x, y) =
    let c: Domain.Coord =
        { X = x
          Y = y }
    c

let mapToCoordPair (x, y, x1, y1) =
    let c1 = mapToCoord (x, y)
    let c2= mapToCoord (x1, y1)

    let cp :CoordPair = {
        c1 = c1
        c2 = c2
    }
    cp



let (|Set|Try|ShowShips|Help|ParseFailed|) (input: string) =
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
            if worked then valueConstructor (x, y) else ParseFailed

    let tryParseCoordPair (arg1: string, arg2: string) valueConstructor =
        if arg1.Length <> 2 || arg2.Length <> 2 then
            ParseFailed
        else
            let x1 = arg1.[0] // char with [0]
            let y1_string = arg1.[1..1] // string with []
            let (worked1, y1) = Int32.TryParse y1_string

            let x2 = arg2.[0] // char with [0]
            let y2_string = arg2.[1..1] // string with []
            let (worked2, y2) = Int32.TryParse y2_string

            // TODO: check if y is a character, not a number
            if (worked1 && worked2) then valueConstructor (x1, y1, x2, y2) else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with

    | [ verb ] when safeEquals verb HelpLabel -> Help
    | [ verb ] when safeEquals verb (nameof Domain.ShowShips) -> ShowShips
    | [ verb; arg ] when safeEquals verb (nameof Domain.Try) ->
        tryParseCoord arg (fun (x, y) -> Try  (mapToCoord (x,y)))
    | [ verb; arg1; arg2 ] when safeEquals verb (nameof Domain.Set) ->
        tryParseCoordPair (arg1 , arg2) (fun (x, y, x1, y1) -> Set (mapToCoordPair (x,y, x1,y1)))
    | _ -> ParseFailed
