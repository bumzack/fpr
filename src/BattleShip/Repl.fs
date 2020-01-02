module Repl

open System
open Parser

type ReplMessage =
    | DomainMessage of Domain.Message
    | HelpRequested
    | NotParsable of string


let read (input: string) =
    match input with
    | TryAt v -> Domain.TryAt v |> DomainMessage
    | Help -> HelpRequested
    | ParseFailed -> NotParsable input

open Domain
open Microsoft.FSharp.Reflection

let createHelpText(): string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")




let evaluate (update : Domain.Message -> Game -> Game) (game : Game) (msg : ReplMessage) =
    match msg with
    | DomainMessage msg ->
        let newState = update msg game
        // TODO: better msg - or remove - or ?!?
        let message = "next try "
        (newState, message)
    | HelpRequested ->
        let message = createHelpText ()
        (game, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable. %s"""  originalInput "You can get information about known commands by typing \"Help\""
        (game, message)

let print (game: Game, outputToPrint: string) =
    printfn "%s\n" outputToPrint
    printf "> "

    game

let rec loop (game : Game) =
    Console.ReadLine()
    |> read
    |> evaluate Domain.update game
    |> print
    |> loop




// TODO: how to move these 2 functions to a new file? Program.fs doesnt like it

// TODO: dummy implementation replace with ships created from user input
let readShipsFromHuman() =
    let c =
        { X = 'A'
          Y = 2 }
    let p11 =
        { Coord = c
          PointStatus = NotHit }

    let c =
        { X = 'A'
          Y = 3 }
    let p12 =
        { Coord = c
          PointStatus = NotHit }

    let c =
        { X = 'A'
          Y = 3 }
    let p13 =
        { Coord = c
          PointStatus = NotHit }

    let c =
        { X = 'D'
          Y = 4 }
    let p14 =
        { Coord = c
          PointStatus = NotHit }

    let ship11 =
        { Points = [ p11; p12 ]
          Status = Alive }

    let ship12 =
        { Points = [ p13; p14 ]
          Status = Alive }

    let ships = [ ship11; ship12 ]

    ships



// TODO: dummy implementation replace with ships created at random positions
let createRandomShips() =
    let c =
        { X = 'A'
          Y = 3 }

    let p21 =
        { Coord = c
          PointStatus = NotHit }

    let c =
        { X = 'C'
          Y = 3 }
    let p22 =
        { Coord = c
          PointStatus = NotHit }

    let c =
        { X = 'E'
          Y = 4 }
    let p23 =
        { Coord = c
          PointStatus = NotHit }

    let c =
        { X = 'F'
          Y = 4 }
    let p24 =
        { Coord = c
          PointStatus = NotHit }


    let ship21 =
        { Points = [ p21; p22 ]
          Status = Alive }

    let ship22 =
        { Points = [ p23; p24 ]
          Status = Alive }

    let ships = [ ship21; ship22 ]

    ships
