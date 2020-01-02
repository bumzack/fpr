module Repl

open System
open Parser

type Message =
    | DomainMessage of Domain.Message
    | HelpRequested
    | NotParsable of string


let read (input: string) =
    match input with
    | Increment -> Domain.Increment |> DomainMessage
    | Decrement -> Domain.Decrement |> DomainMessage
    | IncrementBy v -> Domain.IncrementBy v |> DomainMessage
    | DecrementBy v -> Domain.DecrementBy v |> DomainMessage
    | Help -> HelpRequested
    | ParseFailed -> NotParsable input

open Domain
open Microsoft.FSharp.Reflection

let createHelpText(): string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")



//
//let evaluate (update : Domain.Message -> State -> State) (state : State) (msg : Message) =
//    match msg with
//    | DomainMessage msg ->
//        let newState = update msg state
//        let message = sprintf "The message was %A. New state is %A" msg newState
//        (newState, message)
//    | HelpRequested ->
//        let message = createHelpText ()
//        (state, message)
//    | NotParsable originalInput ->
//        let message =
//            sprintf """"%s" was not parsable. %s"""  originalInput "You can get information about known commands by typing \"Help\""
//        (state, message)

let print (state: State, outputToPrint: string) =
    printfn "%s\n" outputToPrint
    printf "> "

    state

//let rec loop (state : State) =
//    Console.ReadLine()
//    |> read
//    |> evaluate Domain.update state
//    |> print
//    |> loop



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
