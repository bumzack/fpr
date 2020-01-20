module Repl

open System
open Parser

type ReplMessage =
    | DomainMessage of Domain.Message
    | HelpRequested
    | NotParsable of string


let read (input: string) =
    match input with
    | Set s -> Domain.Set s |> DomainMessage
    | Try v -> Domain.Try v |> DomainMessage
    | ShowShips -> Domain.ShowShips |> DomainMessage
    | Help -> HelpRequested
    | ParseFailed -> NotParsable input

open Domain
open Microsoft.FSharp.Reflection

let createHelpText(): string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")


let evaluate (update: Domain.Message -> Game -> Game) (game: Game) (msg: ReplMessage) =
    match msg with
    | DomainMessage msg ->
        let newState = update msg game
        let message = ""
        (newState, message)
    | HelpRequested ->
        let message = createHelpText()
        (game, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable. %s""" originalInput
                "You can get information about known commands by typing \"Help\""
        (game, message)

let print (game: Game, outputToPrint: string) =
    printfn "%s\n" outputToPrint
    game

let rec loop (game: Game) =
    let runLoop(g: Game) =
        Console.ReadLine()
            |> read
            |> evaluate DomainFunctions.update g
            |> print
            |> loop

    match game.Status with
        | WonBy p ->
            printfn ("game over - player %A won! - see u next time!") p
            game
        | SetupShips idx ->
            printfn ("setup a ship of length %i on the field ") game.RequiredShips.[idx]
            runLoop game
        | Running ->
            let remaining = getRemainingShipsForBoard(game.ComputerBoard).Length
            printfn ("your turn. %i fields with a ship are remaining ") remaining
            printfn ""
            runLoop game
