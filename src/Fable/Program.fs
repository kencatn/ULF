﻿module Program

open Elmish
open Feliz
open Elmish.React
open Common
open Vector

#if DEBUG
    open Elmish.HMR
#endif
type Page =
    | ULFPage
type Msg =
    | ULFMsg of ULF.Msg
    | Err of exn
type Model = {
    ulf: ULF.Model
    currentPage: Page
}

let init () =
    let ulf, ulfCmd = ULF.init ()
    {
        ulf = ulf
        currentPage = ULFPage
    }, Cmd.batch [ulfCmd |> Cmd.map ULFMsg]

let update msg model =
    try
        match msg with
        | ULFMsg msg ->
            let um, uc = ULF.update msg model
            {model with
                ulf = um
            }, uc |> Cmd.map ULFMsg
        | Err exn ->
            printfn "%O" exn
            model, Cmd.none
    with
    | err -> model, Cmd.ofMsg (Err err)
let view model dispatch =
    match model.currentPage with
    | ULFPage ->
        ULF.view model.ulf (ULFMsg>>dispatch)

Program.mkProgram init update view
|> Program.withReactSynchronous "app"
|> Program.run