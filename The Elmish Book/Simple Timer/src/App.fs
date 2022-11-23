module App

open Elmish
open Elmish.React
open Feliz

open System

// convert Async<'msg> to Cmd<'msg>
module Cmd = 
  let fromAsync operation = 
    let asyncCommand dispatch = 
      let asyncDispatch = async {
        let! resultMessage = operation
        dispatch resultMessage
      }
      Async.StartImmediate asyncDispatch
    Cmd.ofSub asyncCommand

type State = 
  { CurrentTime: DateTime }

type Msg = 
  | Tick

let update msg state = 
  match msg with
  | Tick ->
    let nextState = 
      {state with CurrentTime = DateTime.Now}
    let step = async {
      do! Async.Sleep 1000
      return Tick
    }
    nextState, Cmd.fromAsync step

let init() = 
  { CurrentTime = DateTime.Now }, Cmd.ofMsg Tick

// formate Datetime as hour:minute:second
let formatTime (time: DateTime) = 
  time.ToString("HH:mm:ss")

// render simple timer
let render state dispatch = 
  Html.div [
    // padding 20
    prop.style [
      style.padding 20
    ]
    // render current time in h1
    prop.children [
      Html.h1 (formatTime state.CurrentTime)
    ]
  ]

// start program react node's ID is elmish-app
Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
