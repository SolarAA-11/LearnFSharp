module App

open Elmish
open Elmish.React
open Feliz

type State = 
  { TodoItems: string list
    NewTodoItem: string }

type Msg = 
  | SetNewTodoItem of string
  | AddNewTodoItem

let init () = 
  { TodoItems = [ "Learn F#" ]
    NewTodoItem = "" }

let update msg state = 
  match msg with
  | SetNewTodoItem text -> 
    { state with NewTodoItem = text }
  | AddNewTodoItem when state.NewTodoItem = "" ->
    state
  | AddNewTodoItem ->
    { state with
        TodoItems = state.TodoItems @ [ state.NewTodoItem ]
        NewTodoItem = "" }

let appTitle = 
  Html.p [
    prop.className "text-xl font-medium text-black"
    prop.text "Elmish To-Do List"
  ]

let todoInputElement state dispatch = 
  Html.div [
    prop.className "flex flex-row"
    prop.children [
      Html.input [
        prop.className "border border-gray-300 rounded-md p-2"
        prop.valueOrDefault state.NewTodoItem
        prop.onChange (SetNewTodoItem >> dispatch)
      ]
      Html.button [
        prop.className "bg-blue-500 text-white rounded-md p-2 ml-2"
        prop.text "Add"
        prop.onClick (fun _ -> dispatch AddNewTodoItem)
      ]
    ]
  ]

let todoListElment state dispatch = 
  Html.div [
    prop.className "mt-4"
    prop.children [
      for item in state.TodoItems do
        Html.p [
          prop.className "text-black"
          prop.text item
        ]
    ]
  ]

let render state dispatch = 
  Html.div [
    prop.className "container mx-auto p-4"
    prop.children [
      appTitle
      todoInputElement state dispatch
      todoListElment state dispatch
    ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "root"
|> Program.run


