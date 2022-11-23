module App

open Elmish 
open Elmish.React
open Feliz

let mutable todoCounter = 0

type Todo = 
  { Index: int
    Content: string
    IsCompleted: bool }

type EditedTodo = 
  { Index: int
    Content: string }

type State = 
  { TodoList: Todo list
    NewTodo: string
    TodoBeingEdited: EditedTodo option  }

type Msg = 
  // 添加新 Todo
  | SetNewTodo of string
  | AddNewTodo

  // 切换完成状态
  | ToggleTodoComplete of int

  // 删除 Todo
  | DeleteDodo of int

  // 编辑 Todo Item
  | StartEdit of int
  | CancelEdit
  | SetEditedTodo of string
  | ApplyEdit 

let init () = 
  { TodoList=List.empty
    NewTodo=""
    TodoBeingEdited=None  }

let update msg state = 
  match msg with
  | SetNewTodo content ->
    {state with NewTodo=content}
  | AddNewTodo ->
    if state.NewTodo = "" then state
    else 
      let newTodoItem = 
        { Index=todoCounter
          Content=state.NewTodo
          IsCompleted=false }
      todoCounter <- todoCounter + 1
      { state with 
          TodoList=newTodoItem::state.TodoList
          NewTodo="" }
  | ToggleTodoComplete idx ->
    let newTodoList = 
      state.TodoList
      |> List.map (fun todo -> 
          if todo.Index = idx 
          then {todo with IsCompleted=not todo.IsCompleted}
          else todo)
    {state with TodoList=newTodoList}
  | DeleteDodo idx ->
    {state with 
      TodoList=(state.TodoList
        |> List.filter (fun {Index=index} -> index <> idx))}
  | StartEdit idx ->
    let originContent = 
      state.TodoList
      |> List.tryFind (fun todo -> todo.Index = idx) 
      |> Option.map (fun todo -> todo.Content)
      |> Option.defaultValue ""
    let editedTodo = 
      {Index=idx;Content=originContent}
    {state with TodoBeingEdited=Some editedTodo}
  | CancelEdit ->
    {state with TodoBeingEdited=None}
  | SetEditedTodo content ->
    match state.TodoBeingEdited with
    | Some {Index=index} ->
      {state with TodoBeingEdited=Some{Index=index;Content=content}}
    | None -> state
  | ApplyEdit ->
    match state.TodoBeingEdited with
    | None -> failwith "bad"
    | Some {Index=idx; Content=content} ->
      let newTodoList = 
        state.TodoList
        |> List.map (fun todo ->
          if todo.Index = idx 
          then {todo with Content=content}
          else todo)
      {state with
        TodoList=newTodoList
        TodoBeingEdited=None}

let appTitle = 
  Html.div [
    Html.h1 "Elmish Todo App"
  ]

let addTodoComponent state dispatch = 
  Html.div [
    Html.input [
      prop.valueOrDefault state.NewTodo
      prop.onChange (SetNewTodo >> dispatch)
    ]
    Html.button [
      prop.text "Add"
      prop.onClick (fun _ -> dispatch AddNewTodo)
    ]
  ]

let todoItemComponent (todo: Todo) dispatch =
  Html.div [
    Html.span todo.Content
    Html.button [
      prop.text (if todo.IsCompleted then "Finished" else "Finish")
      prop.onClick (fun _ -> todo.Index |> ToggleTodoComplete |> dispatch)
    ]
    Html.button [
      prop.text "Edit"
      prop.onClick (fun _ -> todo.Index |> StartEdit |> dispatch)
    ]
    Html.button [
      prop.text "Delete"
      prop.onClick (fun _ -> todo.Index |> DeleteDodo |> dispatch)
    ]
  ]

let editTodoComponent (todo: EditedTodo) dispatch =
  Html.div [
    Html.input [
      prop.valueOrDefault todo.Content
      prop.onChange (SetEditedTodo >> dispatch)
    ]
    Html.button [
      prop.text "Save"
      prop.onClick (fun _ -> dispatch ApplyEdit)
    ]
    Html.button [
      prop.text "Cancel"
      prop.onClick (fun _ -> dispatch CancelEdit)
    ]
  ]

let todoListComponent state dispatch =
  Html.ul [
    for todo in state.TodoList do
      match state.TodoBeingEdited with
      | Some ({Index=editTodoIdx} as editTodo) when todo.Index=editTodoIdx->
        Html.li [
          editTodoComponent editTodo dispatch
        ]
      | _ -> 
        Html.li [
          todoItemComponent todo dispatch
        ]
  ]

let render state dispatch = 
  Html.div [
    appTitle
    addTodoComponent state dispatch
    todoListComponent state dispatch
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run