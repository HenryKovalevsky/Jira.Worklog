open System

open Thuja
open Thuja.Styles
open Thuja.Elements

open Jira.Worklog
open Jira.Worklog.Client
open Jira.Worklog.View
open Jira.Worklog.Credentials

// model
type Screen =
  | Loading
  | Navigation
  | Prompt of issueKey: Input
  | Error of message: string

type Model =
  { Screen: Screen
    Cursor: Cursor
    Calendar: WorklogCalendar }
  with
    static member init screen cursor calendar = 
      { Screen = screen
        Cursor = cursor 
        Calendar = calendar }

// view
let view model =
  // root
  region [] [

    // main
    columns [ Fraction 60; Fraction 40 ] [
      
      // calendar
      Calendar.view model.Calendar model.Cursor

      // worklog info
      rows [ Absolute 10 ] [
        panel [ BorderStyle Rounded; ] [
          rows [ Absolute 1; Fraction 1 ] [
            text [ Attributes [ Attribute.Underlined ] ] "Worklog"
            text [ Overflow Ellipsis ] (Worklog.view model.Calendar model.Cursor)
          ]
        ]
      ]
    ]
    
    // modal
    region [ Width 100; Height 3; Align Center ] [
      match model.Screen with
      | Prompt issueKey -> panel [] [ Input.view "Issue Key" issueKey ]
      | Loading -> panel [] [ text [] "Loading..." ]
      | Error message -> panel [] [ text [ Color Color.DarkRed ] $"Error: {message}" ]
      | Navigation -> empty
    ]
  ]

// update
let update (client : JiraClient) model msg =
  match msg with
  | Choice1Of3 (keyInput, keyModifiers) ->
      match model.Screen, keyInput with
      // reset
      | _, Escape ->
          { model with Screen = Navigation; Cursor = model.Cursor.Reset() }, Cmd.none

      // prompt
      | Prompt input, keyInput ->
          match keyInput with
          | Input.TextInput input result ->
              { model with Screen = Prompt result }, Cmd.none
          | KeyInput.Enter when model.Cursor.Selection.IsSome ->
              let started, ended = (model.Cursor.Selection.Value.FromRow, model.Cursor.Column), (model.Cursor.Selection.Value.ToRow, model.Cursor.Column)
              let started, ended = model.Calendar.GetDateByIndex started, model.Calendar.GetDateByIndex ended
              let started, ended = min started ended, max started ended
              let spent = ended - started + model.Calendar.Interval
              { model with Screen = Loading }, Cmd.ofAsync(async {
                try
                  let! worklog = client.AddWorklog input.Value started spent "" |> Async.AwaitTask

                  return Choice2Of3 worklog
                with exc ->
                  return Choice3Of3 exc
              })
          | _ -> model, Cmd.none

      // exit
      | _, Char 'q' -> model, Program.exit()
      | _, Char 'c' when keyModifiers = KeyModifiers.Ctrl -> model, Program.exit()

      // navigation
      | Navigation, keyInput ->
          match keyInput with
          | KeyInput.Down | Char 'j' ->
              { model with Cursor = model.Cursor.MoveDown() }, Cmd.none
          | KeyInput.Up | Char 'k' ->
              { model with Cursor = model.Cursor.MoveUp() }, Cmd.none
          | KeyInput.Right | Char 'l' ->
              { model with Cursor = model.Cursor.MoveRight() }, Cmd.none
          | KeyInput.Left | Char 'h' ->
              { model with Cursor = model.Cursor.MoveLeft() }, Cmd.none
          | KeyInput.Spacebar | Char 'm' ->
              { model with Cursor = model.Cursor.Select() }, Cmd.none
          | KeyInput.Enter when model.Cursor.Selection.IsSome ->
              { model with Screen = Prompt <| Input.init "" }, Cmd.none
          | Char 'n' ->
              let monday, sunday = DateTime.getWeekRange (model.Calendar.StartDate.AddDays 7)
              let worklogs = client.GetWorklogs monday sunday |> Async.AwaitTask |> Async.RunSynchronously
              let calendar = WorklogCalendar(monday, sunday, 06, 23, worklogs)

              { Screen = Navigation; Calendar = calendar; Cursor = model.Cursor.Reset() }, Cmd.none
          | Char 'p' ->
              let monday, sunday = DateTime.getWeekRange (model.Calendar.StartDate.AddDays -7)
              let worklogs = client.GetWorklogs monday sunday |> Async.AwaitTask |> Async.RunSynchronously
              let calendar = WorklogCalendar(monday, sunday, 06, 23, worklogs)

              { Screen = Navigation; Calendar = calendar; Cursor = model.Cursor.Reset() }, Cmd.none
          
          | _ -> model, Cmd.none

      // skip
      | _ -> model, Cmd.none
  
  // reload
  | Choice2Of3 worklog -> 
      let monday, sunday = model.Calendar.StartDate, model.Calendar.EndDate
      let worklogs = client.GetWorklogs monday sunday |> Async.AwaitTask |> Async.RunSynchronously
      let calendar = WorklogCalendar(monday, sunday, 06, 23, worklogs)

      { Screen = Navigation; Calendar = calendar; Cursor = model.Cursor.Reset() }, Cmd.none
  
  // error
  | Choice3Of3 (exc : exn) ->
      { model with Screen = Error exc.Message }, Cmd.none

// program
module Program =
  let run (credentials : JiraCredentials) = 
    let monday, sunday = DateTime.getWeekRange DateTime.Now

    let client = new JiraClient(credentials.Url, credentials.Token)
    let worklogs = client.GetWorklogs monday sunday |> Async.AwaitTask |> Async.RunSynchronously
    
    let calendar = WorklogCalendar(monday, sunday, 06, 23, worklogs)

    let rowsCount = calendar.Schedule[*, 0].Length
    let columnsCount = calendar.Schedule[0, *].Length
            
    let cursor = Cursor.init (columnsCount, rowsCount)   
    let model = Model.init Navigation cursor calendar
    let update = update client

    Program.make model view update
    |> Program.withKeyBindings (Cmd.ofMsg << Choice1Of3)
    |> Program.run

// entry point
[<EntryPoint>]
let main args =
  match args with
  | [| "--help" |] | [| "-h" |] -> 
      printfn "Usage:"
      printfn "  jira-worklog login <url> <token>   Save Jira credentials"
      printfn "  jira-worklog                       Run worklog viewer"
      printfn "  jira-worklog --help                Show this help"
      0

  | [| "login"; url; token |] -> JiraCredentials.save { Url = url; Token = token }; 0
  | [| "login" |] -> eprintfn "Usage: jira-worklog login <url> <token>"; 1

  | [| |] -> 
      match JiraCredentials.load() with
      | Some credentials -> Program.run credentials; 0
      | None -> eprintfn "No Jira credentials found. Login first: jira-worklog login <url> <token>"; 1

  | _ -> eprintfn "Unknown command. Use --help for usage."; 1