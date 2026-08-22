open System
open System.IO
open System.Text
open System.Net.Http
open System.Text.Json

let [<Literal>] Token = "<personal-access-token>"
let [<Literal>] JiraUrl = "https://localhost:8080/"

module DateTime =
  let floor (interval: TimeSpan) (datetime: DateTime)  =
    datetime.AddTicks(-datetime.Ticks % interval.Ticks)

  let getWeekRange (datetime: DateTime) =
    let monday =
      match datetime.DayOfWeek with
      | DayOfWeek.Monday -> datetime.Date
      | DayOfWeek.Tuesday -> datetime.Date.AddDays -1
      | DayOfWeek.Wednesday -> datetime.Date.AddDays -2
      | DayOfWeek.Thursday -> datetime.Date.AddDays -3
      | DayOfWeek.Friday -> datetime.Date.AddDays -4
      | DayOfWeek.Saturday -> datetime.Date.AddDays -5
      | DayOfWeek.Sunday -> datetime.Date.AddDays -6
      | _ -> failwith "not a valid day of the week"

    let sunday = monday.AddDays 6
    
    monday, sunday

  let fromTo (startDate: DateTime) (endDate: DateTime) =
    Seq.initInfinite id
    |> Seq.map startDate.AddDays
    |> Seq.takeWhile (fun date -> date <= endDate)
    |> Seq.toList

type Worklog =
  { Summary: string
    IssueKey: string
    Started: DateTime
    TimeSpent: TimeSpan
    Comment: string }
  with
    member this.Ended = this.Started.Add this.TimeSpent

#r "nuget: SwaggerProvider, 3.2.0"

open SwaggerProvider

let [<Literal>] Schema = "Jira.yaml"
type Jira = OpenApiClientProvider<Schema, IgnoreOperationId=true>

let httpClient = new HttpClient(BaseAddress = Uri JiraUrl)
httpClient.DefaultRequestHeaders.Add("Authorization", $"Bearer {Token}")
httpClient.DefaultRequestHeaders.Add("Accept", "application/json")

let client = Jira.Client httpClient

let getWorklogs startDate endDate = task {
  let jql = $"worklogAuthor = currentUser() AND worklogDate>='{startDate:``yyyy-MM-dd``}' AND worklogDate<='{endDate:``yyyy-MM-dd``}'"
  let! jira = client.GetRestApi2Search(jql = jql, fields = [| "key"; "worklog"; "summary" |])
  let! user = client.GetRestApi2Myself()

  let worklogs = seq {
    for issue in jira.Issues do
      for wl in issue.Fields.Worklog.Worklogs do
        if String.Equals(wl.Author.EmailAddress, user.EmailAddress, StringComparison.OrdinalIgnoreCase) then 
          yield
            { Summary = issue.Fields.Summary
              IssueKey = issue.Key
              Started = DateTime.floor (TimeSpan.FromMinutes 30.) (DateTime.Parse wl.Started)
              TimeSpent = TimeSpan.FromSeconds (float wl.TimeSpentSeconds)
              Comment = wl.Comment }
  }

  return
    worklogs
    |> Seq.sortBy (fun w -> w.Started)
    |> Seq.toList
}

let addWorklog issueKey (started : DateTime) (timeSpent : TimeSpan) comment = task {
  let started = started.ToString "yyyy-MM-ddTHH:mm:ss.fffzz00"
  let timeSpent = $"{timeSpent.TotalMinutes}m"
  let request = Jira.WorklogRequest(started,  timeSpent)
  let! worklog = client.PostRestApi2IssueWorklog(issueKey, request)
  ()
}

#r "nuget: Thuja, 0.1.3"

open Thuja
open Thuja.Styles
open Thuja.Elements

type Index = 
  { Row: int; Col: int
    RowsCount: int; ColumnsCount: int }

module Index =
  let init (rowsCount, columnsCount) = 
    { Row = 0; Col = 0 
      RowsCount = rowsCount; ColumnsCount = columnsCount }

  let update (model : Index) = function
    | KeyInput.Down | Char 'j'  -> 
        let row = min (model.RowsCount - 1) (model.Row + 1) 
        { model with Row = row }
    | KeyInput.Up | Char 'k' ->
        let row = max 0 (model.Row - 1)
        { model with Row = row }
    | KeyInput.Right | Char 'l' ->
        let col = min (model.ColumnsCount - 1) (model.Col + 1) 
        { model with Col = col }
    | KeyInput.Left | Char 'h' -> 
        let col = max 0 (model.Col - 1)
        { model with Col = col }
    | _ -> model

type Range = { From: int; To: int }

module Range =
  let init from' to' = { From = from'; To = to' }

type Input(value : string) =
  let content = ResizeArray<_> value
  let mutable index = content.Count

  member _.Write(ch: char) = 
    content.Insert(index, ch)
    index <- index + 1

  member _.Backspace() = 
    if index > 0 && index <= content.Count then
      content.RemoveAt(index - 1)
      index <- index - 1

  member _.Delete() = 
    if index >= 0 && index < content.Count then
      content.RemoveAt index

  member _.MoveRight() = 
    index <- min (index + 1) content.Count

  member _.MoveLeft() = 
    index <- max (index - 1) 0

  member _.Index with get() = index
  member _.Char with get() = 
    if index >= 0 && index < content.Count 
    then content.[index] 
    else ' '

  member _.Value = String(content.ToArray()) 

module Input =
  let init value = Input value

  let update (model : Input) = function
  | Char ch -> model.Write ch; model
  | Backspace -> model.Backspace(); model
  | Delete -> model.Delete(); model
  | KeyInput.Right -> model.MoveRight(); model
  | KeyInput.Left ->  model.MoveLeft(); model
  | _ -> (); model

  let view (label : string) (model : Input) =
    region [] [
      // value
      text [] $"{label}: {model.Value}"

      // cursor
      let margin = model.Index + label.Length + 2
      region [ Width 1; Margin (Margin.Left margin) ] [ 
        text [ Attributes [ Attribute.Invert ] ] (string model.Char) 
      ]
    ]

type Screen =
  | Navigation
  | Selection
  | Loading
  | Prompt of issueKey: Input
  | Error of message: string

type Model =
  { Cursor: Index
    Selection: Range }

let date= DateTime.Parse "2026-08-07"

let monday, sunday = DateTime.getWeekRange date
let weekDays = DateTime.fromTo monday sunday

let worklogs =
  // getWorklogs monday sunday |> Async.AwaitTask |> Async.RunSynchronously
  File.ReadAllText "worklogs.json" |> JsonSerializer.Deserialize<Worklog list>

let headers =
  weekDays
  |> Seq.map (fun day -> day.ToString "ddd dd.MM")
  |> Seq.append [ " " ]
  |> Seq.toList

let calendar =
  [ for hour in [ 06..23 ] do
      for minute in [ 00; 30 ] do
        yield [
          for day in weekDays do
            yield day.Date.AddHours(hour).AddMinutes(minute) ] ]

let data =
  [ for hour in [ 06..23 ] do
      for minute in [ 00; 30 ] do
        yield [
          yield $"{hour:d2}:{minute:d2}"
          for day in weekDays do
            let dateTime = day.AddHours(hour).AddMinutes(minute)

            yield 
              worklogs
              |> Seq.tryFind (fun wl -> wl.Started <= dateTime && dateTime < wl.Ended)
              |> Option.map _.IssueKey
              |> Option.defaultValue "        " ] ]


let buildInfo (row, column) =
  let dateTime = calendar[row][column]
  worklogs
  |> Seq.tryFind (fun wl -> wl.Started <= dateTime && dateTime < wl.Ended)
  |> Option.map (fun wl ->
      let total = 
        worklogs 
        |> Seq.filter (fun w -> w.IssueKey = wl.IssueKey) 
        |> Seq.map _.TimeSpent 
        |> Seq.reduce (+)
        
      let value text = if String.IsNullOrEmpty text then "—" else text
      let format (dateTime: DateTime) = dateTime.ToString "dd.MM HH:mm"

      [ $"Key:      {wl.IssueKey}"
        $"Summary:  {value wl.Summary}"
        $"Started:  {format wl.Started}"
        $"Ended:    {format wl.Ended}"
        $"Spent:    {wl.TimeSpent}"
        $"Total:    {total}"
        $"Comment:  {value wl.Comment}" ]
      |> String.concat Environment.NewLine)
  |> Option.defaultValue "No worklog in this cell"

let view (screen : Screen, model : Model) =
  let cursor =
    [ CellStyle (model.Cursor.Row, model.Cursor.Col + 1, Color.Black, Color.Yellow) ]
  
  let selection =
    let { From = from'; To = to' } = model.Selection 
    if not screen.IsNavigation then [ min from' to' .. max from' to' ] else []
    |> Seq.map (fun row -> CellStyle (row, model.Cursor.Col + 1, Color.Black, Color.Green))
    |> Seq.toList
  
  let props = 
    [ Columns [ Absolute 5; yield! Seq.replicate 7 (Fraction 1) ] ]
    @ [ TableProps.BorderStyle Normal; TableProps.TextAlign Center ]
    @ selection @ cursor 
    
  // root
  region [] [

    // main
    columns [ Fraction 60; Fraction 40 ] [
      
      // calendar
      table props headers data

      // worklog info
      rows [ Absolute 10 ] [
        panel [ BorderStyle Rounded; ] [
          rows [ Absolute 1; Fraction 1 ] [
            text [ Attributes [ Attribute.Underlined ] ] "Worklog"
            text [ Overflow Ellipsis ] (buildInfo (model.Cursor.Row, model.Cursor.Col))
          ]
        ]
      ]
    ]
    
    // modal
    region [ Width 100; Height 3; Align Center ] [
      match screen with
      | Prompt input -> panel [] [ Input.view "Issue Key" input ]
      | Loading -> panel [] [ text [] "Loading..." ]
      | Error message -> panel [] [ text [ Color Color.DarkRed ] $"Error: {message}" ]
      | Navigation | Selection -> empty
    ]
  ]

let rowsCount = data.Length
let columnsCount = weekDays.Length
        
let screen, model =
  Navigation,
  { Cursor = Index.init (rowsCount, columnsCount)
    Selection = Range.init 0 0 }     

let update (screen, model) msg =
  match msg with
  | Choice1Of2 (keyInput, keyModifiers) ->
      match screen, keyInput with
      // reset
      | _, Escape ->
          let range =  Range.init 0 0
          (Navigation, { model with Selection = range }), Cmd.none

      // prompt
      | Prompt issueKey, KeyInput.Enter ->
          let { From = from'; To = to' }, col = model.Selection, model.Cursor.Col
          let from', to' = calendar[from'][col], calendar[to'][col]
          let started, ended = min from' to', max from' to'
          let spent = ended - started
          (Loading, model), Cmd.ofAsync(async {
            do! addWorklog issueKey.Value started spent "" |> Async.AwaitTask
            return KeyInput.Escape, KeyModifiers.None
          } |> Async.Catch)
      | Prompt issueKey, keyInput -> 
          let issueKey = Input.update issueKey keyInput
          (Prompt issueKey, model), Cmd.none

      // exit
      | _, Char 'q' -> (screen, model), Program.exit()
      | _, Char 'c' when keyModifiers = KeyModifiers.Ctrl -> (screen, model), Program.exit()

      // navigation
      | Navigation, Char 'm'
      | Navigation, KeyInput.Enter ->
          let range = Range.init model.Cursor.Row model.Cursor.Row
          (Selection , { model with Selection = range}), Cmd.none
      | Navigation, _ -> 
          let cursor = Index.update model.Cursor keyInput
          (screen, { model with Cursor = cursor }), Cmd.none

      // selection
      | Selection, KeyInput.Enter ->
          let input = Input.init ""
          (Prompt input, model ), Cmd.none
      | Selection, _ ->
          let cursor = Index.update model.Cursor keyInput
          (screen, { model with Cursor = cursor; Model.Selection.To = cursor.Row }), Cmd.none

      // skip
      | _ -> (screen, model), Cmd.none
      
  | Choice2Of2 (exc : exn) ->
      (Error exc.Message, model), Cmd.none

// program
Program.make (screen, model) view update
|> Program.withKeyBindings (Cmd.ofMsg << Choice1Of2)
|> Program.run