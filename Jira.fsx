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
    |> Seq.map startDate.Date.AddDays
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

  return! client.PostRestApi2IssueWorklog(issueKey, request)
}

#r "nuget: Thuja, 0.1.3"

open Thuja
open Thuja.Styles
open Thuja.Elements

// elements
type Input private =
  { Chars: char array 
    Index: int }
  with
    member this.Write(ch: char) = 
      { this with
          Chars = this.Chars |> Array.insertAt this.Index ch
          Index = this.Index + 1 }

    member this.Backspace() = 
      if this.Index > 0 && this.Index <= this.Chars.Length then
        { this with
            Chars = this.Chars |> Array.removeAt (this.Index - 1)
            Index = this.Index - 1 }
      else this

    member this.Delete() = 
      if this.Index >= 0 && this.Index < this.Chars.Length then
        { this with
            Chars = this.Chars |> Array.removeAt this.Index }
      else this

    member this.MoveRight() = 
      { this with
          Index = min (this.Index + 1) this.Chars.Length }

    member this.MoveLeft() = 
      { this with
          Index = max (this.Index - 1) 0 }

    member internal this.Current = 
      if this.Index >= 0 && this.Index < this.Chars.Length 
      then this.Chars.[this.Index] 
      else ' '

    member this.Value = String this.Chars

    static member init (value : string) =
      { Chars = value.ToCharArray()
        Index = value.Length }

module Input =
  let view (label : string) (model : Input) =
    region [] [
      // value
      text [] $"{label}: {model.Value}"

      // cursor
      let margin = model.Index + label.Length + 2
      region [ Width 1; Margin (Margin.Left margin) ] [ 
        text [ Attributes [ Attribute.Invert ] ] (string model.Current) 
      ]
    ]

  let (|TextInput|_|) (input : Input) = function
    | Char ch -> Some <| input.Write ch
    | Backspace -> Some <| input.Backspace()
    | Delete -> Some <| input.Delete()
    | KeyInput.Right -> Some <| input.MoveRight()
    | KeyInput.Left -> Some <| input.MoveLeft()
    | _ -> None

  let update (input : Input) = function
  | TextInput input result -> result
  | _ -> input

// model
type Screen =
  | Loading
  | Navigation
  | Prompt of issueKey: Input
  | Error of message: string

type Cursor private =
  { Row: int; Column: int
    Selection: {| FromRow: int; ToRow: int |} option
    Width: int; Height: int }
  with
    member this.MoveDown() =
      let this = { this with Row = min (this.Height - 1) (this.Row + 1) }
      let selection = this.Selection |> Option.map (fun s -> {| s with ToRow = this.Row |})
      { this with Selection = selection }
    member this.MoveUp() =
      let this = { this with Row = max 0 (this.Row - 1) }
      let selection = this.Selection |> Option.map (fun s -> {| s with ToRow = this.Row |})
      { this with Selection = selection }
    member this.MoveRight() =
      let this = { this with Column = min (this.Width - 1) (this.Column + 1) }
      let selection = this.Selection |> Option.map (fun _ -> {| FromRow = this.Row; ToRow = this.Row |})
      { this with Selection = selection }
    member this.MoveLeft() =
      let this = { this with Column = max 0 (this.Column - 1) }
      let selection = this.Selection |> Option.map (fun _ -> {| FromRow = this.Row; ToRow = this.Row |})
      { this with Selection = selection }

    member this.Select() =
      { this with Selection = Some {| FromRow = this.Row; ToRow = this.Row |} }
    member this.Reset() =
      { this with Selection = None }

    static member init(width, height) =
      { Row = 0; Column = 0
        Selection = None
        Width = width; Height = height }

type WorklogCalendar(startDate : DateTime, endDate : DateTime, startTime : int, endTime : int, worklogs : Worklog list) =
  let dateRange = DateTime.fromTo startDate endDate
  let schedule =
    [ for hour in [ startTime..endTime ] do
        for minute in [ 00; 30 ] do
          yield [
            for day in dateRange do
              yield day.Date.AddHours(hour).AddMinutes(minute) ] ]
    |> array2D

  let worklogsTable =
    schedule
    |> Array2D.map (fun dateTime -> 
        worklogs
        |> Seq.tryFind (fun wl -> wl.Started <= dateTime && dateTime < wl.Ended))

  member _.StartDate = startDate
  member _.EndDate = endDate
  member _.Interval = TimeSpan.FromMinutes 30.
  member _.GetTotalTime(issueKey : string) = 
    worklogs
    |> Seq.filter (fun w -> w.IssueKey = issueKey) 
    |> Seq.map _.TimeSpent 
    |> Seq.reduce (+)

  member _.GetWorklogByIndex(row, column) = worklogsTable.[row, column]
  member _.GetDateByIndex(row, column) = schedule.[row, column]

  member _.Schedule = schedule
  member _.Worklogs = 
    [ for row in 0 .. Array2D.length1 worklogsTable - 1 do 
        yield List.ofArray worklogsTable.[row, *] ]

type Model =
  { Screen: Screen
    Cursor: Cursor
    Calendar: WorklogCalendar }

// view
let worklogInfo model =
  model.Calendar.GetWorklogByIndex(model.Cursor.Row, model.Cursor.Column)
  |> Option.map (fun wl ->
      let total = model.Calendar.GetTotalTime wl.IssueKey
        
      let formatText text = if String.IsNullOrEmpty text then "—" else text
      let formatDate (dateTime : DateTime) = dateTime.ToString "dd.MM HH:mm"
      let formatTime (timeSpan : TimeSpan) = sprintf "%ih %im" (int timeSpan.TotalHours) timeSpan.Minutes

      [ $"Key:      {wl.IssueKey}"
        $"Summary:  {formatText wl.Summary}"
        $"Started:  {formatDate wl.Started}"
        $"Ended:    {formatDate wl.Ended}"
        $"Spent:    {formatTime wl.TimeSpent}"
        $"Total:    {formatTime total}"
        $"Comment:  {formatText wl.Comment}" ]
      |> String.concat Environment.NewLine)
  |> Option.defaultValue "No worklog in this cell"

let view model =
  let headers =
    model.Calendar.Schedule[0, *]
    |> Seq.map (fun day -> day.ToString "ddd dd.MM")
    |> Seq.append [ " " ]
    |> Seq.toList

  let timeMarks = 
    model.Calendar.Schedule[*, 0]
    |> Seq.map (fun time -> $"{time.Hour:d2}:{time.Minute:d2}")

  let data =
    [ for time, row in Seq.zip timeMarks model.Calendar.Worklogs do
        yield [
            yield time
            for item in row do
              yield item
                |> Option.map _.IssueKey
                |> Option.defaultValue "        " ] ]

  let index =
    [ CellStyle (model.Cursor.Row, model.Cursor.Column + 1, Color.Black, Color.Yellow) ]
  
  let selection =
    model.Cursor.Selection
    |> Option.map (fun s -> [ min s.FromRow s.ToRow .. max s.FromRow s.ToRow ]) 
    |> Option.defaultValue []
    |> Seq.map (fun row -> CellStyle (row, model.Cursor.Column + 1, Color.Black, Color.Green))
    |> Seq.toList
  
  let props = 
    [ Columns [ Absolute 5; yield! Seq.replicate 7 (Fraction 1) ] ]
    @ [ TableProps.BorderStyle Normal; TableProps.TextAlign Center ]
    @ selection @ index 
    
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
            text [ Overflow Ellipsis ] (worklogInfo model)
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
let update model msg =
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
                  let! worklog = addWorklog input.Value started spent "" |> Async.AwaitTask

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
          | _ -> model, Cmd.none

      // skip
      | _ -> model, Cmd.none
  
  // reload
  | Choice2Of3 worklog -> 
      let monday, sunday = model.Calendar.StartDate, model.Calendar.EndDate
      let worklogs = getWorklogs monday sunday |> Async.AwaitTask |> Async.RunSynchronously
      let calendar = WorklogCalendar(monday, sunday, 06, 23, worklogs)

      { Screen = Navigation; Calendar = calendar; Cursor = model.Cursor.Reset() }, Cmd.none
  
  // error
  | Choice3Of3 (exc : exn) ->
      { model with Screen = Error exc.Message }, Cmd.none

// program
let date = DateTime.Parse "2026-08-07"
let monday, sunday = DateTime.getWeekRange date

let worklogs =
  // getWorklogs monday sunday |> Async.AwaitTask |> Async.RunSynchronously
  File.ReadAllText "worklogs.json" |> JsonSerializer.Deserialize<Worklog list>

let calendar = WorklogCalendar(monday, sunday, 06, 23, worklogs)

let rowsCount = calendar.Schedule[*, 0].Length
let columnsCount = calendar.Schedule[0, *].Length
        
let cursor = Cursor.init (columnsCount, rowsCount)   

let model = 
  { Screen = Navigation
    Cursor = cursor 
    Calendar = calendar }

Program.make model view update
|> Program.withKeyBindings (Cmd.ofMsg << Choice1Of3)
|> Program.run