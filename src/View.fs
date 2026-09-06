module Jira.Worklog.View

open System 

open Thuja
open Thuja.Styles
open Thuja.Elements

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

module Calendar =
  let view (model : WorklogCalendar) (cursor : Cursor) =
    let headers =
      model.Schedule[0, *]
      |> Seq.map (fun day -> day.ToString "ddd dd.MM")
      |> Seq.append [ " " ]
      |> Seq.toList

    let timeMarks = 
      model.Schedule[*, 0]
      |> Seq.map (fun time -> $"{time.Hour:d2}:{time.Minute:d2}")

    let data =
      [ for time, row in Seq.zip timeMarks model.Worklogs do
          yield [
              yield time
              for item in row do
                yield item
                  |> Option.map _.IssueKey
                  |> Option.defaultValue "        " ] ]

    let index =
      [ CellStyle (cursor.Row, cursor.Column + 1, Color.Black, Color.Yellow) ]
    
    let selection =
      cursor.Selection
      |> Option.map (fun s -> [ min s.FromRow s.ToRow .. max s.FromRow s.ToRow ]) 
      |> Option.defaultValue []
      |> Seq.map (fun row -> CellStyle (row, cursor.Column + 1, Color.Black, Color.Green))
      |> Seq.toList
    
    let props = 
      [ Columns [ Absolute 5; yield! Seq.replicate 7 (Fraction 1) ] ]
      @ [ TableProps.BorderStyle Normal; TableProps.TextAlign Center ]
      @ selection @ index 

    table props headers data
      
module Worklog =
  let view (model : WorklogCalendar) (cursor : Cursor) =
    model.GetWorklogByIndex(cursor.Row, cursor.Column)
    |> Option.map (fun wl ->
        let total = model.GetTotalTime wl.IssueKey
          
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