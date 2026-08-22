open System
open System.IO
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

#r "nuget: Thuja, 0.1.3"

open Thuja
open Thuja.Styles
open Thuja.Elements

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
              |> Option.defaultValue "        "
        ] ] 


let view =
  let layout = Columns [ Absolute 5; yield! Seq.replicate 7 (Fraction 1) ]
  let borderStyle = TableProps.BorderStyle Normal
  let textAlign = TableProps.TextAlign Center

  table [ layout; borderStyle; textAlign ] headers data

Program.makeStatic view
|> Program.run

