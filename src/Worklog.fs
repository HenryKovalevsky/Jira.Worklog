namespace Jira.Worklog 

open System

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
