module Jira.Worklog.Client

open System
open System.Net.Http

open SwaggerProvider

let [<Literal>] Schema = "Jira.yaml"
type Jira = OpenApiClientProvider<Schema, IgnoreOperationId=true>

type JiraClient(baseUrl : string, accessToken : string) =
  let httpClient = new HttpClient(BaseAddress = Uri baseUrl)

  do
    httpClient.DefaultRequestHeaders.Add("Authorization", $"Bearer {accessToken}")
    httpClient.DefaultRequestHeaders.Add("Accept", "application/json")

  let client = Jira.Client httpClient

  member _.GetWorklogs startDate endDate = task {
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

  member _.AddWorklog issueKey (started : DateTime) (timeSpent : TimeSpan) comment = task {
    let started = started.ToString "yyyy-MM-ddTHH:mm:ss.fffzz00"
    let timeSpent = $"{timeSpent.TotalMinutes}m"
    let request = Jira.WorklogRequest(started,  timeSpent)

    return! client.PostRestApi2IssueWorklog(issueKey, request)
  }

  member _.Dispose() = httpClient.Dispose()

  interface IDisposable with 
    member this.Dispose() = this.Dispose()