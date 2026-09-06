module Jira.Worklog.Credentials

open System
open System.IO
open System.Text
open System.Text.Json
open System.Security.Cryptography

type JiraCredentials =
  { Url: string
    Token: string }

module JiraCredentials =
  let private appDataDir = 
    Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "Jira.Worklog")

  let private credentialsFile = Path.Combine(appDataDir, "credentials.json")

  let save (credentials: JiraCredentials) =
    if not (Directory.Exists appDataDir) then
        Directory.CreateDirectory appDataDir |> ignore

    let json = JsonSerializer.Serialize credentials
    let bytes = Encoding.UTF8.GetBytes json
    let protectedBytes = ProtectedData.Protect(bytes, [||], DataProtectionScope.CurrentUser)

    File.WriteAllBytes(credentialsFile, protectedBytes)

  let load() : JiraCredentials option =
    if File.Exists credentialsFile then
      try
        let bytes = File.ReadAllBytes credentialsFile
        let bytes = ProtectedData.Unprotect(bytes, [||], DataProtectionScope.CurrentUser)
        let json = Encoding.UTF8.GetString bytes
        Some <| JsonSerializer.Deserialize<JiraCredentials> json
      with _ -> None
    else None