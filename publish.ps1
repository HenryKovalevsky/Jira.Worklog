param(
    [Parameter(Mandatory=$true)]
    [string]$Version
)

dotnet publish src\Jira.Worklog.fsproj --sc -o publish -p:AssemblyName=jira-worklog
Compress-Archive -Force publish scoop\publish.zip

$hash = (Get-FileHash -Path scoop\publish.zip -Algorithm SHA256).Hash.ToLower()
$json = Get-Content scoop\jira-worklog.json -Raw | ConvertFrom-Json
$json.hash = $hash
$json.version = $Version
$json | ConvertTo-Json -Depth 4 | Set-Content scoop\jira-worklog.json -Encoding UTF8