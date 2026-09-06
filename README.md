# Jira.Worklog

_A simple terminal app that helps you manage Jira worklogs._

> NB: A really raw version.

Made with [F#](https://fsharp.org) and [Thuja](https://github.com/HenryKovalevsky/Thuja).

## Installation

> You can easily install it with [`Scoop`](https://scoop.sh/).

```pwsh
scoop install https://raw.githubusercontent.com/HenryKovalevsky/Jira.Worklog/refs/heads/master/scoop/jira-worklog.json
```

Or download [latest release](https://raw.githubusercontent.com/HenryKovalevsky/Jira.Worklog/refs/heads/master/scoop/publish.zip).


## Usage

### Initial setup

Login first (credentials stored securely using OS keychain):

```pwsh
jira-worklog login <jira-server-url> <personal-access-token>
```

### Run the application

```pwsh
jira-worklog
```

### Controls

| Key | Action |
|-----|--------|
| `j` / `↓` | Move cursor down |
| `k` / `↑` | Move cursor up |
| `l` / `→` | Move cursor right |
| `h` / `←` | Move cursor left |
| `Space` / `m` | Start selection (mark  worklog range) |
| `Enter` | Confirm |
| `Escape` | Cancel and return to navigation |
| `n` | Move to the next week |
| `p` | Move to the previous week |
| `q` | Quit application |

## Todo (future features list)

- improve documentation;
- provide in app manual;
- add worklog comments;
- add worklog deletion;
- errors handling;
- handle crossing worklogs;
- ...