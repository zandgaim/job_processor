# JobProcessor

A simple Erlang job processor with HTTP API to sort tasks topologically and generate bash scripts.

---

## Setup & Run

```bash
# Fetch dependencies
mix deps.get

# Start interactive shell with the app
iex -S mix
```


## Test
Utit tests:
```
mix test
```
API tests:
POST (return sorted tasks):
```
  curl -X POST http://localhost:4020
    -H "Content-Type: application/json" 
    -d
      '{
        "tasks": [
          {"command": "touch /tmp/file1", "name": "task-1"},
          {"command": "echo '\''Hello World!'\'' > /tmp/file1", "name": "task-3"},
          {"command": "cat /tmp/file1", "name": "task-2"},
          {"command": "rm /tmp/file1", "name": "task-4"}
        ]
      }'
```

GET (return last generated script): 
```
  curl http://localhost:4020/script
```
