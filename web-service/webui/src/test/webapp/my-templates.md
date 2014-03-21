# Request
method: POST
       
parameters: 
- token = authentication token (from login)
       
# Response
- If exception = `{ "exception": "message" }`
- Else = Either `[]` (no template), or:

```json
[
  { "id": "unique-ascii-id-of-template", "name": "Display name" }
  // , ...
]
```
