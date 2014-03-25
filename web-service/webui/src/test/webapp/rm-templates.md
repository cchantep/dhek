# Request
method: POST
       
parameters: 
- token = authentication token (from login)
- template[] = array of template id
       
# Response
- If exception = `{ "exception": "message" }`
- Else = Either `[]` (no template), or template list after removal(s):

```json
[
  { "id": "unique-ascii-id-of-template", "name": "Display name" }
  // , ...
]
```
