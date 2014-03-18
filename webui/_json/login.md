# Request
method: POST
       
parameters: 
- username = email address as username
- password
       
# Response
- If exception = `{ "exception": "message" }`
- Else = Either `null` (authentication mismatch), or:

```json
{
    "token": "fb096fd5-3254-47cb-8f46-128d8c1ae1f2" // successful auth token
}
```
