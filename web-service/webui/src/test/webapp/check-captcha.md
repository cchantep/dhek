# Request
method: POST

parameters: 
- code = Captcha code
- text = Typed text
       
# Response
- If exception = `{ "exception": "message" }`
- Else: `true` or `false`,

```json
true
```
