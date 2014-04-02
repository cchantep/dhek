# Request
method: POST
       
# Response
- If exception = `{ "exception": "message" }`
- Else:

```json
{"code":"CaptchaInfo.code", "value":"CaptchaInfo.value"}
```
