# Request
method: POST
       
parameters: 
- token = authentication token (from login)
- id = template ID (option, none for new)
- name = template (display) name
- pdf (file)
- json (file)
       
# Response
- Status 400 with error message as text/plain
- Else: Status 400 with "OK:id" as text/plain (`id` = ID of created/updated template)