# Request
method: POST
       
parameters: 
- token = authentication token (from login)
- id = template ID (option, none for new)
- name = template (display) name
- pdf (file)
- json (file)
       
# Response
- Status 200 with error message as text/plain
- Else: Status 200 with "OK:id" as text/plain (`id` = ID of created/updated template)