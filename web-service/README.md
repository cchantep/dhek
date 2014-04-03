# Dhek web service

## Setup

- MongoDB
- Java 1.7

## Configuration

Expecting a file name `dhek.conf` in runtime directory (CWD):

```
dhek.secret.key = secret
dhek.app.secret.key = app-secret
dhek.request.timeout = 1s
dhek.repository = repo
dhek.db.name = my-dhek
dhek.db.users.name = my-users
```
