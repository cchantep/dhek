# Testing

`mvn clean process-test-resources`

# Packaging

`mvn -Ddhek.staging=true clean package`

# Deploy

As for resources are minified and gzip'ed, some configuration is required at server side.

## Apache

```
DirectoryIndex index.gz.html

<FilesMatch "\.gz\..*$">
  Header set Content-Encoding "gzip"
</FilesMatch>
```

## Nginx

???