<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
  <xsl:output xmlns:s="uri:saxy"
              method="s:saxy.StripXHTMLEmitter" 
              doctype-public="html"
              omit-xml-declaration="yes" />

  <xsl:param name="page" />
  <xsl:param name="title" />
  <xsl:param name="cssBundle" />
  <xsl:param name="jsBundle" />

  <xsl:template match="/">
    <html lang="en">
      <head>
        <title><xsl:value-of select="$title" /></title>
        <meta charset="utf-8" />
        <meta http-equiv="X-UA-Compatible" content="IE=edge" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />

        <link href="css/{$cssBundle}" rel="stylesheet" />
        <link href="//netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.min.css" rel="stylesheet" />

        <!--[if lt IE 9]>
        <script src="js/html5shiv.gz.js"></script>
        <script src="js/respond.gz.js"></script>
        <![endif]-->
      </head>
      <body>
        <xsl:copy-of select="." />

        <script src="js/{$jsBundle}"></script>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
