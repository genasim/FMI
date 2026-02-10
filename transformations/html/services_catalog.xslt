<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" encoding="UTF-8" indent="yes"/>
  
  <xsl:template match="/">
    <html>
      <head>
        <title>Каталог услуги - <xsl:value-of select="AutoService/@name"/></title>
        <style>
          body { font-family: Arial, sans-serif; margin: 20px; background: #ecf0f1; }
          h1 { color: #2c3e50; text-align: center; }
          .services { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 20px; margin-top: 30px; }
          .service { background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
          .service-id { color: #7f8c8d; font-size: 0.9em; }
          .service-desc { font-size: 1.1em; margin: 10px 0; color: #2c3e50; }
          .service-price { font-size: 1.5em; color: #27ae60; font-weight: bold; }
        </style>
      </head>
      <body>
        <h1>Каталог услуги - <xsl:value-of select="AutoService/@name"/></h1>
        <div class="services">
          <xsl:apply-templates select="AutoService/Services/Service"/>
        </div>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="Service">
    <div class="service">
      <div class="service-id">ID: <xsl:value-of select="@serviceID"/></div>
      <div class="service-desc"><xsl:value-of select="Description"/></div>
      <div class="service-price"><xsl:value-of select="Price"/> лв.</div>
    </div>
  </xsl:template>
</xsl:stylesheet>
