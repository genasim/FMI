<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" encoding="UTF-8" indent="yes"/>
  
  <xsl:template match="/">
    <html>
      <head>
        <title>Служители - <xsl:value-of select="AutoService/@name"/></title>
        <style>
          body { font-family: Arial, sans-serif; margin: 20px; }
          h1 { color: #2c3e50; }
          table { border-collapse: collapse; width: 100%; margin-top: 20px; }
          th { background: #3498db; color: white; padding: 12px; text-align: left; }
          td { padding: 10px; border-bottom: 1px solid #ddd; }
          tr:hover { background: #f5f5f5; }
        </style>
      </head>
      <body>
        <h1>Служители - <xsl:value-of select="AutoService/@name"/></h1>
        <table>
          <tr>
            <th>ID</th>
            <th>Име</th>
            <th>Фамилия</th>
            <th>Специализация</th>
            <th>Опит (години)</th>
            <th>Телефон</th>
          </tr>
          <xsl:apply-templates select="AutoService/Employees/Mechanic"/>
        </table>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="Mechanic">
    <tr>
      <td><xsl:value-of select="@mechanicID"/></td>
      <td><xsl:value-of select="FirstName"/></td>
      <td><xsl:value-of select="LastName"/></td>
      <td><xsl:value-of select="Specialization"/></td>
      <td><xsl:value-of select="Experience"/></td>
      <td><xsl:value-of select="Phone"/></td>
    </tr>
  </xsl:template>
</xsl:stylesheet>
