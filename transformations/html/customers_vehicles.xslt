<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" encoding="UTF-8" indent="yes"/>
  
  <xsl:template match="/">
    <html>
      <head>
        <title>Отчет клиенти и автомобили - <xsl:value-of select="AutoService/@name"/></title>
        <style>
          body { font-family: Arial, sans-serif; margin: 20px; }
          h1 { color: #2c3e50; }
          .customer { background: #fff; padding: 20px; margin: 15px 0; border: 1px solid #ddd; border-radius: 5px; }
          .customer-name { font-size: 1.3em; font-weight: bold; color: #2980b9; }
          .detail { margin: 5px 0; }
          .vehicle { background: #ecf0f1; padding: 10px; margin: 10px 0; border-left: 3px solid #3498db; }
        </style>
      </head>
      <body>
        <h1>Отчет клиенти и автомобили - <xsl:value-of select="AutoService/@name"/></h1>
        <xsl:apply-templates select="AutoService/Customers/Customer"/>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="Customer">
    <div class="customer">
      <div class="customer-name">
        <xsl:value-of select="FirstName"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="LastName"/>
      </div>
      <div class="detail"><strong>Телефон:</strong> <xsl:value-of select="Phone"/></div>
      <div class="detail"><strong>Email:</strong> <xsl:value-of select="Email"/></div>
      <div class="detail"><strong>Адрес:</strong> <xsl:value-of select="Address"/></div>
      
      <xsl:variable name="custID" select="@customerID"/>
      <xsl:if test="//Repair[CustomerRef/@customerID=$custID]">
        <h3>Автомобили на клиента:</h3>
        <xsl:for-each select="//Repair[CustomerRef/@customerID=$custID]">
          <xsl:variable name="vin" select="VehicleRef/@VIN"/>
          <xsl:if test="not(preceding::Repair[VehicleRef/@VIN=$vin and CustomerRef/@customerID=$custID])">
            <div class="vehicle">
              <strong><xsl:value-of select="//Vehicle[@VIN=$vin]/Make"/>
              <xsl:text> </xsl:text>
              <xsl:value-of select="//Vehicle[@VIN=$vin]/Model"/></strong>
              (<xsl:value-of select="//Vehicle[@VIN=$vin]/@year"/>г.) - 
              <xsl:value-of select="//Vehicle[@VIN=$vin]/RegistrationNumber"/>
            </div>
          </xsl:if>
        </xsl:for-each>
      </xsl:if>
    </div>
  </xsl:template>
</xsl:stylesheet>
