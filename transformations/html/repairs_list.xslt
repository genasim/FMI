<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" encoding="UTF-8" indent="yes"/>
  
  <xsl:template match="/">
    <html>
      <head>
        <title>Списък на ремонтите - <xsl:value-of select="AutoService/@name"/></title>
        <style>
          body { font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }
          h1 { color: #2c3e50; }
          .repair { background: white; padding: 15px; margin: 10px 0; border-left: 4px solid #3498db; }
          .status-open { border-left-color: #e74c3c; }
          .status-in_progress { border-left-color: #f39c12; }
          .status-completed { border-left-color: #27ae60; }
          .info { margin: 5px 0; }
          .label { font-weight: bold; color: #34495e; }
        </style>
      </head>
      <body>
        <h1>Списък на ремонтите - <xsl:value-of select="AutoService/@name"/></h1>
        <xsl:apply-templates select="AutoService/Repairs/Repair"/>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="Repair">
    <div class="repair status-{@status}">
      <div class="info">
        <span class="label">Статус:</span>
        <xsl:choose>
          <xsl:when test="@status='open'">Отворен</xsl:when>
          <xsl:when test="@status='in_progress'">В процес</xsl:when>
          <xsl:when test="@status='completed'">Завършен</xsl:when>
        </xsl:choose>
      </div>
      <div class="info">
        <span class="label">Клиент:</span>
        <xsl:variable name="custID" select="CustomerRef/@customerID"/>
        <xsl:value-of select="//Customer[@customerID=$custID]/FirstName"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="//Customer[@customerID=$custID]/LastName"/>
      </div>
      <div class="info">
        <span class="label">Автомобил:</span>
        <xsl:variable name="vin" select="VehicleRef/@VIN"/>
        <xsl:value-of select="//Vehicle[@VIN=$vin]/Make"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="//Vehicle[@VIN=$vin]/Model"/>
        (<xsl:value-of select="//Vehicle[@VIN=$vin]/RegistrationNumber"/>)
      </div>
      <div class="info">
        <span class="label">Механик:</span>
        <xsl:variable name="mechID" select="MechanicRef/@mechanicID"/>
        <xsl:value-of select="//Mechanic[@mechanicID=$mechID]/FirstName"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="//Mechanic[@mechanicID=$mechID]/LastName"/>
      </div>
      <div class="info">
        <span class="label">Начална дата:</span>
        <xsl:value-of select="StartDate"/>
      </div>
      <xsl:if test="EndDate">
        <div class="info">
          <span class="label">Крайна дата:</span>
          <xsl:value-of select="EndDate"/>
        </div>
      </xsl:if>
      <div class="info">
        <span class="label">Обща цена:</span>
        <xsl:value-of select="TotalCost"/> лв.
      </div>
    </div>
  </xsl:template>
</xsl:stylesheet>
