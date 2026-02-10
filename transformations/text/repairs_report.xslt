<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8"/>
  
  <xsl:template match="/">
    <xsl:text>═══════════════════════════════════════════════════════════════════════════&#10;</xsl:text>
    <xsl:text>                    ОТЧЕТ ЗА ИЗВЪРШЕНИ РЕМОНТИ&#10;</xsl:text>
    <xsl:text>                    </xsl:text>
    <xsl:value-of select="AutoService/@name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>═══════════════════════════════════════════════════════════════════════════&#10;</xsl:text>
    <xsl:text>&#10;</xsl:text>
    
    <xsl:apply-templates select="AutoService/Repairs/Repair"/>
    
    <xsl:text>&#10;</xsl:text>
    <xsl:text>───────────────────────────────────────────────────────────────────────────&#10;</xsl:text>
    <xsl:text>ОБЩА СТАТИСТИКА:&#10;</xsl:text>
    <xsl:text>───────────────────────────────────────────────────────────────────────────&#10;</xsl:text>
    <xsl:text>Общо ремонти: </xsl:text>
    <xsl:value-of select="count(AutoService/Repairs/Repair)"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>Отворени: </xsl:text>
    <xsl:value-of select="count(AutoService/Repairs/Repair[@status='open'])"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>В процес: </xsl:text>
    <xsl:value-of select="count(AutoService/Repairs/Repair[@status='in_progress'])"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>Завършени: </xsl:text>
    <xsl:value-of select="count(AutoService/Repairs/Repair[@status='completed'])"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>Общ приход: </xsl:text>
    <xsl:value-of select="sum(AutoService/Repairs/Repair[@status='completed']/TotalCost)"/>
    <xsl:text> лв.&#10;</xsl:text>
    <xsl:text>═══════════════════════════════════════════════════════════════════════════&#10;</xsl:text>
  </xsl:template>
  
  <xsl:template match="Repair">
    <xsl:text>&#10;</xsl:text>
    <xsl:text>РЕМОНТ #</xsl:text>
    <xsl:value-of select="position()"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>───────────────────────────────────────────────────────────────────────────&#10;</xsl:text>
    
    <xsl:text>Статус: </xsl:text>
    <xsl:choose>
      <xsl:when test="@status='open'">ОТВОРЕН</xsl:when>
      <xsl:when test="@status='in_progress'">В ПРОЦЕС</xsl:when>
      <xsl:when test="@status='completed'">ЗАВЪРШЕН</xsl:when>
    </xsl:choose>
    <xsl:text>&#10;</xsl:text>
    
    <xsl:variable name="custID" select="CustomerRef/@customerID"/>
    <xsl:text>Клиент: </xsl:text>
    <xsl:value-of select="//Customer[@customerID=$custID]/FirstName"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="//Customer[@customerID=$custID]/LastName"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>Телефон: </xsl:text>
    <xsl:value-of select="//Customer[@customerID=$custID]/Phone"/>
    <xsl:text>&#10;</xsl:text>
    
    <xsl:variable name="vin" select="VehicleRef/@VIN"/>
    <xsl:text>Автомобил: </xsl:text>
    <xsl:value-of select="//Vehicle[@VIN=$vin]/Make"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="//Vehicle[@VIN=$vin]/Model"/>
    <xsl:text> (</xsl:text>
    <xsl:value-of select="//Vehicle[@VIN=$vin]/RegistrationNumber"/>
    <xsl:text>)&#10;</xsl:text>
    
    <xsl:variable name="mechID" select="MechanicRef/@mechanicID"/>
    <xsl:text>Механик: </xsl:text>
    <xsl:value-of select="//Mechanic[@mechanicID=$mechID]/FirstName"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="//Mechanic[@mechanicID=$mechID]/LastName"/>
    <xsl:text>&#10;</xsl:text>
    
    <xsl:text>Начална дата: </xsl:text>
    <xsl:value-of select="StartDate"/>
    <xsl:text>&#10;</xsl:text>
    
    <xsl:if test="EndDate">
      <xsl:text>Крайна дата: </xsl:text>
      <xsl:value-of select="EndDate"/>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>
    
    <xsl:text>Услуги:&#10;</xsl:text>
    <xsl:for-each select="ServiceRef">
      <xsl:variable name="svcID" select="@serviceID"/>
      <xsl:text>  - </xsl:text>
      <xsl:value-of select="//Service[@serviceID=$svcID]/Description"/>
      <xsl:text>&#10;</xsl:text>
    </xsl:for-each>
    
    <xsl:if test="UsedParts/Part">
      <xsl:text>Използвани части:&#10;</xsl:text>
      <xsl:for-each select="UsedParts/Part">
        <xsl:text>  - </xsl:text>
        <xsl:value-of select="PartName"/>
        <xsl:text> (Количество: </xsl:text>
        <xsl:value-of select="Quantity"/>
        <xsl:text>, Цена: </xsl:text>
        <xsl:value-of select="PartPrice"/>
        <xsl:text> лв.)&#10;</xsl:text>
      </xsl:for-each>
    </xsl:if>
    
    <xsl:text>&#10;</xsl:text>
    <xsl:text>ОБЩА ЦЕНА: </xsl:text>
    <xsl:value-of select="TotalCost"/>
    <xsl:text> лв.&#10;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
