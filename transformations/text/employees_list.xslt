<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8"/>
  
  <xsl:template match="/">
    <xsl:text>═══════════════════════════════════════════════════════════════════════════&#10;</xsl:text>
    <xsl:text>                         СПИСЪК СЛУЖИТЕЛИ&#10;</xsl:text>
    <xsl:text>                    </xsl:text>
    <xsl:value-of select="AutoService/@name"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>═══════════════════════════════════════════════════════════════════════════&#10;</xsl:text>
    <xsl:text>&#10;</xsl:text>
    
    <xsl:apply-templates select="AutoService/Employees/Mechanic">
      <xsl:sort select="LastName"/>
    </xsl:apply-templates>
    
    <xsl:text>&#10;</xsl:text>
    <xsl:text>───────────────────────────────────────────────────────────────────────────&#10;</xsl:text>
    <xsl:text>Общо механици: </xsl:text>
    <xsl:value-of select="count(AutoService/Employees/Mechanic)"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>═══════════════════════════════════════════════════════════════════════════&#10;</xsl:text>
  </xsl:template>
  
  <xsl:template match="Mechanic">
    <xsl:text>&#10;</xsl:text>
    <xsl:text>МЕХАНИК #</xsl:text>
    <xsl:value-of select="@mechanicID"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:text>───────────────────────────────────────────────────────────────────────────&#10;</xsl:text>
    
    <xsl:text>Име: </xsl:text>
    <xsl:value-of select="FirstName"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="LastName"/>
    <xsl:text>&#10;</xsl:text>
    
    <xsl:text>Специализация: </xsl:text>
    <xsl:value-of select="Specialization"/>
    <xsl:text>&#10;</xsl:text>
    
    <xsl:if test="@specialty">
      <xsl:text>Допълнителна специалност: </xsl:text>
      <xsl:value-of select="@specialty"/>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>
    
    <xsl:text>Опит: </xsl:text>
    <xsl:value-of select="Experience"/>
    <xsl:text> години&#10;</xsl:text>
    
    <xsl:text>Телефон: </xsl:text>
    <xsl:value-of select="Phone"/>
    <xsl:text>&#10;</xsl:text>
    
    <xsl:variable name="mechID" select="@mechanicID"/>
    <xsl:variable name="repairCount" select="count(//Repair[MechanicRef/@mechanicID=$mechID])"/>
    <xsl:text>Извършени ремонти: </xsl:text>
    <xsl:value-of select="$repairCount"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
