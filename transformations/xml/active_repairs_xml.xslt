<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" encoding="UTF-8" indent="yes"/>
  
  <xsl:template match="/">
    <ActiveRepairsReport>
      <GeneratedFor><xsl:value-of select="AutoService/@name"/></GeneratedFor>
      <RepairsInProgress>
        <xsl:apply-templates select="AutoService/Repairs/Repair[@status='in_progress' or @status='open']"/>
      </RepairsInProgress>
      <Summary>
        <TotalActiveRepairs><xsl:value-of select="count(AutoService/Repairs/Repair[@status='in_progress' or @status='open'])"/></TotalActiveRepairs>
        <ExpectedRevenue><xsl:value-of select="sum(AutoService/Repairs/Repair[@status='in_progress' or @status='open']/TotalCost)"/></ExpectedRevenue>
      </Summary>
    </ActiveRepairsReport>
  </xsl:template>
  
  <xsl:template match="Repair">
    <RepairRecord status="{@status}">
      <xsl:variable name="custID" select="CustomerRef/@customerID"/>
      <xsl:variable name="vin" select="VehicleRef/@VIN"/>
      <xsl:variable name="mechID" select="MechanicRef/@mechanicID"/>
      
      <CustomerName>
        <xsl:value-of select="//Customer[@customerID=$custID]/FirstName"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="//Customer[@customerID=$custID]/LastName"/>
      </CustomerName>
      <CustomerPhone><xsl:value-of select="//Customer[@customerID=$custID]/Phone"/></CustomerPhone>
      <Vehicle>
        <Make><xsl:value-of select="//Vehicle[@VIN=$vin]/Make"/></Make>
        <Model><xsl:value-of select="//Vehicle[@VIN=$vin]/Model"/></Model>
        <RegNumber><xsl:value-of select="//Vehicle[@VIN=$vin]/RegistrationNumber"/></RegNumber>
      </Vehicle>
      <AssignedMechanic>
        <xsl:value-of select="//Mechanic[@mechanicID=$mechID]/FirstName"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="//Mechanic[@mechanicID=$mechID]/LastName"/>
      </AssignedMechanic>
      <StartDate><xsl:value-of select="StartDate"/></StartDate>
      <Services>
        <xsl:for-each select="ServiceRef">
          <xsl:variable name="svcID" select="@serviceID"/>
          <Service><xsl:value-of select="//Service[@serviceID=$svcID]/Description"/></Service>
        </xsl:for-each>
      </Services>
      <EstimatedCost><xsl:value-of select="TotalCost"/></EstimatedCost>
    </RepairRecord>
  </xsl:template>
</xsl:stylesheet>
