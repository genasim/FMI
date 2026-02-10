<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" encoding="UTF-8" indent="yes"/>
  
  <xsl:template match="/">
    <AutoServiceSummary>
      <ServiceName><xsl:value-of select="AutoService/@name"/></ServiceName>
      <Statistics>
        <TotalMechanics><xsl:value-of select="count(AutoService/Employees/Mechanic)"/></TotalMechanics>
        <TotalCustomers><xsl:value-of select="count(AutoService/Customers/Customer)"/></TotalCustomers>
        <TotalVehicles><xsl:value-of select="count(AutoService/Vehicles/Vehicle)"/></TotalVehicles>
        <TotalServices><xsl:value-of select="count(AutoService/Services/Service)"/></TotalServices>
        <TotalRepairs><xsl:value-of select="count(AutoService/Repairs/Repair)"/></TotalRepairs>
        <OpenRepairs><xsl:value-of select="count(AutoService/Repairs/Repair[@status='open'])"/></OpenRepairs>
        <InProgressRepairs><xsl:value-of select="count(AutoService/Repairs/Repair[@status='in_progress'])"/></InProgressRepairs>
        <CompletedRepairs><xsl:value-of select="count(AutoService/Repairs/Repair[@status='completed'])"/></CompletedRepairs>
        <TotalRevenue><xsl:value-of select="sum(AutoService/Repairs/Repair[@status='completed']/TotalCost)"/></TotalRevenue>
      </Statistics>
      <MechanicsList>
        <xsl:apply-templates select="AutoService/Employees/Mechanic"/>
      </MechanicsList>
    </AutoServiceSummary>
  </xsl:template>
  
  <xsl:template match="Mechanic">
    <MechanicInfo id="{@mechanicID}">
      <FullName><xsl:value-of select="FirstName"/> <xsl:value-of select="LastName"/></FullName>
      <Specialty><xsl:value-of select="@specialty"/></Specialty>
      <RepairsCount><xsl:value-of select="count(//Repair[MechanicRef/@mechanicID=current()/@mechanicID])"/></RepairsCount>
      <Contact><xsl:value-of select="Phone"/></Contact>
    </MechanicInfo>
  </xsl:template>
</xsl:stylesheet>
