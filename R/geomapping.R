par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
library(RColorBrewer)

#getting example data and joining to a map
cd <- read.csv2("data/ccodes.csv", sep = " ")
cd$Count <- cd$Count
sPDF <- rworldmap::joinCountryData2Map(cd
                                , joinCode = "ISO2"
                                , nameJoinColumn = "Country"
                                , mapResolution="coarse"
                                )
mapParams <- rworldmap::mapCountryData(
  sPDF, 
  nameColumnToPlot="Count", 
  addLegend = FALSE  )


do.call( rworldmap::addMapLegend, 
         c(mapParams, 
           legendWidth=0.5, 
           legendMar = 2,
           horizontal = TRUE))
