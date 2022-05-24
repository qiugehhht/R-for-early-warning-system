library(RMySQL)
mydb = dbConnect(dbDriver("MySQL"), user='root', password='123456', dbname='cluster', host='47.113.187.45')
dbListTables(mydb)
dbListFields(mydb, 'covid_case')
dbListFields(mydb, 'loc')

dbGetQuery(mydb, "SHOW VARIABLES LIKE 'character_set_%';")

dbGetQuery(mydb, "SET NAMES 'gbk'") 
covid_case = dbSendQuery(mydb, "select * from covid_case")
covid_case = fetch(covid_case, n=-1)
loc = dbSendQuery(mydb, "select * from loc")
loc = fetch(loc, n=-1)

dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}
dbDisconnectAll()

library(rsatscan)

invisible(ss.options(reset=TRUE))
#ss.options()
ss.options(list(CaseFile="covid.cas", PrecisionCaseTimes=3))
ss.options(c("StartDate=2022/2/16","EndDate=2022/3/6"))
ss.options(list(CoordinatesFile="covid.geo", AnalysisType=3, ModelType=2, TimeAggregationUnits=3))

td = tempdir()
write.ss.prm(td, "covid")
covid_case$date <- as.factor(covid_case$date)
write.cas(covid_case, td, "covid")
write.geo(loc, td, "covid")

covid_huhhot = satscan(td, "covid", sslocation="E:/SatScan9.6")
summary(covid_huhhot)
circle<-covid_huhhot$col
covid_huhhot$sci
covid_huhhot$gis

class(covid_huhhot$shapeclust)

library(leafletCN)
library(leaflet)
library(sf)

points <- st_as_sf(x = covid_huhhot$gis,
                   coords = c("LOC_LONG", "LOC_LAT"),
                   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
virusIcon <- makeIcon("F:/epi/virus.png",25,25)

m1 <- leaflet(covid_huhhot$shapeclust) %>%
                      amap() %>% 
                      addPolygons(label = paste("起始时间：",circle$START_DATE,"，",
                                                "结束时间：",circle$END_DATE),
                                  stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2,color = "blue") 

m2 <- m1 %>%
  addCircleMarkers(data = points, color = "red",fillOpacity = 0.8,radius = 3,
             label = paste("病例: ", points$LOC_ID)
  )
m2

library(shiny)
ui <- fillPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  leafletOutput("m2", width = "100%", height = "100%")
  
)
server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$m2 <-renderLeaflet({m2})
  }
shinyApp(ui, server)
