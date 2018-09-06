
library(XML)
library(plyr)
library(plotly)
library(shiny)
library(leaflet)
library(data.table)
library(DT)
#library(ggplot2)


read_activity <-function(file = file.choose()) {
    data <- switch(tools::file_ext(file),
                   "fit" = read_fit(file),
                   "gpx" = read_gpx(file),
                   "tcx" = read_tcx(file),
                   {message("not recognised file ext, return NULL."); NULL})
    data
}

# code used: 
# https://stackoverflow.com/a/8280881/2534699
read_tcx <- function(file = file.choose()) {
    doc <- xmlParse(file)
    nodes <- getNodeSet(doc, "//ns:Trackpoint", "ns")
    #rows <- lapply(nodes, function(x) as.data.frame(xmlToList(x))) 
    #data <- do.call("rbind", rows) 
    #data <- rbindlist(rows, fill=T)  # same as above but faster, neither working :(
    data <- plyr::ldply(nodes, as.data.frame(xmlToList))
    data$alt <- as.numeric(as.character(data$value.AltitudeMeters))
    data$lat <- as.numeric(as.character(data$value.Position.LatitudeDegrees))
    data$long <-as.numeric(as.character(data$value.Position.LongitudeDegrees))
    data$hr <-as.numeric(as.character(data$value.Value))
    data$t <- as.POSIXct(strptime(data$value.Time, format="%Y-%m-%dT%H:%M:%SZ"))
    data
}

# code used:
# https://stackoverflow.com/a/36392499/2534699
read_gpx <- function(file = file.choose()) {
    gpx.raw <- xmlTreeParse(file, useInternalNodes = TRUE)
    rootNode <- xmlRoot(gpx.raw)
    gpx.rawlist <- xmlToList(rootNode)$trk
    gpx.list <- unlist(gpx.rawlist[names(gpx.rawlist) == "trkseg"], recursive = FALSE)
    gpx <- do.call(rbind.fill, lapply(gpx.list, function(x) as.data.frame(t(unlist(x)), stringsAsFactors=F)))
    #names(gpx) <- c("ele", "time", "hr", "lon", "lat")
    gpx$lat <- as.double(gpx$.attrs.lat)
    gpx$long <- as.double(gpx$.attrs.lon)
    gpx$t <- as.POSIXct(strptime(gpx$time, format="%Y-%m-%dT%H:%M:%SZ"))
    gpx$alt <- as.numeric(as.character(gpx$ele))
    gpx
}

# code used off package cycleRtools
read_fit <- function(file = file.choose()) {
    #fitf <- "./data/1018058198.fit"
    #FitCSVTool <- system.file("/home/andi_sp/spone/ipleaf/FitCSVTool.jar")
    #FitCSVTool <- "/home/andi_sp/spone/ipleaf/FitCSVTool.jar"
    FitCSVTool <- "./FitCSVTool.jar"
    tmpf <- tempfile()
    system2("java", args = c("-jar", FitCSVTool, "-b", file, tmpf, "--data", "record", "--defn", "none"), stdout=FALSE)
    message("Reading .fit file...")
    d <- read.csv(paste0(tmpf, "_data.csv"))
    d$lat <- d$record.position_lat.semicircles. * (180/2^31)
    d$long <- d$record.position_long.semicircles. * (180/2^31)
    d$alt <- d$record.altitude.m.
    d$t <- as.POSIXct(d$record.timestamp.s., origin="1989-12-31")
    d$hr <- d$record.heart_rate.bpm.
    d
}

#leaflet
#library(leaflet)
#m <- leaflet(g) %>% addTiles() %>% addPolylines(lat=  ~lat, lng = ~long) %>% addMarkers(lat = ~lat[1], lng = ~long[1]) 
#m



r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


ui <- fluidPage(
        titlePanel("activity browser"),
        sidebarLayout(
                  sidebarPanel(
                       #p("files"),
                       #tableOutput("ftable")
                       DT::dataTableOutput("ftable")
                  ),
                  mainPanel(
                       leafletOutput("mymap"),
                       #dygraphOutput("dygraph")
                       plotlyOutput(outputId="plotly0", height="500px")
                       #plotOutput("plot1", click="plot1_click"),

                  ), 
                      #position = c("left", "right"), 
                      fluid = TRUE)
)

server <- function(input, output, session) {
    flist <- as.data.frame(list.files('data'))
    names(flist) <- c("fileList")
    g <- read_activity('./data/1019380246.fit')
    output$mymap <- renderLeaflet({
        m <- leaflet() %>% addTiles() 
    })

    observe({
            eventdata <- event_data("plotly_hover", source = "source")
            if (!is.null(eventdata)) {
                datapoint <- as.numeric(eventdata$pointNumber)[1]
                dpf <- g[datapoint,][c("lat","long")]
                names(dpf) <- c("latitude", "longitude")
                leafletProxy("mymap") %>% removeShape("A")
                leafletProxy("mymap") %>% addMarkers(data=dpf, layerId="A")
            }
    })

    observe({
        leafletProxy("mymap") %>% clearShapes()
        curFi <- input$ftable_rows_selected
        for (ind in curFi) {
            g <<- read_activity(paste("data", flist$fileList[ind], sep="/"))

            leafletProxy("mymap", data=g) %>% addPolylines(lat=g$lat, lng=g$long)
            leafletProxy("mymap") %>% fitBounds(min(g$long), min(g$lat), max(g$long), max(g$lat))

            output[["plotly0"]] <- renderPlotly({
                ay <- list(overlaying="y", side="right", title="bpm")
                p <- plot_ly(data=g, x=~t, y=~alt, name="altitude", mode="lines", type="scatter", source="source")
                p %>% layout(hovermode = 'x', title="dat", yaxis2=ay) %>% 
                    add_trace(x=~t, y=~hr, name="heartrate", yaxis="y2")
            })
        }
    })
    output$ftable <- DT::renderDataTable(flist, server=FALSE)
}
shinyApp(ui, server)












