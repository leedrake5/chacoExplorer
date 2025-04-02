library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(data.table)
library(DT)

function(input, output, session) {
    
    
    # Leaflet bindings are a bit slow; for now we'll just sample to compensate
    set.seed(100)
    
    
    zipdataInput <- reactive({
    data
    
    })
    
    
    # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
    # will be drawn last and thus be easier to see
    zipdataToo <- reactive(
    {zipdata <- zipdataInput()
    #zipdata[order(zipdata$centile),]
    })
    


  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
      leaflet() %>%
            addTiles() %>%
      setView(lng = -107.9559, lat = 36.053, zoom = 13)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
      zipdata <- zipdataToo()
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      Latitude >= latRng[1] & Latitude <= latRng[2] &
        Longitude >= lngRng[1] & Longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  #centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    hist(zipsInBounds()$centile,
      breaks = 20, #centileBreaks,
      main = "Chaco Isotope Data",
      xlab = "Percentile",
      #xlim = range(allzips$centile),
      col = '#00DD00',
      border = 'white')
  })
  
  output$isodensselectui <- renderUI({
      
      selectInput("isodensselect", "Isotope", choices=colnames(data)[36:104], selected="Sr87Sr86_Standard")
      
  })
  
  
  isoDensData <- reactive({
      
      data <- zipsInBounds()
      data$Selected <- as.numeric(zipsInBounds()[,input$isodensselect])
      
      dist.plot <- if(input$uselabs){
          ggplot(data, aes(Selected, fill = SiteID)) +
          geom_density(alpha=0.6) +
          #coord_cartesian(xlim = c(0.705, 0.717)) +
          scale_x_continuous(input$isodensselect) +
          ylab("Density") +
          theme_light() +
          theme(legend.position="bottom")
      } else {
          ggplot(data, aes(Selected, fill = SiteID)) +
          geom_density(alpha=0.6) +
          #coord_cartesian(xlim = c(0.705, 0.717)) +
          scale_x_continuous(input$isodensselect) +
          ylab("Density") +
          theme_light() +
          theme(legend.position="none")
      }
      

      
      dist.plot
      
  })
  
  output$isoDens <- renderPlot({
      
      isoDensData()
      
  })

  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
      
      colorBy <- input$color
      sizeBy <- input$size
      
      cor.test <- lm(as.numeric(as.vector(zipsInBounds()[[colorBy]])) ~ as.numeric(as.vector(zipsInBounds()[[sizeBy]])))
      
      r2 <- summary(cor.test)$r.squared
      intercept <- cor.test$coef[1]
      slope <- cor.test$coef[2]
      

#print(xyplot(zipsInBounds()[[colorBy]] ~ zipsInBounds()[[sizeBy]], xlab=sizeBy, ylab=colorBy, type=c("p", "r"), main=expression(paste("y ="*paste(slope)*"x + "*paste(intercept)*", r"^2*paste(r2)))))

    temp.frame <- data.frame(as.numeric(as.vector(zipsInBounds()[[sizeBy]])), as.numeric(as.vector(zipsInBounds()[[colorBy]])))
    colnames(temp.frame) <- c("x", "y")
    temp.frame <- na.omit(temp.frame)
    
    scatter <- ggplot(aes(x, y), data=temp.frame) +
    geom_point(colour="blue") +
    stat_smooth(method="lm") +
    theme_light() +
    scale_x_continuous(sizeBy) +
    scale_y_continuous(colorBy) +
     annotate("text", label=lm_eqn(cor.test), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE) 
    
    scatter
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
     zipdata <- zipdataToo()
    colorBy <- input$color
    sizeBy <- input$size

    if (colorBy == "superzip") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
      pal <- colorFactor("Spectral", colorData)
    } else {
      colorData <- zipdata[[colorBy]]
      pal <- colorFactor(
        palette = 'Dark2',
        domain = zipdata[,colorBy]
      )

    }

    #if (sizeBy == "superzip") {
    #  # Radius is treated specially in the "superzip" case.
    #  radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    #} else {
    #  radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    #}

    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircleMarkers(~Longitude, ~Latitude, radius=20, layerId=~SiteNumber,
        stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
        layerId="colorLegend")
  })

  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- data[data$ChacoIsotopeDatabaseID == zipcode,]
    content <- as.character(tagList(
      tags$h4("ChacoIsotopeDatabaseID:", selectedZip$ChacoIsotopeDatabaseID),
      tags$strong(HTML(sprintf("%s, %s %s",
        selectedZip$ModernArch., selectedZip$SampleType, selectedZip$SiteID
      )))
      )
    )
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  ####When zipcode is selected, show popup with city info
  
  #observe({
  #    leafletProxy("map") %>% clearPopups()
  #    event <- as.numeric(paste(input$yourzipcode))
  #    zipframe <- subset(zipcodes, zipcodes$zip_code==event)
      
      
      
  #    if (is.null(event))
  #    return()
      
  #    isolate({
  #        showZipcodePopup(event, zipframe$latitude, zipframe$longitude)
  #    })
  #})



  ## Data Explorer ###########################################
  
  reactiveZip <- reactive({
      
      smalls <- zipsInBounds()
      
      smalls
      
  })



  output$ziptable <- DT::renderDataTable({
      
      
    df <- reactiveZip()
    
    df <- df[,c("ChacoIsotopeDatabaseID", "OriginalSampleID", "SampleType", "SiteID", "Description", "Source")]

    DT::datatable(df)
  })
  
  
  output$downloaddata <- downloadHandler(
  filename = function() { paste("chacoData", '.csv', sep=',') },
  content = function(file
  ) {
      write.csv(reactiveZip(), file)
  }
  )
  
  
  
}
