library(leaflet)

# Choices for drop-downs
varsColor <- c(
  "Isotope System" = "IsotopeSystem.s.",
  "Sample Type" = "SampleType",
  "Tissue Type" = "TissueTType",
  "Genus Order" = "GenusOrder",
  "Sample Time" = "ModernArch."
)

navbarPage("Chaco Isotope Database", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 630, height = "auto",

        h2("Chaco Data Explorer"),

        #checkboxInput("fullmodel", "Full", value=FALSE),

        tags$hr(),


        #textInput("yourzipcode", "Zip Code", value="87108"),

        tags$hr(),

        selectInput("color", "Map Point Color", varsColor, selected="Sample Type"),
        #selectInput("size", "Size", varsSize, selected = "income"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        ),

        #plotOutput("histCentile", height = 200),
        uiOutput("isodensselectui"),
        plotOutput("isoDens", height = 250),
        checkboxInput("uselabs", "Site Legend", value=FALSE)
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('The UNM Chaco Project'), ' Directed by Chip Wills.'
      )
    )
  ),

  tabPanel("Data exporer",
    downloadButton('downloaddata'),
    tags$hr(),
    #fluidRow(
    #  column(3,
    #    selectInput("SiteID", "SiteID", c("All Sites"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
    #  ),
    #  column(3,
    #    conditionalPanel("input.states",
    #      selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
    #    )
    #  ),
    #  column(3,
    #    conditionalPanel("input.states",
    #      selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
    #    )
    #  )
    #),
    #hr(),
    DT::dataTableOutput("ziptable")
  ),






  conditionalPanel("false", icon("crosshair"))
)
