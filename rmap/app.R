library(shiny)
library(dplyr)
library(leaflet)
library(sp)
library(dplyr)

#library(rstudioapi)

# Getting the path of your current open file
#current_path = rstudioapi::getActiveDocumentContext()$path 
#setwd(dirname(current_path ))

#https://www.timlrx.com/2017/09/13/using-leaflet-in-r-tutorial/
#https://www.timlrx.com/2017/09/10/examining-the-changes-in-religious-beliefs-part-2/

df_prop <- readRDS("data/mobility_map_data.rds")
sg_map <- readRDS("data/SG_map.rds")
plot_select<-"nothing"
ui <- fluidPage(
                titlePanel("#StayHomeForSG Mobility Dashboard"),
                sidebarLayout(
                              sidebarPanel(
                                           p("What administrative regions in Singapore are better at adhering to the circutbreaker, as compared to mobility based on pre-crisis levels? This map illustrates decrease in the movement of groups of people from different neighborhoods over a period of several hours. By understanding these patterns, response organizations can better predict where COVID-19 clusters might emerge, or where resources will be needed. "),
                                           radioButtons("colourscheme", "Plot Colour Scheme:",
                                                        choices = c("Divergent", "Sequential"), selected="Divergent",
                                                        inline = TRUE),
                                           selectInput("cat",
                                                       "Select a category:",
                                                       c("Mobility", "Clusters")),

                                           uiOutput("controls"),

                                           tags$hr(),
                                           p("Source: Facebook Data for Good program"),
                                           p("Note: Mobility data is based on opt-in settings by users of the Facebook mobile app. It is intended as an approximation. Data from Boon Lay and Pioneer is clubbed together.",
                                             "For more information, please contact",a("Asst Prof Kokil Jaidka at the National University of Singapore",href="mailto:jaidka@nus.edu.sg")),

                                           p("Thanks to Timlrx for the ", a("R tutorial", href="https://www.timlrx.com/2017/09/13/using-leaflet-in-r-tutorial/"), "and the", a("source code", href="https://github.com/timlrx/sg-mapping"))
                                           ),
                              ##    Full set of code and data to reproduce the plot can be found",
                              ##        #a("here.", href="https://github.com/kj2013/"),

                              # Show a plot of the generated distribution
                              mainPanel(
                                        h3(textOutput("maptitle")),
                                        leafletOutput("map"),
                                        textOutput("totalstat"),
                                        sliderInput("endDate",
                                                    "End Date:",
                                                    min = as.Date("2020-02-12","%Y-%m-%d"),
                                                    max = as.Date("2020-04-18","%Y-%m-%d"),
                                                    value=as.Date("2020-04-18"),
                                                    timeFormat="%Y-%m-%d",ticks = TRUE, step=1,width='1200px',animate=TRUE)
                                        )
                              )
                )

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$controls <- renderUI({
    selectInput("selection",
                "Select a variable to visualise:",
                switch(input$cat,
                       "Mobility"= c("Show all","Decrease in Mobility <= 25th percentile", "Decrease in Mobility >= 75th percentile"),
                       "Clusters" = c("All","Foreign worker clusters","Other")))

  })

  data <- reactive({

    if (is.null(input$selection)) {
      return(NULL)
    }
    date_select <- input$endDate

    plot_select <- switch(input$selection,
                          "Decrease in Mobility <= 25th percentile"="pc25",
                          "Decrease in Mobility >= 75th percentile"="pc75",
                          "All"="all_cluster",
                          "Foreign worker cluster"="foreign_cluster",
                          "Other clusters"="other_cluster",
                          input$selection)
    if(input$cat=="Mobility"){
      tmpdata <- df_prop[c("date", "Level_3_cap","percent_change")]
      tmpdata<-tmpdata[tmpdata$date==date_select,]
      ## print(head(tmpdata))
      if(plot_select=="pc25")
      {

        thresh=quantile(tmpdata$percent_change)[2]
        tmpdata<-tmpdata[tmpdata$percent_change<=thresh,]
        ## print(head(tmpdata))
      }
      if(plot_select=="pc75")
      {
        thresh=quantile(tmpdata$percent_change)[3]
        tmpdata<-tmpdata[tmpdata$percent_change>=thresh,]
        ##print(head(tmpdata))
      }
    }
    if(input$cat=="Clusters"){
      tmpdata <- df_prop[c("date", "Level_3_cap","N","percent_change")]
      tmpdata<-tmpdata[tmpdata$date=="2020-04-18",]
      tmpdata<-unique(tmpdata)
      ## print(head(tmpdata))
    }
    # tmpdata<- rename_(tmpdata, Value=protectStr(plot_select))


    sp::merge(y=tmpdata,x = sg_map, by.y="Level_3_cap",by.x="PLN_AREA_N",duplicateGeoms = TRUE )
  })


  palette_choice1 <- reactive({
    if(input$colourscheme=="Divergent"){
      col <- "RdYlGn"


    } else{
      col <- "Blues"
    }

    colorBin(col, domain = as.numeric(unlist(data()@data["percent_change"])), bins = 5, reverse = FALSE)}

  )

  palette_choice2 <- reactive({
    col <- "Reds"

    colorBin(col, domain = as.numeric(unlist(data()@data["N"])), bins = 5, reverse =FALSE)
  })

  output$maptitle <- renderText({
    if (is.null(input$selection)) {
      return(NULL)
    }
    if(input$cat=="Mobility"){
      grp <- "Mobility"
      paste0("Relative decrease in Mobility on ", input$endDate," as compared to the same days of the week in January (greener is better)")
    } else{
      grp <- "Clusters"
      paste0("Clusters detected as of 2020-04-18")
    }

  })


  output$map <- renderLeaflet({

    if (is.null(data())) {
      return()
    }

    leaflet(sg_map) %>% 
      addProviderTiles("CartoDB.Positron",
                       options = tileOptions(minZoom=10, maxZoom=13)) %>%
    setView(103.851959,1.3521,zoom=11)

  })

  observe({
    if (is.null(data())) {
      return()
    }




    if(input$cat=="Mobility"){
      labels <- sprintf(
                        "<strong>%s</strong><br/>%g &#37;",
                        data()$PLN_AREA_N, round(as.numeric(unlist(data()@data["percent_change"])),2)
                        ) %>% lapply(htmltools::HTML)
      pal <- palette_choice1()  
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data=data(),
                    fillColor = pal(as.numeric(unlist(data()@data["percent_change"]))),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                                                 weight = 5,
                                                 color = "#666",
                                                 dashArray = "",
                                                 fillOpacity = 0.7,
                                                 bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                                textsize = "15px",
                                                direction = "auto")
                    #popup = labels
                    ) %>%
        clearControls() %>%
        addLegend(pal = pal, values = as.numeric(unlist(data()@data["percent_change"])), opacity = 0.7,
                  title = NULL, position= "bottomright",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    }
    if(input$cat=="Clusters")
    {
      labels <- sprintf(
                        "<strong>%s</strong><br/>Number infected = %g;",
                        data()$PLN_AREA_N, round(as.numeric(unlist(data()@data["N"])),2)
                        ) %>% lapply(htmltools::HTML)

      pal <- palette_choice2()  
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data=data(),
                    fillColor = pal(as.numeric(unlist(data()@data["N"]))),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                                                 weight = 5,
                                                 color = "#666",
                                                 dashArray = "",
                                                 fillOpacity = 0.7,
                                                 bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                                textsize = "15px",
                                                direction = "auto")
                    #popup = labels
                    ) %>%
        clearControls() %>%
        addLegend(pal = pal, values = as.numeric(unlist(data()@data["N"])), opacity = 0.7,
                  title = NULL, position= "bottomright",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE)))
    }

  } )

}

# Run the application 
shinyApp(ui = ui, server = server)
#https://stackoverflow.com/questions/46575316/output-pure-html-file-from-r-shiny-app
