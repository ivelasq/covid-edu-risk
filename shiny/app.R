# Data --------------------------------------------------------------------

source("packages.R")

MAcovid <- # processed MA data
    read_csv(here::here("shiny", "data", "MAcovid.csv"))

# UI ----------------------------------------------------------------------

ui <-
    dashboardPage(
        dashboardHeader(title = "COVID Community Risk Explorer",
                        titleWidth = 450),
        
        # Sidebar -----------------------------------------------------------------
        
        dashboardSidebar(
            width = 300,
            column(width = 12,
                   
                   # Header
                   h2("COVID Community Risk Explorer"),
                   br("The purpose of this explorer is to showcase the COVID risk in communities around Massachusetts based on ZIP codes."),
                   br(),
                   sidebarMenu(

                       textInput(inputId = "org",
                                 label = "Write in Name of School Here",
                                 value = "School XYZ"
                                 ),
                       
                       fileInput("file1", "Upload File with ZIP Codes",
                                 accept = c(
                                     "text/csv",
                                     "text/comma-separated-values, text/plain",
                                     ".csv")),
                    
                        textOutput(outputId = "latest")
                       
                       ) # end sidebarMenu
                   ) # end column
            ), # end dashboardSidebar
        
        # Dashboard Body
        
        dashboardBody(
            tags$head(
                includeCSS(path = "www/style.css")
            ),
            
            fluidRow(
                box(
                    status = "warning",
                    width = 12,
                    plotOutput("plot1",
                               width = "100%")
                    ),
                
                box(status = "warning",
                    width = 6,
                    plotOutput("plot2",
                               width = "100%"),
                               res = 96
                    ),
                
                box(status = "warning",
                    width = 6,
                    plotOutput("plot3")
                    )
                
            ) # end fluid Row
            

        ) # end Dashboard Body
        ) # end Dashboard Sidebar
          

# Server -----------------------------------------------------------------

server <- function(input, output, session) { 
    df <- reactive({
        inFile <- input$file1
        dat <- read_csv(inFile$datapath,
                        col_types = cols(.default = "c"))
    })
    
    output$latest <- renderText({
        paste0("Date of last available data: ",
               (max(MAcovid$date, na.rm = TRUE)))
    })
    
    observeEvent(input$file1, {
        req(input$file1)
        
        ZipWeights <-
            df() %>%
            group_by(zipcode) %>% 
            tally() %>% 
            mutate(weight = n/sum(n))
        
        Org.MAcovid <- 
            MAcovid %>% 
            filter(zipcode %in% df()$zipcode) %>% 
            left_join(., ZipWeights, by = "zipcode")
        
        # calculate town-specific weights and rates
        
        Org <-
            Org.MAcovid %>%
            distinct(Town, date, .keep_all = TRUE) %>% #need to include otherwise towns with multiple zips have multiple rows
            group_by(Town) %>%
            dplyr::arrange(date, .by_group = TRUE) %>% #avoids the issue with negative incidence for first date for each town due to substracting prior town cases
            mutate(Count.wt=(Count*weight),
                   population.wt=town_population*weight,
                   Rate=(Count/town_population)*100000,
                   Tests.wt=(Tests*weight),
                   incidence7day=Rate-(lag(Rate,1)),
                   incidence1day=(Rate-lag(Rate,1))/7,
                   incidence14day=Rate-lag(Rate,2), 
                   incidence1day2wk=(Rate-lag(Rate,2))/14,
                   Tests2wk=Tests-(lag(Tests,2)),
                   Count2wk=Count-(lag(Count,2)),
                   positivity2wk=Count2wk/Tests2wk, 
                   town_population=(Count/Rate)*100000,
                   testingrate=100000*Tests2wk/ town_population
            )
        
        Org.community<-
            Org %>%
            arrange(Town,date) %>% 
            group_by(date) %>% 
            summarise(Count=sum(Count),
                      Count.weighted=sum(Count.wt),
                      Tests=sum(Tests),
                      Tests.weighted=sum(Tests.wt),
                      population=sum(town_population),
                      population.weighted=sum(population.wt)) %>% 
            arrange(date)   %>% 
            mutate(incidence14day=((Count.weighted-lag(Count.weighted,2))/population.weighted)*100000,
                   incidence1day=incidence14day/14,
                   positivity14day=100*((Count.weighted-lag(Count.weighted,2))/(Tests.weighted-lag(Tests.weighted,2)))
            )
        
        output$plot1 <- renderPlot({
            
            Org %>%
                mutate(incidence1day=replace(incidence1day,incidence1day > 22, 22)) %>% #overwrite peak to fit in plot
                ggplot( aes(x=date, group=Town, color=Town)) +
                theme_classic() + 
                theme(plot.title = element_text(size = rel(1.5)),
                      axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
                geom_line(aes(y=incidence1day2wk), alpha=.7, size=1) +
                scale_color_viridis(discrete = TRUE, end=0.8, option = "D")+
                geom_label_repel(data=Org %>%
                                     filter(date == last(date)), aes(label= Town,
                                                                     x = date ,  y = incidence1day2wk), 
                                 hjust=0,
                                 xlim=c(Sys.Date(), as.Date(NA)))+
                guides(color=FALSE) +
                scale_y_continuous(limits= c(0,22), breaks= c(2,4,6,8,10, 12, 14, 16,18,20))+
                # scale_fill_viridis(discrete = TRUE, end=0.85, option = "D")+
                guides(color=FALSE) +
                labs(x="Date", y="Two-Week Rolling Average of Daily Cases, per 100,000",
                     title=paste0("Estimated Daily Covid-19 Cases in\nCommunities of ", input$org,", 2020"),
                     caption="Data source: Massachusetts Department of Public Health and Boston Public Health Commission" )+
                scale_x_date(limits= c(as.Date("2020-05-01"), Sys.Date()+45))
        })
        
        output$plot2 <- renderPlot ({
            Org.community %>%
                filter(date > as.Date("2020-05-07")) %>%  #data not available for all prior 
                mutate(incidence1day=replace(incidence1day,incidence1day > 22, 22)) %>% #overwrite peak average (~23) to fit in plot
                add_row(date=as.Date(Sys.Date()+55), incidence1day=NA) %>% #add row with xmax so that ribbbon plots to max date
                ggplot( aes(x=date)) +
                geom_ribbon(aes(ymin=0, ymax=4), fill="#20854E99")+
                geom_ribbon(aes(ymin=4, ymax=8), fill="#FFDC9199")+
                geom_ribbon(aes(ymin=8, ymax=22), fill="#BC3C2999")+
                theme_classic() + theme(plot.title = element_text(size = rel(1.5)),
                                        axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
                geom_line(aes(y=incidence1day), alpha=1, size=2, color="#6F99ADFF") +
                geom_label_repel(data=Org.community %>%
                                     filter(date == last(date)), 
                                 aes(label=paste0(input$org, "\nCommunity Average\n","(",round(incidence1day,1),"%)"),
                                     x = date ,  y = incidence1day),
                                 hjust=0,
                                 xlim=c(Sys.Date(), as.Date(NA)))+
                guides(color=FALSE) +
                scale_y_continuous(limits= c(0,22), breaks= c(2,4,6,8,10, 12, 14, 16,18,20))+
                scale_x_date(limits= c(as.Date("2020-05-14"), Sys.Date()+55), date_breaks = "1 month",date_labels = "%b")+
                guides(color=FALSE) +
                labs(x="Date", y="Average Daily Cases, per 100,000",
                     title=paste0("Covid-19 Cases in Communities of ", input$org,", 2020"),
                     subtitle = "Green, Yellow, Red risk categorization as per Commonwealth Guidelines",
                     caption=paste0("Two-week average incidence in towns/neighborhoods of\n", input$org, " members weighted by number of households\nData Source: MA Dept of Public Health and Boston Public Health Commission")
                )
        })
        
        output$plot3 <- renderPlot ({
            Org.community %>%
                filter(date > as.Date("2020-05-20")) %>%  #data not available for all prior 
                mutate(positivity14day=replace(positivity14day,positivity14day > 10, 10)) %>% #overwrite peak average  to fit in plot
                add_row(date=as.Date(Sys.Date()+55), incidence1day=NA) %>% #add row with xmax so that ribbbon plots to max date
                ggplot( aes(x=date)) +
                geom_ribbon(aes(ymin=0, ymax=2), fill="#20854E99")+
                geom_ribbon(aes(ymin=2, ymax=10), fill="#BC3C2999")+
                theme_classic() + theme(plot.title = element_text(size = rel(1.5)),
                                        axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)))+
                geom_line(aes(y=positivity14day), alpha=1, size=2, color="#6F99ADFF") +
                geom_label_repel(data=Org.community %>%
                                     filter(date == last(date)), 
                                 aes(label=paste0(input$org, "\nCommunity Average\n","(",round(positivity14day,2),"%)"),
                                     x = date ,  y = positivity14day),
                                 hjust=0,
                                 xlim=c(Sys.Date(), as.Date(NA)))+
                guides(color=FALSE) +
                scale_y_continuous(limits= c(0,10))+
                scale_x_date(limits= c(as.Date("2020-05-14"), Sys.Date()+55), date_breaks = "1 month",date_labels = "%b")+
                guides(color=FALSE) +
                labs(x="Date", y="Positivity\n(% of molecular tests positive)",
                     title=paste0("Positivity in Communities of ", input$org,", 2020"),
                     subtitle = "Green and Red risk categorization as per Commonwealth Guidelines",
                     caption=paste0("Molecular test positivity in towns/neighborhoods of\n", input$org," members weighted by number of households\nData Source: MA Dept of Public Health and Boston Public Health Commission")
                )
        })
        
    })
    
}

shinyApp(ui, server)
        