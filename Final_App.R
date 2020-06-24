library(shiny)
library(plotly)
library(DT)
library(tidyverse)
library(lubridate)

ui <- fluidPage(
    sidebarPanel(
        # define your widgets
        fileInput(inputId = 'datainput',
                  label = "Choose Files to Import", 
                  multiple = TRUE),
        
        dateRangeInput(inputId = "daterange",
                       label = "Select Date Range:"),
        
        selectInput(inputId = "test", 
                    label = "Select Test:", 
                    choices = "<All>"),
        
        selectInput(inputId = "analyzer",
                    label =  "Select Analyzer:", 
                    choices = "<All>"),
        
        selectInput(inputId = "qcproduct", 
                    label = "Select QC Product:",
                    choices = "<All>"),
        
        radioButtons(inputId = "timeframe", 
                     label = "Summarize data by:", 
                     choices = list("Day"= 1, "Week"= 2, "Month" = 3)),
        width = 3
    ),
    
    mainPanel(
        tabsetPanel(id = "viewing",
                    
                    tabPanel(title = "QC Summary", dataTableOutput("sumtable")),
                    tabPanel(title = "Summary Plots", 
                             plotlyOutput('chart'),
                             checkboxInput("analyzeroverlay", 
                                           label = "Overlay Multiple Analyzers:",
                                           value = FALSE),
                             conditionalPanel(
                                 condition = "input.analyzeroverlay == true",
                                 selectInput(inputId = "analyzerlist",
                                             label = "Select Analyer:",
                                             choices = "<All>",
                                             multiple = TRUE)),
                             checkboxInput("qcoverlay", 
                                           label = "Overlay Multiple QC Levels:",
                                           value = FALSE),
                             conditionalPanel(
                                 condition = "input.qcoverlay == true",
                                 selectInput(inputId = "qc",
                                             label = "Select QC Level:",
                                             choices = "<All>",
                                             multiple = TRUE))
                    )
        )
    )
)

server <- function(session, input, output) {
    
    
    my_filter <- function(df, ...){
        dots<- list(...)
        
        dots <- lapply(dots, function(x){
            if(is.character(x) && x=="<All>")
                NA_character_
            else
                x
        })
        
        filters <- list(quote(Date >= isolate(dots$from_date)),
                        quote(Date <= isolate(dots$to_date)))
        
        if(!is.na(dots$test)){
            filters <- append(filters, 
                              list(quote(Test == isolate(dots$test))))
        }
        
        if(!is.na(dots$analyzer)){
            filters <- append(filters, 
                              list(quote(Analyzer == isolate(dots$analyzer))))
        }
        
        if(!is.na(dots$qcproduct)){
            filters <- append(filters,
                              list(quote(QC_Mnemonic == isolate(dots$qcproduct))))
        }
        
        filtered_df <- df %>% filter(!!!filters)
        return(filtered_df)
    }
    
    
    raw.data <- reactive({
        
        # Tell user to select QC data if none selected
        validate(
            need(!is.null(input$datainput$datapath), "Select QC Data File")
        )
        
        #if the file import filter is not empty, the user has imported 
        #different data files, so make a new list
        list <- input$datainput$datapath
        
        # same as above, but no need to sort
        raw.qcdata <- do.call("rbind", lapply(list, read.csv,
                                              stringsAsFactors = FALSE )) 
        
        # Format the date column, same as above
        raw.qcdata$Date <- ymd_hms(raw.qcdata$Date) 
        
        # Tell the date range box in the UI what ranges it can display
        updateDateRangeInput(session, "daterange", "Select Date Range:", 
                             min = min(round_date(raw.qcdata$Date, unit = "days")), 
                             max = max(round_date(raw.qcdata$Date, unit = "days")),
                             start = min(round_date(raw.qcdata$Date, unit = "days")),
                             end = max(round_date(raw.qcdata$Date, unit = "days"))
        )
        
        updateSelectInput(session, "test", "Select Test:", 
                          choices = c("<All>", unique(raw.qcdata$Test)))
        updateSelectInput(session, "analyzer", "Select Analyzer:", 
                          choices = c("<All>", unique(raw.qcdata$Analyzer)))
        updateSelectInput(session, "qcproduct", "Select QC Product:", 
                          choices = c("<All>", unique(raw.qcdata$QC_Mnemonic)))
        
        # Tells R to assign the imported data to the reactive variable raw.data
        return(raw.qcdata)  
    })
    
    filtered.data <- reactive({
        df <- my_filter(df = raw.data(), 
                        from_date = input$daterange[1],
                        to_date = input$daterange[2], 
                        analyzer = input$analyzer,
                        test = input$test, 
                        qcproduct = input$qcproduct)
        
        updateSelectInput(session, "test", "Select Test:", 
                          choices = c("<All>", unique(df$Test)))
        updateSelectInput(session, "analyzer", "Select Analyzer:", 
                          choices = c("<All>", unique(df$Analyzer)))
        updateSelectInput(session, "qcproduct", "Select QC Product:", 
                          choices = c("<All>", unique(df$QC_Mnemonic)))
        return(df)
    })
    
    summary.data <- reactive({
        group_by_cols <- c("Test", "Analyzer", "QC_Mnemonic")
        
        if(input$timeframe ==1){
            group_by_cols <- c("Day", group_by_cols)
        } else if(input$timeframe ==2){
            group_by_cols <- c("Week", group_by_cols)
        } else {
            group_by_cols <- c("Month", group_by_cols)
        }
        
        p4 <- filtered.data() %>%  
            mutate(Date =  ymd_hms(Date),
                   Day = floor_date(Date, unit = "days"),
                   Week = floor_date(Date, unit = "week"),
                   Month = floor_date(Date, unit = "month")) %>%
            group_by_at(vars(one_of(group_by_cols))) %>%
            summarize(N = length(Result),
                      Mean = round(mean(Result), 2),
                      SD = round(sd(Result), 3),
                      "%CV" = round(100*SD/Mean, 2))
        
        return(p4)
    })
    
    
    ############################################################################
    # End of filter application. At this point, all raw data has been processed 
    # according to the filters applied
    ############################################################################
    
    #Generates the output table wich is displayed in the Summary Tab
    
    output$sumtable <- renderDT(
        datatable(summary.data(), rownames = FALSE)
    )
    
    ############################################################################
    # End of "Summary" tab data.
    ############################################################################
    
    output$chart <- renderPlotly({
        plot.data <- summary.data() %>% ungroup()
        
        validate(
            need(input$qcproduct != "<All>", "Please Select QC Product")
        )
        
        if(input$timeframe == 1){
            p <- plot_ly(data = plot.data, x = ~Day, y = ~Mean,
                         type = 'scatter', mode = 'lines+markers',
                         error_y = ~list(array = SD,
                                         color = '#000000')
            )
        } else if (input$timeframe == 2) {
            p <- plot_ly(data = plot.data, x = ~Week, y = ~Mean,
                         type = 'scatter', mode = 'lines+markers',
                         error_y = ~list(array = SD,
                                         color = '#000000')
            )
        } else if (input$timeframe == 3) {
            p <- plot_ly(data = plot.data, x = ~Month, y = ~Mean,
                         type = 'scatter', mode = 'lines+markers',
                         error_y = ~list(array = SD,
                                         color = '#000000')
            )
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)