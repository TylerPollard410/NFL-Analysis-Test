## Create Output table for Player Offense Passing

# UI module ----

modDataPlotOutput <- function(id){
  tagList(
    #verbatimTextOutput(outputId = NS(id, "modDataPrint")),
    withSpinner(
      reactableOutput(NS(id, "modDataTable")), type = 8
    ),
    fluidRow(
      sliderInput(
        inputId = NS(id, "plotWidth"),
        label = "Plot Width",
        min = 600, max = 1200, value = 1000,
        sep = "", step = 10
      ),
      column(width = 1),
      sliderInput(
        inputId = NS(id, "plotHeight"),
        label = "Plot Height",
        min = 400, max = 1500, value = 600,
        sep = "", step = 10
      )
    ),
    fluidRow(
      uiOutput(outputId = NS(id, "modDataPlotUI"))
    )
  )
}


# Server Module ----
modDataPlotServer <- function(id,
                              teamsData,
                              modData,
                              modPlotInputs){
  moduleServer(id, function(input, output, session){
    seasons <- reactive(modPlotInputs$seasons())
    gameType <- reactive(modPlotInputs$gameType())
    teams <- reactive(modPlotInputs$teams())
    statType <- reactive(modPlotInputs$statType())
    xVar <- reactive(modPlotInputs$xVar())
    yVar <- reactive(modPlotInputs$yVar())
    colorVar <- reactive(modPlotInputs$colorVar())
    facetVar <- reactive(modPlotInputs$facetVar())
    fitLine <- reactive(modPlotInputs$fitLine())
    fitLineType <- reactive(modPlotInputs$fitLineType())
    fitLineSE <- reactive(modPlotInputs$fitLineSE())
    #corType <- reactive(modPlotInputs$corType())
    
    modPlotData <- reactive({
      data <- modData |>
        filter(season %in% seasons()[1]:seasons()[2],
               season_type %in% gameType(),
               home_team %in% teams() | away_team %in% teams()) |>
        #team %in% teams()) |>
        filter(!is.na(result)) |>
        mutate(
          season = factor(season),
          week = factor(week)
        ) 
      
      if(statType() == "Team"){
        data |>
          clean_homeaway(invert = c("result", "spread_line")) |>
          select(
            season,
            season_type,
            week,
            team,
            opponent,
            xVar(),
            yVar(),
            colorVar(),
            facetVar()
          ) |>
          mutate(
            across(where(is.numeric),
                   ~round(.x, 2))
          )
      }else{
        data |>
          select(
            season,
            season_type,
            week,
            home_team,
            away_team,
            xVar(),
            yVar(),
            colorVar(),
            facetVar()
          ) |>
          mutate(
            across(where(is.numeric),
                   ~round(.x, 2))
          )
      }
    })
    
    # output$modDataPrint <- renderPrint({
    #   modPlotData()
    # })
    output$modDataTable <- renderReactable({
      tableData <- modPlotData()
      reactable(
        data = tableData,
        theme = espn(),
        highlight = TRUE,
        compact = TRUE,
        pagination = TRUE,
        wrap = FALSE,
        outlined = TRUE,
        showSortable = FALSE
      )
    })
    
    output$modDataPlot <- renderPlotly({
      validate(
        need(xVar(), "Please select x variable to plot"),
        need(yVar(), "Please select y variable to plot")
      )
      
      req(modPlotData())
      
      if(!is.null(colorVar())){
        if(colorVar() %in% c("team", "opponent", "home_team", "away_team")){
          plot <- ggplot(data = modPlotData(), 
                         aes(x = !!sym(xVar()), 
                             y = !!sym(yVar()),
                             color = !!sym(colorVar()),
                             group = !!sym(colorVar()))
          ) +
            geom_point() +
            scale_color_nfl(type = "primary", name = colorVar(), guide = "legend")
          #geom_nfl_logos(aes(team_abbr = !!sym(colorVar())), width = 0.01)
        }else{
          plot <- ggplot(data = modPlotData(), 
                         aes(x = !!sym(xVar()), 
                             y = !!sym(yVar()),
                             color = !!sym(colorVar()),
                             group = !!sym(colorVar()))
          ) +
            geom_point()
        }
      }else{
        plot <- ggplot(data = modPlotData(), 
                       aes(x = !!sym(xVar()), 
                           y = !!sym(yVar()))
                       ) +
          geom_point()
      }
      
      if(!is.null(facetVar())){
        plot2 <- plot + facet_wrap(vars(!!sym(facetVar())))
      }else{
        plot2 <- plot
      }
      
      if(fitLine()){
        if(!is.null(colorVar())){
          plot3 <- plot2 + 
            geom_smooth(aes(fill = !!sym(colorVar())),
                        method = fitLineType(), se = fitLineSE(), alpha = 0.3,
                        show.legend = FALSE) +
            guides(fill = "none")
        }else{
          plot3 <- plot2 + 
            geom_smooth(method = fitLineType(), se = fitLineSE(), alpha = 0.3,
                        show.legend = FALSE) +
            guides(fill = "none")
        }
      }else{
        plot3 <- plot2
      }
      
      finalPlot <- plot3 + theme_bw()
      
      # Convert ggplot to plotly and add dynamic correlation label as annotation
      # ggplotly(finalPlot, tooltip = c("x", "y", "colour")) %>%
      #   layout(annotations = list(
      #     text = cor_label,
      #     x = 0.05,  # Adjust annotation position as needed
      #     y = 0.95,
      #     xref = "paper",
      #     yref = "paper",
      #     showarrow = FALSE
      #   ))
      if(!is.null(colorVar())){
        finalPlotly <- ggplotly(finalPlot, tooltip = c("y", "x", "color"))
      }else{
        finalPlotly <- ggplotly(finalPlot, tooltip = c("y", "x"))
      }
      return(finalPlotly)
    })
    
    plotWidth <- reactive({input$plotWidth})
    plotHeight <- reactive({input$plotHeight})
    output$modDataPlotUI <- renderUI({
      #req(modPlotData(), input$plotWidth, input$plotHeight)
      plotlyOutput(
        outputId = NS(id, "modDataPlot"),
        width = plotWidth(),
        height = plotHeight()
      )
    })
  })
}

