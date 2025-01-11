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
    testSplitWeek <- reactive(modPlotInputs$testSplitWeek())
    xVar <- reactive(modPlotInputs$xVar())
    yVar <- reactive(modPlotInputs$yVar())
    colorVar <- reactive(modPlotInputs$colorVar())
    facetVar <- reactive(modPlotInputs$facetVar())
    fitLine <- reactive(modPlotInputs$fitLine())
    fitLineType <- reactive(modPlotInputs$fitLineType())
    fitLineSE <- reactive(modPlotInputs$fitLineSE())
    #corType <- reactive(modPlotInputs$corType())
    
    modPlotData <- reactive({
      req(seasons(), gameType(), teams(), testSplitWeek())
      data <- modData |>
        filter(season %in% seasons()[1]:seasons()[2],
               season_type %in% gameType(),
               home_team %in% teams() | away_team %in% teams()) |>
        #team %in% teams()) |>
        filter(!is.na(result)) |>
        mutate(
          split = ifelse(season == seasons()[2] & week >= testSplitWeek(), "Test", "Train"),
          .after = week
        ) |>
        mutate(
          season_fac = factor(season),
          week_fac = factor(week),
          split = factor(split, levels = c("Train", "Test")),
          .after = week
        ) 
      
      if(statType() == "Team"){
        data <- data |>
          clean_homeaway(invert = c("result", "spread_line")) |>
          select(any_of(c(
            "season",
            "season_type",
            "week",
            "split",
            "team",
            "opponent",
            xVar(),
            yVar(),
            colorVar(),
            facetVar()
          ))) 
        # mutate(
        #   across(where(is.numeric),
        #          ~round(.x, 2))
        # )
      }else{
        data <- data |>
          select(any_of(c(
            "season",
            "season_type",
            "week",
            "split",
            "home_team",
            "away_team",
            xVar(),
            yVar(),
            colorVar(),
            facetVar()
          ))) 
        # mutate(
        #   across(where(is.numeric),
        #          ~round(.x, 2))
        # )
      }
      
      # Round numeric columns
      data <- data |> 
        mutate(across(where(is.numeric), ~ round(.x, 2)))
      
      data
      
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
    
    # output$modDataPlot <- renderPlotly({
    #   validate(
    #     need(xVar(), "Please select x variable to plot"),
    #     need(yVar(), "Please select y variable to plot")
    #   )
    #   
    #   data <- modPlotData()
    #   
    #   if(!is.null(colorVar())){
    #     if(colorVar() %in% c("team", "opponent", "home_team", "away_team")){
    #       plot <- ggplot(data = modPlotData(), 
    #                      aes(x = !!sym(xVar()), 
    #                          y = !!sym(yVar()),
    #                          color = !!sym(colorVar()),
    #                          group = !!sym(colorVar()))
    #       ) +
    #         geom_point() +
    #         scale_color_nfl(type = "primary", name = colorVar(), guide = "legend")
    #       #geom_nfl_logos(aes(team_abbr = !!sym(colorVar())), width = 0.01)
    #     }else{
    #       plot <- ggplot(data = modPlotData(), 
    #                      aes(x = !!sym(xVar()), 
    #                          y = !!sym(yVar()),
    #                          color = !!sym(colorVar()),
    #                          group = !!sym(colorVar()))
    #       ) +
    #         geom_point()
    #     }
    #   }else{
    #     plot <- ggplot(data = modPlotData(), 
    #                    aes(x = !!sym(xVar()), 
    #                        y = !!sym(yVar()))
    #                    ) +
    #       geom_point()
    #   }
    #   
    #   if(!is.null(facetVar())){
    #     plot2 <- plot + facet_wrap(vars(!!sym(facetVar())))
    #   }else{
    #     plot2 <- plot
    #   }
    #   
    #   if(fitLine()){
    #     if(!is.null(colorVar())){
    #       plot3 <- plot2 + 
    #         geom_smooth(aes(fill = !!sym(colorVar())),
    #                     method = fitLineType(), se = fitLineSE(), alpha = 0.3,
    #                     show.legend = FALSE) +
    #         guides(fill = "none")
    #     }else{
    #       plot3 <- plot2 + 
    #         geom_smooth(method = fitLineType(), se = fitLineSE(), alpha = 0.3,
    #                     show.legend = FALSE) +
    #         guides(fill = "none")
    #     }
    #   }else{
    #     plot3 <- plot2
    #   }
    #   
    #   finalPlot <- plot3 + theme_bw()
    #   
    #   # Convert ggplot to plotly and add dynamic correlation label as annotation
    #   # ggplotly(finalPlot, tooltip = c("x", "y", "colour")) %>%
    #   #   layout(annotations = list(
    #   #     text = cor_label,
    #   #     x = 0.05,  # Adjust annotation position as needed
    #   #     y = 0.95,
    #   #     xref = "paper",
    #   #     yref = "paper",
    #   #     showarrow = FALSE
    #   #   ))
    #   if(!is.null(colorVar())){
    #     finalPlotly <- ggplotly(finalPlot, tooltip = c("y", "x", "color"))
    #   }else{
    #     finalPlotly <- ggplotly(finalPlot, tooltip = c("y", "x"))
    #   }
    #   return(finalPlotly)
    # })
    
    output$modDataPlot <- renderPlotly({
      # Ensure xVar and yVar exist and are not empty
      validate(
        need(xVar(), "Please select x variable to plot"),
        need(yVar(), "Please select y variable to plot")
      )
      
      data <- modPlotData()
      
      # 1. Check that xVar(), yVar(), colorVar(), facetVar() are in the dataset
      validate(
        need(xVar() %in% names(data), 
             paste("Column", xVar(), "not found in data")),
        need(yVar() %in% names(data), 
             paste("Column", yVar(), "not found in data"))
      )
      
      # Basic aes
      plt <- ggplot(data, aes(
        x = .data[[ xVar() ]],
        y = .data[[ yVar() ]]
      )) + geom_point()
      
      # 2. If colorVar() is valid, add color mapping
      if (!is.null(colorVar()) && colorVar() %in% names(data)) {
        if(colorVar() %in% c("team", "opponent", "home_team", "away_team")){
          plt <- plt + aes(color = .data[[ colorVar() ]]) +
            scale_color_nfl(type = "primary", name = colorVar(), guide = "legend")
        }else{
          plt <- plt + aes(color = .data[[ colorVar() ]])
        }
      }
      
      
      # 3. If facetVar() is valid, facet
      if (!is.null(facetVar()) && facetVar() %in% names(data)) {
        plt <- plt + facet_wrap(vars(.data[[ facetVar() ]]))
      }
      
      # 4. Fit line if requested
      if (fitLine()) {
        # If colorVar is valid, add 'fill' the same
        if (!is.null(colorVar()) && colorVar() %in% names(data)) {
          plt <- plt + geom_smooth(
            aes(fill = .data[[ colorVar() ]]),
            method = fitLineType(),
            se = fitLineSE(),
            alpha = 0.3,
            show.legend = FALSE
          )
        } else {
          plt <- plt + geom_smooth(
            method = fitLineType(),
            se = fitLineSE(),
            alpha = 0.3,
            show.legend = FALSE
          )
        }
      }
      
      plt <- plt + theme_bw()
      
      # Convert to plotly
      # If colorVar is used, provide it in the tooltip
      if (!is.null(colorVar()) && colorVar() %in% names(data)) {
        ggplotly(plt, tooltip = c("x", "y", "color"))
      } else {
        ggplotly(plt, tooltip = c("x", "y"))
      }
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

