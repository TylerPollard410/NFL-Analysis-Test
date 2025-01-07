## Create Output table for Player Offense Passing

# UI module ----

modDataPlotInputUI <- function(id, teamsDataPickerInput){
  tagList(
    noUiSliderInput(
      inputId = NS(id, "seasons"),
      label = "Select seasons",
      min = 2006,
      max = get_current_season(),
      step = 1,
      value = c(get_current_season(),get_current_season()),
      limit = 5,
      behaviour = "drag",
      format = wNumbFormat(decimals = 0)
    ),
    prettyCheckboxGroup(
      inputId = NS(id, "gameType"),
      label = "Game Type", 
      choices = c("Regular Season" = "REG",
                  "Playoffs" = "POST"),
      selected = c("REG", "POST"),
      inline = FALSE, 
      status = "info",
      fill = TRUE
    ),
    virtualSelectInput(
      inputId = NS(id, "teams"),
      label = "Select team to analyze", 
      choices = prepare_choices(
        .data = teamsDataPickerInput,
        label = team_name,
        value = team_abbr,
        group_by = team_division
      ),
      multiple = TRUE,
      selected = teamsDataPickerInput$team_abbr,
      showSelectedOptionsFirst = TRUE
    ),
    radioGroupButtons(
      inputId = NS(id, "statType"),
      label = "Data Level Type",
      choices = c("Team", "Game"),
      status = "info"
    ),
    uiOutput(outputId = NS(id, "testSplitWeekUI")),
    br(),
    hr(),
    uiOutput(outputId = NS(id, "xVarUI")),
    uiOutput(outputId = NS(id, "yVarUI")),
    uiOutput(outputId = NS(id, "colorVarUI")),
    uiOutput(outputId = NS(id, "facetVarUI")),
    materialSwitch(
      inputId = NS(id, "fitLine"),
      label = "Fit line?",
      value = FALSE,
      status = "info", 
      inline = TRUE
    ),
    conditionalPanel(condition = "input.fitLine",
                     ns = NS(id),
                     tagList(
                       radioGroupButtons(
                         inputId = NS(id, "fitLineType"),
                         label = "Smoothing method",
                         choices = c("auto", "lm", "glm", "gam", "loess"),
                         selected = "auto",
                         status = "info"
                       ),
                       materialSwitch(
                         inputId = NS(id, "fitLineSE"),
                         label = "Show standard error?",
                         value = FALSE,
                         status = "info", 
                         inline = TRUE
                       )
                     )
    )
    # conditionalPanel(condition = "input.fitLine && input.fitLineType == 'lm'",
    #                  ns = NS(id),
    #                  tagList(
    #                    radioGroupButtons(
    #                      inputId = NS(id, "corType"),
    #                      label = "Correlation Type",
    #                      choices = c("pearson", "kendall", "spearman"),
    #                      status = "info"
    #                    )
    #                  )
    # )
  )
}


# Server Module ----
modDataPlotInputServer <- function(id,
                                   teamsData,
                                   modData){
  
  
  moduleServer(id, function(input, output, session){
    
    output$testSplitWeekUI <- renderUI({
      fullData <- modData |> filter(!is.na(result))
      testSeason <- input$seasons[2]
      weekOptions <- fullData |> 
        filter(season == testSeason) |> 
        pull(week) |>
        unique() |>
        sort()
      sliderInput(
        inputId = NS(id, "testSplitWeek"),
        label = "First week of test data",
        min = head(weekOptions, 1),
        max = tail(weekOptions, 1),
        value = head(weekOptions, 1),
        step = 1
      )
    })
    
    modPlotData <- reactive({
      req(input$testSplitWeek)
      data <- modData |>
        filter(!is.na(result)) |>
        mutate(
          split = ifelse(season == input$seasons[2] & week >= input$testSplitWeek, "Test", "Train"),
          .after = week
        )
      
      if(input$statType == "Team"){
        data |>
          clean_homeaway(invert = c("result", "spread_line"))
      }else{
        data
      }
    })
    
    # modPlotData2 <- reactive({
    #   #req(input$seasons, input$testSplitWeek)
    #   modPlotData() |>
    #     mutate(
    #       split = ifelse(season == input$seasons[2] & week >= input$testSplitWeek, "Test", "Train"),
    #       .after = week
    #     )
    # })
    # 
    output$xVarUI <- renderUI({
      xVarOptions <- modPlotData() |> select(c(-contains("id"))) |> colnames()
      
      # 1. Grab old selection
      oldSelection <- isolate(input$xVar)
      # 2. Keep only the old selection that is still valid
      validSelection <- intersect(oldSelection, xVarOptions)
      
      pickerInput(
        inputId = NS(id, "xVar"),
        label = "X variable", 
        choices = xVarOptions,
        selected = validSelection,
        multiple = TRUE,
        options = pickerOptions(
          maxOptions = 1,
          dropupAuto = FALSE,
          header = TRUE,
          liveSearch = TRUE
        )
      )
    })
    
    output$yVarUI <- renderUI({
      yVarOptions <- modPlotData() |> select(c(-contains("id"))) |> colnames()
      
      oldSelection <- isolate(input$yVar)
      validSelection <- intersect(oldSelection, yVarOptions)
      
      pickerInput(
        inputId = NS(id, "yVar"),
        label = "Y variable", 
        choices = yVarOptions,
        selected = validSelection,
        multiple = TRUE,
        options = pickerOptions(
          maxOptions = 1,
          dropupAuto = FALSE,
          header = TRUE,
          liveSearch = TRUE
        )
      )
    })
    
    output$colorVarUI <- renderUI({
      colorVarOptions <- modPlotData() |> select(c(-contains("id"))) |> colnames()
      
      oldSelection <- isolate(input$colorVar)
      validSelection <- intersect(oldSelection, colorVarOptions)
      
      pickerInput(
        inputId = NS(id, "colorVar"),
        label = "Color by:", 
        choices = colorVarOptions,
        selected = validSelection,
        multiple = TRUE,
        options = pickerOptions(
          maxOptions = 1,
          dropupAuto = FALSE,
          header = TRUE,
          liveSearch = TRUE
        )
      )
    })
    
    output$facetVarUI <- renderUI({
      facetVarOptions <- modPlotData() |> 
        select(
          c(season, week, split, where(is.character), 
            -contains("id"))) |> 
        colnames()
      
      oldSelection <- isolate(input$facetVar)
      validSelection <- intersect(oldSelection, facetVarOptions)
      
      pickerInput(
        inputId = NS(id, "facetVar"),
        label = "Facet by:", 
        choices = facetVarOptions,
        selected = validSelection,
        multiple = TRUE,
        options = pickerOptions(
          maxOptions = 1,
          dropupAuto = FALSE,
          header = TRUE,
          liveSearch = TRUE
        )
      )
    })
    
    # output$corTypeUI <- renderUI({
    #   conditionalPanel(condition = "input.fitLine",
    #                    ns = NS(id),
    #                    radioGroupButtons(
    #                      inputId = NS(id, "corType"),
    #                      label = "Correlation Type",
    #                      choices = c("pearson", "kendall", "spearman"),
    #                      status = "info"
    #                    ))
    # })
    
    list(
      seasons = reactive(input$seasons),
      gameType = reactive(input$gameType),
      teams = reactive(input$teams),
      statType = reactive(input$statType),
      testSplitWeek = reactive(input$testSplitWeek),
      xVar = reactive(input$xVar),
      yVar = reactive(input$yVar),
      colorVar = reactive(input$colorVar),
      facetVar = reactive(input$facetVar),
      fitLine = reactive(input$fitLine),
      fitLineType = reactive(input$fitLineType),
      fitLineSE = reactive(input$fitLineSE)
      #corType = reactive(input$corType)
    )
  })
}


