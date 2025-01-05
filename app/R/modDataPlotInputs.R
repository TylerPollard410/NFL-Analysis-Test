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
      selected = "REG",
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
    
    modPlotData <- reactive({
      # data <- modData |>
      #   filter(season %in% seasons()[1]:seasons()[2],
      #          season_type %in% gameType(),
      #          home_team %in% teams() | away_team %in% teams()) |>
      #   #team %in% teams()) |>
      #   filter(!is.na(result)) |>
      #   mutate(
      #     season = factor(season),
      #     week = factor(week)
      #   ) |>
      #   select(
      #     season,
      #     season_type,
      #     week,
      #     home_team,
      #     away_team,
      #     xVar(),
      #     yVar(),
      #     colorVar(),
      #     facetVar()
      #   ) |>
      #   mutate(
      #     across(where(is.numeric),
      #            ~round(.x, 2))
      #   )
      
      if(input$statType == "Team"){
        modData |>
          clean_homeaway(invert = c("result", "spread_line"))
      }else{
        modData
      }
    })
    
    output$xVarUI <- renderUI({
      xVarOptions <- modPlotData() |> select(c(-contains("id"))) |> colnames()
      pickerInput(
        inputId = NS(id, "xVar"),
        label = "X variable", 
        choices = xVarOptions,
        selected = NULL,
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
      pickerInput(
        inputId = NS(id, "yVar"),
        label = "Y variable", 
        choices = yVarOptions,
        selected = NULL,
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
      pickerInput(
        inputId = NS(id, "colorVar"),
        label = "Color by:", 
        choices = colorVarOptions,
        selected = NULL,
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
          c(season, week, where(is.character), 
            -contains("id"))) |> 
        colnames()
      pickerInput(
        inputId = NS(id, "facetVar"),
        label = "Facet by:", 
        choices = facetVarOptions,
        selected = NULL,
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


