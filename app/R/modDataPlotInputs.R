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
      label = "Table Statistic",
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
      status = "info"
    )
  )
}


# Server Module ----
modDataPlotInputServer <- function(id,
                                   teamsData,
                                   modData){
  
  xVarOptions <- modData |> select(c(-contains("id"))) |> colnames()
  moduleServer(id, function(input, output, session){
    output$xVarUI <- renderUI({
      pickerInput(
        inputId = NS(id, "xVar"),
        label = "X variable", 
        choices = xVarOptions,
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
          maxOptions = 1
        )
      )
    })
    
    yVarOptions <- modData |> select(c(-contains("id"))) |> colnames()
    output$yVarUI <- renderUI({
      pickerInput(
        inputId = NS(id, "yVar"),
        label = "Y variable", 
        choices = yVarOptions,
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
          maxOptions = 1
        )
      )
    })
    
    colorVarOptions <- modData |> select(c(-contains("id"))) |> colnames()
    output$colorVarUI <- renderUI({
      pickerInput(
        inputId = NS(id, "colorVar"),
        label = "Color by:", 
        choices = colorVarOptions,
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
          maxOptions = 1
        )
      )
    })
    
    facetVarOptions <- modData |> select(c(season, week, where(is.character), -contains("id"))) |> colnames()
    output$facetVarUI <- renderUI({
      pickerInput(
        inputId = NS(id, "facetVar"),
        label = "Facet by:", 
        choices = facetVarOptions,
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
          maxOptions = 1
        )
      )
    })
    
    list(
      seasons = reactive(input$seasons),
      gameType = reactive(input$gameType),
      teams = reactive(input$teams),
      statType = reactive(input$statType),
      xVar = reactive(input$xVar),
      yVar = reactive(input$yVar),
      colorVar = reactive(input$colorVar),
      facetVar = reactive(input$facetVar),
      fitLine = reactive(input$fitLine)
    )
  })
}


