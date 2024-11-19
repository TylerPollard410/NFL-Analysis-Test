## Create Standings Tables ----

# UI ----
standingsTableOutput <- function(id){
  uiOutput(NS(id, "standingsTableUI"))
}


# Server ----
standingsTableServer <- function(id,
                                 standingsSeason,
                                 teamsData,
                                 standingsTableData,
                                 conference){
  moduleServer(id, function(input, output, session){
    
    conf_logo <- reactive({
      teamsData |> 
        filter(team_conf == conference) |>
        pull(team_conference_logo) |>
        unique()
    })
    
    output$standingsTable <- render_gt({
      standingsSeason <- standingsSeason()
      
      standingsTableData() |>
        filter(team_conf == conference) |>
        select(
          "team",
          "team_name",
          "team_division",
          "div_rank",
          "GP",
          "W",
          "L",
          "T",
          "W-L%",
          "PF",
          "PA",
          "PD",
          "MOV",
          "SOS",
          "SRS",
          "OSRS",
          "DSRS"
        ) |>
        group_by(team_division) |>
        arrange(team_division, div_rank) |>
        gt() |>
        cols_hide(
          columns = "div_rank"
        ) |>
        gt_nfl_logos(
          columns = "team",
          height = "25px"
        ) |>
        fmt_percent(
          columns = "W-L%",
          decimals = 1
        ) |>
        fmt_number(
          columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS"),
          decimals = 2
        ) |>
        data_color(
          columns = c("SRS"),
          method = "numeric",
          palette = c("red", "green")
        ) |>
        # tab_header(
        #   title = div(style = "display: flex; align-items: center;",
        #               img(src = conf_logo(), style = "height: 25px;"),
        #               strong(standingsSeason, style = "margin-left: 6px"),
        #               strong("Standings", style = "margin-left: 4px")
        #   )
        # ) |>
        tab_options(
          data_row.padding = 0,
          column_labels.font.weight = "bold",
          #heading.title.font.size = "150%",
          table.font.size = "90%"
        ) |>
        tab_style(
          style = cell_borders(sides = "right"),
          locations = cells_body(
            columns = c("team_name", "W-L%", "PD")
          )
        ) |>
        tab_style(
          style = cell_borders(sides = "right", weight = "0.5px"),
          locations = cells_body(
            columns = c("GP")
          )
        ) |>
        cols_label(
          team = "",
          team_name = "Team"
        )
    }) # end renderGT
    
    output$standingsTableUI <- renderUI({
      box(
        title = div(style = "display: flex; align-items: center;",
                    img(src = conf_logo(), style = "height: 25px;"),
                    strong(standingsSeason(), style = "margin-left: 6px; font-size: 25px"),
                    strong("Standings", style = "margin-left: 4px; font-size: 25px")
        ),
        width = 12,
        maximizable = TRUE,
        withSpinner(
          gt_output(NS(id, "standingsTable")), type = 8
        )
      )
    })
  }) # end module Server
} # end standingsTableServer