## Create Standings Playoffs Tables ----

# UI ----
standingsPlayoffsTableOutput <- function(id){
  withSpinner(
    uiOutput(NS(id, "standingsPlayoffsTableUI")),
    type = 8
  )
}


# Server ----
standingsPlayoffsTableServer <- function(id,
                                         standingsSeason,
                                         teamsData,
                                         standingsTableData,
                                         conference){
  moduleServer(id, function(input, output, session){
    
    output$standingsPlayoffsTable <- renderReactable({
      # Prepare the data
      tableData <- standingsTableData() |> 
        filter(team_conf == conference) |> 
        select(
          seed,
          #team,
          team_logo_espn,
          team_name,
          GP,W, L, T,
          `W-L%`, `CON%`, `DIV%`
        ) |> 
        arrange(seed, desc(`W-L%`), desc(`CON%`), desc(`DIV%`))
      
      # Render reactable
      reactable(
        tableData,
        theme = espn(
          centered = TRUE,
          header_font_size = 14,
          font_size = 14
        ),
        highlight = TRUE,
        compact = TRUE,
        pagination = FALSE,
        wrap = FALSE,
        outlined = FALSE,
        sortable = FALSE,
        showSortable = FALSE,
        fullWidth = TRUE,
        #defaultSorted = "seed",
        columns = list(
          seed = colDef(
            name = "Seed", 
            align = "center", 
            minWidth = 50,
            sticky = "left"
            ),
          team_logo_espn = colDef(
            name = "", 
            minWidth = 30,
            sticky = "left",
            cell = embed_img(height = "25px")
          ),
          team_name = colDef(
            name = "Team",
            minWidth = 150, 
            style = list(borderRight = "1px solid black")
          ),
          GP = colDef(
            name = "GP",
            align = "center", 
            minWidth = 30,
            style = list(borderRight = "1px solid #d3d3d3")
            ),
          W = colDef(
            name = "W",
            align = "center", 
            minWidth = 30
            ),
          L = colDef(
            name = "L", 
            align = "center", 
            minWidth = 30
            ),
          T = colDef(
            name = "T",
            align = "center", 
            minWidth = 30,
            style = list(borderRight = "1px solid #d3d3d3")
            ),
          `W-L%` = colDef(
            name = "W-L%",
            format = colFormat(percent = TRUE, digits = 1),
            align = "center",
            minWidth = 50,
            style = list(borderRight = "1px solid #d3d3d3")
          ),
          `CON%` = colDef(
            name = "CON%",
            format = colFormat(percent = TRUE, digits = 1),
            align = "center",
            minWidth = 50,
            style = list(borderRight = "1px solid #d3d3d3")
          ),
          `DIV%` = colDef(
            name = "DIV%",
            format = colFormat(percent = TRUE, digits = 1),
            align = "center",
            minWidth = 50
          )
        )
      )
    })
    
    # Surrounding box UI
    output$standingsPlayoffsTableUI <- renderUI({
      conf_logo <- teamsData |> 
        filter(team_conf == conference) |> 
        pull(team_conference_logo) |> 
        unique()
      
      box(
        title = div(
          style = "display: flex; align-items: center;",
          img(src = conf_logo, style = "height: 25px;"),
          strong(standingsSeason(), style = "margin-left: 6px; font-size: 25px;"),
          strong("Playoff Standings", style = "margin-left: 4px; font-size: 25px;")
        ),
        width = 12,
        status = "primary",
        reactableOutput(NS(id, "standingsPlayoffsTable"))
      )
    })
  }) # end module Server
} # end standingsTableServer

# output$standingsPlayoffsTableUI <- renderUI({
#   conf_logo <- teamsData |> 
#     filter(team_conf == conference) |>
#     pull(team_conference_logo) |>
#     unique()
#   
#   box(
#     title = div(style = "display: flex; align-items: center;",
#                 img(src = conf_logo, style = "height: 25px;"),
#                 strong(standingsSeason(), style = "margin-left: 6px; font-size: 25px"),
#                 strong("Playoff Standings", style = "margin-left: 4px; font-size: 25px")
#     ),
#     width = 12,
#     status = "primary",
#     #withSpinner(
#     gt_output(NS(id, "tablePlaceholder"))#, type = 8
#     #)
#   )
# })
# 
# output$tablePlaceholder <- renderUI({
#   withSpinner(
#     gt_output(NS(id, "standingsPlayoffsTable")), type = 8
#   )
# })
# 
# output$standingsPlayoffsTable <- render_gt({
#   standingsSeason <- standingsSeason()
#   
#   standingsTableData() |>
#     filter(team_conf == conference) |>
#     select(
#       "seed",
#       "team",
#       "team_name",
#       "GP",
#       "W",
#       "L",
#       "T",
#       "W-L%",
#       "CON%",
#       "DIV%"
#     ) |>
#     arrange(seed, desc(`W-L%`), desc(`CON%`), desc(`DIV%`)) |>
#     gt() |>
#     sub_missing(
#       columns = "seed"
#     ) |>
#     gt_nfl_logos(
#       columns = "team",
#       height = "25px"
#     ) |>
#     fmt_percent(
#       columns = c("W-L%", "CON%", "DIV%"),
#       decimals = 1
#     ) |>
#     # tab_header(
#     # title = div(style = "display: flex; align-items: center;",
#     #             img(src = conf_logo, style = "height: 25px;"),
#     #             strong(standingsSeason, style = "margin-left: 6px"),
#     #             strong("Playoff Standings", style = "margin-left: 4px")
#     # )
#     # ) |>
#     tab_options(
#       data_row.padding = 0,
#       column_labels.font.weight = "bold",
#       #heading.title.font.size = "150%",
#       table.font.size = "90%"
#     ) |>
#     tab_style(
#       style = cell_borders(sides = "right"),
#       locations = cells_body(
#         columns = c("team_name", "T")
#       )
#     ) |>
#     tab_style(
#       style = cell_borders(sides = "right", weight = "0.5px"),
#       locations = cells_body(
#         columns = c("GP")
#       )
#     ) |>
#     tab_style(
#       style = cell_borders(sides = "bottom", weight = "2px"),
#       locations = cells_body(
#         rows =  7
#       )
#     ) |>
#     cols_label(
#       seed = "Seed",
#       team = "",
#       team_name = "Team"
#     )
# }) # end renderGT
