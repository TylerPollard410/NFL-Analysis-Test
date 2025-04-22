## Create Standings Tables ----

# UI ----
teamRankingsOverviewUI <- function(id){
  # tagList(
  #   withSpinner(
  #     reactableOutput(NS(id, "teamRankingOverviewTable")), type = 8
  #   )
  # )
}


# Server ----
teamRankingsOverviewServer <- function(id,
                                      rankingsSeason,
                                      data,
                                      team_data){
  moduleServer(id, function(input, output, session){

    # output$teamRankingOverviewTable <- renderReactable({
    # 
    #   overviewData <- data() |>
    #     slice_tail(n = 1, by = team)
    #   filter(team_conf == conference) |>
    #     select(
    #       "team_division",
    #       "team_logo_espn",
    #       "team_name",
    #       "div_rank",
    #       "GP",
    #       "W",
    #       "L",
    #       "T",
    #       "W-L%",
    #       "PF",
    #       "team_PPG",
    #       "PA",
    #       "opp_PPG",
    #       "PD",
    #       "MOV",
    #       "SOS",
    #       "SRS",
    #       "OSRS",
    #       "DSRS"
    #     ) |>
    #     collect() |>
    #     group_by(team_division) |>
    #     arrange(team_division, div_rank) |>
    #     ungroup() |>
    #     select(-div_rank)
    # 
    #   ## Reactable ----
    #   ## Total ----
    #   if(Stat == "Total"){
    #     standingsTableReact <- reactable(
    #       data = standingsTableDataReact |> select(-c(team_PPG, opp_PPG)),
    #       theme = espn(
    #         centered = TRUE,
    #         header_font_size = 14,
    #         font_size = 14
    #       ),
    #       highlight = TRUE,
    #       compact = TRUE,
    #       pagination = FALSE,
    #       wrap = FALSE,
    #       outlined = TRUE,
    #       sortable = FALSE,
    #       showSortable = FALSE,
    #       fullWidth = TRUE,
    #       defaultSorted = "team_division",
    #       rowStyle = group_border_sort(columns = "team_division",
    #                                    border_color = "black",
    #                                    border_width = "1.5px",
    #                                    border_style = "solid"),
    #       defaultColDef = colDef(vAlign = "center",
    #                              minWidth = 50
    #                              #headerStyle = list(fontSize = "14px")
    #       ),
    #       columnGroups = list(
    #         colGroup(name = "Record",
    #                  columns = c("GP", "W", "L", "T", "W-L%")),
    #         colGroup(name = "Points",
    #                  columns = c("PF", "PA", "PD")),
    #         colGroup(name = "Performance",
    #                  columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS"))
    #       ),
    #       columns = list(
    #         ### Team Division ----
    #         team_division = colDef(
    #           name = "",
    #           minWidth = 80,
    #           style = group_merge_sort("team_division"),
    #           #style = cell_style(font_color = "red")
    #         ),
    #         ### Team Logo ----
    #         team_logo_espn = colDef(
    #           name = "",
    #           sticky = "left",
    #           maxWidth = 25,
    #           style = background_img()
    #         ),
    #         ### Team Name ----
    #         team_name = colDef(
    #           name = "Team",
    #           minWidth = 175,
    #           style = list(borderRight = "1px solid black")
    #         ),
    #         ### Games Played ----
    #         GP = colDef(
    #           name = "GP",
    #           minWidth = 30,
    #           align = "center",
    #           style = list(borderRight = "1px solid #d3d3d3")
    #         ),
    #         ### Win ----
    #         W = colDef(
    #           minWidth = 30
    #         ),
    #         ### Loss ----
    #         L = colDef(
    #           minWidth = 30
    #         ),
    #         ### Tie ----
    #         `T` = colDef(
    #           minWidth = 30
    #         ),
    #         ### Win Loss Perc ----
    #         `W-L%` = colDef(
    #           minWidth = 75,
    #           format = colFormat(percent = TRUE, digits = 2),
    #           align = "center",
    #           style = list(borderRight = "1px solid #d3d3d3")
    #         ),
    #         ### PF ----
    #         ### PA ----
    #         ### PD ----
    #         PD = colDef(
    #           align = "center",
    #           style = list(borderRight = "1px solid #d3d3d3")
    #         ),
    #         ### MOV ----
    #         MOV = colDef(
    #           format = colFormat(digits = 2)
    #         ),
    #         ### SOS ----
    #         SOS = colDef(
    #           format = colFormat(digits = 2)
    #         ),
    #         ### SRS ----
    #         SRS = colDef(
    #           format = colFormat(digits = 2),
    #           style = color_scales(
    #             data = standingsTableData(),
    #             colors = c("red","pink", "whitesmoke", "palegreen", "green"),
    #             bias = 1,
    #             brighten_text = FALSE
    #           )
    #         ),
    #         ### OSRS ----
    #         OSRS = colDef(
    #           format = colFormat(digits = 2)
    #         ),
    #         ### DSRS ----
    #         DSRS = colDef(
    #           format = colFormat(digits = 2)
    #         )
    #       )
    #     )
    #   }else{
    #     ## Game ----
    #     standingsTableReact <- reactable(
    #       data = standingsTableDataReact |> select(-c(PF, PA)),
    #       theme = espn(
    #         centered = TRUE,
    #         header_font_size = 14,
    #         font_size = 14
    #       ),
    #       highlight = TRUE,
    #       compact = TRUE,
    #       pagination = FALSE,
    #       wrap = FALSE,
    #       outlined = TRUE,
    #       sortable = FALSE,
    #       showSortable = FALSE,
    #       fullWidth = TRUE,
    #       defaultSorted = "team_division",
    #       rowStyle = group_border_sort(columns = "team_division",
    #                                    border_color = "black",
    #                                    border_width = "1.5px",
    #                                    border_style = "solid"),
    #       defaultColDef = colDef(vAlign = "center",
    #                              minWidth = 50
    #                              #headerStyle = list(fontSize = "14px")
    #       ),
    #       columnGroups = list(
    #         colGroup(name = "Record",
    #                  columns = c("GP", "W", "L", "T", "W-L%")),
    #         colGroup(name = "Points",
    #                  columns = c("team_PPG", "opp_PPG", "PD")),
    #         colGroup(name = "Performance",
    #                  columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS"))
    #       ),
    #       columns = list(
    #         ### Team Division ----
    #         team_division = colDef(
    #           name = "",
    #           minWidth = 80,
    #           style = group_merge_sort("team_division"),
    #           #style = cell_style(font_color = "red")
    #         ),
    #         ### Team Logo ----
    #         team_logo_espn = colDef(
    #           name = "",
    #           sticky = "left",
    #           maxWidth = 25,
    #           style = background_img()
    #         ),
    #         ### Team Name ----
    #         team_name = colDef(
    #           name = "Team",
    #           minWidth = 175,
    #           style = list(borderRight = "1px solid black")
    #         ),
    #         ### Games Played ----
    #         GP = colDef(
    #           name = "GP",
    #           minWidth = 30,
    #           align = "center",
    #           style = list(borderRight = "1px solid #d3d3d3")
    #         ),
    #         ### Win ----
    #         W = colDef(
    #           minWidth = 30
    #         ),
    #         ### Loss ----
    #         L = colDef(
    #           minWidth = 30
    #         ),
    #         ### Tie ----
    #         `T` = colDef(
    #           minWidth = 30
    #         ),
    #         ### Win Loss Perc ----
    #         `W-L%` = colDef(
    #           minWidth = 75,
    #           format = colFormat(percent = TRUE, digits = 2),
    #           align = "center",
    #           style = list(borderRight = "1px solid #d3d3d3")
    #         ),
    #         ### PF ----
    #         team_PPG = colDef(
    #           name = "PF",
    #           format = colFormat(digits = 2)
    #         ),
    #         ### PA ----
    #         opp_PPG = colDef(
    #           name = "PA",
    #           format = colFormat(digits = 2)
    #         ),
    #         ### PD ----
    #         PD = colDef(
    #           align = "center",
    #           style = list(borderRight = "1px solid #d3d3d3")
    #         ),
    #         ### MOV ----
    #         MOV = colDef(
    #           format = colFormat(digits = 2)
    #         ),
    #         ### SOS ----
    #         SOS = colDef(
    #           format = colFormat(digits = 2)
    #         ),
    #         ### SRS ----
    #         SRS = colDef(
    #           format = colFormat(digits = 2),
    #           style = color_scales(
    #             data = standingsTableData(),
    #             colors = c("red","pink", "whitesmoke", "palegreen", "green"),
    #             bias = 1
    #           )
    #         ),
    #         ### OSRS ----
    #         OSRS = colDef(
    #           format = colFormat(digits = 2)
    #         ),
    #         ### DSRS ----
    #         DSRS = colDef(
    #           format = colFormat(digits = 2)
    #         )
    #       )
    #     )
    #   }
    # 
    #   return(standingsTableReact)
    # })
  })
}
# 
# 
# overviewData <- modDataLong |>
#   filter(season == 2024) |>
#   # mutate(
#   #   across(c(team_wins, team_losses, team_ties),
#   #          ~lead(.x, n = 1, default = 0)
#   #   ),
#   #   .by = team, .keep = "all"
#   # ) |>
#   slice_tail(n = 1, by = team) |>
#   arrange(game_id) |>
#   select(
#     team,
#     # team_games_played,
#     # team_wins,
#     # team_losses,
#     # team_ties,
#     # team_win_pct_cum,
#     team_elo,
#     team_off_epa_mean_cum,
#     team_def_epa_mean_cum,
#     team_off_epa_sum_cum,
#     team_def_epa_sum_cum,
#     team_PFG_cum,
#     team_PAG_cum,
#     team_MOV_cum,
#     team_SOS_cum,
#     team_SRS_cum,
#     team_OSRS_cum,
#     team_DSRS_cum
#   ) |>
#   left_join(
#     seasonWeekStandings |> 
#       slice_tail(n = 1, by = team) |> 
#       select(team, games_played, win, loss, tie, win_loss_percent),
#     by = 
#   ) |>
#   left_join(
#     teamsData |> select(team_abbr, team_logo_espn),
#     by = join_by(team == team_abbr)
#   ) |>
#   select(team_logo_espn, everything()) |>
#   rename_with(~str_remove(.x, pattern = "team_"), .cols = -c(team_logo_espn, team)) |>
#   rename_with(~str_remove(.x, pattern = "_cum"), .cols = everything()) |>
#   mutate(
#     win_pct = wins/(wins + losses),
#     .after = ties
#   )





