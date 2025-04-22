## Create Standings Tables ----

# UI ----
standingsTableOutput <- function(id){
  tagList(
    withSpinner(
      uiOutput(NS(id, "standingsTableUI")),
      type = 8
      )
    # uiOutput(NS(id, "standingsTableTitle")),
    # withSpinner(
    #   reactableOutput(NS(id, "standingsTable")), type = 8
    # )
  )
}


# Server ----
standingsTableServer <- function(id,
                                 standingsSeason,
                                 standingsStat,
                                 teamsData,
                                 standingsTableData,
                                 conference){
  moduleServer(id, function(input, output, session){
    
    # output$standingsTableUI <- renderUI({
    #   conf_logo <- teamsData |>
    #     filter(team_conf == conference) |>
    #     pull(team_conference_logo) |>
    #     unique()
    # 
    #   box(
    #     title = div(style = "display: flex; align-items: center;",
    #                 img(src = conf_logo, style = "height: 25px;"),
    #                 strong(standingsSeason(), style = "margin-left: 6px; font-size: 25px"),
    #                 strong("Standings", style = "margin-left: 4px; font-size: 25px")
    #     ),
    #     width = 12,
    #     status = "primary",
    #     maximizable = TRUE,
    #     #withSpinner(
    #     uiOutput(NS(id, "tablePlaceholder"))#, type = 8
    #     #)
    #   )
    # })
    # 
    # output$tablePlaceholder <- renderUI({
    #   withSpinner(
    #     reactableOutput(NS(id, "standingsTable")), type = 8
    #   )
    # })
    
    # output$standingsTableTitle <- renderUI({
    #   conf_logo <- teamsData |>
    #     filter(team_conf == conference) |>
    #     pull(team_conference_logo) |>
    #     unique()
    #   
    #   title <- div(style = "display: flex; align-items: center;",
    #                img(src = conf_logo, style = "height: 25px;"),
    #                strong(standingsSeason(), style = "margin-left: 6px; font-size: 25px"),
    #                strong("Standings", style = "margin-left: 4px; font-size: 25px")
    #   )
    #   title
    # })
    
    output$standingsTable <- renderReactable({
      conf_logo <- teamsData |>
        filter(team_conf == conference) |>
        pull(team_conference_logo) |>
        unique()
      Stat <- standingsStat()
      Season <- standingsSeason()
      
      standingsTableDataReact <- standingsTableData() |>
        filter(team_conf == conference) |>
        select(
          "team_division",
          "team_logo_espn",
          "team_name",
          "div_rank",
          "GP",
          "W",
          "L",
          "T",
          "W-L%",
          "PF",
          "team_PPG",
          "PA",
          "opp_PPG",
          "PD",
          "MOV",
          "SOS",
          "SRS",
          "OSRS",
          "DSRS"
        ) |>
        group_by(team_division) |>
        arrange(team_division, div_rank) |>
        ungroup() |>
        select(-div_rank)
      
      ## Reactable ----
      ### Total ----
      if(Stat == "Total"){
        standingsTableReact <- reactable(
          data = standingsTableDataReact |> select(-c(team_PPG, opp_PPG)),
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
          bordered = FALSE,
          sortable = FALSE,
          showSortable = FALSE,
          fullWidth = TRUE,
          defaultSorted = "team_division",
          rowStyle = group_border_sort(columns = "team_division",
                                       border_color = "black",
                                       border_width = "1.5px",
                                       border_style = "solid"),
          defaultColDef = colDef(vAlign = "center",
                                 minWidth = 50
                                 #headerStyle = list(fontSize = "14px")
          ),
          columns = list(
            ### Team Division ----
            team_division = colDef(
              name = "",
              minWidth = 80,
              style = group_merge_sort("team_division"),
              #style = cell_style(font_color = "red")
            ),
            ### Team Logo ----
            team_logo_espn = colDef(
              name = "",
              minWidth = 30,
              sticky = "left",
              cell = embed_img(height = "25px")
            ),
            ### Team Name ----
            team_name = colDef(
              name = "Team",
              minWidth = 175,
              style = list(borderRight = "1px solid black")
            ),
            ### Games Played ----
            GP = colDef(
              name = "GP",
              minWidth = 30,
              align = "center",
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            ### Win ----
            W = colDef(
              name = "W",
              align = "center", 
              minWidth = 30
            ),
            ### Loss ----
            L = colDef(
              name = "L", 
              align = "center", 
              minWidth = 30
            ),
            ### Tie ----
            T = colDef(
              name = "T",
              align = "center", 
              minWidth = 30
            ),
            ### Win Loss Perc ----
            `W-L%` = colDef(
              name = "W-L%",
              format = colFormat(percent = TRUE, digits = 1),
              align = "center",
              minWidth = 50,
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            ### PF ----
            ### PA ----
            ### PD ----
            PD = colDef(
              align = "center",
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            ### MOV ----
            MOV = colDef(
              format = colFormat(digits = 2)
            ),
            ### SOS ----
            SOS = colDef(
              format = colFormat(digits = 2)
            ),
            ### SRS ----
            SRS = colDef(
              format = colFormat(digits = 2),
              style = color_scales(
                data = standingsTableData(),
                colors = c("red","pink", "whitesmoke", "palegreen", "green"),
                bias = 1,
                brighten_text = FALSE
              )
            ),
            ### OSRS ----
            OSRS = colDef(
              format = colFormat(digits = 2)
            ),
            ### DSRS ----
            DSRS = colDef(
              format = colFormat(digits = 2)
            )
          )
        )
      }else{
        ## Game ----
        standingsTableReact <- reactable(
          data = standingsTableDataReact |> select(-c(PF, PA)),
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
          bordered = FALSE,
          sortable = FALSE,
          showSortable = FALSE,
          fullWidth = TRUE,
          defaultSorted = "team_division",
          rowStyle = group_border_sort(columns = "team_division", 
                                       border_color = "black", 
                                       border_width = "1.5px",
                                       border_style = "solid"),
          defaultColGroup = colGroup(
            align = "center",
            headerStyle = list(
              border = "none"
            )
          ),
          columnGroups = list(
            colGroup(name = "Record",
                     columns = c("GP", "W", "L", "T", "W-L%")),
            colGroup(name = "Points",
                     columns = c("team_PPG", "opp_PPG", "PD")),
            colGroup(name = "Performance",
                     columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS"))
          ),
          defaultColDef = colDef(vAlign = "center",
                                 minWidth = 50
                                 #headerStyle = list(fontSize = "14px")
          ),
          columns = list(
            ### Team Division ----
            team_division = colDef(
              name = "",
              minWidth = 80, 
              style = group_merge_sort("team_division"), 
              #style = cell_style(font_color = "red")
            ),
            ### Team Logo ----
            team_logo_espn = colDef(
              name = "",
              minWidth = 30,
              sticky = "left",
              cell = embed_img(height = "25px")
            ),
            ### Team Name ----
            team_name = colDef(
              name = "Team",
              minWidth = 175,
              style = list(borderRight = "1px solid black")
            ),
            ### Games Played ----
            GP = colDef(
              name = "GP",
              minWidth = 30,
              align = "center",
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            ### Win ----
            W = colDef(
              name = "W",
              align = "center", 
              minWidth = 30
            ),
            ### Loss ----
            L = colDef(
              name = "L", 
              align = "center", 
              minWidth = 30
            ),
            ### Tie ----
            T = colDef(
              name = "T",
              align = "center", 
              minWidth = 30
            ),
            ### Win Loss Perc ----
            `W-L%` = colDef(
              name = "W-L%",
              format = colFormat(percent = TRUE, digits = 1),
              align = "center",
              minWidth = 50,
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            ### PF ----
            team_PPG = colDef(
              name = "PF",
              format = colFormat(digits = 2)
            ),
            ### PA ----
            opp_PPG = colDef(
              name = "PA",
              format = colFormat(digits = 2)
            ), 
            ### PD ----
            PD = colDef(
              align = "center",
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            ### MOV ----
            MOV = colDef(
              format = colFormat(digits = 2)
            ),
            ### SOS ----
            SOS = colDef(
              format = colFormat(digits = 2)
            ),
            ### SRS ----
            SRS = colDef(
              format = colFormat(digits = 2),
              style = color_scales(
                data = standingsTableData(),
                colors = c("red","pink", "whitesmoke", "palegreen", "green"),
                bias = 1
              )
            ),
            ### OSRS ----
            OSRS = colDef(
              format = colFormat(digits = 2)
            ),
            ### DSRS ----
            DSRS = colDef(
              format = colFormat(digits = 2)
            )
          )
        )
      }
      
      return(standingsTableReact)
    })
    
    # Surrounding box UI
    output$standingsTableUI <- renderUI({
      conf_logo <- teamsData |> 
        filter(team_conf == conference) |> 
        pull(team_conference_logo) |> 
        unique()
      
      box(
        title = div(
          style = "display: flex; align-items: center;",
          img(src = conf_logo, style = "height: 25px;"),
          strong(standingsSeason(), style = "margin-left: 6px; font-size: 25px;"),
          strong("Standings", style = "margin-left: 4px; font-size: 25px;")
        ),
        width = 12,
        status = "primary",
        reactableOutput(NS(id, "standingsTable"))
      )
    })
  }) # end module Server
} # end standingsTableServer

