# Parallel processing can be activated via the following line
future::plan("multisession")

# Read in current data ----
## Games data ----
games_df2 <- load_schedules()
games_df2 <- games_df2 %>% filter(season >= 2003)
games_df <- games_df2 %>% 
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  )

# Get long format of games data
games_long_df <- clean_homeaway(games_df, invert = c("result", "spread_line"))

## Play-by-Play data ----
# Load play by play data from downloaded files (takes about 9 minutes)
# pbp_data <- list()
# pbp_files <- list.files("../Play-by-Play rds Files")
# pbp_seasons <- as.numeric(str_extract(pbp_files, "[:digit:]+"))
# pbp_files <- pbp_files[pbp_seasons >= 2003]
# for(i in pbp_files){
#   pbp_season <- str_extract(i, "[:digit:]+")
#   pbp_data[[i]] <- with_progress(readRDS(paste0("../Play-by-Play rds Files/", i)))
# }
# pbp_df <- rbindlist(pbp_data)

## Teams data ----
# Load team ids
nfl_teams <- load_teams()

# Define server logic ----
shinyServer(function(input, output, session) {
  # Homepage / Navbar ==========
  observeEvent(input$about, {
    showModal(
      modalDialog(title = "About",
                  div(strong("Created by: "), "Tyler Pollard"),
                  div(strong("Version: "), "1.0"),
                  div(strong("Release Date: "), "27 July 2021"))
    )
  })
  
  output$image <- renderImage({
    filename <- normalizePath(file.path("www/nfl_logo.jpeg"))
    list(src = filename,
         width = 800,
         height = 400,
         align = "center")
  }, deleteFile = FALSE)
  
  
  # Data Tab =====================
  ## Create data frame specific to data tab ----
  data_game_table <- games_df2 %>%
    dplyr::select(
      -c(
        "game_id",
        "old_game_id",
        "gsis",
        "nfl_detail_id",
        "pfr",
        "pff",
        "espn",
        "ftn",
        "away_qb_id",
        "home_qb_id",
        "stadium_id"
      )
    )
  
  ## Data Filters ----
  ### Column Filters ----
  #### Column select filter ----
  output$data_column_select_out <- renderUI({
    pickerInput(
      inputId = "data_column_select",
      label = "Select columns to include in table",
      choices = names(data_game_table),
      multiple = TRUE,
      selected = names(data_game_table),
      options = pickerOptions(
        actionsBox = TRUE
      )
    )
  })
  
  #### Column hold filter ----
  output$data_column_hold_out <- renderUI({
    pickerInput(
      inputId = "data_column_hold",
      label = "Select columns to lock in place",
      choices = input$data_column_select,
      selected = NULL,
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE
      )
    )
  })
  
  observe({
    updatePickerInput(session, 
                      inputId = "data_column_hold", 
                      choices = input$data_column_select,
                      selected = input$data_column_hold)
  }) %>%
    bindEvent(input$data_column_select)
  
  # output$dimensions_out <- renderText({
  #   paste("Width:", get_width(), "\n", "Height:", get_height())
  # })
  
  ## Create data table of game data ----
  data_game_table_filtered <- reactive({
    data_game_table %>%
      select(
        input$data_column_hold,
        input$data_column_select
      )
  })
  
  
  ## Output table ----
  output$game_datatable_out <- renderDataTable({
    datatable(data_game_table_filtered(),
              class = "cell-border stripe",
              rownames = FALSE,
              extensions = "FixedColumns",
              options = list(
                scrollX = TRUE,
                #scrollY = "50vh",
                #scrollCollapse = TRUE,
                autoWidth = FALSE,
                nowrap = TRUE,
                paging = TRUE,
                pageLength = 5,
                lengthMenu = c(5, 10, 20, 50, 100),
                searching = FALSE,
                fixedColumns = list(leftColumns = length(input$data_column_hold))
              )
    )
  })
  
  # Data Exploration Tab ==============
  
  ## Betting Statistics ----
  
  ### Data Filters ----
  
  #### Team Filter ----
  output$betting_team_filter_out <- renderUI({
    pickerInput(
      inputId = "betting_team_filter",
      label = "Select team to analyze", 
      choices = nfl_teams$team_name,
      selected = "Baltimore Ravens",
      multiple = FALSE,
      choicesOpt = list(
        content = paste0("<div style='background: ", nfl_teams$team_color, "; color: white;'>", nfl_teams$team_name, "</div>")
      )
    )
  })
  
  #### Time Filter ----
  output$betting_time_filter_out <- renderUI({
    sliderInput(
      inputId = "betting_time_filter",
      label = "Select seasons to include",
      min = min(games_long_df$season),
      max = max(games_long_df$season),
      value = c(min(games_long_df$season), max(games_long_df$season)),
      step = 1
    )
  })
  
  ### Output Tables ----
  #### Overall Cover Percent Table ----
  output$betting_overall_spread_cover_table <- render_gt({
    validate(
      need(input$betting_time_filter, "Please enter time to view")
    )
    
    spread_gt <- games_long_df |>
      filter(!is.na(result)) |>
      filter(
        #between(season, input$betting_time_filter[1], input$betting_time_filter[2])
        between(season, 2003, 2023)
      ) |>
      mutate(
        spread_cover = ifelse(result > spread_line, "Cover",
                              ifelse(result < spread_line, "Loss", "Push")),
        spread_difference = result - spread_line
      ) |>
      group_by(team) |>
      summarise(
        `Games Played` = n(),
        `Cover` = sum(spread_cover == "Cover", na.rm = TRUE),
        `Push` = sum(spread_cover == "Push", na.rm = TRUE),
        `Loss` = sum(spread_cover == "Loss", na.rm = TRUE),
        `ATS +/-` = mean(spread_difference)
      ) |>
      mutate(
        `Cover %` = Cover/(Cover + Loss)
      ) |>
      select(
        c(1:5,7,6)
      ) |>
      arrange(desc(`Cover %`)) |>
      rename("Team" = team) |>
      gt(
        #rowname_col = "Team"
      ) |>
      fmt_percent(
        columns = `Cover %`,
        decimals = 2
      ) |>
      fmt_number(
        columns = `ATS +/-`,
        decimals = 2
      ) |>
      data_color(
        columns = `Cover %`,
        method = "numeric",
        palette = c("red", "white", "green")
      ) |>
      gt_nfl_logos(
        columns = "Team", height = "20px"
      ) |>
      cols_align(
        align = "center"
      ) |>
      tab_options(
        table.align = "left",
        table.font.size = "80%",
        data_row.padding = px(3)
      ) |>
      tab_style(
        style = cell_borders(sides = "right"),
        locations = cells_body(columns = c("Team"))
      ) |>
      cols_width(
        everything() ~ px(80)
      ) |>
      opt_interactive(
        use_pagination = FALSE,
        use_compact_mode = TRUE
      )
    spread_gt
  })
  
  #### Overall Moneyline Percent Table ----
  output$betting_overall_moneyline_table <- render_gt({
    validate(
      need(input$betting_time_filter, "Please enter time to view")
    )
    
    moneyline_gt <- games_long_df |>
      filter(!is.na(result)) |>
      filter(
        between(season, input$betting_time_filter[1], input$betting_time_filter[2])
      ) |>
      mutate(
        spread_cover = ifelse(result > 0, "Win",
                              ifelse(result < 0, "Loss", "Tie"))
      ) |>
      group_by(team) |>
      summarise(
        `Games Played` = n(),
        `Win` = sum(spread_cover == "Win", na.rm = TRUE),
        `Tie` = sum(spread_cover == "Tie", na.rm = TRUE),
        `Loss` = sum(spread_cover == "Loss", na.rm = TRUE)
        
      ) |>
      mutate(
        `Win %` = Win/(Win + Loss)
      ) |>
      arrange(desc(`Win %`)) |>
      rename("Team" = team) |>
      gt(
        #rowname_col = "Team"
      ) |>
      fmt_percent(
        columns = `Win %`,
        decimals = 2
      ) |>
      data_color(
        columns = `Win %`,
        method = "numeric",
        palette = c("red", "white", "green")
      ) |>
      gt_nfl_logos(
        columns = "Team", height = "20px"
      ) |>
      cols_align(
        align = "center"
      ) |>
      tab_options(
        table.align = "left",
        table.font.size = "90%",
        data_row.padding = px(3)
      ) |>
      tab_style(
        style = cell_borders(sides = "right"),
        locations = cells_body(columns = c("Team"))
      )
    moneyline_gt
  })
  
  #### Overall Moneyline Percent Table ----
  output$betting_overall_over_under_table <- render_gt({
    validate(
      need(input$betting_time_filter, "Please enter time to view")
    )
    
    over_under_gt <- games_long_df |>
      filter(!is.na(result)) |>
      filter(
        between(season, input$betting_time_filter[1], input$betting_time_filter[2])
      ) |>
      mutate(
        spread_cover = ifelse(total > total_line, "Over",
                              ifelse(total < total_line, "Under", "Push"))
      ) |>
      group_by(team) |>
      summarise(
        `Games Played` = n(),
        `Over` = sum(spread_cover == "Over", na.rm = TRUE),
        `Push` = sum(spread_cover == "Push", na.rm = TRUE),
        `Under` = sum(spread_cover == "Under", na.rm = TRUE)
        
      ) |>
      mutate(
        `Over %` = Over/(Over + Under),
        `Under %` = Over/(Over + Under)
      ) |>
      arrange(desc(`Over %`)) |>
      rename("Team" = team) |>
      gt(
        #rowname_col = "Team"
      ) |>
      fmt_percent(
        columns = c(`Over %`,`Under %`),
        decimals = 2
      ) |>
      data_color(
        columns = `Over %`,
        method = "numeric",
        palette = c("red", "white", "green")
      ) |>
      data_color(
        columns = `Under %`,
        method = "numeric",
        palette = c("green", "white", "red")
      ) |>
      gt_nfl_logos(
        columns = "Team", height = "20px"
      ) |>
      cols_align(
        align = "center"
      ) |>
      tab_options(
        table.align = "left",
        table.font.size = "90%",
        data_row.padding = px(3)
      ) |>
      tab_style(
        style = cell_borders(sides = "right"),
        locations = cells_body(columns = c("Team"))
      )
    over_under_gt
  })
  
  #### Team Cover Precent Table ----
  output$betting_team_spread_cover_table <- render_gt({
    validate(
      need(input$betting_time_filter, "Please enter time to view"),
      need(input$betting_team_filter, "Please enter team to view"),
    )
    
    filtered_team <- nfl_teams %>% filter(team_name == input$betting_team_filter) %>% pull(team_abbr)
    
    team_spread_gt <- games_long_df |>
      filter(!is.na(result)) |>
      filter(
        (between(season, input$betting_time_filter[1], input$betting_time_filter[2])) &
          (team == filtered_team)
      ) |>
      mutate(
        spread_cover = ifelse(result > spread_line, "Cover",
                              ifelse(result < spread_line, "Loss", "Push")),
        spread_difference = result - spread_line
      ) |>
      select(
        season,
        game_type,
        week,
        opponent,
        opponent_score,
        team,
        team_score,
        location,
        result,
        spread_line,
        spread_cover,
        spread_difference
      ) |>
      gt()
    team_spread_gt
  })
  
  
  # Modelling Tab ==================
}) # end server





