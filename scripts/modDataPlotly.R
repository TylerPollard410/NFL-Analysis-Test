seasons <- c(2024,2024)
gameType <- c("REG", "POST")
teams <- unique(modDataLong$team)
statType <- "Team"
xVar <- "team_SRS"
yVar <- "result"
colorVar <- "team"
facetVar <- NULL
fitLine <- TRUE
corType <- "kendall"

modPlotData <- modDataLong |>
  filter(season %in% seasons[1]:seasons[2],
         season_type %in% gameType,
         #home_team %in% teams() | away_team %in% teams()) |>
         team %in% teams) |>
  filter(!is.na(result)) |>
  mutate(
    season = factor(season),
    week = factor(week)
  ) |>
  select(
    season,
    season_type,
    week,
    team,
    opponent,
    #away_team,
    xVar,
    yVar,
    colorVar,
    facetVar
  )


if(!is.null(colorVar)){
  if(colorVar %in% c("team", "opponent")){
    plot <- ggplot(data = modPlotData, 
                   aes(x = !!sym(xVar), 
                       y = !!sym(yVar),
                       colour = !!sym(colorVar),
                       group = !!sym(colorVar))
    ) +
      geom_point() +
      scale_color_nfl(type = "primary", name = colorVar, guide = "legend")
    #geom_nfl_logos(aes(team_abbr = !!sym(colorVar())), width = 0.01)
  }else{
    plot <- ggplot(data = modPlotData, 
                   aes(x = !!sym(xVar), 
                       y = !!sym(yVar),
                       colour = !!sym(colorVar),
                       group = !!sym(colorVar))
    ) +
      geom_point()
  }
}else{
  plot <- ggplot(data = modPlotData, 
                 aes(x = !!sym(xVar), 
                     y = !!sym(yVar))
  ) +
    geom_point()
}

if(!is.null(facetVar)){
  plot2 <- plot + facet_wrap(vars(!!sym(facetVar)))
}else{
  plot2 <- plot
}

if(fitLine){
  plot3 <- plot2 + 
    geom_smooth(method = "lm", se = FALSE) +
    # sm_statCorr(
    #   corr_method = corType, 
    #   # label_x = ifelse(x == "Date", 
    #   #                  max(plotData |> pull(x)) - months(3),
    #   #                  0.9*max(plotData |> pull(x))),
    #   legends = TRUE
    # )
}else{
  plot3 <- plot2
}

finalPlot <- plot3 + theme_bw()
finalPlot

cor_label_layer <- ggplot_build(finalPlot)$data[[2]]
t2 <- modPlotData |>
  group_by(!!sym(colorVar)) |>
  mutate(
    rVal = round(cor.test(!!sym(xVar), 
                          !!sym(yVar), 
                          method = "kendall", 
                          exact = FALSE)$estimate,
                 2),
    pVal = round(cor.test(!!sym(xVar), 
                          !!sym(yVar), 
                          method = "kendall", 
                          exact = FALSE)$p.value,
                 2)
  )

t3 <- cor.test(modPlotData |> pull(!!sym(finalPlot$labels$x)),
               modPlotData |> pull(!!sym(finalPlot$labels$y)),
               method = "kendall", continuity = TRUE)
t3$estimate
t3 <- cor.test(modPlotData |> pull(!!sym(finalPlot$labels$x)),
               modPlotData |> pull(!!sym(finalPlot$labels$y)))

finalPlotDat <- finalPlot |> 
  ggplotly(layerData = 3, originalData = FALSE, tooltip = c("y", "x", "colour")) |> 
  mutate(
    y2 = y - vjust,
    label2 = paste0("R = ", r, ", p = ", p)
  ) |>
  plotly_data()

finalPlotly <- finalPlot |> 
  ggplotly(layerData = 3, originalData = FALSE, tooltip = c("y", "x", "colour")) |> 
  mutate(
    y2 = y - vjust,
    label2 = paste0("R = ", r, ", p = ", p)
  ) |>
  add_text(
    x = ~x,
    y = ~y2,
    text = ~label2,
    xref = "paper",
    yref = "paper",
    showarrow = FALSE,
    showlegend = TRUE,
    legendgroup = ~group_plotlyDomain,
    color = ~I(colour),
    hoverinfo = "none"
  )
finalPlotly

"#97233F"

# Convert ggplot to plotly and add dynamic correlation label as annotation
ggplotly(finalPlot, tooltip = c("x", "y", "colour")) %>%
  layout(annotations = list(
    text = cor_label_layer$label,
    x = cor_label_layer$x,  # Adjust annotation position as needed
    y = cor_label_layer$y - cor_label_layer$vjust,
    #vjust = cor_label_layer$vjust,
    xref = "paper",
    yref = "paper",
    showarrow = FALSE
  ))
#ggplotly(finalPlot, tooltip = c("y", "x", "colour"))
