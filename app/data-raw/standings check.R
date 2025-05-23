seasonStandingsNFLverse <- calculate_standings(
  nflverse_object = gameData |> filter(!is.na(result)),
  tiebreaker_depth = 2
)

seasonStandingsNFLverse <- nfl_standings(
  gameData |> filter(!is.na(result))
)
glimpse(seasonStandingsNFLverse)

seasonStandingsComp <- seasonStandings |>
  arrange(season, team)
colnames(seasonStandingsComp)
seasonStandingsNFLverse2Comp <- seasonStandingsNFLverse2 |>
  arrange(season, team)
colnames(seasonStandingsNFLverse2Comp)

sum(seasonStandingsComp$GP != seasonStandingsNFLverse2Comp$games)
sum(seasonStandingsComp$W != seasonStandingsNFLverse2Comp$true_wins)
sum(seasonStandingsComp$L != seasonStandingsNFLverse2Comp$losses)
sum(seasonStandingsComp$T != seasonStandingsNFLverse2Comp$ties)

sum(seasonStandingsComp$`W-L%` != seasonStandingsNFLverse2Comp$win_pct)
which(seasonStandingsComp$`W-L%` != seasonStandingsNFLverse2Comp$win_pct)
sum(seasonStandingsComp$`CON%` != seasonStandingsNFLverse2Comp$conf_pct)
sum(seasonStandingsComp$`DIV%` != seasonStandingsNFLverse2Comp$div_pct)

sum(seasonStandingsComp$PF != seasonStandingsNFLverse2Comp$pf)
sum(seasonStandingsComp$PA != seasonStandingsNFLverse2Comp$pa)
sum(seasonStandingsComp$PD != seasonStandingsNFLverse2Comp$pd)

sum(seasonStandingsComp$team_PPG != seasonStandingsNFLverse2Comp$pf/seasonStandingsNFLverse2Comp$games)
sum(seasonStandingsComp$opp_PPG != seasonStandingsNFLverse2Comp$pa/seasonStandingsNFLverse2Comp$games)
sum(seasonStandingsComp$MOV != seasonStandingsNFLverse2Comp$pd/seasonStandingsNFLverse2Comp$games)

seasonStandingsNFLverse2Comp <- seasonStandingsNFLverse2Comp |>
  mutate(
    SOS = sos*games,
    SOV = sov*games,
    .after = sos
  )

cor <- cor

which(seasonStandingsComp$GP != seasonStandingsNFLverse2Comp$games)
