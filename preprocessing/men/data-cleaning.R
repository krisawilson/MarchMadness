# grab functions
source("preprocessing/men/functions/data-cleaning-functions.R")

# grab bracket data ----
# 2024
t24 <- "https://www.sports-reference.com/cbb/postseason/men/2024-ncaa.html"
b24 <- scrape_bracket_mbb(url = t24, year = 2024)
# 2023
t23 <- "https://www.sports-reference.com/cbb/postseason/men/2023-ncaa.html"
b23 <- scrape_bracket_mbb(url = t23, year = 2023)
# manually fix the Stanford vs Sacred Heart error
b23[46,"seed2"] <- 16; b23[46,"score1"] <- 92; 
b23[46, "score2"] <- 49; b23[46,"winner"] <- "Stanford";

# 2022
t22 <- "https://www.sports-reference.com/cbb/postseason/men/2022-ncaa.html"
b22 <- scrape_bracket_mbb(url = t22, year = 2022)

# 2019
t19 <- "https://www.sports-reference.com/cbb/postseason/men/2019-ncaa.html"
b19 <- scrape_bracket_mbb(url = t19, year = 2019)

# 2018
t18 <- "https://www.sports-reference.com/cbb/postseason/men/2018-ncaa.html"
b18 <- scrape_bracket_mbb(url = t18, year = 2018)
rm(t22,t23,t24,t19,t18)

# get ratings/stats data: ----
rat24 <- "https://www.sports-reference.com/cbb/seasons/men/2024-ratings.html"
rat23 <- "https://www.sports-reference.com/cbb/seasons/men/2023-ratings.html"
rat22 <- "https://www.sports-reference.com/cbb/seasons/men/2022-ratings.html"
rat19 <- "https://www.sports-reference.com/cbb/seasons/men/2019-ratings.html"
rat18 <- "https://www.sports-reference.com/cbb/seasons/men/2018-ratings.html"

ratings24 <- scrape_adv_stats_mbb(url = rat24, year = 2024)
ratings23 <- scrape_adv_stats_mbb(url = rat24, year = 2023)
ratings22 <- scrape_adv_stats_mbb(url = rat24, year = 2022)
ratings19 <- scrape_adv_stats_mbb(url = rat19, year = 2019)
ratings18 <- scrape_adv_stats_mbb(url = rat18, year = 2018)
rm(rat22,rat23,rat24,rat19,rat18)


# get pace ----
u24 <- "https://www.sports-reference.com/cbb/seasons/men/2024-advanced-school-stats.html"
u23 <- "https://www.sports-reference.com/cbb/seasons/men/2023-advanced-school-stats.html"
u22 <- "https://www.sports-reference.com/cbb/seasons/men/2022-advanced-school-stats.html"
u19 <- "https://www.sports-reference.com/cbb/seasons/men/2019-advanced-school-stats.html"
u18 <- "https://www.sports-reference.com/cbb/seasons/men/2018-advanced-school-stats.html"
p24 <- scrape_pace_mbb(url = u24, year = 2024)
p23 <- scrape_pace_mbb(url = u23, year = 2023)
p22 <- scrape_pace_mbb(url = u22, year = 2022)
p19 <- scrape_pace_mbb(url = u19, year = 2019)
p18 <- scrape_pace_mbb(url = u19, year = 2018)
rm(u24,u23,  u22, u19, u18)

# combine data ----
brackets <- bind_rows(b24, b23, b22, b19, b18)
rm(b24, b23, b22, b19, b18)
paces <- bind_rows(p24, p23, p22, p19, p18)
rm(p22, p23, p24, p19, p18)
ratings <- bind_rows(ratings24, ratings23, 
                     ratings22, ratings19, ratings18)
rm(ratings22, ratings23, ratings24, ratings19, ratings18)


# join data! 
team_stats <- inner_join(ratings, paces, 
                         by = c("School", "year"))
# small cleanup
team_stats <- team_stats |> 
  # move pace around
  relocate(Pace, .before = MOV) |> select(-Rk) |> 
  # consistent team names
  mutate(School = team_names_mbb(School))
rm(paces, ratings)

# final join! team 1 first
full_dat <- brackets |>
  left_join(team_stats, 
            # schools and years should match
            by = c("team1" = "School", "year" = "year")) |>
  rename_with(~ paste0(., "_team1"), 
              # want stats for team 1 to be identified
              .cols = setdiff(names(team_stats), 
                              c("School", "year")))
# now team 2
full_dat <- full_dat |>
  left_join(team_stats, 
            # again, schools and years should match
            by = c("team2" = "School", "year" = "year")) |>
  rename_with(
    ~ paste0(., "_team2"), 
    # again, want stats for team 2 to be identified
    .cols = setdiff(names(team_stats), 
                    c("School", "year", 
                      grep("_team1$", names(full_dat), 
                           value = TRUE))))

# update UMBC by hand since they're not in NCAA database???
# order is ppg, ppg allowed, pace, mov, sos, osrs, dsrs,  adj_or adj_dr
umbc_stats <- c(25, 11, 73.1, 68.5,
                mean(team_stats$Pace, na.rm = TRUE),
                mean(team_stats$MOV, na.rm = TRUE), -3.84,
                mean(team_stats$OSRS, na.rm = TRUE), 
                mean(team_stats$DSRS, na.rm = TRUE), 
                105.6, 99)
full_data <- full_dat |> 
  mutate(
    # For team1 (columns 11 to 21), replace only NA values
    across(11:21, ~ if_else(team1 == "UMBC" & is.na(.),
                            umbc_stats[match(cur_column(), names(full_dat)[11:21])], .)),
    # For team2 (columns 23 to 33), replace only NA values
    across(23:33, ~ if_else(team2 == "UMBC" & is.na(.),
                            umbc_stats[match(cur_column(), names(full_dat)[23:33])], .)),
    # Update Conf_team1 and Conf_team2 for UMBC
    Conf_team1 = if_else(team1 == "UMBC", "AEC", Conf_team1),
    Conf_team2 = if_else(team2 == "UMBC", "AEC", Conf_team2)
  )
# write CSVs!
write.csv(brackets, "data/men/brackets.csv")
write.csv(team_stats, "data/men/team-stats.csv")
write.csv(full_data, "data/men/full-data.csv")
