# Run characterization of counterfactual scenarios

library(tidyverse)

# Set plotting parameters -----------------------------------------------------
n_trajectories_plot <- 100
max_day <- 150

scenarios_to_plot <- tibble(
  label = c("No vax, no int", "Baseline"),
  vaccine_scenario = c("no_vaccination", "baseline"),
  int_scenario = c("no_intervention", "baseline")
)

# Read in data -----------------------------------------------------------------

case_series <- read_csv("input/case_series.csv")

results_path <- "output/results"

#' Check that the arrow dataset has unique label/iteration/day combinations,
#' which means eg there are not multiple, unexpected gridded parameter values
check_scenarios <- function(df) {
  stopifnot(nrow(collect(head(df))) > 0)

  ns <- df |>
    count(vaccine_scenario, int_scenario, int_day, replicate, day) |>
    select(n) |>
    distinct() |>
    collect() |>
    pull(n)

  stopifnot(all(ns == 1))
  df
}

results <- griddleR::query_cache(results_path)

# check that we don't have unexpected varying parameters
results |>
  check_scenarios()

# get the (unique) start date
start_date <- results |>
  select(start_date) |>
  distinct() |>
  collect() |>
  pull(start_date) |>
  lubridate::ymd()

stopifnot(length(start_date) == 1)

# Plot a selection of simulations ----------------------------------------------

results |>
  select(vaccine_scenario, int_scenario, replicate, day, reported_cases) |>
  inner_join(scenarios_to_plot, by = c("vaccine_scenario", "int_scenario")) |>
  filter(day < max_day, replicate <= n_trajectories_plot) |>
  collect() |>
  mutate(date = start_date + lubridate::ddays(day - 1)) |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = reported_cases, group = replicate), alpha = 0.1) +
  scale_x_date(breaks = "1 month", date_labels = "%b") +
  geom_point(
    data = case_series,
    aes(y = observed_cases),
    color = "orange"
  ) +
  facet_grid(rows = vars(label)) +
  labs(
    title = "Scenario projections",
    x = NULL,
    y = "Cumulative no. reported cases"
  ) +
  cowplot::theme_cowplot()

ggsave("output/diagnostic_scenarios.png", bg = "white")

# Table 2 ----------------------------------------------------------------------

table2_data <- results |>
  select(vaccine_scenario, int_scenario, replicate, day, reported_cases) |>
  collect() |>
  arrange(vaccine_scenario, int_scenario, replicate, day, reported_cases) |>
  group_by(vaccine_scenario, int_scenario, replicate) |>
  mutate(incident_reported_cases = diff(c(0, reported_cases))) |>
  summarize(
    final_size = max(reported_cases),
    last_day = max(day[incident_reported_cases > 0]),
    .groups = "drop"
  ) |>
  mutate(
    final_size_additional = final_size - 1,
    last_date = start_date + lubridate::ddays(last_day - 1)
  ) |>
  mutate(size_cat_additional = cut(
    final_size_additional,
    c(0, 1, 10, 50, 100, Inf),
    c("0", "1-9", "10-49", "50-99", "100+"),
    right = FALSE
  ))

table2_sizes <- table2_data |>
  count(vaccine_scenario, int_scenario, size_cat_additional) |>
  group_by(vaccine_scenario, int_scenario) |>
  mutate(p = scales::percent(n / sum(n), 1)) |>
  ungroup() |>
  select(vaccine_scenario, int_scenario, size_cat_additional, p) |>
  pivot_wider(
    names_from = size_cat_additional,
    values_from = p,
    values_fill = "0%"
  )

table2_last_dates <- table2_data |>
  group_by(vaccine_scenario, int_scenario) |>
  summarize(median_last_date = median(last_date), .groups = "drop")

table2 <- full_join(
  table2_sizes, table2_last_dates,
  by = c("vaccine_scenario", "int_scenario")
) |>
  mutate(
    vaccine_scenario = factor(
      vaccine_scenario,
      levels = c("no_vaccination", "delayed", "baseline", "early")
    ),
    int_scenario = factor(
      int_scenario,
      levels = c("no_intervention", "baseline")
    )
  ) |>
  arrange(vaccine_scenario, int_scenario) |>
  # drop the not-meaningful combination
  filter(!(vaccine_scenario == "no_vaccination" & int_scenario == "baseline"))

write_csv(table2, "output/table2.csv")

# Figure 1 --------------------------------------------------------------------

figure1_data <- table2_data |>
  filter(final_size >= 3) |>
  filter(
    !(vaccine_scenario == "no_vaccination" & int_scenario == "baseline")
  ) |>
  mutate(label = interaction(
    factor(int_scenario, levels = c("baseline", "no_intervention")),
    factor(vaccine_scenario, levels = c(
      "early", "baseline", "delayed", "no_vaccination"
    )),
    sep = "\n"
  ))

figure1_data |>
  ggplot(aes(label, final_size)) +
  # coef=NULL puts the whisker limits at min/max
  geom_boxplot(coef = NULL) +
  scale_y_continuous(limits = c(0, 300), expand = c(0, 0)) +
  labs(
    title = "Distributions of outbreak sizes, counterfactual scenarios",
    x = "Active case finding / Vaccination",
    y = "No. measles cases"
  ) +
  cowplot::theme_cowplot()

ggsave("output/figure1.png", bg = "white")

figure1_summary <- figure1_data |>
  group_by(vaccine_scenario, int_scenario) |>
  summarize(
    min = min(final_size),
    q25 = quantile(final_size, 0.25),
    median = median(final_size),
    q75 = quantile(final_size, 0.75),
    max = max(final_size),
    .groups = "drop"
  )

write_csv(figure1_summary, "output/figure1_summary.csv")
