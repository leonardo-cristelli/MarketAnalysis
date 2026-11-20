# =============================================================================
# Demand side of the French beer market: extra checks for Section 3
# =============================================================================
#
# This script reproduces the numbers we added to the demand section:
#   1. a check of our volume figures against published elasticities,
#   2. beer litres per resident compared with real income,
#   3. beer and wine relative prices.
#
# It needs the Passport file d.volume.comprisson.xlsx (on-trade, off-trade and
# total beer volume, 2016-2025) in a folder called DATA next to this script.

library(eurostat)
library(tidyverse)
library(readxl)

years <- 2016:2025


# -----------------------------------------------------------------------------
# Load the data
# -----------------------------------------------------------------------------

# Beer volume in France, million litres (Passport / Euromonitor)
volume <- read_excel("DATA/d.volume.comprisson.xlsx", skip = 5) |>
  filter(Geography == "France",
         `Data Type` %in% c("Total Volume", "On-trade Volume", "Off-trade Volume")) |>
  pivot_longer(as.character(years), names_to = "Year", values_to = "litres_m") |>
  mutate(Year = as.integer(Year), litres_m = as.numeric(litres_m)) |>
  select(Year, `Data Type`, litres_m) |>
  pivot_wider(names_from = `Data Type`, values_from = litres_m)

# Population on 1 January (Eurostat), converted to millions
population <- get_eurostat("tps00001", time_format = "num") |>
  filter(geo == "FR", TIME_PERIOD %in% years) |>
  transmute(Year = TIME_PERIOD, pop_m = values / 1e6)

# Real GDP per head, in chain-linked 2020 euros (Eurostat)
gdp <- get_eurostat("nama_10_pc", time_format = "num") |>
  filter(geo == "FR", unit == "CLV20_EUR_HAB", na_item == "B1GQ", TIME_PERIOD %in% years) |>
  transmute(Year = TIME_PERIOD, gdp_pc = values)

# Consumer price indices (HICP, annual average). The COICOP codes are:
#   CP00 all items, CP0211 spirits, CP0212 wine, CP0213 beer
prices <- get_eurostat("prc_hicp_aind", time_format = "num") |>
  filter(geo == "FR", unit == "INX_A_AVG",
         coicop %in% c("CP00", "CP0211", "CP0212", "CP0213"),
         TIME_PERIOD %in% years) |>
  select(Year = TIME_PERIOD, coicop, values) |>
  pivot_wider(names_from = coicop, values_from = values)

# Join everything by year and work out the per-head and relative-price measures
d <- volume |>
  left_join(population, by = "Year") |>
  left_join(gdp, by = "Year") |>
  left_join(prices, by = "Year") |>
  mutate(
    litres_pc = `Total Volume` / pop_m,       # litres per resident
    on_pc     = `On-trade Volume` / pop_m,    # bars, restaurants
    off_pc    = `Off-trade Volume` / pop_m,   # shops, supermarkets
    lpc_index = 100 * litres_pc / litres_pc[Year == 2016],
    gdp_index = 100 * gdp_pc / gdp_pc[Year == 2016],
    beer_wine = CP0213 / CP0212,              # beer price relative to wine
    real_wine = CP0212 / CP00                 # wine price relative to all prices
  )


# -----------------------------------------------------------------------------
# 1. Elasticity check
# -----------------------------------------------------------------------------
# Nelson (2013) finds an own-price elasticity of -0.29 and an income elasticity
# of +0.51 for beer. If those hold, how much should volume have changed after
# the 2022 price rises, and how does that compare with what actually happened?

# Percentage change in x between year a and year b
pct_change <- function(x, a, b) 100 * (x[d$Year == b] / x[d$Year == a] - 1)

check <- tibble(
  period         = c("2022-2023", "2022-2025"),
  beer_price     = c(pct_change(d$CP0213, 2022, 2023), pct_change(d$CP0213, 2022, 2025)),
  rel_beer_price = c(pct_change(d$CP0213 / d$CP00, 2022, 2023), pct_change(d$CP0213 / d$CP00, 2022, 2025)),
  real_gdp_pc    = c(pct_change(d$gdp_pc, 2022, 2023), pct_change(d$gdp_pc, 2022, 2025)),
  volume_actual  = c(pct_change(d$`Total Volume`, 2022, 2023), pct_change(d$`Total Volume`, 2022, 2025))
) |>
  mutate(pred_price     = -0.29 * beer_price,
         pred_rel_price = -0.29 * rel_beer_price,
         pred_income    =  0.51 * real_gdp_pc)

print(check)


# -----------------------------------------------------------------------------
# 2. Beer per head vs real income
# -----------------------------------------------------------------------------
# Both series are indexed to 2016 = 100 so they can share one axis.
# The correlations below are descriptive only, not causal.

index_data <- pivot_longer(d, c(lpc_index, gdp_index), names_to = "Measure", values_to = "Index")

ggplot(index_data, aes(Year, Index, colour = Measure)) +
  geom_hline(yintercept = 100, colour = "grey60") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c(lpc_index = "#0072B2", gdp_index = "#999999"),
                      labels = c(lpc_index = "Beer litres per resident",
                                 gdp_index = "Real GDP per capita")) +
  scale_x_continuous(breaks = years) +
  labs(title = "France: beer per head vs real income",
       subtitle = "Index, 2016 = 100", x = NULL, colour = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

dir.create("charts", showWarnings = FALSE)
ggsave("charts/D_litres_pc_vs_gdp_R.png", width = 9, height = 5.5, dpi = 200, bg = "white")
write.csv(d, "charts/demand_series.csv", row.names = FALSE)

# Correlation over the whole period, and before / after Covid
tibble(
  period = c("2016-2025", "2016-2019", "2021-2025"),
  r = c(cor(d$litres_pc, d$gdp_pc),
        with(filter(d, Year <= 2019), cor(litres_pc, gdp_pc)),
        with(filter(d, Year >= 2021), cor(litres_pc, gdp_pc)))
) |> print()


# -----------------------------------------------------------------------------
# 3. Related goods: is wine a substitute?
# -----------------------------------------------------------------------------
d |> select(Year, litres_pc, on_pc, off_pc, beer_wine, real_wine) |> print()

c(r_beer_wine_ratio = cor(d$litres_pc, d$beer_wine),
  r_real_wine       = cor(d$litres_pc, d$real_wine)) |> print()
