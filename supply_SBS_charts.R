# =============================================================================
# Supply side of the French beer market: Eurostat Structural Business Statistics
# =============================================================================
#
# This script builds the three supply charts in the report (Figures 18-20) and
# downloads the French barley and hop prices we use for input costs.
#
# A note on the data: Eurostat changed how it collects business statistics in
# 2021. Up to 2020 the figures come from the old table (sbs_na_ind_r2); from
# 2021 they come from the new EBS table (sbs_ovw_act). The two are not fully
# comparable, so in every chart we draw a dashed line at the break and do not
# join the lines across it.
#
# Packages needed (run once if they are missing):
# install.packages(c("eurostat", "dplyr", "tidyr", "ggplot2", "patchwork"))

library(eurostat)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

dir.create("charts", showWarnings = FALSE)

# We compare beer (C11.05) with the wider beverages industry (C11)
sectors <- c("C11", "C1105")
sector_names <- c(C1105 = "Manufacture of beer (C11.05)",
                  C11   = "Manufacture of beverages (C11)")
sector_colours <- c("Manufacture of beer (C11.05)"   = "#0072B2",
                    "Manufacture of beverages (C11)" = "#555555")


# -----------------------------------------------------------------------------
# 1. Download the business statistics
# -----------------------------------------------------------------------------

# Old methodology, 2016-2020. The V-codes are Eurostat's indicator codes:
#   V11110 enterprises, V16110 persons employed, V16130 employees,
#   V13320 wages and salaries, V91110 labour productivity,
#   V94415 investment / value added, V12120 production value,
#   V13110 purchases, V13310 personnel costs, V20110 energy purchases,
#   V92110 gross operating rate (V12150 and V15110 are downloaded but not used)
old_sbs <- get_eurostat("sbs_na_ind_r2", time_format = "num",
                        filters = list(geo = "FR", nace_r2 = sectors,
                                       indic_sb = c("V11110", "V16110", "V16130", "V13320",
                                                    "V12150", "V91110", "V15110", "V94415",
                                                    "V12120", "V13110", "V13310", "V20110",
                                                    "V92110")))

# New methodology (EBS), 2021 onwards. Here the codes are easier to read.
new_sbs <- get_eurostat("sbs_ovw_act", time_format = "num",
                        filters = list(geo = "FR", nace_r2 = sectors,
                                       indic_sbs = c("ENT_NR", "EMP_NR", "SAL_NR", "HW_NR",
                                                     "WAGE_MEUR", "LABPRY_TEUR", "AV_MEUR",
                                                     "GRSINV_TNCA_AV_PC", "GRSINV_MAC_MEUR",
                                                     "VAL_OUT_MEUR", "PUR_MEUR",
                                                     "EXPN_SAL_BEN_MEUR", "PUR_NRG_MEUR",
                                                     "GOR_PC")))

# Put both tables into the same shape, with the same column names:
#   enterprises, employed, hours, wage (EUR thousand per employee),
#   productivity (EUR thousand), investment shares (% of value added),
#   cost shares (% of production value) and gross operating rate.
old_clean <- old_sbs %>%
  filter(time >= 2016, time <= 2020) %>%
  select(nace_r2, indicator = indic_sb, time, values) %>%
  pivot_wider(names_from = indicator, values_from = values) %>%
  transmute(nace_r2, time,
            enterprises   = V11110,
            employed      = V16110,
            hours         = NA_real_,   # not available before 2021
            wage          = 1000 * V13320 / V16130,
            productivity  = V91110,
            investment    = V94415,
            invest_mach   = NA_real_,   # not available before 2021
            purchases_sh  = 100 * V13110 / V12120,
            labour_sh     = 100 * V13310 / V12120,
            energy_sh     = 100 * V20110 / V12120,
            gross_op_rate = V92110)

new_clean <- new_sbs %>%
  filter(time >= 2021) %>%
  select(nace_r2, indicator = indic_sbs, time, values) %>%
  pivot_wider(names_from = indicator, values_from = values) %>%
  transmute(nace_r2, time,
            enterprises   = ENT_NR,
            employed      = EMP_NR,
            hours         = HW_NR,
            wage          = 1000 * WAGE_MEUR / SAL_NR,
            productivity  = LABPRY_TEUR,
            investment    = GRSINV_TNCA_AV_PC,
            invest_mach   = 100 * GRSINV_MAC_MEUR / AV_MEUR,
            purchases_sh  = 100 * PUR_MEUR / VAL_OUT_MEUR,
            labour_sh     = 100 * EXPN_SAL_BEN_MEUR / VAL_OUT_MEUR,
            energy_sh     = 100 * PUR_NRG_MEUR / VAL_OUT_MEUR,
            gross_op_rate = GOR_PC)

# Stack the two periods and add a few derived measures
sbs <- bind_rows(old_clean, new_clean) %>%
  mutate(emp_per_firm = employed / enterprises,
         prod_to_wage = 100 * productivity / wage,
         period       = ifelse(time <= 2020, "legacy", "EBS"),
         series       = sector_names[nace_r2])

write.csv(sbs, "charts/supply_sbs_series.csv", row.names = FALSE)

# Quick look at the beer figures in the console
sbs %>%
  filter(nace_r2 == "C1105") %>%
  select(time, enterprises, employed, emp_per_firm, hours, wage, productivity,
         prod_to_wage, investment, purchases_sh, labour_sh, energy_sh, gross_op_rate) %>%
  print()


# -----------------------------------------------------------------------------
# 2. A helper that draws one panel
# -----------------------------------------------------------------------------
# Every panel has the same look: beer vs beverages, a dashed line at the 2020/21
# methodology break, and lines grouped by period so they stop at the break.

source_note <- "Source: Eurostat sbs_na_ind_r2 (2016-2020, legacy) and sbs_ovw_act (2021-2024, EBS), France."

make_panel <- function(variable, title, log_scale = FALSE, y_limits = NULL) {
  p <- ggplot(sbs, aes(x = time, y = .data[[variable]],
                       colour = series, shape = series, linetype = series,
                       group = interaction(series, period))) +
    geom_vline(xintercept = 2020.5, linetype = "dashed", linewidth = 0.4) +
    annotate("text", x = 2020.6, y = Inf, label = "method\nbreak",
             hjust = 0, vjust = 1.1, size = 2.6) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_point(size = 1.8, na.rm = TRUE) +
    scale_colour_manual(values = sector_colours) +
    scale_linetype_manual(values = setNames(c("solid", "dashed"), names(sector_colours))) +
    scale_x_continuous(breaks = 2016:2024) +
    labs(title = title, x = NULL, y = NULL, colour = NULL, shape = NULL, linetype = NULL) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position    = "bottom",
          plot.title         = element_text(face = "bold", size = 10))

  if (log_scale) {
    p <- p + scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale()))
  }
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  p
}


# -----------------------------------------------------------------------------
# Chart 1 (Figure 18): number of firms, employment, firm size and hours
# -----------------------------------------------------------------------------
chart1 <- (make_panel("enterprises", "(a) Active enterprises (log)", log_scale = TRUE) +
           make_panel("employed", "(b) Persons employed (log)", log_scale = TRUE)) /
          (make_panel("emp_per_firm", "(c) Persons employed per enterprise", y_limits = c(0, 25)) +
           make_panel("hours", "(d) Hours worked by employees (log; 2021- only)", log_scale = TRUE)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("charts/S1_SBS_labour_size_R.png",
       chart1 + plot_annotation(title = "Labour and firm size in French brewing", caption = source_note),
       width = 11, height = 7.6, dpi = 200, bg = "white")


# -----------------------------------------------------------------------------
# Chart 2 (Figure 19): wages, productivity and the ratio between them
# -----------------------------------------------------------------------------
chart2 <- make_panel("wage", "(a) Average wage per employee, EUR k", y_limits = c(0, 60)) +
  make_panel("productivity", "(b) Apparent labour productivity, EUR k", y_limits = c(0, 170)) +
  make_panel("prod_to_wage", "(c) Productivity / wage, %", y_limits = c(0, 450)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("charts/S2_SBS_wage_productivity_R.png",
       chart2 + plot_annotation(title = "Wages, productivity and their ratio", caption = source_note),
       width = 12, height = 4.8, dpi = 200, bg = "white")


# -----------------------------------------------------------------------------
# Chart 3 (Figure 20): how much the industry invests
# -----------------------------------------------------------------------------
# Solid/dashed lines: all tangible investment. Dotted: machinery only (2021 on).
chart3 <- make_panel("investment",
                     "Gross investment in tangible goods / value added, % (dotted: machinery only, 2021-)",
                     y_limits = c(0, 36)) +
  geom_line(aes(y = invest_mach), linetype = "dotted", linewidth = 0.8, na.rm = TRUE) +
  geom_point(aes(y = invest_mach), shape = 1, na.rm = TRUE)

ggsave("charts/S3_SBS_investment_R.png", chart3 + labs(caption = source_note),
       width = 9.5, height = 5, dpi = 200, bg = "white")


# -----------------------------------------------------------------------------
# 3. Input prices: French barley and hops
# -----------------------------------------------------------------------------
# These could not be downloaded when the report was first built, so we fetch
# them here. We search the product labels for "barley" and "hops" rather than
# hard-coding Eurostat's product codes.

# (a) Selling prices in EUR per 100 kg. Note: for France these stop in 2016.
crop_prices <- get_eurostat("apri_ap_crpouta", time_format = "num", filters = list(geo = "FR"))
crop_prices <- label_eurostat(crop_prices, fix_duplicated = TRUE, code = "prod_veg")

print(unique(crop_prices$prod_veg[grepl("barley|hops", crop_prices$prod_veg, ignore.case = TRUE)]))

barley_hops <- crop_prices %>%
  filter(grepl("barley|hops", prod_veg, ignore.case = TRUE), time >= 2016)
write.csv(barley_hops, "charts/FR_barley_hops_prices.csv", row.names = FALSE)

# (b) Price indices, which do cover recent years. The old apri_pi20_outa table
# has been retired; apri_pi_outa now holds every base year. We keep two:
#   I15 = 2015 base (runs to about 2022-23) and I20 = 2020 base (2020-2025),
# so together they cover the whole 2016-2025 period.
price_index <- get_eurostat("apri_pi_outa", time_format = "num",
                            filters = list(geo = "FR", unit = c("I15", "I20")))
price_index <- label_eurostat(price_index, fix_duplicated = TRUE, code = "am_item")

price_index %>%
  filter(grepl("barley|hops|cereal", am_item, ignore.case = TRUE), time >= 2016) %>%
  write.csv("charts/FR_barley_hops_price_indices.csv", row.names = FALSE)
