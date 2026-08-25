
# Run Apply_Advisor_RC.R first (needs discharge_daily_advisor.csv).

library(tidyverse)
library(lme4)

setwd("C:/Dissertation")

#### my rating curves, refit exactly as in Dilution Gaging.R ####
DG <- read_csv("04_Output/compiled_DG.csv", show_col_types = FALSE) %>%
  mutate(day = as.Date(Date))

depth_daily <- read_csv("02_Clean_data/depth.csv", show_col_types = FALSE) %>%
  mutate(day = as.Date(Date)) %>%
  group_by(day, ID) %>% summarize(depth = mean(depth, na.rm = TRUE), .groups = "drop")

DG_rC <- left_join(DG, depth_daily, by = c("ID", "day")) %>%
  select(Date, ID, Q, u_mean, m_0, m_1, depth) %>%
  mutate(logQ = log10(Q), logh = log10(depth)) %>%
  filter(!ID %in% c('14', '6.3', '9.2', '5.4'))

rC <- lmList(logQ ~ logh | ID, data = DG_rC)
cf <- coef(rC)
my_params <- tibble(ID = rownames(cf),
                    a_mine = 10^cf[, 1],
                    b_mine = cf[, 2])

#### Matt's curve parameters (Summarized tab) ####
matt_params <- tribble(
  ~ID,  ~a_matt,     ~b_matt,   ~h0,  ~note,
  "3",  393.169842,  2.566649,  0,    "until 2022-11-14; then a=433.61 b=2.5461",
  "5",  1340.054121, 1.398252,  0,    "",
  "5a", 173.44,      1.1165,    0.3,  "pre-2023 era",
  "6",  621.352764,  1.347599,  0,    "",
  "6a", 1264.678333, 2.957642,  0,    "",
  "7",  1780.837932, 3.068458,  0,    "",
  "9",  1259.610691, 1.748390,  0,    "",
  "13", 1060.565024, 2.204870,  0,    "",
  "15", 2194.324008, 2.889457,  0,    ""
)

coef_tbl <- full_join(my_params, matt_params, by = "ID") %>%
  select(ID, a_mine, b_mine, a_matt, b_matt, h0, note)

#### daily Q both ways ####
# mine: my curve on raw depth (as in my script)
q_mine <- read_csv("02_Clean_data/depth.csv", show_col_types = FALSE) %>%
  mutate(day = as.Date(Date)) %>%
  filter(!is.na(depth)) %>%
  inner_join(my_params, by = "ID") %>%
  mutate(Q = a_mine * depth^b_mine) %>%
  group_by(ID, day) %>%
  summarize(Q_mine = mean(Q), .groups = "drop")

# Matt: the spliced advisor product (his vetted dailies + RC extension)
q_matt <- read_csv("scratch pad/discharge_daily_advisor.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID)) %>%
  rename(day = Date, Q_matt = Q)

cmp <- inner_join(q_mine, q_matt, by = c("ID", "day")) %>%
  filter(Q_mine > 0, Q_matt > 0) %>%
  mutate(ratio = Q_mine / Q_matt)

#### figures ####
# both curves over my DG measurement points
curve_grid <- DG_rC %>% filter(!is.na(depth), Q > 0) %>%
  group_by(ID) %>%
  reframe(depth = seq(min(depth), max(depth), length.out = 50)) %>%
  inner_join(my_params, by = "ID") %>%
  left_join(matt_params, by = "ID") %>%
  mutate(mine = a_mine * depth^b_mine,
         Matt = if_else(depth > h0, a_matt * (depth - h0)^b_matt, NA_real_)) %>%
  pivot_longer(c(mine, Matt), names_to = "method", values_to = "Q")

ggplot() +
  geom_point(data = DG_rC %>% filter(Q > 0), aes(depth, Q), alpha = 0.6) +
  geom_line(data = curve_grid, aes(depth, Q, color = method)) +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~ID, scales = "free") +
  labs(x = "Depth (m)", y = "Q (L/s)",
       title = "Rating curves over my DG measurements")

# daily time series overlay
ggplot(cmp %>% pivot_longer(c(Q_mine, Q_matt), names_to = "method",
                            values_to = "Q"),
       aes(day, Q, color = method)) +
  geom_line(linewidth = 0.3) +
  scale_y_log10() +
  facet_wrap(~ID, scales = "free_y") +
  labs(x = NULL, y = "Q (L/s)", title = "Daily discharge, mine vs Matt")

# ratio over time
ggplot(cmp, aes(day, ratio)) +
  geom_line(linewidth = 0.3) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_y_log10() +
  facet_wrap(~ID) +
  labs(x = NULL, y = "Q_mine / Q_matt", title = "Where the methods diverge")
