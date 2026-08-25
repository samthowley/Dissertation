
library(tidyverse)
library(readxl)

wb_path      <- "01_Raw_data/rating curves_clean.xlsx"
offsets_path <- "scratch pad/stage_offset_schedule.csv"

#### rating-curve parameters (Summarized-tab vintage) ####
params <- tribble(
  ~ID,  ~a,          ~b,        ~h0,
  "3",  393.169842,  2.566649,  0,     # until 2022-11-14
  "5",  1340.054121, 1.398252,  0,
  "5a", 173.44,      1.1165,    0.3,
  "6",  621.352764,  1.347599,  0,
  "6a", 1264.678333, 2.957642,  0,
  "7",  1780.837932, 3.068458,  0,
  "9",  1259.610691, 1.748390,  0,
  "13", 1060.565024, 2.204870,  0,
  "15", 2194.324008, 2.889457,  0
)
p3B <- list(a = 433.61, b = 2.5461)    # site 3 from 2022-11-15 on

#### stage offsets: his reference = her depth + offset(t) ####
offsets <- read_csv(offsets_path, show_col_types = FALSE) %>%
  mutate(ID = as.character(ID))

get_offset <- function(id, dates) {
  ss <- offsets %>% filter(ID == id) %>% arrange(from)
  if (nrow(ss) == 0) return(rep(0, length(dates)))
  idx <- findInterval(as.numeric(dates), as.numeric(ss$from))
  ss$offset[pmax(idx, 1)]          # before first segment: use first offset
}                                  # after last segment: carry last forward

#### read Matt's vetted Summarized tab ####
sm  <- read_excel(wb_path, sheet = "Summarized", .name_repair = "minimal")
nms <- names(sm)
site_starts <- which(str_detect(nms, "^Site "))
summ <- map_dfr(site_starts, function(st) {
  id <- str_remove(nms[st], "^Site ")
  block <- sm[, st:(st + 6)]
  bn <- names(block)
  b <- block[, c(which(str_detect(bn, "^Date"))[1],
                 which(str_detect(bn, regex("^Q ", ignore_case = TRUE)))[1])]
  names(b) <- c("Date", "Q")
  b %>% mutate(Date = as.Date(Date), Q = as.numeric(Q), ID = id) %>%
    filter(!is.na(Date), !is.na(Q))
})

#### compute RC discharge from depth ####
depth <- read_csv("02_Clean_data/depth.csv", show_col_types = FALSE) %>%
  mutate(ID = as.character(ID))%>%
  filter(!is.na(depth), ID %in% params$ID)

off_tbl <- depth %>% distinct(ID, Date) %>%
  group_by(ID) %>%
  group_modify(~ mutate(.x, off = get_offset(.y$ID, Date))) %>%
  ungroup()%>%
  filter(ID != '14')

q15 <- depth %>%
  left_join(params, by = "ID") %>%
  left_join(off_tbl, by = c("ID", "Date")) %>%
  mutate(a = if_else(ID == "3" & Date >= as.Date("2022-11-15"), p3B$a, a),
         b = if_else(ID == "3" & Date >= as.Date("2022-11-15"), p3B$b, b),
         stage = depth + off,
         Q = if_else(stage > h0, a * (stage - h0)^b, 0)) %>%
  select(Date, ID, Q)


write_csv(q15%>%filter(Q>=0.01), "02_Clean_data/discharge.csv")



#### figures ####
ggplot(q15 %>% filter(ID %in% c('5','15','5a','9','13')),
       aes(Date, Q, color = ID)) +
  geom_line(linewidth = 0.4) +
  scale_y_log10() +
  ylab("Discharge (L/s)") +
  scale_color_brewer(palette = "Set1") +
  ggtitle("South Basin — advisor RC")

ggplot(discharge_daily %>% filter(ID %in% c('7','3','6a','6')),
       aes(Date, Q, color = ID)) +
  geom_line(linewidth = 0.4) +
  scale_y_log10() +
  ylab("Discharge (L/s)") +
  scale_color_brewer(palette = "Set1") +
  ggtitle("North Basin — advisor RC")
