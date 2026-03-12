source("03_Scripts/Streams/analysis/data for analysis.R")
library(nlme)
library(performance)

int.ext <- int.ext %>%
  left_join(temperature)%>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  mutate(hour_index = row_number()) %>%
  ungroup()%>%
  distinct(ID, Date, .keep_all = T)

#figure#####
# Extract model statistics
slope     <- round(fixef(model)["log10(Q)"], 3)
pval      <- summary(model)$tTable["log10(Q)", "p-value"]
pval_lab  <- ifelse(pval < 0.0001, "p < 0.0001", paste0("p = ", round(pval, 4)))
phi_lab   <- round(0.4781408, 2)  # AR(1) phi
r2_vals   <- r2(model)
r2_margin <- round(r2_vals$R2_marginal, 3)
r2_cond   <- round(r2_vals$R2_conditional, 3)
# Build annotation string
stats_label <- paste0(
  "β = ", slope, "\n",
  pval_lab, "\n",
  "AR(1)= ", phi_lab, "\n",
  "R²m = ", r2_margin, ", R²c = ", r2_cond
)

ggplot(int.ext,
       aes(x = Q, y = internal, group = ID, color = ID)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm, se = FALSE) +
  scale_y_log10() + scale_x_log10() +
  annotate("text", x = Inf, y = Inf, label = stats_label,
           hjust = 1.1, vjust = 1.5, size = 3.5, fontface = "italic") +
  ggtitle(expression('Internal'~'Pathway'~'Responses'~'to'~'Discharge')) +
  ylab(expression(CO[2]~'g'/m^2/'day')) +
  xlab(expression('Discharge'~'L'~s^-1)) +
  theme_classic()


#internal~Q#################
model <- lme(
  fixed       = log10(internal) ~ log10(Q),
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)

#internal~T#################
int.ext<-left_join(int.ext, temperature)%>%
  drop_na(Temp_PT)%>%
  distinct(Date, ID, .keep_all = T)

model <- lme(
  fixed       = internal ~ TempC,
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)

#external~Q#################
model <- lme(
  fixed       = log10(external) ~ log10(Q),
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)

#external~T#################
int.ext<-left_join(int.ext, temperature)%>%
  drop_na(Temp_PT)%>%
  distinct(Date, ID, .keep_all = T)

model <- lme(
  fixed       = external ~ TempC,
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)
#CO2~Q#####
model <- lme(
  fixed       = log10(CO2) ~ log10(Q),
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)


#CO2 flux~Q#####
model <- lme(
  fixed       = log10(CO2_flux) ~ log10(Q),
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)


#CO2~T#####

int.ext<-int.ext%>%drop_na(TempC)
model <- lme(
  fixed       = CO2 ~ TempC,
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)


#CO2 flux~T#####
model <- lme(
  fixed       = CO2_flux ~ TempC,
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)


#int.ext.ratio~Q#################
model <- lme(
  fixed       = log10(int.ext.ratio) ~ log10(Q),
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)



#int.ext.ratio~T#################
int.ext<-left_join(int.ext, temperature)%>%
  drop_na(Temp_PT)%>%
  distinct(Date, ID, .keep_all = T)

model <- lme(
  fixed       = int.ext.ratio ~ TempC,
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)
