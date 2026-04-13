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

watershed.inundation<-watershed.inundation%>%
  select(ID, total.wetland.inun, contrib.wetland.inun, Date)
         
#figure#####

plot_lme_results <- function(model,        # fitted lme model
                             data,         # dataframe used in model
                             x_var,        # x variable name as string e.g. "Q"
                             y_var,        # y variable name as string e.g. "internal"
                             group_var,    # grouping variable as string e.g. "ID"
                             fixed_var,    # fixed effect name as in model e.g. "log10(Q)"
                             x_lab,        # x axis label expression
                             y_lab,        # y axis label expression
                             title         # plot title expression
) {
  
  # Extract model statistics
  slope     <- round(fixef(model)[fixed_var], 3)
  pval      <- summary(model)$tTable[fixed_var, "p-value"]
  pval_lab  <- ifelse(pval < 0.0001, "p < 0.0001", paste0("p = ", round(pval, 4)))
  phi_lab   <- round(coef(model$modelStruct$corStruct, unconstrained = FALSE), 2)
  r2_vals   <- r2(model)
  r2_margin <- round(r2_vals$R2_marginal, 3)
  r2_cond   <- round(r2_vals$R2_conditional, 3)
  
  stats_label <- paste0(
    "β = ", slope, "\n",
    pval_lab, "\n",
    "AR(1) φ = ", phi_lab, "\n",
    "R²m = ", r2_margin, ", R²c = ", r2_cond
  )
  
  ggplot(data,
         aes(x = .data[[x_var]], y = .data[[y_var]],
             group = .data[[group_var]], color = .data[[group_var]])) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = lm, se = FALSE) +
    scale_y_log10() + #scale_x_log10() +
    # annotate("text", x=0.05, y = 30, label = stats_label,
    #          hjust = 0, vjust = 1.5, size = 3.5, fontface = "italic") +
    annotate("text", x=-0.5, y = Inf, label = stats_label,
             hjust = 0, vjust = 1.5, size = 3.5, fontface = "italic") +
    
    labs(title = title, x = x_lab, y = y_lab) +
    theme_classic()
}




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


a<-plot_lme_results(
  model     = model,
  data      = int.ext,
  x_var     = "TempC",
  y_var     = "internal",
  group_var = "ID",
  fixed_var = "TempC",
  x_lab     = expression("Temperature"),
  y_lab     = expression(CO[2] ~ "g/" ~ m^2 / "day"),
  title     = expression("Internal Response to Temperature")
)


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

b<-plot_lme_results(
  model     = model,
  data      = int.ext,
  x_var     = "TempC",
  y_var     = "external",
  group_var = "ID",
  fixed_var = "TempC",
  x_lab     = expression("Temperature"),
  y_lab     = expression(CO[2] ~ "g/" ~ m^2 / "day"),
  title     = expression("External Response to Temperature")
)


plot_grid(a,b)

#int.ext.ratio~T#################

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


c<-plot_lme_results(
  model     = model,
  data      = int.ext,
  x_var     = "TempC",
  y_var     = "int.ext.ratio",
  group_var = "ID",
  fixed_var = "TempC",
  x_lab     = expression("Temperature"),
  y_lab     = expression("Internal/External"),
  title     = expression("Internal-External Ratio Response to Temperature")
)


plot_grid(a,b,c, ncol=1)

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


a<-plot_lme_results(
  model     = model,
  data      = int.ext,
  x_var     = "Q",
  y_var     = "internal",
  group_var = "ID",
  fixed_var = "log10(Q)",
  x_lab     = expression("Discharge L" ~ s^-1),
  y_lab     = expression(CO[2] ~ "g/" ~ m^2 / "day"),
  title     = expression("Internal Pathway Responses to Discharge")
)

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


b<-plot_lme_results(
  model     = model,
  data      = int.ext,
  x_var     = "Q",
  y_var     = "external",
  group_var = "ID",
  fixed_var = "log10(Q)",
  x_lab     = expression("Discharge L" ~ s^-1),
  y_lab     = expression(CO[2] ~ "g/" ~ m^2 / "day"),
  title     = expression("External Pathway Responses to Discharge")
)
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


c<-plot_lme_results(
  model     = model,
  data      = int.ext,
  x_var     = "Q",
  y_var     = "int.ext.ratio",
  group_var = "ID",
  fixed_var = "log10(Q)",
  x_lab     = expression("Discharge L" ~ s^-1),
  y_lab     = expression(CO[2] ~ "g/" ~ m^2 / "day"),
  title     = expression("Internal-External Ratio Response to Discharge")
)

plot_grid(a,b,c, ncol=1)
#int.ext.ratio~total.basin.inundation#################

int.ext.inun<-int.ext%>%
  select(ID, external, internal,int.ext.ratio, Date, hour_index)%>%
  mutate(Date=as.Date(Date))%>%
  left_join(watershed.inundation)%>%
  arrange(ID, Date)%>%
  drop_na(total.wetland.inun)


model <- lme(
  fixed       = log10(int.ext.ratio) ~ log10(total.wetland.inun),
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext.inun,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)


a<-plot_lme_results(
  model     = model,
  data      = int.ext.inun,
  x_var     = "total.wetland.inun",
  y_var     = "int.ext.ratio",
  group_var = "ID",
  fixed_var = "log10(total.wetland.inun)",
  x_lab     = expression('Watershed Inundation'~'(Basin Wetland Percent * Mean Watertable Depth)'),
  y_lab     = expression(CO[2] ~ "g/" ~ m^2 / "day"),
  title     = expression("Internal-External Ratio Response to Watershed Inundation")
)

#internal~total.basin.inundation#################

model <- lme(
  fixed       = log10(internal) ~ total.wetland.inun,
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext.inun,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)

b<-plot_lme_results(
  model     = model,
  data      = int.ext.inun,
  x_var     = "total.wetland.inun",
  y_var     = "internal",
  group_var = "ID",
  fixed_var = "total.wetland.inun",
  x_lab     = expression('Watershed Inundation'~'(Basin Wetland Percent * Mean Watertable Depth)'),
  y_lab     = expression(CO[2] ~ "g/" ~ m^2 / "day"),
  title     = expression("Internal Pathway Response to Watershed Inundation")
)


#external~total.basin.inundation#################
model <- lme(
  fixed       = log10(external) ~ total.wetland.inun,
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext.inun,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)



c<-plot_lme_results(
  model     = model,
  data      = int.ext.inun,
  x_var     = "total.wetland.inun",
  y_var     = "external",
  group_var = "ID",
  fixed_var = "total.wetland.inun",
  x_lab     = expression('Watershed Inundation'~'(Basin Wetland Percent * Mean Watertable Depth)'),
  y_lab     = expression(CO[2] ~ "g/" ~ m^2 / "day"),
  title     = expression("External Pathway Response to Watershed Inundation")
)


plot_grid(a, b, c, ncol=1)


#internal~contrib.basin.inundation#################

model <- lme(
  fixed       = log10(internal) ~ contrib.basin.inundation,
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext.inun,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)

a<-plot_lme_results(
  model     = model,
  data      = int.ext.inun,
  x_var     = "contrib.basin.inundation",
  y_var     = "internal",
  group_var = "ID",
  fixed_var = "contrib.basin.inundation",
  x_lab     = expression('Watershed Inundation'~'(Contributing Area Wetland Percent * Mean Watertable Depth)'),
  y_lab     = expression(CO[2] ~ "g/" ~ m^2 / "day"),
  title     = expression("Internal Pathway Response to Watershed Inundation")
)


#external~contrib.basin.inundation#################

model <- lme(
  fixed       = log10(external) ~ contrib.basin.inundation,
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext.inun,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)


b<-plot_lme_results(
  model     = model,
  data      = int.ext.inun,
  x_var     = "contrib.basin.inundation",
  y_var     = "external",
  group_var = "ID",
  fixed_var = "contrib.basin.inundation",
  x_lab     = expression('Watershed Inundation'~'(Contributing Area Wetland Percent * Mean Watertable Depth)'),
  y_lab     = expression(CO[2] ~ "g/" ~ m^2 / "day"),
  title     = expression("External Pathway Response to Watershed Inundation")
)


#int.ext.ratio~contrib.basin.inundation#################

model <- lme(
  fixed       = log10(int.ext.ratio) ~contrib.basin.inundation,
  random      = ~ 1 | ID,
  correlation = corAR1(form = ~ hour_index | ID),
  data        = int.ext.inun,
  method      = "REML"
)

summary(model)
anova(model)
r2(model)


c<-plot_lme_results(
  model     = model,
  data      = int.ext.inun,
  x_var     = "total.basin.inundation",
  y_var     = "int.ext.ratio",
  group_var = "ID",
  fixed_var = "contrib.basin.inundation",
  x_lab     = expression('Watershed Inundation'~'(Contributing Area Wetland Percent * Mean Watertable Depth)'),
  y_lab     = expression(CO[2] ~ "g/" ~ m^2 / "day"),
  title     = expression("Internal-External Ratio Response to Watershed Inundation")
)

plot_grid(a, b, c, ncol=1)
