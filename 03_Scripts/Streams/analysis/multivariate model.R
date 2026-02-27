source("03_Scripts/Streams/analysis/data for analysis.R")
library(posterior)
library(patchwork)
library(brms)


df2<-int.ext%>%
  left_join(DO%>%
              mutate(Date=as.Date(Date),
                     TempC=fahrenheit.to.celsius(Temp_DO),
                     )%>%
              group_by(Date, ID) %>%
              summarise(
                across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                .groups = "drop"
              ) %>%
              select(Date, ID, TempC),
            by=c('Date','ID'))%>%
  drop_na(CO2_flux, Q)%>%
  mutate(
    lQ=log10(Q),
    lext=log10(external),
    lint=log10(internal)
  )


df2 <- df2 %>%
  filter(
    is.finite(lQ), is.finite(TempC), is.finite(lint), is.finite(lext)) %>%
  droplevels()

#formulas##########
bf_int_full <- bf(lint ~ lQ + TempC + (1 | ID))
bf_ext_full <- bf(lext ~ lQ + TempC + (1 | ID))

# Drop TEMP 
bf_int_noT  <- bf(lint ~ lQ + (1 | ID))
bf_ext_noT  <- bf(lext ~ lQ + (1 | ID))

# Drop Q 
bf_int_noQ  <- bf(lint ~ TempC + (1 | ID))
bf_ext_noQ  <- bf(lext ~ TempC + (1 | ID))

# models
pri <- tryCatch(prior_summary(fit_full), error = function(e) NULL)

fit <- brm(
  bf_int_full + bf_ext_full + set_rescor(TRUE),
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models"
)


#remove temp######
fit_int_noT <- brm(
  bf_int_noT + bf_ext_full + set_rescor(TRUE),
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  iterations =1000,
  file = "04_Output/stream/models"
)

fit_ext_noT <- brm(
  bf_ext_noT + bf_int_full + set_rescor(TRUE),
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  iterations =1000,
  file = "04_Output/stream/models"
)


#remove Q############
fit_int_noQ <- brm(
  bf_int_noQ + bf_ext_full + set_rescor(TRUE),
  data = df2c,
  family = student(),
  prior = pri,
  cores = 4,
  iterations =1000,
  file = "04_Output/stream/models"
)

fit_ext_noQ <- brm(
  bf_int_full + bf_ext_noQ + set_rescor(TRUE),
  data = df2,
  family = student(),
  prior = pri,
  cores = 4,
  file = "04_Output/stream/models"
)

# 3) Compute Bayes R² draws per response#######
R2 <- function(fit, resp) {
  as.numeric(bayes_R2(fit, resp = resp))
}

R2_int_full <- R2(fit_full, "lint")
R2_ext_full <- R2(fit_full, "lext")

R2_int_noT  <- R2(fit_int_noT, "lint")
R2_int_noQ  <- R2(fit_int_noQ, "lint")

R2_ext_noT  <- R2(fit_ext_noT, "lext")
R2_ext_noQ  <- R2(fit_ext_noQ, "lext")

# ----------------------------
# 4) Unique + shared variance decomposition (posterior draws)
# ----------------------------
# Unique contribution = R²_full - R²_reduced
unique_T_int <- pmax(0, R2_int_full - R2_int_noT)
unique_Q_int <- pmax(0, R2_int_full - R2_int_noQ)

unique_T_ext <- pmax(0, R2_ext_full - R2_ext_noT)
unique_Q_ext <- pmax(0, R2_ext_full - R2_ext_noQ)

# Shared contribution (overlap due to predictor covariance)
shared_int <- pmax(0, R2_int_full - unique_T_int - unique_Q_int)
shared_ext <- pmax(0, R2_ext_full - unique_T_ext - unique_Q_ext)

# Bundle into long draws dataframe for plotting
draws_long <- bind_rows(
  tibble(pathway = "Internal", component = "Unique log(Q)", value = unique_Q_int),
  tibble(pathway = "Internal", component = "Unique Temp",   value = unique_T_int),
  tibble(pathway = "Internal", component = "Shared",        value = shared_int),
  tibble(pathway = "External", component = "Unique log(Q)", value = unique_Q_ext),
  tibble(pathway = "External", component = "Unique Temp",   value = unique_T_ext),
  tibble(pathway = "External", component = "Shared",        value = shared_ext)
) %>%
  mutate(
    component = factor(component, levels = c("Unique log(Q)", "Unique Temp", "Shared")),
    pathway = factor(pathway, levels = c("Internal", "External"))
  )

# Also compute full R² for reference
R2_full_long <- bind_rows(
  tibble(pathway = "Internal", component = "Total R²", value = R2_int_full),
  tibble(pathway = "External", component = "Total R²", value = R2_ext_full)
) %>%
  mutate(pathway = factor(pathway, levels = c("Internal", "External")))

# ----------------------------
# 5) Summaries table (mean + 95% CrI)
# ----------------------------
summ_ci <- function(x) {
  tibble(
    mean = mean(x),
    q2.5 = quantile(x, 0.025),
    q97.5 = quantile(x, 0.975)
  )
}

summary_table <- draws_long %>%
  group_by(pathway, component) %>%
  summarise(
    mean = mean(value),
    q2.5 = quantile(value, 0.025),
    q97.5 = quantile(value, 0.975),
    .groups = "drop"
  ) %>%
  arrange(pathway, component)

print(summary_table)

# ----------------------------
# 6) Conference-ready figure
#    Panel A: Stacked bars (means) with error bars for each component
#    Panel B: Total R² per pathway (point + interval)
# ----------------------------

# Data for stacked means
means_df <- draws_long %>%
  group_by(pathway, component) %>%
  summarise(
    mean = mean(value),
    q2.5 = quantile(value, 0.025),
    q97.5 = quantile(value, 0.975),
    .groups = "drop"
  ) %>%
  group_by(pathway) %>%
  arrange(component, .by_group = TRUE) %>%
  mutate(
    ymin_stack = cumsum(lag(mean, default = 0)),
    ymax_stack = ymin_stack + mean
  ) %>%
  ungroup()

p_stack <- ggplot(means_df, aes(x = pathway, y = mean, fill = component)) +
  geom_col(width = 0.7, color = "black") +
  # Optional: show component-level 95% CrI as error bars at the top of each stack segment
  geom_errorbar(
    aes(ymin = ymin_stack + q2.5, ymax = ymin_stack + q97.5),
    width = 0.15,
    linewidth = 0.7
  ) +
  labs(
    title = "Unique vs shared variance explained (ΔBayes R²)",
    subtitle = "Unique contributions from dropped-model comparisons; Shared reflects overlap due to predictor covariance",
    x = NULL, y = "Proportion of variance explained (R² decomposition)",
    fill = NULL
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )

# Total R² intervals
R2_summ <- R2_full_long %>%
  group_by(pathway) %>%
  summarise(
    mean = mean(value),
    q2.5 = quantile(value, 0.025),
    q97.5 = quantile(value, 0.975),
    .groups = "drop"
  )

p_total <- ggplot(R2_summ, aes(x = pathway, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.15, linewidth = 0.8) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Total explained variance",
    subtitle = "Bayesian R² (posterior mean ± 95% CrI)",
    x = NULL, y = "Total R²"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )

final_fig <- p_stack | p_total
final_fig

ggsave(
  filename = "FIG_R2_decomposition_internal_external.png",
  plot = final_fig,
  width = 14, height = 6.5, units = "in", dpi = 320
)

# ----------------------------
# 7) OPTIONAL: also report predictor covariance (simple correlation)
# ----------------------------
cov_overall <- cor(df2c$lQ, df2c$TempC, use = "complete.obs")
cov_by_site <- df2c %>%
  group_by(ID) %>%
  summarise(n = n(), r = cor(lQ, TempC, use = "complete.obs"), .groups = "drop")

print(paste("Overall cor(logQ, TempC) =", round(cov_overall, 3)))
print(cov_by_site)



#poster figure##########
df<-left_join(int.ext, temperature)%>%
  mutate(
    TempC=fahrenheit.to.celsius(Temp_PT),
    ID = factor(as.character(ID), levels = facet_order)
  )%>%
  drop_na(TempC, Q, internal, external)%>%
  group_by(ID)%>%
  mutate(
    class=
      case_when(
        Q>mean(Q, na.rm=T) & TempC>mean(TempC, na.rm=T)~'high flow, warm',
        Q<mean(Q, na.rm=T) & TempC<mean(TempC, na.rm=T)~'low flow, cool',
        Q>mean(Q, na.rm=T) & TempC<mean(TempC, na.rm=T)~'high flow, cool',
        Q<mean(Q, na.rm=T) & TempC>mean(TempC, na.rm=T)~'low flow, warm'
      ),
    temp.class=
      case_when(
        TempC>mean(TempC, na.rm=T)~'warm',
        TempC<mean(TempC, na.rm=T)~'cool')
  )%>%ungroup()%>%
  group_by(class, ID)%>%
  summarise(
    Internal=mean(internal, na.rm=T),
    External=mean(external, na.rm=T),
    Total=mean(CO2_flux, na.rm=T))#%>%
pivot_longer(
  cols      = c(Internal, External, Total),
  names_to  = "Pathway",
  values_to = "flux"
)


df$class <- factor(df$class, levels = c('high flow, warm', 'low flow, warm', 'high flow, cool', 'low flow, cool'))

df$class <- factor(df$class, levels = c('high flow, warm', 'high flow, cool', 'low flow, warm', 'low flow, cool'))

common.list<-list(
  geom_jitter(),
  ylab(expression(C~'g'/m^2/'day')),
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=13, color='black')
  ),
  scale_fill_manual(values=c('white', 'darkred', 'darkgray')),
  scale_y_log10()
)

plot_grid(
  df%>%
    ggplot(aes(
      x=class,
      y = flux,
      fill=Pathway)) +
    geom_boxplot()+
    ggtitle("Mean Internal Pathway Response to Flow and Temperature")+
    common.list
  ,
  df%>%
    ggplot(aes(
      x=class,
      y = flux,
      fill = Pathway)) +
    geom_boxplot()+
    ggtitle("Mean External Pathway Response to Flow and Temperature")+
    common.list,
  ncol=1
)

