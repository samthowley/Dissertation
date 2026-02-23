source("03_Scripts/Streams/analysis/data for analysis.R")
library(brms)

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
    Total=mean(CO2_flux, na.rm=T))%>%
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



Ddf2 <- df %>%
  mutate(
    lQ  = log(Q),
    lint = log(internal),
    lext = log(external)
  )

bf_int <- bf(lint ~ lQ + TempC + (1 | ID))
bf_ext <- bf(lext ~ lQ + TempC + (1 | ID))

fit <- brm(
  bf_int + bf_ext + set_rescor(TRUE),
  data = df2,
  family = student(),
  cores = 4,
  iter = 1000
)

