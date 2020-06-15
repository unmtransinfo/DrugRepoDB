library(data.table)
library(DT)
library(ggplot2)
library(plotly)

########
# Load Data
##
load('R/drugrepodb/data/drugrepodb.RData')

status_dt <- drug_dt[, .(count = .N), by=status]

ggplot(status_dt, aes(factor(status), count)) +
  geom_bar(stat='identity', width=0.5, aes(fill=factor(status))) +
  geom_text(aes(label=count), vjust = 1.5, color=c('black', 'white', 'white', 'white'))+
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.position='none') +
  scale_fill_manual(values=c('grey', 'black', 'black', 'black'))

###
status_counts <- data.table::dcast(drug_dt[, .(count = .N), by=c("status", "sem_type")], status ~ sem_type,  value.var = "count")

plot_ly(type="bar", orientation="v", data=status_counts, x=~status, y=~`Acquired Abnormality`, name="Acquired Abnormality") %>%
  add_trace(y=~`Disease or Syndrome`, name="Disease or Syndrome") %>%
  add_trace(y=~`Sign or Symptom`, name="Sign or Symptom") %>%
  add_trace(y=~`Neoplastic Process`, name="Neoplastic Process") %>%
  add_trace(y=~`Cell or Molecular Dysfunction`, name="Cell or Molecular Dysfunction") %>%
  add_trace(y=~`Congenital Abnormality`, name="Congenital Abnormality") %>%
  add_trace(y=~`Finding`, name="Finding") %>%
  add_trace(y=~`Injury or Poisoning`, name="Injury or Poisoning") %>%
  add_trace(y=~`Mental or Behavioral Dysfunction`, name="Mental or Behavioral Dysfunction") %>%
  add_trace(y=~`Pathologic Function`, name="Pathologic Function") %>%
  layout(barmode="stack", yaxis=list(title="Count"), 
         title = paste0("DrugCentral indication counts (N = ", nrow(drug_dt), ")"),
       font=list(family="Arial", size=14),
       legend=list(x=.8, y=1.1),
       margin=list(t=100, l=20, b=20, r=20))
