# Libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Load and clean data -----------------------------------------------------
raw_data = read.csv('GDH2016_Daten_xlsx.csv', sep=';', stringsAsFactors = F)
colnames(raw_data)[2:4] = c('Regional.Schluessel', 'Gemeinde.Schluessel.Nr', 'Vgem.Schluessel')
data = raw_data[!is.na(raw_data$Gemeinde.Schluessel.Nr), ]

rm(raw_data)

# Convert non-alphanumeric values to NA
data[data == '•'] = NA

# Load column names and insert them into data -----------------------------
headers = read.csv('spaltennr_labels.csv', sep=',', header = F, stringsAsFactors = F)
COLLABELS = as.list(headers[2, ])
names(COLLABELS) = as.character(headers[2, ])
colnames(data)[startsWith(colnames(data), 'Spalte')] <- COLLABELS

rm(headers)


# Add information about regions -------------------------------------------
# Regierungsbezirke
data$Regierungsbezirk = data$Regional.Schluessel %>%
  substr(1, 2) %>%
  sapply(function(schl) {
    switch(schl,
           "91" = "Oberbayern",
           "92" = "Niederbayern",
           "93" = "Oberpfalz",
           "94" = "Oberfranken",
           "95" = "Mittelfranken",
           "96" = "Unterfranken",
           "97" = "Schwaben")
  })

# Landkreise
reg_gemeinden <- read.csv('./reginalschl_gemeinden.csv', sep=';', stringsAsFactors = F)
reg_kreis <- reg_gemeinden %>%
  filter(!is.na(rs_kreis) & is.na(rs_vb)) %>%
  mutate(rs_rb_kreis = paste(rs_rb, rs_kreis, sep='')) %>%
  select(rs_gemeindename, rs_rb_kreis)

get_lk_by_regschl <- function(regschl) {
  reg_kreis %>%
    filter(rs_rb_kreis == substr(regschl, 2, 4)) %>%
    select(rs_gemeindename) %>%
    as.list() %>%
    as.character()
}

data$Landkreis <- data$Regional.Schluessel %>%
  sapply(get_lk_by_regschl)

# Development -------------------------------------------------------------

highlight = "Engelsberg"
# v = COLLABELS[5]
v = COLLABELS$bevoelkerung.insgesamt
Landkreis = "Traunstein"
# myplot <- function(v, highlight, Landkreis = NULL) {
v = as.character(v)

data.vector = data %>%
  filter(Landkreis == Landkreis) %>%
  select(v)

lower_break = data.vector %>%
  pull(1) %>%
  min()
upper_break <- data.vector %>%
  pull(1) %>%
  quantile(0.99)
  # quantile(1)
breaks = seq(lower_break, upper_break, length.out = 30)


data %>%
  filter(Landkreis == Landkreis) %>%
  select(v) %>%
  mutate(!!v := ifelse(.[[v]] > upper_break, upper_break, .[[v]])) %>%
  ggplot(aes_string(x = v)) +
  geom_histogram() +
  geom_vline(xintercept = data %>%
    filter(Bezeichnung == highlight) %>%
    select(v) %>%
    as.numeric())



# loop --------------------------------------------------------------------
highlight = "Engelsberg"
Landkreis = "Traunstein"

for(v in COLLABELS[52]) {
  v = as.character(v)
  
  data.vector = data %>%
    filter(Landkreis == Landkreis) %>%
    select(v)
  
  data.vector = data.vector[!is.na(data.vector)] %>%
    as.numeric
  
  lower_break = data.vector %>%
    min()
  upper_break <- data.vector %>%
    quantile(0.99)
  # quantile(1)
  breaks = seq(lower_break, upper_break, length.out = 30)
  
  image = data %>%
    filter(Landkreis == Landkreis) %>%
    select(v) %>%
    mutate(!!v := ifelse(.[[v]] > upper_break, upper_break, .[[v]])) %>%
    ggplot(aes_string(x = v)) +
    geom_histogram() +
    geom_vline(xintercept = data %>%
                 filter(Bezeichnung == highlight) %>%
                 select(v) %>%
                 as.numeric())
  
  ggsave(filename = paste0('plots/plot_', v, '.png'), plot = image, width = 10, height = 8)
}
