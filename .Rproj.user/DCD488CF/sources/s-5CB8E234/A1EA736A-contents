# Libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Load files for colname <-> colnumber conversion -------------------------
headers = read.csv('spaltennr_labels.csv', sep=',', header = F, stringsAsFactors = F)
COLLABELS = as.list(headers[2, ])
names(COLLABELS) = as.character(headers[2, ])
get_colnr_by_label <- function(label) {
  return(as.numeric(headers[1, headers[2, ] == label]))
}
# get_colnr_by_label(COLLABELS$bevoelkerung.1987)



# Load and clean data -----------------------------------------------------
raw_data = read.csv('GDH2016_Daten_xlsx.csv', sep=';', stringsAsFactors = F)
colnames(raw_data)[2:4] = c('Regional.Schluessel', 'Gemeinde.Schluessel.Nr', 'Vgem.Schluessel')
data = raw_data[!is.na(raw_data$Gemeinde.Schluessel.Nr), ]


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
ggplot(data %>%
         filter(Landkreis == 'Traunstein'), aes_string(x = 'Spalte014')) +
  geom_histogram()
