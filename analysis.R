# Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# Load and clean data -----------------------------------------------------
raw_data = read.csv('GDH2016_Daten_xlsx.csv', sep=';', stringsAsFactors = F)
colnames(raw_data)[2:4] = c('Regional.Schluessel', 'Gemeinde.Schluessel.Nr', 'Vgem.Schluessel')
data = raw_data[!is.na(raw_data$Gemeinde.Schluessel.Nr) & raw_data$Bezeichnung != "Gemeindefreie Gebiete", ]

rm(raw_data)

# Convert non-alphanumeric values to NA
data[data == '•'] = NA

# Load column names and insert them into data. Filter. Numerize -----------
headers = read.csv('spaltennr_labels.csv', sep=',', header = F, stringsAsFactors = F)
COLLABELS = as.list(headers[2, ])
names(COLLABELS) = as.character(headers[2, ])
colnames(data)[startsWith(colnames(data), 'Spalte')] <- COLLABELS

# Filter variables
vars = headers[3, ] %>% as.numeric() %>% as.logical()
vars = COLLABELS[vars] %>% as.character()
data = data %>%
  select(c("Laufende.Nummer", "Regional.Schluessel", "Gemeinde.Schluessel.Nr", "Bezeichnung", vars))

# Numerize
for(var in vars) {
  data[, var] <- as.numeric(data[, var])
}

collabels = COLLABELS %>% as.character()

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


# Plot Settings -----------------------------------------------------------
HIGHLIGHT = "Engelsberg"
REGIERUNGSBEZIRK = "Oberbayern"
LANDKREIS = NULL # set to NULL to use REGIERUNGSBEZIRK instead

data.filtered = data %>%
  filter(
    if(is.null(LANDKREIS)) {
      Regierungsbezirk == REGIERUNGSBEZIRK
    } else {
      Landkreis == LANDKREIS
    }
  )
plot.subtitle = paste0(HIGHLIGHT, " im Vergleich zu Gemeinden im ", ifelse(is.null(LANDKREIS), paste0("Regierungsbezirk ", REGIERUNGSBEZIRK), paste0("Landkreis ", LANDKREIS)))


# Flächen -----------------------------------------------------------------
data.flaechen = data.filtered %>%
  mutate(gebfreifl.rel = gebiet.davon.geb.und.freiflaeche/gebiet.flaeche.gesamt,
         betrfl.rel = gebiet.davon.bietriebsflaeche/gebiet.flaeche.gesamt,
         erholfl.rel = gebiet.davon.erholung/gebiet.flaeche.gesamt,
         landwfl.rel = gebiet.davon.landwirtschaft/gebiet.flaeche.gesamt,
         waldfl.rel = gebiet.davon.wald/gebiet.flaeche.gesamt)

data.flaechen %>%
  ggplot(aes(x = gebfreifl.rel)) +
  geom_histogram() +
  geom_vline(xintercept = data.flaechen %>%
               filter(Bezeichnung == HIGHLIGHT) %>%
               select(gebfreifl.rel) %>%
               as.numeric()) +
  scale_x_continuous(labels = scales::percent) +
  ggtitle("Anteil Gebäude- und Freifläche", plot.subtitle)

data.flaechen %>%
  ggplot(aes(x = betrfl.rel)) +
  geom_histogram() +
  geom_vline(xintercept = data.flaechen %>%
               filter(Bezeichnung == HIGHLIGHT) %>%
               select(betrfl.rel) %>%
               as.numeric()) +
  scale_x_continuous(labels = scales::percent) +
  ggtitle("Anteil Betriebsfläche", plot.subtitle)

data.flaechen %>%
  ggplot(aes(x = erholfl.rel)) +
  geom_histogram() +
  geom_vline(xintercept = data.flaechen %>%
               filter(Bezeichnung == HIGHLIGHT) %>%
               select(erholfl.rel) %>%
               as.numeric()) +
  scale_x_continuous(labels = scales::percent) +
  ggtitle("Anteil Erholungsfläche", plot.subtitle)

data.flaechen %>%
  ggplot(aes(x = landwfl.rel)) +
  geom_histogram() +
  geom_vline(xintercept = data.flaechen %>%
               filter(Bezeichnung == HIGHLIGHT) %>%
               select(landwfl.rel) %>%
               as.numeric()) +
  scale_x_continuous(labels = scales::percent) +
  ggtitle("Anteil Landwirtschaftsfläche", plot.subtitle)

data.flaechen %>%
  ggplot(aes(x = waldfl.rel)) +
  geom_histogram() +
  geom_vline(xintercept = data.flaechen %>%
               filter(Bezeichnung == HIGHLIGHT) %>%
               select(waldfl.rel) %>%
               as.numeric()) +
  scale_x_continuous(labels = scales::percent) +
  ggtitle("Anteil Waldfläche", plot.subtitle)
  
  


# Plot Altersstruktur -----------------------------------------------------
# Altersstruktur
vars.altersstruktur = collabels[collabels %>% startsWith("bevoelkerung.altersstruktur")]
vars.altersstruktur.rel = sapply(vars.altersstruktur, paste, ".rel", sep='')
for(altersgruppe in vars.altersstruktur) {
  data[, paste0(altersgruppe, ".rel")] = data[, altersgruppe]/rowSums(data[, vars.altersstruktur])
}

bayern.altersstruktur.rel = data %>%
  select(vars.altersstruktur.rel) %>%
  colMeans() %>%
  append(c(Bezeichnung = "Bayern"), .)
oberbayern.altersstruktur.rel = data %>%
  filter(Regierungsbezirk == "Oberbayern") %>%
  select(vars.altersstruktur.rel) %>%
  colMeans() %>%
  append(c(Bezeichnung = "Oberbayern"), .)
traunstein.altersstruktur.rel = data %>%
  filter(Landkreis == "Traunstein") %>%
  select(vars.altersstruktur.rel) %>%
  colMeans() %>%
  append(c(Bezeichnung = "LK Traunstein"), .)

data.altersstruktur = data %>%
  filter(Bezeichnung == "Engelsberg") %>%
  select(c("Bezeichnung", vars.altersstruktur.rel)) %>%
  rbind(bayern.altersstruktur.rel, oberbayern.altersstruktur.rel, traunstein.altersstruktur.rel)

data.altersstruktur[, colnames(data.altersstruktur) != "Bezeichnung"] = data.altersstruktur[, colnames(data.altersstruktur) != "Bezeichnung"] %>%
  sapply(as.numeric)

data.altersstruktur = data.altersstruktur %>%
  gather("Gruppe", "Anteil", -Bezeichnung) %>%
  # filter(Bezeichnung %in% c("Engelsberg", "Bayern")) %>%
  mutate(Gruppe = replace(Gruppe, Gruppe == "bevoelkerung.altersstruktur.unter6.rel", "< 6")) %>%
  mutate(Gruppe = replace(Gruppe, Gruppe == "bevoelkerung.altersstruktur.6bis14.rel", "6-14")) %>%
  mutate(Gruppe = replace(Gruppe, Gruppe == "bevoelkerung.altersstruktur.15bis17.rel", "15-17")) %>%
  mutate(Gruppe = replace(Gruppe, Gruppe == "bevoelkerung.altersstruktur.18bis24.rel", "18-24")) %>%
  mutate(Gruppe = replace(Gruppe, Gruppe == "bevoelkerung.altersstruktur.25bis29.rel", "25-29")) %>%
  mutate(Gruppe = replace(Gruppe, Gruppe == "bevoelkerung.altersstruktur.30bis49.rel", "30-49")) %>%
  mutate(Gruppe = replace(Gruppe, Gruppe == "bevoelkerung.altersstruktur.50bis64.rel", "50-64")) %>%
  mutate(Gruppe = replace(Gruppe, Gruppe == "bevoelkerung.altersstruktur.65bis.rel", "ab 65"))

data.altersstruktur$Gruppe = factor(data.altersstruktur$Gruppe)
data.altersstruktur$Gruppe = factor(data.altersstruktur$Gruppe, levels(data.altersstruktur$Gruppe)[c(1,7,2:6,8)])

data.altersstruktur %>%
  filter(Bezeichnung == "Engelsberg") %>%
  ggplot(aes(x = Gruppe, y = Anteil)) +
  geom_bar(stat = 'identity') +
  geom_line(data = data.altersstruktur %>% filter(Bezeichnung == "LK Traunstein"), aes(group = 1, color = "blue")) +
  geom_line(data = data.altersstruktur %>% filter(Bezeichnung == "Oberbayern"), aes(group = 1, color = "red")) +
  geom_line(data = data.altersstruktur %>% filter(Bezeichnung == "Bayern"), aes(group = 1, color = "green")) +
  scale_colour_manual(name = 'Vergleichswerte', values = c('blue' = 'blue', 'red' = 'red', 'green' = 'green'), labels = c('LK Traunstein', 'Oberbayern', 'Bayern')) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Altersstruktur")
  


# Plot Kinderbetreuungsplätze ---------------------------------------------
data %>%
  filter(Bezeichnung == "Engelsberg") %>%
  select(COLLABELS$bildung.kindertages.plaetze)

data.kinderbetr = data %>%
  mutate(bildung.kindertages.plaetze.pro100 = bildung.kindertages.plaetze/bevoelkerung.insgesamt*100)

data.kinderbetr %>%
  filter(
    if(is.null(LANDKREIS)) {
      Regierungsbezirk == REGIERUNGSBEZIRK
    } else {
      Landkreis == LANDKREIS
    }
  ) %>%
  ggplot(aes(x = bildung.kindertages.plaetze.pro100)) +
  geom_histogram() +
  ggtitle("Kindertageseinrichtungen: Plätze pro 100 Einwohner", paste0(HIGHLIGHT, " im Vergleich zu Gemeinden im ", ifelse(is.null(LANDKREIS), paste0("Regierungsbezirk ", REGIERUNGSBEZIRK), paste0("Landkreis ", LANDKREIS)))) +
  geom_vline(xintercept = data.kinderbetr %>%
               filter(Bezeichnung == "Engelsberg") %>%
               select(bildung.kindertages.plaetze.pro100) %>%
               as.numeric())


# Schüler -----------------------------------------------------------------
c(COLLABELS$bildung.grundmittelhauptschulen.schueler, COLLABELS$bildung.realschulen.schueler, COLLABELS$bildung.gymnasien.schueler, COLLABELS$bildung.gymnasien.schueler)
data.schueler = data %>%
  mutate(schueler.insg.rel = (bildung.grundmittelhauptschulen.schueler +
           bildung.realschulen.schueler +
           bildung.gymnasien.schueler +
           bildung.gymnasien.schueler)/bevoelkerung.insgesamt)

data.schueler %>%
  filter(
    if(is.null(LANDKREIS)) {
      Regierungsbezirk == REGIERUNGSBEZIRK
    } else {
      Landkreis == LANDKREIS
    }
  ) %>%
  ggplot(aes(x = schueler.insg.rel)) +
  geom_histogram() +
  scale_x_continuous(labels = scales::percent) +
  ggtitle("Anteil Schüler an Gesamtbevölkerung", paste0(HIGHLIGHT, " im Vergleich zu Gemeinden im ", ifelse(is.null(LANDKREIS), paste0("Regierungsbezirk ", REGIERUNGSBEZIRK), paste0("Landkreis ", LANDKREIS))))

# Sozialhilfeempfänger ----------------------------------------------------
data.sozhilfe = data.filtered %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  mutate(sozialhilfe_empf.insg.rel = (sozialhilfe_empf.kapitel3 + sozialhilfe_empf.kapitel4 + sozialhilfe_empf.kapitel5bis9) /bevoelkerung.insgesamt)

data.sozhilfe %>%
  ggplot(aes(x = sozialhilfe_empf.insg.rel)) +
  geom_histogram() +
  geom_vline(xintercept = data.sozhilfe %>%
               filter(Bezeichnung == HIGHLIGHT) %>%
               select(sozialhilfe_empf.insg.rel) %>%
               as.numeric()) +
  scale_x_continuous(labels = scales::percent) +
  ggtitle("Anteil Sozialhilfeempfänger", plot.subtitle)
