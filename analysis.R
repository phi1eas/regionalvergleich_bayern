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


# Prepare Plots -----------------------------------------------------------
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

std.plot = function(var.name, var.title, d = data.filtered, lower_break = NULL, upper_break = NULL, binsize = 30) {
  min.val = min(d[, var.name], na.rm = T)
  max.val = max(d[, var.name], na.rm = T)
  breaks = if(is.null(lower_break) & !is.null(upper_break)) {
    c(seq(min.val, upper_break, (upper_break-min.val)/binsize), max.val)
  } else if(!is.null(lower_break) & is.null(upper_break)) {
    c(min.val, seq(lower_break, max.val, (max.val-lower_break)/binsize))
  } else if(!is.null(lower_break) & !is.null(upper_break)) {
    c(min.val, seq(lower_break, upper_break, (upper_break-lower_break)/binsize), max.val)
  } else {
    NULL
  }
  
  # breaks = if(!is.null(upper_break)) c(seq(min.val, upper_break, (upper_break-min.val)/30), max.val) else NULL
  
  d %>%
    ggplot(aes_string(x = var.name)) +
    geom_histogram(breaks = breaks) +
    { if(is.null(lower_break) & !is.null(upper_break)) coord_cartesian(xlim = c(min.val, upper_break)) } +
    { if(!is.null(lower_break) & is.null(upper_break)) coord_cartesian(xlim = c(lower_break, max.val)) } +
    { if(!is.null(lower_break) & !is.null(upper_break)) coord_cartesian(xlim = c(lower_break, upper_break)) } +
    geom_vline(xintercept = d %>%
                 filter(Bezeichnung == HIGHLIGHT) %>%
                 select_(var.name) %>%
                 as.numeric()) +
    ggtitle(var.title, plot.subtitle) %>%
    return()
}

# Flächen
data.flaechen = data.filtered %>%
  mutate(gebfreifl.rel = gebiet.davon.geb.und.freiflaeche/gebiet.flaeche.gesamt,
         betrfl.rel = gebiet.davon.bietriebsflaeche/gebiet.flaeche.gesamt,
         erholfl.rel = gebiet.davon.erholung/gebiet.flaeche.gesamt,
         landwfl.rel = gebiet.davon.landwirtschaft/gebiet.flaeche.gesamt,
         waldfl.rel = gebiet.davon.wald/gebiet.flaeche.gesamt)

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

# Kinderbetreuungsplätze pro 100 Einwohner
data.kinderbetr = data.filtered %>%
  mutate(bildung.kindertages.plaetze.pro100 = bildung.kindertages.plaetze/bevoelkerung.insgesamt*100)

# Schüler, relativ zu Bevölkerung
data.schueler = data.filtered %>%
  mutate(schueler.insg.rel = (bildung.grundmittelhauptschulen.schueler +
                                bildung.realschulen.schueler +
                                bildung.gymnasien.schueler +
                                bildung.gymnasien.schueler)/bevoelkerung.insgesamt)

# Sozialhilfeempfänger
data.sozhilfe = data.filtered %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  mutate(sozialhilfe_empf.insg.rel = (sozialhilfe_empf.kapitel3 + sozialhilfe_empf.kapitel4 + sozialhilfe_empf.kapitel5bis9) /bevoelkerung.insgesamt)


# Plots -------------------------------------------------------------------

# Flächen
std.plot(d = data.flaechen, "gebfreifl.rel", var.title = "Anteil Gebäude- und Freifläche")
std.plot(d = data.flaechen, "betrfl.rel", var.title = "Anteil Betriebsfläche")
std.plot(d = data.flaechen, "erholfl.rel", var.title = "Anteil Erholungsfläche")
std.plot(d = data.flaechen, "landwfl.rel", var.title = "Anteil Landwirtschaftsfläche")
std.plot(d = data.flaechen, "waldfl.rel", var.title = "Anteil Waldfläche")

# Altersstruktur
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
  
# Kinderbetreuungsplätze
std.plot(d = data.kinderbetr, "bildung.kindertages.plaetze.pro100", "Kindertageseinrichtungen: Plätze pro 100 Einwohner")

# Schüler
std.plot(d = data.schueler, "schueler.insg.rel", "Anteil Schüler an Bevölkerung")

# Sozialhilfeempfänger
std.plot(d = data.sozhilfe, "sozialhilfe_empf.insg.rel", "Anteil Sozialhilfeempfänger an Bevölkerung")

# Bevölkerung
std.plot(COLLABELS$bevoelkerung.insgesamt, var.title = "Bevölkerung 2015", upper_break = 20000)

# Bevölkerungszuabnahme 2015 vs 1987/2011
std.plot(COLLABELS$bevoelkerung.veraenderung.vs.1987, var.title = "Bevölkerungsveränderung im Vgl. zu 1987")
std.plot(COLLABELS$bevoelkerung.veraenderung.vs.2011, var.title = "Bevölkerungsveränderung im Vgl. zu 2011")

# Bevölkerungsdichte
std.plot(COLLABELS$bevoelkerung.je_km2, var.title = "Einwohner je km2", upper_break = 1000)

# Bevölkerungsbewegung Zugezogene
std.plot(COLLABELS$bevoelkerung.bewegung.zugezogene, "Bevölkerungsbewegung Zugezogene", upper_break = 3000)
std.plot(COLLABELS$bevoelkerung.bewegung.fortgezogene, "Bevölkerungsbewegung Fortgezogene", upper_break = 2000)
std.plot(COLLABELS$bevoelkerung.bewegung.wanderungsgewinn, "Bevölkerungsbewegung Wanderungsgewinn", upper_break = 500)

# Beschäftigte
std.plot(COLLABELS$erwerb.sozialverspfl_beschaeftigte_am_arbeitsort.insgesamt, "Beschäftigte am Arbeitsort", upper_break = 10000)
std.plot(COLLABELS$erwerb.sozialverspfl_beschaeftigte_am_wohnort.darunter_auspendler.prozent, "Beschäftigte am Wohnort, darunter Auspendler in Prozent")
std.plot(COLLABELS$erwerb.sozialverspfl_beschaeftigte_am_arbeitsort.darunter_einpendler.prozent, "Beschäftigte am Arbeitsort, darunter Einpendler in Prozent")
std.plot(COLLABELS$erwerb.pendlersaldo, "Pendlersaldo", lower_break = -3000, upper_break = 3000)

# Landwirtschaft
std.plot(COLLABELS$landforst.betriebe_von_flaeche_ha.insgesamt, "Anzahl land- und forstwirtschaftlicher Betriebe", upper_break = 200)

# Wasser pro Kopf Verbrauch
std.plot(COLLABELS$umwelt.wasser_pro_kopf_verbrauch, "Wasserverbrauch in Litern pro Person")

# Wohnungen
std.plot(COLLABELS$bauwohn.baugenehmigungen.wohnungen, "Erteilte Baugenehmigungen: Wohnungen", upper_break = 150)
std.plot(COLLABELS$bauwohn.bestand_wohnungen.insgesamt, "Bestand an Wohnungen", upper_break = 10000)

# Finanzen
std.plot(COLLABELS$lohneinksteuer.gesamtbetrag_einkuenfte.insgesamt, "Lohn- und Eink.-St.: Gesamtbetrag der Einkünfte in 1000€", upper_break = 500000)
std.plot(COLLABELS$lohneinksteuer.gesamtbetrag_einkuenfte.je_steuerpfl, "Lohn- und Eink.-St.: Einkünfte je Steuerpflichtiger in 1000€", upper_break = 60000)
std.plot(COLLABELS$lohneinksteuer.bruttolohn.je_arbeitnehmer, "Lohn- und Eink.-St.: Bruttolohn je Arbeitnehmer", upper_break = 50000)
std.plot(COLLABELS$kommunale_finanzen.gemeindesteuereinnahmen.insgesamt, "Gemeindesteuereinnahmen insgesamt in 1000€", upper_break = 30000)
std.plot(COLLABELS$kommunale_finanzen.steuereinnahmen_insgesamt, "Steuereinnahmen insgesamt, € je Einwohner", upper_break = 2500)
std.plot(COLLABELS$kommunale_finanzen.realsteueraufbringungskraft, "Realsteueraufbringungskraft, € je Einwohner", upper_break = 2500)
std.plot(COLLABELS$kommunale_finanzen.steuereinnahmekraft.je_einwohner, "Steuereinnahmekraft, € je Einwohner", upper_break = 3000)

