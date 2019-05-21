### THIS IS FOR POPULATION FILTERED ANALYSIS

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




# SETTINGS ----------------------------------------------------------------
HIGHLIGHT = "Engelsberg"
REGIERUNGSBEZIRK = "Oberbayern"
LANDKREIS = NULL # set to NULL to use REGIERUNGSBEZIRK instead

FILTER_MAXIMUM_POPULATION = 5000

data.filtered = data %>%
  filter(
    if(is.null(LANDKREIS)) {
      Regierungsbezirk == REGIERUNGSBEZIRK
    } else {
      Landkreis == LANDKREIS
    }
  )
plot.subtitle = paste0(HIGHLIGHT, " im Vergleich zu Gemeinden im ", ifelse(is.null(LANDKREIS), paste0("Regierungsbezirk ", REGIERUNGSBEZIRK), paste0("Landkreis ", LANDKREIS)))

data.filtered.pre_maxpop = data.filtered
if(is.numeric(FILTER_MAXIMUM_POPULATION)) {
  data.filtered = data[data$bevoelkerung.insgesamt <= FILTER_MAXIMUM_POPULATION, ]
  dim(data.filtered)
}

COMPARISON.COLOR = "#F2B134"
HIGHLIGHT.COLOR = "#ED553B"

SAVE.PLOTS = T
PLOT.HEIGHT=4.7
if(is.numeric(FILTER_MAXIMUM_POPULATION)) {
  PLOT.FOLDER = paste0('plots_oberbayern_MAXPOP', FILTER_MAXIMUM_POPULATION, '/')
  dir.create(PLOT.FOLDER)
} else {
  PLOT.FOLDER = 'plots_oberbayern/'
}

# Prepare Plots -----------------------------------------------------------
std.plot = function(var.name, plot.title, xlab = var.name, d = data.filtered, lower_break = NULL, upper_break = NULL, binsize = 30, percent = F, x.ticks.n = 8, plot.folder = PLOT.FOLDER) {
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
  
  highlight.x = d %>%
    filter(Bezeichnung == HIGHLIGHT) %>%
    select_(var.name) %>%
    as.numeric()
  
  x = d %>%
    select_(var.name) %>%
    .[[1]]
  
  upper_break.y = if(!is.null(upper_break)) sum(x >= upper_break) else NULL
  
  print(sum(x < highlight.x, na.rm = T)/length(x))
  
  percent.lower = scales::percent(sum(x < highlight.x, na.rm = T)/length(x), accuracy = 1)
  percent.greater = scales::percent(sum(x > highlight.x, na.rm = T)/length(x), accuracy = 1)
  
  plt = d %>%
    ggplot(aes_string(x = var.name)) +
    geom_histogram(breaks = breaks, color='black', fill = COMPARISON.COLOR) +
    { if(is.null(lower_break) & !is.null(upper_break)) coord_cartesian(xlim = c(min.val, upper_break)) } +
    { if(!is.null(lower_break) & is.null(upper_break)) coord_cartesian(xlim = c(lower_break, max.val)) } +
    { if(!is.null(lower_break) & !is.null(upper_break)) coord_cartesian(xlim = c(lower_break, upper_break)) } +
    { if(!is.null(upper_break)) geom_text(x = upper_break, y = 0, hjust = -.15, vjust = -.5, size = 4, label = '>=') } + # paste0('>= ', if(!percent) upper_break else scales::percent(upper_break))
    { if(!is.null(lower_break)) geom_text(x = lower_break, y = 0, hjust = 1.1, vjust = -.5, size = 4, label = '<=') } +
    # geom_text(x = highlight.x, y = 0, vjust = -37, hjust = 1.2, label = percent.lower) +
    # geom_text(x = highlight.x, y = 0, vjust = -37, hjust = -.3, label = percent.greater) +
    annotate("text", x = highlight.x, y = Inf, vjust = 1.5, hjust = 1.2, label = percent.lower) + # vjust = -43.4
    annotate("text", x = highlight.x, y = Inf, vjust = 1.5, hjust = -.3, label = percent.greater) +
    # geom_segment(x = highlight.x, y = 0, xend = highlight.x-1, yend = 0, arrow = arrow(length = unit(0.5, "cm"))) +
    geom_vline(xintercept = highlight.x, linetype = 'dashed', col=HIGHLIGHT.COLOR, size = 2) +
    ggtitle(plot.title, plot.subtitle) +
    scale_x_continuous(breaks = unique(sort(c(pretty(if(is.null(upper_break)) x else x[x <= upper_break], n = x.ticks.n), round(highlight.x)))), labels = if(percent == T) scales::percent(accuracy = 1, unique(c(pretty(if(is.null(upper_break)) x else x[x <= upper_break], n = x.ticks.n), round(highlight.x)))) else waiver()) +
    ylab("Anzahl Gemeinden") +
    xlab(xlab) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text = element_text(colour = 'black', size = '10'),
          axis.title = element_text(color = 'black', size = '12'),
          plot.title = element_text(size = 14, face = 'bold'),
          plot.subtitle = element_text(size = 9))
  
  if(SAVE.PLOTS) {
    ggsave(filename = paste0(PLOT.FOLDER, var.name, ".pdf"), plot = plt, device = cairo_pdf, height = PLOT.HEIGHT)
  } else {
    return(plt)
  }
}


# Flächen - Fläche gesamt is in km2, Fläche.davon is in ha
data.flaechen = data.filtered %>%
  mutate(gebfreifl.rel = gebiet.davon.geb.und.freiflaeche*0.01/gebiet.flaeche.gesamt,
         betrfl.rel = gebiet.davon.bietriebsflaeche*0.01/gebiet.flaeche.gesamt,
         erholfl.rel = gebiet.davon.erholung*0.01/gebiet.flaeche.gesamt,
         landwfl.rel = gebiet.davon.landwirtschaft*0.01/gebiet.flaeche.gesamt,
         waldfl.rel = gebiet.davon.wald*0.01/gebiet.flaeche.gesamt)

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

if(is.numeric(FILTER_MAXIMUM_POPULATION)) {
  data.altersstruktur = data[data$bevoelkerung.insgesamt <= FILTER_MAXIMUM_POPULATION, ]
} else {
  data.altersstruktur = data
}
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

# Bevölkerungsveränderung: in Prozent hinterlegt
# data$bevoelkerung.veraenderung.vs.1987 = data$bevoelkerung.veraenderung.vs.1987/100
# data$bevoelkerung.veraenderung.vs.2011 = data$bevoelkerung.veraenderung.vs.2011/100

# Anteil Pendler an Beschäftigten am Wohnort: in Prozent hinterlegt
data$erwerb.sozialverspfl_beschaeftigte_am_wohnort.darunter_auspendler.prozent = data$erwerb.sozialverspfl_beschaeftigte_am_wohnort.darunter_auspendler.prozent/100
data$erwerb.sozialverspfl_beschaeftigte_am_arbeitsort.darunter_einpendler.prozent = data$erwerb.sozialverspfl_beschaeftigte_am_arbeitsort.darunter_einpendler.prozent/100




# Plots: Compare with Oberbayern ------------------------------------------
# Flächen
std.plot(d = data.flaechen, "gebfreifl.rel", plot.title = "Anteil Gebäude- und Freifläche an Gesamtfläche", xlab = "Flächenanteil",  percent = T, upper_break = 0.2)
std.plot(d = data.flaechen, "betrfl.rel", plot.title = "Anteil Betriebsfläche an Gesamtfläche", xlab = "Flächenanteil",  percent = T, upper_break = 0.03, x.ticks.n = 4)
std.plot(d = data.flaechen, "erholfl.rel", plot.title = "Anteil Erholungsfläche an Gesamtfläche", xlab = "Flächenanteil",  percent = T, upper_break = 0.03, x.ticks.n = 4)
std.plot(d = data.flaechen, "landwfl.rel", plot.title = "Anteil Landwirtschaftsfläche an Gesamtfläche", xlab = "Flächenanteil",  percent = T)
std.plot(d = data.flaechen, "waldfl.rel", plot.title = "Anteil Waldfläche an Gesamtfläche", xlab = "Flächenanteil",  percent = T)

# Altersstruktur
# altersstruktur.plt = data.altersstruktur %>%
#   filter(Bezeichnung == "Engelsberg") %>%
#   ggplot(aes(x = Gruppe, y = Anteil)) +
#   geom_bar(stat = 'identity', color='black', fill='#F2B134') +
#   geom_point(data = data.altersstruktur %>% filter(Bezeichnung == "LK Traunstein"), aes(group = 1, color = "blue")) +
#   geom_point(data = data.altersstruktur %>% filter(Bezeichnung == "Oberbayern"), aes(group = 1, color = "red")) +
#   geom_point(data = data.altersstruktur %>% filter(Bezeichnung == "Bayern"), aes(group = 1, color = "green")) +
#   guides(color=guide_legend(title="Vergleichsgruppen")) +
#   # scale_colour_manual(name = 'Vergleichswerte', values = c('blue' = 'blue', 'red' = 'red', 'green' = 'green'), labels = c('LK Traunstein', 'Oberbayern', 'Bayern')) +
#   scale_color_brewer(palette = 'Set1', labels = c('LK Traunstein', 'Oberbayern', 'Bayern')) +
#   scale_y_continuous(labels = scales::percent) +
#   xlab("Altersgruppe") +
#   ylab("Anteil an Gesamtbevölkerung") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         axis.text = element_text(colour = 'black', size = '10'),
#         axis.title = element_text(color = 'black', size = '12'),
#         plot.title = element_text(size = 17, face = 'bold'),
#         plot.subtitle = element_text(size = 12)) +
#   ggtitle("Altersstruktur")
# if(SAVE.PLOTS) ggsave(filename = paste0(PLOT.FOLDER, "altersstruktur.pdf"), plot = altersstruktur.plt, device = "pdf") else altersstruktur.plt

altersstruktur.plt = data.altersstruktur %>%
  filter(Bezeichnung %in% c("Engelsberg", "Oberbayern")) %>%
  ggplot(aes(x = Gruppe, y = Anteil)) +
  geom_bar(aes(fill = Bezeichnung), stat = 'identity', position = position_dodge2()) +
  # geom_point(data = data.altersstruktur %>% filter(Bezeichnung == "LK Traunstein"), aes(group = 1, color = "blue")) +
  # geom_point(data = data.altersstruktur %>% filter(Bezeichnung == "Oberbayern"), aes(group = 1, color = "red")) +
  # geom_point(data = data.altersstruktur %>% filter(Bezeichnung == "Bayern"), aes(group = 1, color = "green")) +
  guides(color=guide_legend(title="Vergleichsgruppen")) +
  # scale_colour_manual(name = 'Vergleichswerte', values = c('blue' = 'blue', 'red' = 'red', 'green' = 'green'), labels = c('LK Traunstein', 'Oberbayern', 'Bayern')) +
  # scale_color_brewer(palette = 'Set1', labels = c('LK Traunstein', 'Oberbayern', 'Bayern')) +
  scale_fill_manual(values = c(HIGHLIGHT.COLOR, COMPARISON.COLOR)) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Altersgruppe") +
  ylab("Anteil an Gesamtbevölkerung") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(colour = 'black', size = '10'),
        axis.text.x = element_text(angle = -90, hjust = 1, colour = 'black', size = '10'),
        axis.title = element_text(color = 'black', size = '12'),
        plot.title = element_text(size = 14, face = 'bold'),
        plot.subtitle = element_text(size = 9)) +
  ggtitle("Altersstruktur")
if(SAVE.PLOTS) ggsave(filename = paste0(PLOT.FOLDER, "altersstruktur.pdf"), plot = altersstruktur.plt, device = "pdf", height = PLOT.HEIGHT) else altersstruktur.plt

# Kinderbetreuungsplätze
std.plot(d = data.kinderbetr, "bildung.kindertages.plaetze.pro100", "Kindertageseinrichtungen", xlab = "Plätze pro 100 Einwohner", upper_break = 9)

# Schüler
std.plot(d = data.schueler, "schueler.insg.rel", "Schüler", xlab = "Anteil an Bevölkerung", percent = T, upper_break = 0.15)

# Sozialhilfeempfänger
std.plot(d = data.sozhilfe, "sozialhilfe_empf.insg.rel", "Sozialhilfeempfänger (SGB XII Kap. 3-9)", xlab = "Anteil an Bevölkerung", upper_break = 0.08, percent = T)

# Bevölkerung
std.plot(d = data.filtered.pre_maxpop, COLLABELS$bevoelkerung.insgesamt, plot.title = "Bevölkerung 2015", xlab = "Bevölkerung", upper_break = 30000)

# Bevölkerungszuabnahme 2015 vs 1987/2011
std.plot(COLLABELS$bevoelkerung.veraenderung.vs.1987, plot.title = "Bevölkerungsveränderung im Vgl. zu 1987", xlab = "Veränderung in %", x.ticks.n = 4, percent = F)
std.plot(COLLABELS$bevoelkerung.veraenderung.vs.2011, plot.title = "Bevölkerungsveränderung im Vgl. zu 2011", xlab = "Veränderung in %", percent = F)

# Bevölkerungsdichte
std.plot(COLLABELS$bevoelkerung.je_km2, plot.title = "Bevölkerungsdichte", xlab = "Einwohner je km²", upper_break = 600, x.ticks.n = 6)

# Bevölkerungsbewegung Zugezogene
std.plot(COLLABELS$bevoelkerung.bewegung.zugezogene, "Bevölkerungsbewegung: Zugezogene (2015)", xlab = "Anzahl Zugezogener", upper_break = 600)
std.plot(COLLABELS$bevoelkerung.bewegung.fortgezogene, "Bevölkerungsbewegung: Fortgezogene (2015)", xlab = "Anzahl Fortgezogener", upper_break = 600, x.ticks.n = 6)
std.plot(COLLABELS$bevoelkerung.bewegung.wanderungsgewinn, "Bevölkerungsbew.: Wanderungsgewinn (2015)", xlab = "Wanderungsgewinn", upper_break = 150)

# Beschäftigte
std.plot(COLLABELS$erwerb.sozialverspfl_beschaeftigte_am_arbeitsort.insgesamt, "Beschäftigte am Arbeitsort", xlab = "Anzahl Beschäftigter", upper_break = 2000, x.ticks.n = 6)
std.plot(COLLABELS$erwerb.sozialverspfl_beschaeftigte_am_wohnort.darunter_auspendler.prozent, "Beschäftigte am Wohnort, darunter Auspendler", xlab = "Anteil Auspendler in %", lower_break = 60, percent = F, x.ticks.n = 2)
std.plot(COLLABELS$erwerb.sozialverspfl_beschaeftigte_am_arbeitsort.darunter_einpendler.prozent, "Beschäftigte am Arbeitsort, darunter Einpendler", xlab = "Anteil Einpendler in %", percent = F)
std.plot(COLLABELS$erwerb.pendlersaldo, "Pendlersaldo", xlab = "Pendlersaldo", lower_break = -1500, upper_break = 1000)

# Landwirtschaft
std.plot(COLLABELS$landforst.betriebe_von_flaeche_ha.insgesamt, "Land- und forstwirtschaftliche Betriebe", xlab = "Anzahl Betriebe", upper_break = 140, x.ticks.n = 3)

# Wasser pro Kopf Verbrauch
std.plot(COLLABELS$umwelt.wasser_pro_kopf_verbrauch, "Wasserverbrauch", xlab = "Liter pro Person", upper_break = 190)

# Wohnungen
std.plot(COLLABELS$bauwohn.baugenehmigungen.wohnungen, "Erteilte Baugenehmigungen: Wohnungen (2015)", xlab = "Anzahl", lower_break = -1, upper_break = 60)
std.plot(COLLABELS$bauwohn.bestand_wohnungen.insgesamt, "Wohnungsbestand", xlab = "Anzahl", upper_break = 2500, x.ticks.n = 6)

# Finanzen
std.plot(COLLABELS$lohneinksteuer.gesamtbetrag_einkuenfte.insgesamt, "Lohn- und Eink.-St.: Gesamtbetrag der Einkünfte", xlab = "in 1000€", upper_break = 100000, x.ticks.n = 3)
std.plot(COLLABELS$lohneinksteuer.gesamtbetrag_einkuenfte.je_steuerpfl, "Lohn- und Eink.-St.: Einkünfte je Steuerpflichtiger", xlab = "in 1000€", upper_break = 60000, x.ticks.n = 2)
std.plot(COLLABELS$lohneinksteuer.bruttolohn.je_arbeitnehmer, "Lohn- und Eink.-St.: Bruttolohn je Arbeitnehmer", xlab = "in €", upper_break = 40000, x.ticks.n = 3)
std.plot(COLLABELS$kommunale_finanzen.gemeindesteuereinnahmen.insgesamt, "Gemeindesteuereinnahmen: insgesamt", xlab = "in 1000€", upper_break = 7000, x.ticks.n = 2)
std.plot(COLLABELS$kommunale_finanzen.steuereinnahmen_insgesamt, "Steuereinnahmen: insgesamt", xlab = "in € je Einwohner", upper_break = 2500, x.ticks.n = 2)
std.plot(COLLABELS$kommunale_finanzen.realsteueraufbringungskraft, "Realsteueraufbringungskraft", xlab = "in € je Einwohner", upper_break = 3000, x.ticks.n = 2)
std.plot(COLLABELS$kommunale_finanzen.steuereinnahmekraft.je_einwohner, "Steuereinnahmekraft", xlab = "in € je Einwohner", upper_break = 2500)
std.plot(COLLABELS$kommunale_finanzen.gemeindesteuereinnahmen.darunter.grundsteuer.a, plot.title = "Gemeindesteuereinnahmen: Grundsteuer A", xlab = "in 1000€", upper_break = 120)
std.plot(COLLABELS$kommunale_finanzen.gemeindesteuereinnahmen.darunter.grundsteuer.b, plot.title = "Gemeindesteuereinnahmen: Grundsteuer B", xlab = "in 1000€", upper_break = 700)


