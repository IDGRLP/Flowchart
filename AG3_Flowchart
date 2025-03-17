#' -----------------------------------------------------------------------------------------------------------------------
#' Erstellung eines Flowcharts für die ZfKD-Tabelle Patient
#' SST KKN 
#' 03.2025
#
#' Quellen:
#' https://cran.r-project.org/web/packages/flowchart/index.html
#' https://cran.r-project.org/web/packages/flowchart/flowchart.pdf
#' https://www.r-bloggers.com/2025/01/flowcharts-made-easy-with-the-package-flowchart/
#'
#' -----------------------------------------------------------------------------------------------------------------------

# 0. Pfade und Pakete ---------------------------------------------------------------------------------------------------

# Daten
dir_dat <- 
  file.path("G:", "2_Registerbereich", "2.2_Auswertung", "01_Rohdaten", "ZfKD_Lieferdatensatz", "2024", "20241022")

# Ergebnisse (keine Ergebnisausgabe)
dir_erg <- 
  file.path("C:", "Users", "s.steinmann", "Documents","_r_forum","2025_Workshop", "AG3", "flowchart", "output")

# Pakete
dir_packages <- 
  file.path("C:", "Users", "s.steinmann", "Documents", "_r_packages")


.libPaths(dir_packages)
.libPaths()

library(flowchart)
library(summarytools)
library(tidyverse)
library(lubridate)


# 1. Daten ---------------------------------------------------------------------------------------------------------------

# Tabelle Patient der ZfKD-Daten einlesen
df_patient <- 
  readRDS(
  file.path(dir_dat, "Patient.rds")
  ) 

colnames(df_patient1)

freq(substr(df_patient1$Geburtsdatum, 1, 4))

df_patient1 <- 
  df_patient |>
  dplyr::mutate(
    geb_jahr = 
      as.numeric(
        substr(Geburtsdatum, 1, 4)
        ),
    aus_juenger18 =
      dplyr::case_when(
        lubridate::year(Sys.Date()) - geb_jahr < 18 ~ 1, TRUE ~ 0
        ),
    aus_aelter100 = 
      dplyr::case_when(
        lubridate::year(Sys.Date()) - geb_jahr > 105 ~ 1, TRUE ~ 0
        ),
    geschlecht = 
      paste("Geschlecht", Geschlecht)
  )


# 2. Abbildung ---------------------------------------------------------------------------------------------------------------

# Quelle: https://www.r-bloggers.com/2025/01/flowcharts-made-easy-with-the-package-flowchart/

df_patient1 |> 
  flowchart::as_fc(label = "Personen ZfKD") |>
  fc_filter(
    aus_juenger18 == 0, 
    text_pattern = "{n} Personen\n ({perc}%)", 
    label_exc = "Alter < 18 Jahre", 
    show_exc = TRUE
    ) |>
  fc_filter(
    aus_aelter100 == 0, 
    text_pattern = "{n} Personen\n ({perc}%)",  
    label_exc = "Alter > 105 Jahre", 
    show_exc = TRUE
    ) |>
  fc_split(
    geschlecht
    ) |> 
  fc_filter(
    Verstorben == "J", 
    label = "Verstorben"
    ) |>
  fc_draw() |>
  fc_export(
    "_r_forum_flowchart_01.png",
    path = dir_erg,
    format = "png",
    width = 2600,
    height = 1800,
    res = 300
    )


# 3. Vorschlaege Weiterbearbeitung ---------------------------------------------------------------------------------------------------------------

# Zahlenformat Anzahl mit Tausender-Trennzeichen "."
# Prozentangaben auf die Gesamtheit der Personen beziehen
# Einfärben eines einzelnen Feldes oder "Strangs"
