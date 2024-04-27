library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(here)

# Aciklamalarin yapildigi satiri sutun ismi yapma islemi
file_path <- here("data", "neden-egitim-goc.xls")
data <- read_excel(file_path)
aciklama_satiri <- data[3, ]
colnames(data) <- as.character(aciklama_satiri)

map_non_english <- function(input_string) {
  mapping <- c("ı" = "i", "ş" = "s", "ğ" = "g", "ç" = "c", "ü" = "u", "ö" = "o", "İ" = "I", "Ş" = "S", "Ğ" = "G", "Ç" = "C", "Ü" = "U", "Ö" = "O")
  
  output_string <- chartr(paste(names(mapping), collapse = ""), paste(mapping, collapse = ""), input_string)
  
  return(output_string)
}

replace_empty_string <- function(input_string) {
  mapping <- c(" " = "_", "\n" = "_", "-" = "_")
  
  output_string <- chartr(paste(names(mapping), collapse = ""), paste(mapping, collapse = ""), input_string)
  
  return(output_string)
}

colnames(data) <- sapply(colnames(data), map_non_english)
colnames(data) <- sapply(colnames(data), replace_empty_string)
colnames(data)

# Toplam göç sayısını ifade eden tablo
immigration_data <- data %>% filter(data$Goc_etme_nedeni_Reason_for_migration == "Toplam-Total")
sorted_immigration_data <- immigration_data[order(immigration_data$Yil_Year), ]

immigration_data
sorted_immigration_data

# 4 yillik surecteki toplam goc, bar plot
ggplot(sorted_immigration_data, aes(x = Yil_Year, y = Toplam_Total)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Toplam Yurtiçi Göç",
    x = "Yıllar",
    y = "Toplam Göç"
  ) +
  theme_minimal()