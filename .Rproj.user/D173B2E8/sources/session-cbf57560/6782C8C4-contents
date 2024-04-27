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
  mapping <- c(" " = "_", "\n" = "_")
  
  output_string <- chartr(paste(names(mapping), collapse = ""), paste(mapping, collapse = ""), input_string)
  
  return(output_string)
}

colnames(data) <- sapply(colnames(data), map_non_english)
colnames(data) <- sapply(colnames(data), replace_empty_string)
colnames(data)


clean_column_names <- function(df) {
  colnames(df) <- gsub("[^a-zA-Z0-9_]", "", colnames(df))
  colnames(df) <- gsub(" ", "_", colnames(df))
  return(df)
}


data <- clean_column_names(data)





# new_column_names <- c("Yil", "Toplam_Goc", "Okuma_Yazma_Bilmeyen", "O")

colnames(data) <- as.character(aciklama_satiri)

# Toplam göç sayısını ifade eden tablo
immigration_data <- data %>% filter(data$`Göç etme nedeni-Reason for migration` == "Toplam-Total")
sorted_immigration_data <- immigration_data[order(immigration_data$`Yıl
Year`), ]
immigration_data
sorted_immigration_data

# Plotting
ggplot(data, aes(x = Category, y = Value)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Basic Bar Plot",
    x = "Category",
    y = "Value"
  ) +
  theme_minimal()