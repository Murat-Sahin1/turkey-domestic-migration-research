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

# sütunları numeric veri tipine çevirme işlemi
data$Okuma_yazma_bilmeyen__Illiterate <- as.numeric(data$Okuma_yazma_bilmeyen__Illiterate)
data$Okuma_yazma_bilen_fakat_bir_okul_bitirmeyen_Literate_without_a_diploma <- as.numeric(data$Okuma_yazma_bilen_fakat_bir_okul_bitirmeyen_Literate_without_a_diploma)
data$Toplam_Total <- as.numeric(data$Toplam_Total)
data$Ilkokul_Primary_school <- as.numeric(data$Ilkokul_Primary_school)
data$`Ilkogretim,_ortaokul_veya_dengi_okul_Primary_education_lower__secondary__school` <- as.numeric(data$`Ilkogretim,_ortaokul_veya_dengi_okul_Primary_education_lower__secondary__school`)
data$Lise_veya_dengi_okul_Upper_secondary_school <- as.numeric(data$Lise_veya_dengi_okul_Upper_secondary_school)
data$Yuksekogretim_Higher_education <- as.numeric(data$Yuksekogretim_Higher_education)
data$Bilinmeyen_Unknown <- as.numeric(data$Bilinmeyen_Unknown)
str(data)

# Toplam göç sayısını ifade eden tablo
immigration_data <- data %>% filter(data$Goc_etme_nedeni_Reason_for_migration == "Toplam-Total")
sorted_immigration_data <- immigration_data[order(immigration_data$Yil_Year), ]

immigration_data
sorted_immigration_data
str(sorted_immigration_data)

# sütunları numeric veri tipine çevirme işlemi
sorted_immigration_data$Okuma_yazma_bilmeyen__Illiterate <- as.numeric(sorted_immigration_data$Okuma_yazma_bilmeyen__Illiterate)
sorted_immigration_data$Okuma_yazma_bilen_fakat_bir_okul_bitirmeyen_Literate_without_a_diploma <- as.numeric(sorted_immigration_data$Okuma_yazma_bilen_fakat_bir_okul_bitirmeyen_Literate_without_a_diploma)
sorted_immigration_data$Toplam_Total <- as.numeric(sorted_immigration_data$Toplam_Total)
sorted_immigration_data$Ilkokul_Primary_school <- as.numeric(sorted_immigration_data$Ilkokul_Primary_school)
sorted_immigration_data$`Ilkogretim,_ortaokul_veya_dengi_okul_Primary_education_lower__secondary__school` <- as.numeric(sorted_immigration_data$`Ilkogretim,_ortaokul_veya_dengi_okul_Primary_education_lower__secondary__school`)
sorted_immigration_data$Lise_veya_dengi_okul_Upper_secondary_school <- as.numeric(sorted_immigration_data$Lise_veya_dengi_okul_Upper_secondary_school)
sorted_immigration_data$Yuksekogretim_Higher_education <- as.numeric(sorted_immigration_data$Yuksekogretim_Higher_education)
sorted_immigration_data$Bilinmeyen_Unknown <- as.numeric(sorted_immigration_data$Bilinmeyen_Unknown)

# 4 yillik surecteki toplam goc, bar plot
ggplot(sorted_immigration_data, aes(x = Yil_Year, y = Toplam_Total)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Toplam Yurt içi Göç",
    x = "Yıllar",
    y = "Toplam Göç"
  ) +
  theme_minimal()

# Okuma yazma bilmenin yurt içi göç davranışına etkisi
ggplot(sorted_immigration_data, aes(x = Yil_Year, y = Toplam_Total)) +
  geom_bar(aes(y = Okuma_yazma_bilen_fakat_bir_okul_bitirmeyen_Literate_without_a_diploma, fill = "Okuma Yazma bilen fakat bir okul bitirmeyen"), stat = "identity", position = "stack") +
  geom_bar(aes(y = Okuma_yazma_bilmeyen__Illiterate, fill = "Okuma yazma bilmeyen"
), stat = "identity", position = "stack") +
  labs(
    title = "Yurt içi göç",
    x = "Yıllar",
    y = "Toplam göç"
  ) +
  scale_fill_manual(values = c("Okuma Yazma bilen fakat bir okul bitirmeyen" = "red", "Okuma yazma bilmeyen" = "blue")) +
  theme_minimal()

# Goc etme nedenlerine göre analizler
data$Goc_etme_nedeni_Reason_for_migration

# Konut alma nedeni ile goc ve egitim seviyesinin iliskisi
konut_alma_nedeni_ile_goc <- data %>% filter(data$Goc_etme_nedeni_Reason_for_migration == "Ev alınması-Buying a house")
years <- c(2022, 2021, 2020, 2019, 2018)
konut_alma_nedeni_ile_goc$Yil_Year <- years
kategorik_sutunlar <- c("Goc_etme_nedeni_Reason_for_migration")
konut_alma_nedeni_ile_goc <- konut_alma_nedeni_ile_goc[, !colnames(konut_alma_nedeni_ile_goc) %in% kategorik_sutunlar]
str(konut_alma_nedeni_ile_goc)

ggplot(data = konut_alma_nedeni_ile_goc, aes(x = Yil_Year)) +
  geom_line(aes(y = Ilkokul_Primary_school, color = "İlk okul")) +
  geom_line(aes(y = Lise_veya_dengi_okul_Upper_secondary_school, color = "Lise")) +
  geom_line(aes(y = Yuksekogretim_Higher_education, color = "Yükseköğretim")) +
  labs(title = "Konut alımı sebebiyle yurt içi göçün eğitim seviyesi ile ilişkisi",
       x = "Yıllar",
       y = "Toplam Göç",
       color = "Eğitim Seviyesi") +
  scale_color_manual(values = c("İlk okul" = "blue", "Lise" = "red", "Yükseköğretim" = "green")) +
  theme_minimal()

# Aile fertlerinden birine bağımlı göç ve egitim seviyesinin iliskisi
aile_fertlerine_bagimli_goc <- data %>% filter(data$Goc_etme_nedeni_Reason_for_migration == "Hane / aile fertlerinden birine bağımlı göç\nMigration related to any member of the household / family")
years <- c(2022, 2021, 2020, 2019, 2018)
aile_fertlerine_bagimli_goc$Yil_Year <- years
kategorik_sutunlar <- c("Goc_etme_nedeni_Reason_for_migration")
aile_fertlerine_bagimli_goc <- aile_fertlerine_bagimli_goc[, !colnames(aile_fertlerine_bagimli_goc) %in% kategorik_sutunlar]
str(konut_alma_nedeni_ile_goc)

ggplot(data = aile_fertlerine_bagimli_goc, aes(x = Yil_Year)) +
  geom_line(aes(y = Ilkokul_Primary_school, color = "İlk okul")) +
  geom_line(aes(y = Lise_veya_dengi_okul_Upper_secondary_school, color = "Lise")) +
  geom_line(aes(y = Yuksekogretim_Higher_education, color = "Yükseköğretim")) +
  labs(title = "Aile fertlerinden birine bağımlı yurt içi göçün eğitim seviyesi ile ilişkisi",
       x = "Yıllar",
       y = "Toplam Göç",
       color = "Eğitim Seviyesi") +
  scale_color_manual(values = c("İlk okul" = "blue", "Lise" = "red", "Yükseköğretim" = "green")) +
  theme_minimal()

# Medeni duruma bağımlı göç ve egitim seviyesinin iliskisi
medeni_duruma_bagimli_goc <- data %>% filter(data$Goc_etme_nedeni_Reason_for_migration == "Medeni durum değişikliği / ailevi nedenler\nChange of marital status / family-related reasons")
years <- c(2022, 2021, 2020, 2019, 2018)
medeni_duruma_bagimli_goc$Yil_Year <- years
kategorik_sutunlar <- c("Goc_etme_nedeni_Reason_for_migration")
medeni_duruma_bagimli_goc <- medeni_duruma_bagimli_goc[, !colnames(medeni_duruma_bagimli_goc) %in% kategorik_sutunlar]
str(medeni_duruma_bagimli_goc)

ggplot(data = medeni_duruma_bagimli_goc, aes(x = Yil_Year)) +
  geom_line(aes(y = Ilkokul_Primary_school, color = "İlk okul")) +
  geom_line(aes(y = Lise_veya_dengi_okul_Upper_secondary_school, color = "Lise")) +
  geom_line(aes(y = Yuksekogretim_Higher_education, color = "Yükseköğretim")) +
  labs(title = "Medeni durum değişikliğine bağımlı göç ve egitim seviyesinin ilişkisi",
       x = "Yıllar",
       y = "Toplam Göç",
       color = "Eğitim Seviyesi") +
  scale_color_manual(values = c("İlk okul" = "blue", "Lise" = "red", "Yükseköğretim" = "green")) +
  theme_minimal()

# Aile yanına dönme nedeni ile göç ve egitim seviyesinin iliskisi
aile_yanina_donme <- data %>% filter(data$Goc_etme_nedeni_Reason_for_migration == "Aile yanına / memlekete geri dönme\nReturning to family home / hometown")
years <- c(2022, 2021, 2020, 2019, 2018)
aile_yanina_donme$Yil_Year <- years
kategorik_sutunlar <- c("Goc_etme_nedeni_Reason_for_migration")
aile_yanina_donme <- aile_yanina_donme[, !colnames(aile_yanina_donme) %in% kategorik_sutunlar]
str(aile_yanina_donme)

ggplot(data = aile_yanina_donme, aes(x = Yil_Year)) +
  geom_line(aes(y = Ilkokul_Primary_school, color = "İlk okul")) +
  geom_line(aes(y = Lise_veya_dengi_okul_Upper_secondary_school, color = "Lise")) +
  geom_line(aes(y = Yuksekogretim_Higher_education, color = "Yükseköğretim")) +
  labs(title = "Aile evine dönme sebebi ile göç ve egitim seviyesinin ilişkisi",
       x = "Yıllar",
       y = "Toplam Göç",
       color = "Eğitim Seviyesi") +
  scale_color_manual(values = c("İlk okul" = "blue", "Lise" = "red", "Yükseköğretim" = "green")) +
  theme_minimal()

# Emeklilik sebebi ile göç ve eğitim seviyesinin ilişkisi
emeklilik_nedeni_ile_goc <- data %>% filter(data$Goc_etme_nedeni_Reason_for_migration == "Emeklilik-Retirement")
years <- c(2022, 2021, 2020, 2019, 2018)
emeklilik_nedeni_ile_goc$Yil_Year <- years
kategorik_sutunlar <- c("Goc_etme_nedeni_Reason_for_migration")
emeklilik_nedeni_ile_goc <- emeklilik_nedeni_ile_goc[, !colnames(emeklilik_nedeni_ile_goc) %in% kategorik_sutunlar]
str(emeklilik_nedeni_ile_goc)

ggplot(data = emeklilik_nedeni_ile_goc, aes(x = Yil_Year)) +
  geom_line(aes(y = Ilkokul_Primary_school, color = "İlk okul")) +
  geom_line(aes(y = Lise_veya_dengi_okul_Upper_secondary_school, color = "Lise")) +
  geom_line(aes(y = Yuksekogretim_Higher_education, color = "Yükseköğretim")) +
  labs(title = "Aile evine dönme sebebi ile göç ve egitim seviyesinin ilişkisi",
       x = "Yıllar",
       y = "Toplam Göç",
       color = "Eğitim Seviyesi") +
  scale_color_manual(values = c("İlk okul" = "blue", "Lise" = "red", "Yükseköğretim" = "green")) +
  theme_minimal()

