library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)

# Aciklamalarin yapildigi satiri sutun ismi yapma islemi
data <- read_excel("neden-egitim-goc.xls")
aciklama_satiri <- data[3, ]
colnames(data) <- as.character(aciklama_satiri)

# Toplam göç sayısını ifade eden tablo
immigration_data <- data %>% filter(data$`Göç etme nedeni-Reason for migration` == "Toplam-Total")
toplam_goc <- as.numeric(immigration_data$`Toplam
Total`)
yillar <- as.numeric(immigration_data$`Yıl
Year`)
sirali_yillar <- sort(yillar)

# Create bar plot
barplot(toplam_goc, 
        names.arg = yillar, 
        col = "blue", 
        main = "Toplam Yurtiçi Göç", 
        xlab = "Yıllar", 
        ylab = "Toplam göç")