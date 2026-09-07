# ============================================
# EDA — Coffee Quality Institute (arabica_data_cleaned.csv)
# Proyecto: Analítica y Métodos Multivariados
# ============================================

data <- read.csv("data/arabica_data_cleaned.csv")

str(data)
dim(data)
head(data)
summary(data)
colSums(is.na(data))

library(naniar)
gg_miss_upset(data)

library(DataExplorer)
create_report(data)

library(dplyr)
datos_limpios <- data %>%
  select(-Lot.Number, -ICO.Number, -Certification.Body,
         -Certification.Address, -Certification.Contact,
         -Expiration, -Owner, -Owner.1, -Farm.Name, -Mill, -Company,
         -Producer, -In.Country.Partner, -Grading.Date, -Harvest.Year)

data$Species           <- factor(data$Species)
data$Country.of.Origin <- factor(data$Country.of.Origin)
data$Processing.Method <- factor(data$Processing.Method)
data$Color             <- factor(data$Color)

library(psych)
describe(data)

library(dataMaid)
makeDataReport(data,
               render = TRUE,
               replace = TRUE,
               smartNum = TRUE,
               addSummaryTable = TRUE)
