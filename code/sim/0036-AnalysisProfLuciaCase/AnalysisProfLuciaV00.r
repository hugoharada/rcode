
rm(list=ls())
if(!require(polycor)) install.packages("polycor"); library(polycor)
if(!require(tidyr)) install.packages("tidyr"); library(tidyr)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(readxl)) install.packages("readxl"); library(readxl)
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)
if(!require(psych)) install.packages("psych"); library(psych)



setwd("C:\\Users\\hugo\\Downloads\\lucia")


# xlsx files
my_data <- read_excel("dados.xlsx",sheet = "Banco_IME")
type <- read_excel("dados.xlsx",sheet = "tipos")
types <- as.character(type[1,])
my_data <- read_excel("dados.xlsx",sheet = "Banco_IME",col_names=TRUE, col_types = types)
my_data

my_data$pesoatual2 <- as.ordered(my_data$pesoatual2)
my_data$insatisfeitopeso <- as.ordered(my_data$insatisfeitopeso)
my_data$tabaco <- as.ordered(my_data$tabaco)
my_data$alcool <- as.ordered(my_data$alcool)
my_data$inativfiscat1 <- as.ordered(my_data$inativfiscat1)
my_data$cafemanha4 <- as.ordered(my_data$cafemanha4)
my_data$estadonut1 <- as.ordered(my_data$estadonut1)
my_data$estadonut2 <- as.ordered(my_data$estadonut2)
my_data$estadonut3 <- as.ordered(my_data$estadonut3)
my_data$tmc3 <- as.ordered(my_data$tmc3)
my_data$fxidade1 <- as.ordered(my_data$fxidade1)
my_data$sexo4 <- as.ordered(my_data$sexo4)
my_data$esc_tipo1 <- as.ordered(my_data$esc_tipo1)
my_data$estudamae2 <- as.ordered(my_data$estudamae2)
my_data$trabalho <- as.ordered(my_data$trabalho)
my_data$cor7 <- as.ordered(my_data$cor7)

my_data[,1:10]
my_data[,11:22]

#creating vector to select columsn
my_data_selection <- rep(TRUE,22)
names(my_data_selection) =names(my_data)

#The following will not make into the model. 
my_data_selection['id_erica'] <-FALSE
my_data_selection['cod_estr_sel'] <-FALSE
my_data_selection['estratopos'] <-FALSE
my_data_selection['pesopos'] <-FALSE
my_data_selection['cod_UPA'] <-FALSE
my_data_selection['estadonut2'] <-FALSE
my_data_selection['estadonut3'] <-FALSE
my_data_selection['fxidade1'] <-FALSE
my_data_selection['pesoatual2'] <-FALSE #removed as hetcor complained #inadmissible correlation set to 0.9999
my_data_selection['esc_tipo1'] <-FALSE #including it causes ultra-Heywood case in fa(). Removing.

my_data[,my_data_selection]

df.my_data <- data.frame(my_data[,my_data_selection])
tp <- hetcor(df.my_data,ML=TRUE, use="complete.obs")
tp
faPC <- fa(r=tp$correlations, nfactors=2, n.obs= 71740, rotate="oblimin")
faPC

my_data_selection['estudamae2'] <-FALSE #loadings too low. Eliminating from analyses.
my_data_selection['cor7'] <-FALSE #loadings too low. Eliminating from analyses.


my_data[,my_data_selection]

summary(my_data[,my_data_selection])

model <- '
d1 =~  tabaco+alcool+idade+trabalho
d2 =~ insatisfeitopeso+inativfiscat1+cafemanha4+estadonut1+tmc3+sexo4
'
fit.model <- lavaan(model=model, data = my_data[,my_data_selection])
fitmeasures(fit.model)["tli"]
fitmeasures(fit.model)["cfi"]
fitmeasures(fit.model)["rmsea"]
