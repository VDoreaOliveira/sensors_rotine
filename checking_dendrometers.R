#### Dendrometer datachecks - Bittencourt, Groenendijk, Leal, Oliveira ### 
#### Load packages and functions ####

library(tidyverse)
library(tidyr)
library(ggplot2)

getVpd = function (temperature, rh) {
  esat = eSat(temperature)
  ea = ea(temperature, rh)
  v = esat - ea
  return(v)
}
celsiusToKelvin = function (temperature) {
  return(temperature + 273.15)
}
eSat = function (temperature) 
{
  temperature = celsiusToKelvin(temperature)
  eSat = 6.1078 * exp((17.2693882 * (temperature - 273.15))/(temperature - 
                                                               35.86))
  eSat = eSat * 100
  return(eSat/1000)
}
ea = function (temperature, rh) {
  esat = eSat(temperature)
  ea = (rh * esat)/100
  return(ea)
}

#### Load data ####

wd_txt <- "G:/Meu Drive/1. Biolopasta/1. Mestrado/1. Unicamp/2. Projeto de Mestrado/Script/Dendrometros Chapada/"
wd_txt <- "C:/Users/Vinicius/Desktop/data"
setwd(wd_txt)
getwd()

# Process data to range (um)
# assuming this datasheet https://www.midoriamerica.com/wp-content/uploads/2018/03/lpfbs3.pdf
totalTravel      = 20*1000 # 20mm travel
totalResistance  = 2000 # 2000 Ohm for 20mm travel
conversionFactor = totalTravel/totalResistance # um per Ohm; 1% linearity, so just assume change is equal across resistance
current = 3.300/2000 # current across the sensor is constant 3.3V/2000Ohm

# read table with the info of all trees connected to each port of each logger
logger_list <- 
  data.frame(id_logger = c("SN097", "SN098", "SN100", "SN102", "SN103"), 
             sen0_adc24 = c("CVAS11_bark", "CVAS10_bark", "CVCE04_bark", "CVCE02_bark", "CVAS13_bark"), 
             sen1_adc24 = c("CVAS11_xylem", "CVAS10_xylem", "CVCE04_xylem", "CVCE02_xylem", "CVAS13_xylem"),
             sen2_adc24 = c("CVAS09_bark", "CVCE01_bark","CVCE03_bark", "CVCE07_bark", "CVAS12_bark"),
             sen3_adc24 = c("CVAS09_xylem", "CVCE01_xylem","CVCE03_xylem", "CVCE07_xylem","CVAS12_xylem")) %>% 
  pivot_longer(-id_logger, 
               names_to = "id_port",
               values_to = "tree_info",
               values_drop_na = TRUE)

#### Join dendrometer data sets (using for loop) ####

# Create file list of the data logger files (per logger + date)
df_files_ms <- data.frame(file_list = dir(path = "C:/Users/Vinicius/Desktop/data/morada/24-02-25/dendrometers")) %>%
                            mutate(file_path = paste("C:/Users/Vinicius/Desktop/data/morada/24-02-25/dendrometers", file_list, sep = "/"),
                                   location = "Morada do Sol")
                          
df_files_am <- data.frame(file_list = dir(path = "C:/Users/Vinicius/Desktop/data/aldeia/24-01/dendrometers")) %>%
                            mutate(file_path = paste("C:/Users/Vinicius/Desktop/data/aldeia/24-01/dendrometers", file_list, sep = "/"),
                                   location = "Aldeia Multiétnica")  

df_files_all <- rbind(df_files_ms, df_files_am)

# Empty data frame to receive the data from the for loop
logger_data <- data.frame()

for (i in df_files_ms$file_path) {
  file_path <- i  # Adicionando essa linha para definir o caminho do arquivo atual
  data_temp <- read.csv(file_path) %>% 
    rename(date = datetime, 
           internalT = internalT_moC,
           battery = battery_mV,
           sen0_adc24 = 4, sen1_adc24 = 5, sen2_adc24 = 6, sen3_adc24 = 7) %>% 
    mutate(id_logger = substr(i, 61, 65),
           date = as.POSIXct(date, tz = "GMT", format = "%y/%m/%d %H:%M:%S"))
  logger_data <- rbind(logger_data, data_temp)  # Adicionando os dados temporários ao quadro de dados principal
} 

unique(df_long$id_port)


df_long <- logger_data %>% 
  relocate(id_logger) %>% 
  pivot_longer(-c("id_logger", "date", "internalT", "battery"), 
               names_to = "id_port",
               values_to = "dend_value",
               values_drop_na = TRUE) %>% 
  mutate(data_mv    = dend_value * (3300 / 2^23),
         Resistance = data_mv / current / 1000,
         Position   = totalTravel - (Resistance * conversionFactor)) %>% #inverse?
  left_join(logger_list) %>% 
  separate(tree_info, into = c("id_tree", "location"), sep = "_") %>% 
  mutate(logger_port = paste(id_logger, id_port, sep = "_")) %>% 
  arrange(id_logger, id_port, date)

str(df_long)


#### Graphs dendrometer data checks ####

## Calculate the mean position of each dendrometer
mean_pos <- df_long %>% 
  group_by(logger_port) %>% 
  summarize(mean_pos = mean(Position, na.rm = T))

## Use mean position to calculate relative changes and absolute deviations
df_long <- df_long %>% 
  left_join(mean_pos) %>% 
  mutate(change_index_abs = Position - mean_pos, 
         change_index_rel = Position / mean_pos)

## Criate the id overview
df_long <- df_long %>%
  mutate(
    den = substr(id_port, 1,4))

df_long <- df_long %>%
  mutate(df_overview = paste(id_tree, location, den, sep = "_"))


## Graph of absolute deviations (in micrometer for each dendrometer)

df_long %>% 
  mutate(logger_tree_id = paste(id_tree, id_logger, location, sep = " - ")) %>% 
  ggplot(aes(x = date, y = change_index_abs, col = id_tree)) +
  geom_line() +
  facet_wrap(~logger_tree_id, ncol = 4, scales = "free") +
  theme_classic() +
  ylab("Growth deviation from mean (um)") +
  xlab("") +
  theme(legend.position = "none")

## Relative dendrometer changes (% change compared to mean)
df_long %>% 
  mutate(logger_tree_id = paste(id_logger, id_tree, sep = " - ")) %>% 
  ggplot(aes(x = date, y = change_index_rel, col = location)) +
  geom_line() +
  facet_wrap(~logger_tree_id) +
  theme_classic()+
  ylab("Relative growth changes")+
  xlab("")+
  theme(legend.position = "top", 
        legend.background = element_blank())

## Internal temp along the days
df_long %>% 
  mutate(logger_tree_id = paste(id_logger, id_tree, sep = " - ")) %>% 
  ggplot(aes(x = date, y = internalT/1000)) +
  geom_line() +
  ylab("Internal T (ºC)")+
  facet_wrap(~logger_tree_id) +
  theme_classic()

## Internal temp versus relative growth changes (should give linear corr for empty ports)
df_long %>% 
  mutate(logger_tree_id = paste(id_logger, id_tree, sep = " - ")) %>% 
  ggplot(aes(x = internalT/1000, y = change_index_rel, col = location)) +
  geom_point(size = 0.8) +
  ylab("Relative growth changes")+
  xlab("Internal T (ºC)")+
  facet_wrap(~logger_tree_id) +
  theme_classic()

#### Old script Bittencourt ####

# Process data to voltage (mV)
for(col in c("dend_value")) {df_long[,col] = df_long[,col]*3300/2^23}

for(col in c("dend_value")) { # calculate resistance and position
  resistance = paste(col,"Resistance", sep = "")
  position =  paste(col,"Position", sep = "")
  df_long[,resistance] = df_long[,col]/current/1000 # we are measuring one of the resistors in the sensor tension divider; "denX" is the voltage in that resistor; divide by 1000 to go from mV to V
  df_long[,position] = df_long[,resistance]*conversionFactor
  df_long[,position] = totalTravel - df_long[,position] # check this line, does the position decrease or increase when you press? I am assuming it increases so we do  have to remove max travel
}




#### Plot time series 
output = paste(tempfile(), ".pdf", sep = "")
pdf(output, width = 20)
par(mar = c(6,6,1,1))

for(col in c("df_overview")){
  resistance = paste(col,"Resistance", sep = "")
  position =  paste(col,"Position", sep = "")
  outlier = boxplot(data[, ..col], plot = F)$out
  workTable = data[!data[, ..col] %in% outlier,]
  plot(data[, ..col] ~ data$date, cex = 2, cex.axis = 2, cex.lab = 2, lwd = 2, xlab = "Date", ylab = "Voltage (mV)")
  mtext(col)
  plot(workTable[, ..col] ~ workTable$date, cex = 2, cex.axis = 2, cex.lab = 2, lwd = 2, xlab = "Date", ylab = "Voltage (mV)")
  mtext(col)
  plot(workTable[, ..resistance] ~ workTable$date, cex = 2, cex.axis = 2, cex.lab = 2, lwd = 2, xlab = "Date", ylab = "Resistance (Ohm)")
  mtext(col)
  plot(workTable[, ..position] ~ workTable$date, cex = 2, cex.axis = 2, cex.lab = 2, lwd = 2, xlab = "Date", ylab = "Position (um)")
  mtext(col)
}

dev.off()
shell(output, wait = F)
# the graphs look ok, although there is very little data to get to conclusions.
# in a longer timer series, the xylem should not move up and down except for daily patterns
# the bark should have growth and shrinkage at scales of week and quickly increase after rain in the dry season


#### Plot correlations -------------------------------------------------------
# outliers to NA
for(col in c("df_overview")){
  outlier = boxplot(df_long[,col], plot = F)$out
  df_long[df_long[,col] %in% outlier, col] = NA
}

plot(den0Position ~ den1Position, data) # this is quite nice, they correlate well as they should. Changes in total tree diameter = change in bark + xylem
# depending on tree some signals may dominate
diff(range(data$den0Position , na.rm = T)); diff(range(data$den1Position , na.rm = T))
# note how both dendrometers are having similar changes of ~200um. I would guess den1 is the one in the bark, with higher fluctuation.
# In a longer time series, the bark one should have a much wider range as bark will grow.
plot(den0Position  ~ den2Position , data) # very week correlation in the ones without sensor, its just a phantom signal from the floating connector
plot(den0Position  ~ den3Position , data)


#### Load T+RH data -----------------------------------------------------------
#file = "SN105-23-07-06.csv"
#enviro = read.csv(file)
#str(enviro)
#names(enviro) = c("date","internalT","battery","humidity", "temperature")
#enviro$date = as.POSIXct(enviro$date, tz = "GMT", format = "%y/%m/%d %H:%M:%S")
#enviro$temperature = enviro$temperature/1000
#enviro$humidity = enviro$humidity/1000
#enviro$vpd = getVpd(enviro$temperature, enviro$humidity)

#### Load soil sensor ---------------------------------------------------------------
#file = "SN106-23-07-06.csv"
#swc = read.csv(file)
#str(swc)
#names(swc) = c("date","internalT","battery","swc")
#swc$date = as.POSIXct(swc$date, tz = "GMT", format = "%y/%m/%d %H:%M:%S")
#SWC needs to be converted to voltage and then to the manual calibration, remember me to give you this in the future



#### Merge datasets ----------------------------------------------------------
data = merge(data, enviro[,c("date","temperature","humidity","vpd")], by = "date", all = T)
data = merge(data, swc[,c("date","swc")], by = "date", all = T)

#### Co-plot -----------------------------------------------------------------
xlim = range(data$date)
par(mar = c(6,6,1,6))
plot(den2Position ~ date, data, cex = 2, cex.axis = 2, cex.lab = 2, lwd = 2, xlab = "Date", ylab = "Position xylem (um)", type = "l", col = "blue", xlim = xlim)
par(new = T)
plot(den3Position ~ date, data, cex = 2, cex.axis = 2, cex.lab = 2, lwd = 2, xlab = "Date",  yaxt = "n", type = "l", col = "green", ylab = "", xlim = xlim)
axis(4)
mtext("Position bark (um)", side = 4, line = 3, col = "green", cex = 2)
par(new = T)
plot(vpd ~ date, data[!is.na(data$vpd),], cex = 2, cex.axis = 2, cex.lab = 2, lwd = 2, xlab = "Date",  yaxt = "n", type = "l", col = "red", ylab = "", xlim = xlim)
par(new = T)
plot(swc ~ date, data[!is.na(data$swc),], cex = 2, cex.axis = 2, cex.lab = 2, lwd = 2, xlab = "Date",  yaxt = "n", type = "l", col = "brown", ylab = "", xlim = xlim)

# Note that the peaks are not trully alligned, this is very good as it means the two sensors are not following some environmental noise like temperature
# the position is at aminimum with high VPD (sensor at ground, so not representing the overall canopy), which seems right
# no response of the tree when the SWC sensor went up (watered?), but the tree is so big it might not even have felt


#### Dendrometer vs VPD -----------------------------------------------------
data$unixDay = trunc(as.numeric(data$date)/60/60/24) # day number counting from unix reference (1970/01/01)
data = data[!data$unixDay %in% min(data$unixDay),] # remove first day as it had weird data (bark wounding?)
coplot(den0Position ~ vpd | as.factor(unixDay), data)
fit = lm(den0Position ~ vpd * as.factor(unixDay), data)
summary(fit)

coplot(den1Position ~ vpd | as.factor(unixDay), data)
fit = lm(den1Position ~ vpd * as.factor(unixDay), data)
summary(fit)


plot(den0Position ~ temperature, data)
fit = lm(den0Position ~ temperature, data)
summary(fit)

plot(den1Position ~ temperature, data)
fit = lm(den1Position ~ temperature, data)
summary(fit)

#den1 is tightly coupled to VPD with a much more sensitive slope. In a long time series you would probably have day as a random factor and remove days with rain to get proper vpd response
