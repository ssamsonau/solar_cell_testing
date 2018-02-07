library(serial)
library(stringr)

# for commands see http://research.physics.illinois.edu/bezryadin/labprotocol/Keithley2400Manual.pdf
# basic examples on page 10-24

####### Make connection

con <- serialConnection(name = "con1",
                        port = "COM3",
                        translation = "cr",
                        mode = "9600,n,8,1",
                        newline = 1)

open(con); Sys.sleep(1)

write.serialConnection(con, "*IDN?"); Sys.sleep(1)
read.serialConnection(con); Sys.sleep(1)


write.serialConnection(con, "*RST ");  
#write.serialConnection(con, ":TRIG:CLEar ");  
write.serialConnection(con, ":SENS:FUNC:CONC OFF ");


#find open circuit current
write.serialConnection(con, ":SOUR:FUNC VOLT "); 
write.serialConnection(con, ":SOUR:VOLT 0 ");
write.serialConnection(con, ":SENS:FUNC 'CURR:DC' ");  
write.serialConnection(con, ":SENS:CURR:PROT 1 ");  
write.serialConnection(con, ":TRIG:COUN 1"); Sys.sleep(1)
write.serialConnection(con, ":OUTP ON "); Sys.sleep(1)
write.serialConnection(con, ":Read?"); Sys.sleep(1)
Isc <- read.serialConnection(con); Sys.sleep(1)
Isc <- str_split(Isc, ",", simplify = T)[2] %>% as.numeric()



write.serialConnection(con, ":SOUR:FUNC CURR ");  
write.serialConnection(con, ":SENS:FUNC 'VOLT:DC' ");  
write.serialConnection(con, ":SENS:VOLT:PROT 3 ");  
write.serialConnection(con, paste0(":SOUR:CURR:START ", 0));  
write.serialConnection(con, paste0(":SOUR:CURR:STOP ", Isc)); 
write.serialConnection(con, paste0(":SOUR:CURR:STEP ", Isc/50)); 
write.serialConnection(con, ":SOUR:CURR:MODE SWE ");  
write.serialConnection(con, ":SOUR:SWE:RANG AUTO ");  


write.serialConnection(con, ":SOUR:SWE:SPAC LIN ");  
write.serialConnection(con, ":TRIG:COUN 50 ");  
write.serialConnection(con, ":SOUR:DEL 0.01 ");  
write.serialConnection(con, ":OUTP ON "); Sys.sleep(1)

write.serialConnection(con, ":Read?"); Sys.sleep(5+0.01*2*150)


a <- read.serialConnection(con); Sys.sleep(1)

write.serialConnection(con, ":OUTP OFF "); 

b <- str_split(a, ",", simplify = T) %>%
  as.numeric()

curr  <- b[seq(2, length(b), by = 5)]
volt  <- b[seq(1, length(b), by = 5)]
plot(x=curr, y =volt)

close(con)

