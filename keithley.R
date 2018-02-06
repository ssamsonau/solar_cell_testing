library(serial)
library(stringr)

# for commands see http://research.physics.illinois.edu/bezryadin/labprotocol/Keithley2400Manual.pdf
# basic examples on page 14-21 (page 345)

####### Make connection

con <- serialConnection(name = "con1",
                        port = "COM3",
                        translation = "cr",
                        mode = "9600,n,8,1",
                        newline = 1)

open(con); Sys.sleep(1)

write.serialConnection(con, "*IDN?"); Sys.sleep(1)
read.serialConnection(con); Sys.sleep(1)



write.serialConnection(con, ":SENS:FUNC 'RES' "); Sys.sleep(1)

write.serialConnection(con, ":SENS:RES:NPLC 1"); Sys.sleep(1)


write.serialConnection(con, "*RST"); Sys.sleep(1)
write.serialConnection(con, ":SENS:FUNC 'RES' "); Sys.sleep(1)
write.serialConnection(con, ":SENS:RES:NPLC 1"); Sys.sleep(1)
write.serialConnection(con, ":SENS:RES:MODE MAN"); Sys.sleep(1)
write.serialConnection(con, ":SOUR:FUNC CURR"); Sys.sleep(1)
write.serialConnection(con, ":SOUR:CURR 0.01"); Sys.sleep(1)
write.serialConnection(con, ":SOUR:CLE:AUTO ON"); Sys.sleep(1)
write.serialConnection(con, ":SENS:VOLT:PROT 10"); Sys.sleep(1)
write.serialConnection(con, ":TRIG:COUN 1"); Sys.sleep(1)
write.serialConnection(con, ":FORM:ELEM RES"); Sys.sleep(1)

write.serialConnection(con, ":Read?"); Sys.sleep(1)
read.serialConnection(con); Sys.sleep(1)




close(con)

