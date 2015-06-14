
# Prerequisite
#require(devtools)
#devtools::install_github('influxdb-r', 'influxdb')


library(influxdb)
results <- influxdb_query('localhost', 8086, 'root', 'root', 'test',
                                   'SELECT * FROM emg limit 50',
                                   time_precision = 'm')


d <- as.data.frame(results)

library(ggplot2)
library(reshape2)

results <- influxdb_query('localhost', 8086, 'root', 'root', 'test',
                        'SELECT * FROM emg limit 50',
                        time_precision = 's')
d <- as.data.frame(results)
m <- melt(d, id.vars=c("emg.time","emg.sequence_number"))
p <- ggplot(m, aes(x = emg.sequence_number, y = value, color = variable)) + geom_line() 
p + facet_wrap(  ~ variable, ncol = 1)


