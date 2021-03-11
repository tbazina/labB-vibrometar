################################################################################
############# LabB - Pripremanje podataka s vibrometra za analizu ##############
################################################################################
# Potrebni paketi
# Rukovanje podacima
library(tidyverse)
# Runs test
# library(tseries)
# Data Table
# library(data.table)
# Cleaning data
# library(magrittr)
# library(broom)


# Data inputa as tibble
df.orig <- read_tsv(
  file = 'prigusenje',
  locale = locale(decimal_mark=c(',')),
  col_types = "nnnnnnnnn"
  )

# Renaming Displacement -> Max Displacement
names(df.orig)[6:8] <- paste('Peak', c('Vel (um/s)', 'Disp (nm)', 'Acc (g)'))

# Calibration constants and velocity range in um/(s*V)
cal_const <- c(4990, 98910)
names(cal_const) <- c('LO', 'HI')


if (df.orig[['Range(HI/LO)']][1] == 1) {
  vel.range <- 'LO'
} else {
  #TODO: Check if 2 corresponds to high velocity range
  vel.range <- 'HI'
}

# Single value information
info <- with(
  df.orig,
  tibble(
    range = vel.range,
    d.time = `dt  (sec)`[1],
    sample.rate = 1 / d.time,
    sample.num = nrow(df.orig)
  )
)

# Time-domain tibble
time.domain <- with(
  df.orig,
  tibble(
    time = (1:info$sample.num - 1) * info$d.time,
    voltage = `Voltage (V)`,
    # Removing DC component from signal before calculating velocity
    velocity = (voltage - mean(voltage)) * cal_const[info$range]
  )
)

time.domain %>% select(time, velocity) %>% write.csv('prigusenje_clean.csv')

