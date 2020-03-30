################################################################################
################################################################################
################ Generate measurement datasets ################ 
library(tidyverse)
library(datapasta)
library(broom)
library(magrittr)
library(purrr)
library(scales)

################################################################################
################################################################################
# Funkcija za generiranje setova za svakog studenta
generate_datasets <- function(JMBAG) {
  set.seed(as.numeric(JMBAG))
  sample.size <- 10000
  sample.max.time <- 1 # [s]
  num.peaks <- round(runif(1, min = 3, max = 5))
  peak.freqs <- sample(seq(10, 1000, by = 10), num.peaks)
  peak.amps <- runif(peak.freqs, 0.1, 0.5)
  time.sample <- seq(1:sample.size)*(sample.max.time/sample.size)
  test.dat <- rnorm(time.sample, mean = 0, sd = 1) # white noise
  for (i in seq(1:length(peak.freqs))) {
    test.dat = test.dat + peak.amps[i]*sin(2*pi*peak.freqs[i]*time.sample)
  }
  # print(tibble(Frequencies = peak.freqs,
  #              Amplitudes = peak.amps))
  return(tibble(
    data = test.dat,
    time = time.sample,
    freqs = replace(peak.freqs, (length(peak.freqs)+1):length(time), 0),
    amps = replace(peak.amps, (length(peak.amps)+1):length(time), 0)
    ))
}

# with(generate_datasets(20),
#      plot(time, data, type = 'l')
#      print(amps)
# )

################################################################################
################################################################################
# Imena studentata (tribble_paste)
dat <- tibble::tribble(
        ~JMBAG,       ~Ime,     ~Prezime,
  "0000000001", "Tomislav",     "Bazina",
  "0069074051",     "Luka",     "Bandov",
  "0069073033",   "Dorian",      "Bojić",
  "0248018000",    "Filip",     "Bratoš",
  "0035199507",    "Josip",      "Brčić",
  "0069073509",    "Filip",     "Čendak",
  "0069074499",    "Boris", "Gašparović",
  "0035177254",   "Hrvoje",     "Koštić",
  "0069072431",  "Adriano",    "Kovaček",
  "0069070512",    "Karlo",    "Matušan",
  "0069072335", "Tomislav",       "Ploh",
  "0069074072",    "Goran",   "Štefanić",
  "0069071268",    "Jasen", "Zenzerović",
  "0035194857",  "Izidora",   "Zgrablić",
  "0069073215",   "Jurica",     "Zlatar"
  )


################################################################################
################################################################################
# Pohrana podataka
dat %>% 
  mutate(sample.dataset = lapply(JMBAG, generate_datasets)) %>%
  mutate(sample.dataset = lapply(
    sample.dataset, function(df) rename(df, "t (sec)" = time, "Voltage (V)" = data)
      )) %>%
  mutate(write.name = paste0("datasets/", JMBAG, "_", Prezime, "_", Ime, ".csv")) %>%
  pmap(function(JMBAG, Ime, Prezime, sample.dataset, write.name)
    write_csv2(sample.dataset, path = write.name))
         