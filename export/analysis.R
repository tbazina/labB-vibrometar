################################################################################
#################### Analiza rezultata mjerenja vibracija ######################
################################################################################
# Potrebni paketi
# Rukovanje podacima
library(tidyverse)
# Dijagrami
library(ggplot2)
# Osi dijagrama
library(scales)
# Pretrazivanje vrsnih vrijednosti
library(pracma)

################################################################################
# Učitavanje podataka
library(readr)
data <- read_delim("datasets/0000000001_Bazina_Tomislav.csv", 
                   ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                   trim_ws = TRUE)
# TODO: Ucitati vlastiti dataset iz foldera dataset pod nazivom JMBAG_Prezime_Ime.csv

# Ćišćenje podataka
data <- data %>% 
  select(-amps, -freqs) %>%
  rename(voltage = `Voltage (V)`, time = `t (sec)`) %>%
  # Centriranje podataka oko nule
  mutate(voltage = voltage - mean(voltage))

################################################################################
# Vizualizacija ulaznih podataka (napon - vrijeme)
ggplot(data = data, aes(x = time, y = voltage)) +
  geom_line(color = 'dodgerblue4') + 
  labs(title="Rezultat mjerenja vibracija", x = "Vrijeme [s]", y = "Napon [V]") +
  scale_x_continuous(
    breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.025)) + # Grid x
  scale_y_continuous(
    breaks = seq(-10 , 10, 1), minor_breaks = seq(-10, 10, 0.25)) + # Grid y
  theme(plot.title = element_text(hjust = 0.5))
# TODO: Prikazati izlazne podatke mjernog uradaja pomocu dijagrama (prilagoditi osi)

################################################################################
#################### Analiza u vremenskoj domeni ######################
# Pretvaranje ulaznog signala u brzinu vibracija
# Kalibracijska konstantna za LOW radno područje - 4990 um/(s*V)
# Brzina vibracija u um/s
data <- data %>%
  mutate(velocity = voltage * 4990)
# TODO: Pretvoriti signal napona u brzinu vibracija (pojasniti vezu i opcije)

# Duljina signala u vremenskoj domeni
time.size <- length(data$velocity)
# Vremensko trajanje signala [s]
time.time <- max(data$time)
# Amplituda [um/s]
time.min <- min(data$velocity)
time.max <- max(data$velocity)
# Peak-to-Peak vrijednost [um/s]
time.peak.to.peak <- time.max - time.min
# Root mean square vrijednost [um/s]
time.RMS <- sqrt(mean(data$velocity^2))

# Ispis parametara dobivenih analizom u vremenskoj domeni
print(paste("Duljina uzorka:", time.size))
print(paste("Vremensko trajanje uzorka:", time.time, "s"))
print(paste("Ampituda (min):", time.min, "um/s"))
print(paste("Ampituda (max):", time.max, "um/s"))
print(paste("Peak-to-Peak:", time.peak.to.peak, "um/s"))
print(paste("Root mean square:", time.RMS, "um/s"))
# TODO: Pojasniti analizu vibracija u vremenskoj domeni

# Vizualizacija brzine vibracija u vremenskoj domeni (brzina - vrijeme)
ggplot(data = data) +
  geom_line(aes(x = time, y = velocity), color="dodgerblue4") +
  geom_hline(aes(yintercept = time.RMS, color="RMS"),
             linetype = "dashed") +
  geom_hline(aes(yintercept = time.min, color="Peak-to-Peak")) +
  geom_hline(aes(yintercept = time.max, color="Peak-to-Peak")) +
  scale_color_manual(values = c(
    'RMS' = 'red',
    'Peak-to-Peak' = 'blueviolet'
  )) +
  labs(title="Brzina vibracija u vremenu", x = "Vrijeme [s]",
       y = expression("Brzina ["*mu*m/s~"]"), color="") +
  scale_x_continuous(
    breaks = seq(0, 1, 0.1),
    minor_breaks = seq(0, 1, 0.025),
    limits = c(0, 1)) + # Grid x
  scale_y_continuous(
    breaks = seq(-50000, 50000, 5000),
    minor_breaks = seq(-50000, 50000, 1250)) + # Grid y
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
# TODO: Prikazati ovisnost brzina - vrijeme pomocu dijagrama (prilagoditi osi)
  
# Test stacionarnosti
# Set podataka podijeljen na 10 segmenata
# RMS segmenta / RMS cijelog seta podataka
# Dijagram stacionarnosti
data %>%
  mutate(split.segments = rep(1:10, each=ceiling(n()/10))[1:n()]) %>%
  group_by(split.segments) %>%
  summarise(segment.rms = sqrt(mean(velocity^2))) %>%
  mutate(segment.rms.n = segment.rms/time.RMS) %>%
  ggplot() +
    geom_line(aes(x = split.segments, y = segment.rms.n), color="dodgerblue4") +
    geom_hline(aes(yintercept = 1, color="RMS"),
               linetype = "dashed") +
    geom_hline(aes(yintercept = min(segment.rms.n), color="Raspon")) +
    geom_hline(aes(yintercept = max(segment.rms.n), color="Raspon")) +
    scale_color_manual(values = c(
      'RMS' = 'red',
      'Raspon' = 'blueviolet'
    )) +
    labs(title="Stacionarnost vibracija po segmentima", x = "Segment [ / ]",
         y = "Normalizirani RMS [ / ]", color="") +
    scale_x_continuous(
      breaks = seq(0, 10, 1), minor_breaks = seq(0, 10, 0.5)) + # Grid x
    scale_y_continuous(
      breaks = seq(0, 2, 0.01), minor_breaks = seq(0, 2, 0.0025)) + # Grid y
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
#TODO: Prikazati stacionarnost vibracija pomocu dijagrama i pojasniti (postotak)

################################################################################
#################### Analiza u frekvencijskoj domeni ######################
# FFT transformacija brzine vibracija
# Zadržava se samo prva polovica FFT-a
# Druga polovica su kompleksno konjugirani brojevi prve polovice
bin.num <- time.size / 2 # Razlucivost FFT-a
fft.complex <- fft(data$velocity)[1:bin.num]
# TODO: Pojasniti kako se određuje razlucivost FFT-a

# Frekvencija uzorkovanja (sample rate) [Hz]
# Duljina signala / ukupno vrijeme uzrkovanja
sample.rate <- time.size / time.time
# Ili 1 / vremenska razlika dva uzastopna uzorka (dt)
sample.rate <- 1 / (data$time[2] - data$time[1])
print(paste("Frekvencija uzorkovanja:", sample.rate, "Hz"))
# TODO: Pojasniti frekvenciju uzorkovanja i kako se odreduje potrebna vrijednost

# Diskretni intervali FFT-a (bin) [Hz]
# Krecu od 0 pa do polovice sampling.rate-a
fft.freq <- seq(
  from=0, by=sample.rate/time.size, length.out=bin.num
)

# Spektralna gustoca snage brzine vibracija po frekvencijskom spektru (PSD)
# [um^2/(s^2*Hz)]
fft.psd <- (1 / (sample.rate * time.size)) * Mod(fft.complex) ^ 2
fft.psd[-1] <- 2 * fft.psd[-1]
# TODO: Pojasniti sto je i kad se koristi gustoca snage vibracija 

# Amplitude brzine vibracija po frekvencijskom spektru [um/s]
fft.amp <- fft.complex / time.size
fft.amp[-1] <- 2 * fft.amp[-1]
fft.amp <- Mod(fft.amp)
# TODO: Pojasniti za sto je korisna amplituda vibracija 

# Pohranjivanje analize u frekvencijskoj domeni u data.frame
data.fft <- tibble(
  amplitude = fft.amp,
  psd = fft.psd,
  freq = fft.freq,
  sample.rate = sample.rate,
  bin.num = bin.num,
  fft.complex = fft.complex
)

# Vizualizacija amplitude brzine vibracija u frekvencijskoj domeni
# (brzina - frekvencija)
ggplot(data = data.fft) +
  geom_segment(aes(x = freq, y=0, xend = freq, yend = amplitude), color="dodgerblue4") +
  labs(title="FFT brzine vibracija", x = "Frekvencija [Hz]",
       y = expression("Amplituda brzine ["*mu*m/s*"]"), color="") +
  scale_x_continuous(
    breaks = seq(0, bin.num, 100), minor_breaks = seq(0, bin.num, 25),
    limits=c(0, 1000)) + # Grid + limits x
  scale_y_continuous(
    breaks = seq(0, max(fft.amp)*1.5, 250),
    minor_breaks = seq(0, max(fft.amp)*1.5, 50)) + # Grid y
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
# TODO: Protumaciti dijagram amplituda - frekvencija (prilagoditi osi)

# Vizualizacija spektralne gustoce snage (PSD) brzine vibracija u frekvencijskoj
# domeni (spektralna gustoca snage - frekvencija)
ggplot(data = data.fft) +
  geom_segment(aes(x = freq, y=10, xend = freq, yend = fft.psd),
               color="dodgerblue4") +
  labs(title="PSD brzine vibracija", x = "Frekvencija [Hz]",
       y = expression("Spektralna gustoća snage brzine ["*mu *m^2/(s^2*Hz)*"]")
       ) +
  scale_x_continuous(
    breaks = seq(0, bin.num, 100), minor_breaks = seq(0, bin.num, 25),
    limits=c(0, 1000)
    ) + # Grid + limits x
  scale_y_log10(
    limits=c(10, NA),
    breaks = trans_breaks('log10', function(x) 10^x),
    labels = trans_format('log10', math_format(10^.x)),
    ) + # Grid y
  annotation_logticks() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
# TODO: Protumaciti dijagram spektralna gustoca snage- frekvencija (prilagoditi osi)
# Napomena: y >= prvi broj u limits

# Top 5 vrsnih vrijednosti amplitude vibracija
peaks <- tibble(
  peak.amplitude.ind = findpeaks(
    data.fft$amplitude, nups=1, ndowns=1, npeaks=5, sortstr=T)[,2],
  peak.frequency = data.fft$freq[peak.amplitude.ind],
  peak.amplitude = data.fft$amplitude[peak.amplitude.ind],
  peak.psd = data.fft$psd[peak.amplitude.ind]
) %>%
  select(-peak.amplitude.ind) %>%
  rename(
    "Frekvencija [Hz]" = peak.frequency,
    "Amplituda [um/s]" = peak.amplitude,
    "PSD [um2/(s2*Hz)]" = peak.psd) %>%
  write_excel_csv2(path = "peaks.csv")
# TODO: Izdvojiti znacajne vrsne vrijednosti
# TODO: Na kojim frekvencijama je sadrzano najvise snage vibracija

