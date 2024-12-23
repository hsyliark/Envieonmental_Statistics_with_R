# round -> site

heat_data <- read.csv("C:/Users/User/Desktop/R MixSIAR 분석결과 정리_영산강/2024 환경기초조사사업 관련/BIMM 적합결과/6차 분석/결과/heatmap_ver1_re/heatmap_isotope_ver1_round_site.csv",
                      sep=",",header=T)

library(reshape2)
heat_data_melt <- melt(heat_data, id.vars=c('site','round'))
heat_data_melt_round01 <- heat_data_melt[heat_data_melt$round=="round01",]
heat_data_melt_round02 <- heat_data_melt[heat_data_melt$round=="round02",]
heat_data_melt_round03 <- heat_data_melt[heat_data_melt$round=="round03",]
heat_data_melt_round04 <- heat_data_melt[heat_data_melt$round=="round04",]
heat_data_melt_round05 <- heat_data_melt[heat_data_melt$round=="round05",]
heat_data_melt_round06 <- heat_data_melt[heat_data_melt$round=="round06",]
heat_data_melt_round07 <- heat_data_melt[heat_data_melt$round=="round07",]
heat_data_melt_round08 <- heat_data_melt[heat_data_melt$round=="round08",]
heat_data_melt_round09 <- heat_data_melt[heat_data_melt$round=="round09",]
heat_data_melt_round10 <- heat_data_melt[heat_data_melt$round=="round10",]
heat_data_melt_round11 <- heat_data_melt[heat_data_melt$round=="round11",]
heat_data_melt_round12 <- heat_data_melt[heat_data_melt$round=="round12",]
heat_data_melt_round13 <- heat_data_melt[heat_data_melt$round=="round13",]
heat_data_melt_round14 <- heat_data_melt[heat_data_melt$round=="round14",]

library(ggplot2)
ggplot(heat_data_melt_round01, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round01",
       x = "source",
       y = "site") 

ggplot(heat_data_melt_round02, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round02",
       x = "source",
       y = "site")

ggplot(heat_data_melt_round03, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round03",
       x = "source",
       y = "site")

ggplot(heat_data_melt_round04, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round04",
       x = "source",
       y = "site")

ggplot(heat_data_melt_round05, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round05",
       x = "source",
       y = "site")

ggplot(heat_data_melt_round06, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round06",
       x = "source",
       y = "site")

ggplot(heat_data_melt_round07, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round07",
       x = "source",
       y = "site")

ggplot(heat_data_melt_round08, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round08",
       x = "source",
       y = "site")

ggplot(heat_data_melt_round09, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round09",
       x = "source",
       y = "site")

ggplot(heat_data_melt_round10, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round10",
       x = "source",
       y = "site")

ggplot(heat_data_melt_round11, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round11",
       x = "source",
       y = "site")

ggplot(heat_data_melt_round12, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round12",
       x = "source",
       y = "site")

ggplot(heat_data_melt_round13, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round13",
       x = "source",
       y = "site")

ggplot(heat_data_melt_round14, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkgreen", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round14",
       x = "source",
       y = "site")


# round

heat_data <- read.csv("C:/Users/User/Desktop/R MixSIAR 분석결과 정리_영산강/2024 환경기초조사사업 관련/BIMM 적합결과/6차 분석/결과/heatmap_ver1_re/heatmap_isotope_ver1_round.csv",
                      sep=",",header=T)

library(reshape2)
heat_data_melt <- melt(heat_data, id.vars=c('round'))

library(ggplot2)
ggplot(heat_data_melt, aes(x = variable, y = round, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("round14", "round13", "round12", "round11", "round10", "round09", "round08", "round07",
                            "round06", "round05", "round04", "round03", "round02", "round01")) +
  scale_fill_gradient(high = "darkblue", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_round",
       x = "source",
       y = "round") 


# site

heat_data <- read.csv("C:/Users/User/Desktop/R MixSIAR 분석결과 정리_영산강/2024 환경기초조사사업 관련/BIMM 적합결과/6차 분석/결과/heatmap_ver1_re/heatmap_isotope_ver1_site.csv",
                      sep=",",header=T)

library(reshape2)
heat_data_melt <- melt(heat_data, id.vars=c('site'))

library(ggplot2)
ggplot(heat_data_melt, aes(x = variable, y = site, fill = value)) +
  geom_tile() +
  scale_y_discrete(limits=c("JS-36", "JS-35", "JS-34", "JS-33", "JS-32", "JS-31",
                            "JS-30", "JS-29", "JS-28", "JS-27", "JS-26", "JS-25",
                            "JS-24", "JS-23", "JS-22", "JS-21", "JS-20", "JS-19",
                            "JS-18", "JS-17", "JS-16", "JS-15", "JS-14", "JS-13",
                            "JS-12", "JS-11", "JS-10", "JS-09", "JS-08", "JS-07",
                            "JS-06", "JS-05", "JS-04", "JS-03", "JS-02", "JS-01")) +
  scale_fill_gradient(high = "darkblue", low = "white") +
  labs(title = "Heatmap (Contribution Rate of Pollutant Source)_isotope_site",
       x = "source",
       y = "site") 
