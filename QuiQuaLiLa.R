library(readxl)
library(ggplot2)
library(tidyverse)

Data <- read_excel("QuiQuaLiLa.xlsx")

DataG <- gather(Data,Genre,Occurrence,3:9)
DataS <- spread(DataG,Word,Occurrence)
DataS$Q <- NA
DataS$L <- NA

for (i in 1:nrow(DataS)) {
  DataS$Q[i] <- 
    binom.test(DataS$qui[i],
               DataS$qui[i] + DataS$qua[i] + 1)$p.value
  }

for (i in 1:nrow(DataS)) {
  DataS$L[i] <- 
    binom.test(DataS$là[i],
               DataS$lì[i] + DataS$là[i] + 1)$p.value
}

DataS$Q <- p.adjust(DataS$Q, method="BH")
DataS$L <- p.adjust(DataS$L, method="BH")

DataQ <- subset(DataS,
                DataS$Q <= 0.1 & DataS$qui > DataS$qua)
DataL <- subset(DataS,
                DataS$L <= 0.1 & DataS$là > DataS$lì)

DataG$Time <- as.factor(DataG$Period) %>% as.numeric()

DataGQ <- subset(DataG,
                 DataG$Word=="qui"|DataG$Word=="qua")

DataGL <- subset(DataG,
                 DataG$Word=="là"|DataG$Word=="lì")

DataGag <- aggregate(Occurrence ~ Time*Word, DataG, sum)
DataGQag <- aggregate(Occurrence ~ Time*Word, DataGQ, sum)
DataGLag <- aggregate(Occurrence ~ Time*Word, DataGL, sum)
DataGenre <- aggregate(Occurrence ~ Word*Genre, DataG, sum)

ggplot(DataGL,
       aes(x = Period,
           y = Occurrence,
           color = Word,
           shape = Genre)) + 
  geom_point(size=3) + 
  scale_shape_manual(values = 0:6) +
  scale_color_manual(values = c("skyblue","darkblue")) +
  theme_classic()

ggplot(DataGQ,
       aes(x = Period,
           y = Occurrence,
           color = Word,
           shape = Genre)) + 
  geom_point(size = 3) + 
  scale_shape_manual(values = 0:6) +
  scale_color_manual(values = c("skyblue","darkblue")) +
  theme_classic()

ggplot(DataGag,
       aes(x = Time,
           y = Occurrence,
           color = Word,
           shape = Word)) + 
  geom_point(size = 2) +
  geom_line() +
  scale_x_continuous("Period", labels = unique(DataG$Period)) +
  theme_classic()

ggplot(DataGenre,
       aes(x = Genre,
           y = Occurrence,
           color = Word,
           shape = Word)) + 
  geom_point(size = 2) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


aov(Occurrence ~ Time * Word, DataGQ) %>% summary()

aov(Occurrence ~ Time * Word, DataGL) %>% summary()
