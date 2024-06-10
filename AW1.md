---
title: "Analiza Wielowymiarowa nr 1"
author: "Barbara Kania"
output:
  pdf_document: default
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(lubridate)
library(zoo)
library(tseries)
library(vars)
library(ggplot2)
library(readr)
library(readxl)
```

# Wstęp

Celem projektu jest opracowanie modelu VAR, który będzie podstawą do analizy przyczynowości w kontekście Grangera. 
VAR(wektorowa autoregresja) to model autoregresyjny dla wielu wymiarów. Służy do znajdowania zależności między wieloma zmiennymi jednocześnie, zmieniającymi się w czasie. W niniejszym projekcie będziemy badać, jak zmiany w cenach na poszczególnych rynkach wpływają na siebie nawzajem. Model VAR pozwoli nam więc zidentyfikować kierunki przepływu wpływów oraz ocenić, jak duży jest ten wpływ.

Dzięki temu uzyskamy informacje jak poszczególne rynki wpływają na siebie. Dane użyte w projekcie pochodzą ze strony stooq.pl, które zakresem obejmują okres od stycznia 2016 do marca 2024 r (dzienne). Zbiór danych będzie zawierał:

- ceny zamknięcia polskiej giełdy - WIG

- ceny ropy naftowej – CB

- ceny zamknięcia 10-letnich obligacji rządowych Polski – 10YPLY.B

- ceny zamknięcia niemieckiej giełdy - DAX

- ceny zamknięcia holenderskiej giełdy - AEX


# 1. Dane
## 1.1 Pobranie danych

```{r}
Aex <- read_csv("https://raw.githubusercontent.com/basia99ka/dane/main/%5Eaex_d.csv")
Dax <- read_csv("https://raw.githubusercontent.com/basia99ka/dane/main/%5Edax_d.csv")
CB <- read_csv("https://raw.githubusercontent.com/basia99ka/dane/main/cb_c_d.csv")
PlnEur <- read_csv("https://raw.githubusercontent.com/basia99ka/dane/main/plneur_d.csv")
UsdEur <- read_csv("https://raw.githubusercontent.com/basia99ka/dane/main/usdeur_d.csv")
Wig <- read_csv("https://raw.githubusercontent.com/basia99ka/dane/main/wig_d.csv")
Yply <- read_csv("https://raw.githubusercontent.com/basia99ka/dane/main/10yply_b_d.csv")
```

## 1.2 Przekształcenie danych
```{r}
# Wybranie odpowiednich kolumn
CB <- CB[, c(1,5)]
Wig <- Wig[, c(1,5)]
Dax <- Dax[, c(1,5)]
Aex <- Aex[, c(1,5)]
UsdEur <- UsdEur[, c(1,5)]
PlnEur <- PlnEur[, c(1,5)]
Yply <- Yply[, c(1,5)]
names(CB) <- c("Date", "CB")
names(Wig) <- c("Date", "Wig")
names(Dax) <- c("Date", "Dax")
names(Aex) <- c("Date", "Aex")
names(UsdEur) <- c("Date", "UsdEur")
names(PlnEur) <- c("Date", "PlnEur")
names(Yply) <- c("Date", "Yply")


# Utworzenie kolumny "Data"
Data <- data.frame(Date = seq(as.Date("2016-01-04"), as.Date("2024-03-22"), by = "day")) %>%
  mutate(Dzien = wday(Date, week_start = 1)) %>%
  filter(!(Dzien == 6 | Dzien == 7))
Data$Dzien <- NULL
Data <- data.frame(Data)
names(Data) <- c("Date")

# Konwersja na obiekt typu Date
CB$Date <- ymd(CB$Date)
Wig$Date <- ymd(Wig$Date)
Aex$Date <- ymd(Aex$Date)
Dax$Date <- ymd(Dax$Date)
UsdEur$Date <- ymd(UsdEur$Date)
PlnEur$Date <- ymd(PlnEur$Date)
Yply$Date <- ymd(Yply$Date)

# Łaczenie danych ze sobą
dane <- Data %>% left_join(Wig, by="Date")
dane <- dane %>% left_join(Aex, by="Date")
dane <- dane %>% left_join(Dax, by="Date")
dane <- dane %>% left_join(CB, by="Date")
dane <- dane %>% left_join(Yply, by="Date")
dane <- dane %>% left_join(UsdEur, by="Date")
dane <- dane %>% left_join(PlnEur, by="Date")
```

## 1.3  Braki w danych

Na początku usunięte zostały wiersze, gdzie występowało więcej niż 5 brakujących wartości.
```{r}
del <- which(rowSums(is.na(dane)) >= 5)
dane <- dane[-del, ]
```

## 1.4  Uzupełnienie braków danych

Następnie zastosowano metodę interpolacji liniowej. Interpolacja liniowa polega na estymowaniu brakujących wartości na podstawie wartości sąsiadujących punktów danych, zakładając liniową zależność między nimi. W R służy do tego funkcja na.approx().

```{r}
for (i in 2:ncol(dane)) 
{ 
  dane[,i] <- na.approx(dane[,i], na.rm = FALSE)
}
```

## 1.5 Przeliczenie wartości 

Dane początkowo zawierały wartości w różnych walutach poszczególnych krajów. Ceny zostały przeliczone na euro (Euro) zgodnie z bieżącymi kursami wymiany walut. Dzięki temu uzyskano spójność w jednostkach pieniężnych, co ułatwia analizę i interpretację danych.
```{r}
dane <- dane %>% dplyr::mutate(Wig = Wig * PlnEur, CB = CB * UsdEur , Yply = Yply * PlnEur)
dane <- dane %>% dplyr::select("Date", "Wig", "Dax","Aex", "CB", "Yply")
head(dane)
```
# Wykres- INDEKSY

```{r}
plot_Index <- ggplot(data = dane) + 
  geom_line(aes(x = Date, y = Wig, color = "Wig")) + 
  geom_line(aes(x = Date, y = Dax, color = "Dax")) + 
  geom_line(aes(x = Date, y = Aex, color = "Aex")) + 
  geom_line(aes(x = Date, y = CB, color = "CB")) +
  geom_line(aes(x = Date, y = Yply, color = "Yply")) + 
  scale_fill_discrete(name = "Indeks") + 
  ggtitle("Wykres - INDEKSY") +
  xlab("Date") + ylab("Euro")+
  theme_minimal() + 
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

plot_Index
```

# 2. Stopy zwrotu

## 2.1 Obliczenie stopy zwrotu

```{r}
stopy <- as.data.frame(matrix(0, nrow(dane)-1, 6))
stopy[,1] = dane$Date[-1]

for(i in 1:nrow(dane)-1)
{ 
  stopy[i,2]=log(dane[i+1,2] / dane[i,2])
  stopy[i,3]=log(dane[i+1,3] / dane[i,3])
  stopy[i,4]=log(dane[i+1,4] / dane[i,4])
  stopy[i,5]=log(dane[i+1,5] / dane[i,5])
  stopy[i,6]=log(dane[i+1,6] / dane[i,6])
}
colnames(stopy) <- colnames(dane)[]
```

## 2.2 Wykres stop zwrotu

```{r}
plot_Log <- ggplot(data = stopy) + 
  geom_line(aes(x = Date, y = Wig, color = "Wig")) + 
  geom_line(aes(x = Date, y = Dax, color = "Dax")) + 
  geom_line(aes(x = Date, y = Aex, color = "Aex")) + 
  geom_line(aes(x = Date, y = CB, color = "CB")) +
  geom_line(aes(x = Date, y = Yply, color = "Yply")) + 
  scale_fill_discrete(name = "Indeks") + 
  ggtitle("Wykres - stopy zwrotu (log)") +
  xlab("Date") + ylab("Euro")+
  theme_minimal() + 
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

plot_Log 
```
# 3. Model VAR

Etapy estymacji VAR:
1.Sprawdzamy czy wszystkie szeregi czasowe są stacjonarne za pomocą testu ADF. W przypadku niestacjonarności należy zmodyfikować dane tak aby były stacjonarne.
2.Wybór odpowiedniego opóźnienia modelu.
3.Estymacja metodą MNK

Podczas modelowania przy użyciu modeli VAR istotne jest, aby uwzględniać jedynie zmienne stacjonarne. Jeśli zmienne są niestacjonarne, istnieją dwa podejścia do rozwiązania tego problemu. Pierwsze to przekształcenie zmiennych poprzez techniki takie jak standaryzacja, normalizacja lub przekształcenie na różnice. Drugie podejście polega na zastosowaniu modeli VECM do analizy, które są przeznaczone do pracy z niestacjonarnymi zmiennymi

# 3.1 Test na stacjonarność

Hipotezy testu:
H0 : szereg jest niestacjonarny
H1 : szereg jest stacjonarny.

```{r}
results_ADF <- as.data.frame(matrix(0, 1, 5))
results_ADF[1,1] <- adf.test(stopy[,2],alternative = c("stationary"))$p.value
results_ADF[1,2] <- adf.test(stopy[,3],alternative = c("stationary"))$p.value
results_ADF[1,3] <- adf.test(stopy[,4],alternative = c("stationary"))$p.value 
results_ADF[1,4] <- adf.test(stopy[,5],alternative = c("stationary"))$p.value
results_ADF[1,5] <- adf.test(stopy[,6],alternative = c("stationary"))$p.value
names(results_ADF) <- c("Wig", "Dax", "Aex", "CB", "Yply")
results_ADF
```
W każdym przypadku wartość p-value nie przekracza 5%.
Otrzymane p-value wskazuję, że dla wszystkich indeksów hipoteza zerowa została odrzucona, co świadczy, że szeregi są stacjonarne.

## 3.2 Rzad opoznien

```{r}
#x <- VARselect(stopy)$selection
#x
```

## 3.2 Model VAR z p=1

VAR(wektorowa autoregresja) to model autoregresyjny dla wielu wymiarów. Służy do znajdowania zależności między wieloma zmiennymi jednocześnie, zmieniającymi się w czasie. W niniejszym projekcie będziemy badać, jak zmiany w cenach na poszczególnych rynkach wpływają na siebie nawzajem. Model VAR pozwoli nam więc zidentyfikować kierunki przepływu wpływów oraz ocenić, jak duży jest ten wpływ.

```{r}
stopy <- stopy[,-1]
model <- VAR(stopy, p=1)
modelVAR <- summary(model)
modelVAR
```


# 4.Weryfikacja modelu

## 4.1 Łączna istotność - Test F

```{r}
results_Ftest <- as.data.frame(matrix(0, 1, 5))
names(results_Ftest) <- c("Wig", "Dax", "Aex", "CB","Yply")
modelVAR
results_Ftest[,1] <- pf(modelVAR$varresult$Wig$fstatistic[1], modelVAR$varresult$Wig$fstatistic[2], modelVAR$varresult$Wig$fstatistic[3], lower.tail = FALSE)
results_Ftest[,2] <- pf(modelVAR$varresult$Dax$fstatistic[1], modelVAR$varresult$Dax$fstatistic[2], modelVAR$varresult$Dax$fstatistic[3], lower.tail = FALSE)
results_Ftest[,3] <- pf(modelVAR$varresult$Aex$fstatistic[1], modelVAR$varresult$Aex$fstatistic[2], modelVAR$varresult$Aex$fstatistic[3], lower.tail = FALSE)
results_Ftest[,4] <- pf(modelVAR$varresult$CB$fstatistic[1], modelVAR$varresult$CB$fstatistic[2], modelVAR$varresult$CB$fstatistic[3], lower.tail = FALSE)
results_Ftest[,5] <- pf(modelVAR$varresult$Yply$fstatistic[1], modelVAR$varresult$Yply$fstatistic[2], modelVAR$varresult$Yply$fstatistic[3], lower.tail = FALSE)
results_Ftest
```



## 4.2 Autokorelacja składnika losowego 

Do oceny autokorelacji reszt można wykorzystać test autokorelacji Ljunga-Boxa, który jest dostępny w paczce tseries (funkcja - Box.test()). W tym teście dzięki opcji lag możemy badać stopień autokorelacji dowolnego rzędu.

H0 : brak autokorelacji składnika losowego 

H1 : istnieje przynajmniej jedna autokorelacja

```{r}
results_Boxtest <- as.data.frame(matrix(0, 1, 5))
names(results_Boxtest) <- c("Wig", "Dax", "Aex", "CB","Yply")

results_Boxtest[,1] <- Box.test(model$varresult$Wig$residuals, lag = 1, type = "Ljung-Box")$p.value
results_Boxtest[,2] <- Box.test(model$varresult$Dax$residuals, lag = 1, type = "Ljung-Box")$p.value
results_Boxtest[,3] <- Box.test(model$varresult$Aex$residuals, lag = 1, type = "Ljung-Box")$p.value
results_Boxtest[,4] <- Box.test(model$varresult$CB$residuals, lag = 1, type = "Ljung-Box")$p.value
results_Boxtest[,5] <- Box.test(model$varresult$Yply$residuals, lag = 1, type = "Ljung-Box")$p.value
results_Boxtest
```

Biorąc pod uwagę otrzymane wartości p-value, widzimy że dla każdej stopy zwrotu indeksu wartość ta jest większa od 5%, stąd nie ma podstaw do odrzucenia hipotezy H0. 
Autokorelacja reszt nie występuje. 

## 4.3 Normalność składnika losowego

Test normalności składnika losowego (residuals) jest ważnym krokiem w analizie statystycznej, aby sprawdzić, czy reszty (residuals) z modelu są zgodne z rozkładem normalnym. W tym celu wykorzystano test Shapiro-Wilka. W R służy do tego funkcji shapiro.test().

Hipoteza zerowa (H0): Próbka pochodzi z populacji o rozkładzie normalnym.

Hipoteza alternatywna (H1): Próbka nie pochodzi z populacji o rozkładzie normalnym.

```{r}
results_normTest <- as.data.frame(matrix(0, 1, 5))
names(results_normTest) <- c("Wig", "Dax", "Aex", "CB", "Yply")

results_normTest[,1] <- shapiro.test(model$varresult$Wig$residuals)$p.value
results_normTest[,2] <- shapiro.test(model$varresult$Dax$residuals)$p.value
results_normTest[,3] <- shapiro.test(model$varresult$Aex$residuals)$p.value
results_normTest[,4] <- shapiro.test(model$varresult$CB$residuals)$p.value
results_normTest[,5] <- shapiro.test(model$varresult$Yply$residuals)$p.value
results_normTest
```

Wartość p-value mniejsza niż 0,05 wskazuje, że istnieją statystycznie istotne dowody na odrzucenie hipotezy zerowej, co sugeruje, że dane nie pochodzą z rozkładu normalnego. Jednakże, zgodnie z Centralnym Twierdzeniem Granicznym (CTG), dla dużych próbek rozkład średnich z próby będzie zbliżony do rozkładu normalnego, niezależnie od kształtu rozkładu populacji. W związku z tym, nawet jeśli test Shapiro-Wilka wskazuje na brak normalności w małych próbkach, możemy przyjąć asymptotyczny rozkład normalny dla dużych próbek, co jest zgodne z CTG

## 4.4 Przyczynowość w sensie Grangera

Przyczynowość w sensie Grangera to definicja przyczynowości ukuta przez Clive'a Grangera, który uważał, że X powoduje Y wtedy i tylko wtedy, gdy włączenie do modelu przewidującego zmienną objaśnianą Y wartości zmiennej objaśniającej X zwiększa trafność predykcji.

Hipoteza zerowa (H0): Brak przyczynowosći w sensie Grangera.

Hipoteza alternatywna (H1): Przycznowość w sensie Grangera.

```{r}
results_grangTest <- as.data.frame(matrix(0, 1, 5))
names(results_grangTest) <- c("Wig", "Dax", "Aex", "CB", "Yply")
results_grangTest[,1] <- causality(model, cause = c("Wig"))$Granger$p.value
results_grangTest[,2] <- causality(model, cause = c("Dax"))$Granger$p.value
results_grangTest[,3] <- causality(model, cause = c("Aex"))$Granger$p.value
results_grangTest[,4] <- causality(model, cause = c("CB"))$Granger$p.value
results_grangTest[,5] <- causality(model, cause = c("Yply"))$Granger$p.value
results_grangTest
```

Wnioski:

Na podstawie otrzymanych wartości p-value dla przyczynowości w sensie Grangera widizmy,że dla każdej zmiennej p-value<5%, stąd mamy podstawy do odrzucenia H0. Dla  danych zachodzi przyczynowość. Stąd:

• Giełda niemiecka (DAX) ma istotny wpływ na przyszłe wartości pozostałych giełd  oraz ceny ropy, co zostało potwierdzone przez test przyczynowości w sensie Grangera. Przeszłe wartości DAX dostarczają istotnych informacji dla prognozowania tych zmiennych.

• Giełda holenderska (BEL) ma istotny wpływ na przyszłe wartości pozostałych giełd  oraz ceny ropy, co zostało potwierdzone przez test przyczynowości w sensie Grangera. Przeszłe wartości BEL dostarczają istotnych informacji dla prognozowania tych zmiennych.

• Giełda polska (WIG) ma istotny wpływ na przyszłe wartości pozostałych giełd  oraz ceny ropy, co zostało potwierdzone przez test przyczynowości w sensie Grangera. Przeszłe wartości WIG dostarczają istotnych informacji dla prognozowania tych zmiennych.

• Cena ropy ma istotny wpływ na przyszłe wartości pozostałych giełd, co zostało potwierdzone przez test przyczynowości w sensie Grangera. Przeszłe wartości cen ropy dostarczają istotnych informacji dla prognozowania tych zmiennych.
 
 
# 5. Reakcja na impuls

Analiza reakcji na impuls (Impulse Response Analysis) pozwala na ocenę, jak zakłócenie (impuls) w jednej zmiennej wpływa na inne zmienne w systemie dynamicznym. W kontekście ekonometrii i analizy szeregów czasowych jest to szczególnie przydatne w modelach wektorowej autoregresji (VAR).

```{r}

WIG <- irf(model, impulse = "Wig", response = c("Dax", "Aex", "CB"), n.ahead=5, ortho=TRUE, runs=100)
DAX <- irf(model, impulse = "Dax", response = c("Wig", "Aex", "CB"), n.ahead=5, ortho=TRUE, runs=100)
AEX <- irf(model, impulse = "Aex", response = c("Wig", "Dax", "CB"), n.ahead=5, ortho=TRUE, runs=100)
CB <- irf(model, impulse = "CB", response = c("Wig", "Dax", "Aex"), n.ahead=5, ortho=TRUE, runs=100)
```

# 5.1 Reakcja na impuls z WIG -Polska

```{r}
plot(WIG, main = "IRF dla WIG", col = "#00008B")
```

Na skutek impulsu pochodzącego z polskiej giełdy(WIG), pozostałe giełdy oraz cena ropy wykazują podobną reakcję: W pierwszym dniu odnotowujemy spadek, natomiast w drugim dniu sytuacja się stabilizuje.

# 5.2 Reakcja na impuls z DAX - Niemcy

```{r}
plot(DAX, main="IRF dla DAX",col = "#00008B")
```

Na skutek impulsu pochodzącego z niemieckiej giełdy, WIG  wykazują znikomą reakcję. Natomist cena ropy i AEX w pierwszym dniu odnotowują spadek,a w drugim dniu sytuacja się stabilizuje. Największą reakcje zauważono na giełdzie holenderskiej.

# 5.3 Rekcja na impuls z AEX - Holandia
```{r}
plot(AEX, main="IRF dla AEX", col = "#00008B")
```

Na skutek impulsu pochodzącego z holenderskiej giełdy, WIG i DAX  wykazują znikomą reakcję. Natomist cena ropy w pierwszym dniu odnotowują spadek,a w drugim dniu sytuacja się stabilizuje.

# 5.4  Reakcja na impuls CB - Ceny ropy
```{r}
plot(CB, main="IRF dla ceny ropy", col = "#00008B")
```


Na skutek impulsu pochodzącego z ceny ropy, DAX w pierwszym dniu odnotowują wzrost, natomiast w drugim dniu zauważono spadek, a w trzecim dniu sytuacja się stablizuję.
Na impuls cen ropy wszytskie WIG reaguję spadkiem dnia pierwszego, następnie wzrostem dnia drugiego, a dnia trzeciego się stablizuję.

## 8. Podsumowanie

Przeprowadzona analiza przyczynowości umożliwiła zidentyfikowanie wpływów oraz relacji przyczynowo-skutkowych pomiędzy stopami zwrotu indeksów giełdowych a cenami ropy na różnych giełdach europejskich.
W projekcie wykazano, że giełdy polska, niemiecka i holenderska oraz ceny ropy są przyczyną w sensie Grangera dla pozostałych giełd. 


