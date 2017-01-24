#'
#'
#' Skrypt na STWUR
#' 
#' Wprowadzanie to tidyverse
#' 
#' pakiety: dplyr, haven, ggplot2
#' 
library(dplyr)

setwd("~/szychta_w_danych/stwur_17_01/")

download.file(url = "https://github.com/michbur/Diagnoza_dane/archive/master.zip", 
							destfile = "diagnoza.zip")
unzip("diagnoza.zip", exdir = getwd())
load("./Diagnoza_dane-master/osoby.RData")
load("./Diagnoza_dane-master/osobyDict.RData")


#' Najpierw sprawdźmy co w ogóle jest w tych danych
dim(osoby)
head(osobyDict)
osoby[1:6, 1:6]

#' zmienne są zakodowane, może znajdziemy coś ciekawego
View(osobyDict)

# Załóżmy, że chcemy wybrać wojewodztwo, status społeczny status9_2015 i znajomość angielskiego fc23

#' składnia pakietu dplyr jest bardzo podobna do SQL
osoby %>% 
	select(wojewodztwo, status9_2015, fc23 ) %>% #wybieramy zmienne
	head

#' wszystkie osoby, które były ankietowane przez 2015 mają brakujące wartości
#' NA - Not Available

#' chcemy wybrać tylko te obserwacje, które mają wartości w tych kolumnach
#' Potrzebujemy czegoś podobnego do selecta, tylko na wierszach
osoby %>% 
	select(wojewodztwo, status9_2015, fc23 ) %>% #wybieramy zmienne
	filter(!is.na(fc23)) %>%
	head

# No dobrze, ale co oznaczają w ogóle te cyferki? 
class(osoby$fc23)
# czyli każda liczba odnosi się do pewnej etykietki
attr(osoby$fc23, "labels")
attr(osoby$status9_2015, "labels")
attr(osoby$wojewodztwo, "labels") #notabene, zgodnie z kodem TERYT

# żeby przetwarzać zmienne typu labelled
library(haven)
is.labelled(osoby$fc23)
summary(as_factor(osoby$fc23)) #uwaga, as_factor, nie as.factor!!!

# Jak przetwarzać zmienne w ramce danych? Służy do tego funkcja mutate
osoby %>% 
	select(wojewodztwo, status9_2015, fc23 ) %>% #wybieramy zmienne
	filter(!is.na(fc23)) %>%
	mutate(fc23=as_factor(fc23),
				 status9_2015=as_factor(status9_2015),
				 wojewodztwo=as_factor(wojewodztwo)) %>% 
	head

# lub nieco szybciej
osoby %>% 
	select(wojewodztwo, status9_2015, fc23 ) %>% #wybieramy zmienne
	filter(!is.na(fc23)) %>%
	mutate_each(funs(as_factor)) %>% 
	head

# wygląda już znacznie lepiej! Spróbujmy się przyjrzeć bliżej tym danym
#' Załóżmy, że interesuje nas, jaka jest znajomość angielskiego w zależności od województwa.
#' Chcemy zatem pogrupować wszystkie dane, tak aby obserwacje związane z każdym
#' województwem były w osobnej grupie. Następnie chcemy podsumować każdą grupę osobno
osoby %>% 
	select(wojewodztwo, status9_2015, fc23 ) %>% #wybieramy zmienne
	filter(!is.na(fc23), !is.na(wojewodztwo)) %>%
	mutate_each(funs(as_factor)) %>%
	group_by(wojewodztwo) %>%
	summarise(czynnie=mean(fc23=="czynnie")) #jaki procent obserwacji ma etykietę ,,czynnie"

osoby %>% 
	select(wojewodztwo, status9_2015, fc23 ) %>% #wybieramy zmienne
	filter(!is.na(fc23), !is.na(wojewodztwo)) %>%
	mutate_each(funs(as_factor)) %>%
	group_by(wojewodztwo) %>%
	summarise(czynnie=mean(fc23=="czynnie"),
						biernie=mean(fc23=="biernie"))

# a może by je uszeregować od największej do najmniejszej?
osoby %>% 
	select(wojewodztwo, status9_2015, fc23 ) %>% #wybieramy zmienne
	filter(!is.na(fc23), !is.na(wojewodztwo)) %>%
	mutate_each(funs(as_factor)) %>%
	group_by(wojewodztwo) %>%
	summarise(czynnie=mean(fc23=="czynnie"),
						biernie=mean(fc23=="biernie")) %>%
	arrange(desc(czynnie))

# 5 z największym procentem
osoby %>% 
	select(wojewodztwo, status9_2015, fc23 ) %>% #wybieramy zmienne
	filter(!is.na(fc23), !is.na(wojewodztwo)) %>%
	mutate_each(funs(as_factor)) %>%
	group_by(wojewodztwo) %>%
	summarise(czynnie=mean(fc23=="czynnie"),
						biernie=mean(fc23=="biernie")) %>%
	top_n(n = 5, wt = czynnie)

# wyszło nam coś sensownego, spróbujmy to namalować!
library(ggplot2)
osoby %>% 
	select(wojewodztwo, status9_2015, fc23 ) %>% #wybieramy zmienne
	filter(!is.na(fc23), !is.na(wojewodztwo)) %>%
	mutate_each(funs(as_factor)) %>%
	group_by(wojewodztwo) %>%
	summarise(czynnie=mean(fc23=="czynnie"),
						biernie=mean(fc23=="biernie")) -> angielski_wojewodztwa

ggplot(angielski_wojewodztwa) +
	geom_bar(aes(x=wojewodztwo, y=czynnie), stat = "identity")

ggplot(angielski_wojewodztwa) +
	geom_bar(aes(x=wojewodztwo, y=biernie), stat = "identity")

# moze sprawdzmy znajomosc jakakolwiek
angielski_wojewodztwa %>%
	mutate(ogolem=czynnie+biernie) -> angielski_wojewodztwa

ggplot(angielski_wojewodztwa) +
	geom_bar(aes(x=wojewodztwo, y=ogolem), stat = "identity")

ggplot(angielski_wojewodztwa) +
	geom_bar(aes(x=wojewodztwo, y=ogolem, fill=wojewodztwo), stat = "identity")

ggplot(angielski_wojewodztwa) +
	geom_bar(aes(x=reorder(wojewodztwo, -ogolem), y=ogolem, fill=wojewodztwo), stat = "identity")

