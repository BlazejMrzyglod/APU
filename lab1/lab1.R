a = 2*log(5)
b = a*3
min(a,b)
help(median)
a = c(25:45)
median(a)
apropos("median",mode="function")
setwd("C:\\Users\\blaze\\Pulpit\\Studia\\APU\\APU\\lab1")
a = "macbooki Apple"
save(a,file="workspace.RData")
remove(a)
a
load("workspace.RData")
a
install.packages("gridExtra")
help(package="gridExtra")
library(gridExtra)
top10 = head(Orange, 10)
grid.table(top10)
wektor = seq(200,130,-5)
a = c(19:5)
b = c(11:23)
d = c(b,a)
d
nazwy = c("Apple MacBook Air", "Apple MacBook Pro 2023", "Apple MacBook Air 2024", "Apple MacBook Air M1", "Apple MacBook Air M1", "Apple MacBook Air 2024", "Apple MacBook Air M1", "Apple MacBook Air 2024", "Apple MacBook Air", "Apple MacBook Air 2024")
ekran = c('13,6"','14,2"','15,3"','13,3"','13,3"','13,6"','13,3"','13,6"','13,6"','13,6"')
pamiec_RAM = c(8,18,8,8,16,8,8,8,16,8)
dysk=c(256,512,512,256,256,256,256,256,256,256)
cena=c(4999,10199,8199,4579,5699,5999,4999,5999,6349,5999)
liczba_opinii=c(8,5,0,78,114,0,72,0,19,0)
Macbooki = data.frame(nazwa = nazwy, ekran, pamiec_RAM,dysk,cena, liczba_opinii)
mean(Macbooki$cena)
nowy_Macbook = data.frame(nazwa="MacBook Air M1", ekran='13,3"', pamiec_RAM=8,dysk=256,cena=4579, liczba_opinii=113)
Macbooki <- rbind(Macbooki, nowy_Macbook)
mean(Macbooki$cena)
ocena = c(5, 4, "brak", 5, 5, "brak", 5, "brak", 4, "brak", 4.5)
Macbooki <- cbind(Macbooki, ocena)
ceny_ocen <- by(Macbooki$cena, Macbooki$ocena, mean)
ceny_ocen
Macbooki <- rbind(Macbooki, data.frame(nazwa="MacBook Air 2024", ekran='13,6"', pamiec_RAM=8,dysk=512,cena=7149, liczba_opinii=0, ocena="brak"),data.frame(nazwa="MacBook Air M1", ekran='13,3"', pamiec_RAM=8,dysk=256,cena=5049, liczba_opinii=409, ocena=5),data.frame(nazwa="MacBook Air 13,3", ekran='13,3"', pamiec_RAM=8,dysk=256,cena=5399, liczba_opinii=262, ocena=5),data.frame(nazwa="MacBook Pro 2021", ekran='14,2"', pamiec_RAM=16,dysk=1024,cena=9999, liczba_opinii=10, ocena=5))
plot(Macbooki$ocena)
pie(table(Macbooki$ocena))
status_opinii = c("mniej niz 50 opinii", "mniej niz 50 opinii", "nie ma", "50-100 opinii", "wiecej niz 100 opinii", "nie ma", "50-100 opinii", "nie ma", "mniej niz 50 opinii", "nie ma", "wiecej niz 100 opinii", "nie ma", "wiecej niz 100 opinii", "wiecej niz 100 opinii","mniej niz 50 opinii")
factor(status_opinii)
Macbooki <- cbind(Macbooki, status_opinii)
pie(table(Macbooki$status_opinii))
for (i in 1:nrow(Macbooki)) {
  print(paste(Macbooki$nazwa[i], "ma ocenę klientów", Macbooki$ocena[i], "bo ma liczbę opinii", Macbooki$liczba_opinii[i]))
}
save(Macbooki,file="Macbooki.csv")
remove(Macbooki)
Macbooki
load("Macbooki.csv")
Macbooki
