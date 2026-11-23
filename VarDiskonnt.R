# ############### Daten Bearbeitung #######################
# #### Einlesen der Daten #####
library ( readxl )
Dax <- read _ excel ("Hier könnten ihre Daten stehen")
VDax <- read _ excel ("Hier könnten ihre Daten stehen")
Zinsen <- read _ excel ("Hier könnten ihre Daten stehen")
# #### Prüfen der Daten #####
# Kurze Kontrolle
str ( Dax )
str ( VDax )
str ( Zinsen )
# Vergleichen des Datum Dax und VDax
for (i in 1:4099){
  if(Dax$ Datum [i]!= VDax $ Datum [i ]){ print (i)}
}
# Vergleich Datum Dax und Zinsen
for (i in 1:4099){
  if(Dax$ Datum [i]!= Zinsen $ Datum [i ]){ print (i)}
}
# Zum Überprüfen der Werte beim Datum
# Dax $ Datum []
# VDax $ Datum []
# Zinsen $ Datum []
# #### Optionspreis Black - Scholes #####
# Preis Call
Call <- function (S,X,rc ,rd =0,t, sigma ){
  C <- S* exp (-rd*t)* pnorm (d1(S,X,rc ,rd ,t,
                                 sigma ) ,0 ,1) -X* exp (-rc*(t))*
    pnorm (d2(S,X,rc ,rd ,t, sigma ) ,0 ,1)
  return (C)
}
# d1
d1 <- function (S,X,rc ,rd ,t, sigma ){
  d <- ( log (S/X)+(rc -rd)*t)/( sigma *t)+
    (1/2)* sigma * sqrt (t)
  return (d)
}
# d2
d2 <- function (S,X,rc ,rd ,t, sigma ){
  dneu <- d1(S,X,rc ,rd ,t, sigma )-
    sigma * sqrt (t)
  return ( dneu )
}
# #### Discont - Zertifikat #####
Discont <- function (S,X2 =0.01 ,
                     rc ,rd =0,t,sigma ,X){
  Call (S,X2 ,rc ,rd ,t, sigma )- Call (S,X,rc ,rd ,t, sigma )
}
# Diskont - Zertifikat berechenen
# Vektor erzeugen
DiskontZertifikat1 <- rep (NA ,4099)
DiskontZertifikat2 <- rep (NA ,4099)
DiskontZertifikat3 <- rep (NA ,4099)
# Funktion erstellen
berechnenDiskont <- function (S, XNeu =100 ,
                              rc , tNeu =252 , sigma ){
  
  # Vektor erzeugen
  DiskontZertifikat <- rep (NA ,4099)
  for (i in 1: tNeu ){
    DiskontZertifikat [i] <- Discont (S=S [4099 -i],
                                      X=S [4099] * XNeu /100 , sigma = sigma [4099 - i],
                                      rc=rc [4099 - i],t=( tNeu -i)/ 252)
  }
  # Lauf Variablen
  day <- 4100
  day2 <- 0
  # Aufteilen in Jahre
  while (504 < day ){
    # Lauf Variablen
    day <- day - 252
    day2 <- day2 + 252
    # Diskont - Zertifikat für ein Jahr berechnen
    DiskontZertifikatNeu <- Discont (S=S[ day ],
                                     X=S[day]* XNeu /100 , sigma = sigma [ day ],
                                     rc=rc[ day ],t=( tNeu )/ 252)
    # Diskont - Zertifikat pro Tag berechnen
    for (i in 1: tNeu ){
      DiskontZertifikat [i+ day2 ] <- Discont (
        S=S[day -i],X=S[day]* XNeu /100 , sigma =
          sigma [day -i],rc=rc[day -i],
        t=( tNeu -i)/ 252) *
        DiskontZertifikat [ day2 ]/
        DiskontZertifikatNeu
    }
  }
  # Kontroll print
  print ( day )
  # Lauf Variablen für letztes Jahr
  day <- day -252
  day2 <- day2 +252
  # Berchnung letztes Jahr
  DiskontZertifikatNeu <- Discont (S=S[ day ],
                                   X=S[day]* XNeu /100 , sigma = sigma [ day ],
                                   rc=rc[ day ],t=( tNeu )/ 252)
  
  for (i in 1:67){
    DiskontZertifikat [i+ day2 ] <- Discont (
      S=S[day -i],X=S[day]* XNeu /100 ,
      sigma = sigma [day -i],rc=rc[day -i],
      t=( tNeu -i)/ 252) * DiskontZertifikat [ day2 ]/
      DiskontZertifikatNeu
    # Kontroll print
    print ( day )
    print ( DiskontZertifikat [i+ day2 ])
  }
  # Rückgabe der Daten
  return ( DiskontZertifikat )
}
# Funktion aufrufen
DiskontZertifikat1 <- berechnenDiskont (
  as.numeric ( Dax $ Schluss ), XNeu =110 ,
  rc=as.numeric ( Zinsen $ Eröffnung )/100 ,
  sigma =as.numeric ( VDax $ Schluss )/ 100)
# Plot
plot (ts( DiskontZertifikat1 ),
      main =" Diskont - Zertifikate mit
verschiedenen Maximalwerten ",ylab =" Kurs ")
# Funktion aufrufen
DiskontZertifikat2 <- berechnenDiskont (
  as.numeric( Dax $ Schluss ),
  XNeu =100 , rc=as.numeric ( Zinsen $ Eröffnung )/100 ,
  sigma =as.numeric ( VDax $ Schluss )/ 100)
# Plot
lines (ts( DiskontZertifikat2 ), col ="red")
# Funktion aufrufen
DiskontZertifikat3 <- berechnenDiskont (
  as.numeric ( Dax $ Schluss ), XNeu =90 ,
  rc=as.numeric ( Zinsen $ Eröffnung )/100 ,
  sigma =as.numeric ( VDax $ Schluss )/ 100)
# Plot
lines (ts( DiskontZertifikat3 ), col =" blue ")
# Legende in Grafik einfügen
legend (" topleft ",lwd=c (2.5 ,2.5 ,2.5) ,
        col = c(" black ","red"," blue "),
        legend = c(" Maximalwert = 110% ",
                   " Maximalwert = 100% "," Maximalwert = 90%"))
# ################### Tests ######################
# #### Ausreiÿer bestimmen #####
aus <- function (Daten , VaR ){
  # Vektor erstellen
  ausreiÿer <- rep(NA ,3846)
  # Ausreiÿer zählen
  for (i in 1:3846){
    if(VaR[i] > Daten [i ]){
      ausreiÿer [i] <- 1
    } else { ausreiÿer [i] <- 0}
  }
  # Ausreiÿer zurückgeben
  return ( ausreiÿer )
}
# Aufrufen der Funktion
A <- aus( diff (log( DiskontZertifikat1 [253:4099])) , VaRG )
sum (A)
# #### Binomialtest #####
BTest <- function (A,t,p0 ){
  # Teststatistik
  Z= sqrt (t)*(A/t-p0)/ sqrt (p0*(1- p0 ))
  # Test Entscheid
  if( qnorm (1 -( p0/2) ,0 ,1) > abs (Z )){ return ( TRUE )}
  if( qnorm (1 -( p0/2) ,0 ,1) < abs (Z )){ return ( FALSE )}
}
# Aufrufen der Funktion
BTest ( sum (A [(252 * 15):(252 * 16)]) ,252 ,0.01)
# #### Kupiec Test #####
KTest <- function (t,A,p0 ){
  p=A/t
  # Teststatistik
  LR =-2* log ((1 - p0 )^(t-A)*p0^A )+2 * log ((1 -p )^(t-A)*p^A)
  # Test Entscheid
  if(is.na(LR )){ return ( FALSE )}
  if(LR <= pchisq (q=p0 , df =1)){ return ( TRUE )}
  if(LR > pchisq (q=p0 , df =1)){ return ( FALSE )}
}
# Aufrufen der Funktion
KTest (252 , sum(A [1:252]) ,0.99)
# #### Christoffersen #####
CMTest <- function ( AusreiÿerAufzeichnung ){
  # Variabeln erstellen
  n11 =0
  n10 =0
  n01 =0
  n00 =0
  # Zählen der Ausreiÿer
  for ( i in 2: length ( AusreiÿerAufzeichnung )){
    if( AusreiÿerAufzeichnung [i ]==1 &&
        AusreiÿerAufzeichnung [i -1]==1){ n11 = n11 +1}
    if( AusreiÿerAufzeichnung [i ]==1 &&
        AusreiÿerAufzeichnung [i -1]==0){ n10 = n10 +1}
    if( AusreiÿerAufzeichnung [i ]==0 &&
        AusreiÿerAufzeichnung [i -1]==1){ n01 = n01 +1}
    if( AusreiÿerAufzeichnung [i ]==0 &&
        AusreiÿerAufzeichnung [i -1]==0){ n00 = n00 +1}
  }
  # Vierfeldertafel erstellen
  A <- matrix (c(n11 ,n10 ,n01 ,n00), byrow =TRUE , ncol =2)
  # Werte berechnen
  p <- (A [1 ,2]+ A[2 ,2]) / length ( AusreiÿerAufzeichnung )
  p10 <- A[1 ,2]/(A[1 ,2]+ A [2 ,2])
  p11 <- A[1 ,1]/(A[1 ,1]+ A [2 ,1])
  # Teststatistik
  LR <- -2* log (1-p )^( A [2 ,2]+A [2 ,1]) *
    p^(A[1 ,2]+ A [1 ,1])+2 * log (1- p10^A [2 ,2])*
    p10 ^A[1 ,2]*(1- p11 )^A[2 ,1]* p11 ^A[1 ,1]
  # Test Entscheid
  if(LR <= qchisq (0.99 , df =1)){
    return ( TRUE )
  }
  if(LR > qchisq (0.99 , df =1)){
    return ( FALSE )
  }
}
# Funktionsaufruf
CMTest (A [252:504])
# ################ Modelle ##########################
# Historische Simulation
hS <- function ( DiskontZertifikat ){
  # Daten in log Renditen verwandeln
  DZ <- diff (log( DiskontZertifikat ))
  # Vector erstellen
  # länge 3846 , da für die ersten
  # 252 Datenpunkte kein VaR erstellt wird
  VaR <- rep(NA ,3846)
  # VaR Berechnen
  for (i in 1:3846){
    print (i)
    # die vorgegebene
    # Ausreiÿerwahrscheinlichkeit 99%
    # entspricht 252 Datenpunkten
    # dem Datenpunkt 250
    VaR [i] <- sort (DZ[i:(i +252)] ,
                     decreasing = TRUE )[250]
  }
  # Rückgabe vom VaR
  return (VaR)
}
# Funktion aufrufen
hS( DiskontZertifikat1 )
# Kerndicheschätzung
# Vector erstellen
# länge 3846 , da für die ersten
# 252 Datenpunkte kein VaR erstellt wird
VaR2 <- rep (NA ,3846)
# Schleife zum Durchlauf der berechneten Tage
for (i in 1:3846){
  # Kontrolle
  print (i)
  # Daten in log Renditen verwandeln
  Diskont <- diff ( log ( DiskontZertifikat1 [i:(i +252)]))
  # VaR historische Simulation als Startwert
  VaR <- VaR1 [i]
  # Vergleichswert für while
  VaRk <- 0
  # Bandbreite berechnen
  h <- ((4 / 3)^(1 /5))*(sd( Diskont )/ 4099^((1 / 5)))
  # While mit Abbruchbedinung
  while ( abs (VaR - VaRk ) > (0.00001)){
    # Abbruchbedinung aktualisieren
    VaRk <- VaR
    # VaR berechnen
    VaR = VaR + ( sum ( pnorm ((- Diskont - VaR )/h),
                        0 ,1)/ 252 -0.01) /( sum ( dnorm ((- Diskont - VaR )/
                                                            h) ,0 ,1)/(h* 252))
  }
  # VaR Speichern
  VaR2 [i] <- VaR *( -1)
}
# ARCH
# GARCH
# Library aufrufen
library ( rugarch )
# Phrameter der Modelle
q <- 2 # GARCH
p <- 1 # ARCH
# Speichvector anlegen
# länge 3846 , da für die ersten
# 252 Datenpunkte kein VaR erstellt wird
VaRG <- rep (NA ,3846)
VaRA <- rep (NA ,3846)
# Variable zum Speichern vom AIC
aic =0
# Model spezifizieren
model <- ugarchspec ( variance.model =
                      list ( model = " sGARCH ", garchOrder = c(p,q)),
                      distribution.model = " norm ")
# Durchlauf der Windows
for (i in 1:3846){
  # Log Renditen erzeugen
  Diskont <- diff ( log ( DiskontZertifikat1 [i:(i +252)]))
  # modellfit
  modelfit <- ugarchfit ( spec =model , data = Diskont ,
                          solver =" hybrid ")
  # AIC 's aufsummieren
  aic <- infocriteria ( modelfit )[1]+ aic
  # Volatilität prognostizieren
  prognose <- ugarchforecast (
    modelfit ,n.ahead =1, data = Diskont ) @forecast $ sigmaFor
  # VaR speichern
  VaRG [i] <- qnorm (0.01 ,0 , as.numeric ( prognose ))
  # VaRA [i] <- qnorm (0.01 ,0 , as. numeric ( prognose ))
  # Kontrolle ob Programm läuft
  print (i)
}