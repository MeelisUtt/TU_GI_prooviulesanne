###########
#15.05.19
#Meelis Utt
###########

########
#Paketid
########

library(readxl)
library(dplyr)
library(rvest)
library(stringr)
library(readr)
library(writexl)

#######################
#Andmete sisse lugemine
#######################
#Eeldame, et andmestik on scriptiga samas kaustas.
#Seame töökaustaks scripti kausta.
#Loeme andmestiku R-i.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
andmestik <- read_excel(paste(getwd(),"AA_naidisandmed.xlsx",sep="/"))

#######################
#Andmestiku puhastamine
#######################

andmestik$`TreatmentService treatmentServiceOrig` <- str_replace_all(andmestik$`TreatmentService treatmentServiceOrig`,"000000000000","")

###########################
#Veebist andmete kraapimine
###########################

url <- "https://www.riigiteataja.ee/akt/126032019021"
riigiteataja_tabelid <- read_html(url) %>% html_table()

#Leiame milliseid ravisid peame netist saadud andmetest otsima 
ravimite_koodid <- andmestik$`TreatmentService treatmentServiceOrig`

#Leiame otsitavate ravimite koodidele vastavad nimetused ja hinnad (see on abitabel hilisemaks).
nimetuse_koodi_hinna_tabel <- data.frame(Nimetus=c(),Kood = c(), Ravi_maksumus_eurodes = c())

for(i in 1:length(riigiteataja_tabelid)){
  #Tabeli abimuutuja.
  temp_tabel <- riigiteataja_tabelid[[i]]
  
  #Abimuutujad, et teaksime millises veerus on nimetused, koodid ja hinnad.
  koodi_veerg <- which(temp_tabel[1,]=="Kood")
  nimetuse_veerg <- ifelse(koodi_veerg!=2,koodi_veerg-1,1)
  hinna_veerg <- which(temp_tabel[1,] %in% c("Piirhindeurodes","Ühe haigevoodipäevapiirhindeurodes"))
  
  #Otsime igast alamtabelist huvipakkuvat koodi.
  #Kõigi sobilike koodide korral lisame vastavad read nimetuse-koodi-hinna tabelisse.
  temp_tabel <- temp_tabel[temp_tabel[,koodi_veerg] %in% ravimite_koodid,c(nimetuse_veerg,koodi_veerg,hinna_veerg)]
  if(nrow(temp_tabel) >0){
    colnames(temp_tabel) <- c("Nimetus","Kood","Ravi_maksumus_eurodes")
    nimetuse_koodi_hinna_tabel <- rbind(nimetuse_koodi_hinna_tabel,temp_tabel)
  }
    
}

####################################################
#Lisame andmestikku nimetused ja raviteenuste hinnad
####################################################

#Osasid ravikoode ei leidunud riigiteatajas. Jätame alles ainult need read, millel olev kood leidus riigiteatajas.
andmestik <- andmestik[which(andmestik$`TreatmentService treatmentServiceOrig` %in% nimetuse_koodi_hinna_tabel$Kood),]

#Lisan andmestikule koodide nimed ja hinnad vastavalt ravi saamiste arvule.
algne_veergude_arv <- ncol(andmestik)
for(i in 1:nrow(nimetuse_koodi_hinna_tabel)){
  mis_ridades_mingi_ravi <- which(andmestik$`TreatmentService treatmentServiceOrig`%in% nimetuse_koodi_hinna_tabel$Kood [i])
  andmestik[mis_ridades_mingi_ravi,algne_veergude_arv+1] <- nimetuse_koodi_hinna_tabel$Nimetus[i]#Lisan nimetuse
  #andmestik[mis_ridades_mingi_ravi,algne_veergude_arv+4] <- nimetuse_koodi_hinna_tabel$Kood[i]#Koodi kontroll
  
  hinna_kuju_teisendus <- str_replace_all(nimetuse_koodi_hinna_tabel$Ravi_maksumus_eurodes[i],"[,]",".") %>%
    parse_double()
  andmestik[mis_ridades_mingi_ravi,algne_veergude_arv+2] <-  hinna_kuju_teisendus*andmestik$`TreatmentService treatmentTimesOrig`[mis_ridades_mingi_ravi]
  andmestik[mis_ridades_mingi_ravi,algne_veergude_arv+3] <-  hinna_kuju_teisendus #Hinna kontroll
}

#Panen ilusad veerunimed.
colnames(andmestik) <- c(names(andmestik)[1:algne_veergude_arv],"Nimetus","Ravi_maksumus_eurodes","Teenuse_hind")

#########################################################
#Prooviülesandes püstitatud küsimustele vastuste otsimine
#########################################################

#1) Iga andmefailis toodud inimese kohta tema raviteenustele kulunud summat
#Leian erinevad inimesed andmefailist
yl_vastused <- andmestik %>% select(Person) %>% unique()

inimesed_andmestikus <-  andmestik %>% select(Person) %>% pull() %>% unique()
#1)
#Lisan iga inimese kohta kulunud raviteenuste summa.
yl_vastused[,2] <- andmestik %>%
  filter(Person %in% inimesed_andmestikus) %>% group_by(Person)%>%
  select(Person,Ravi_maksumus_eurodes) %>%
  summarise(arve_summa = sum(Ravi_maksumus_eurodes)) %>% select(arve_summa)%>% ungroup() #Leiame raviteenustele kulunud summa igal inimesel


#2)Iga andmefailis toodud inimese kohta keskmist raviarve kestvust.
#3) Iga andmefailis toodud inimese kohta kolme kulukaimat raviarvet ning nendes sisalduvate raviteenuste nimesid.

#Millistes ridades on vaadeldav inimene
millised_selle_inimese_arved <- andmestik %>% filter(Person %in% inimesed_andmestikus) %>%
  select(BillNr) %>% unique() %>% pull()

######
#2) Eeldan, et igal arvel on sama lõppaeg.
ajavahed <- andmestik %>% filter(Person %in% inimesed_andmestikus & BillNr %in% millised_selle_inimese_arved) %>% 
  select(Person,BillNr,`TreatmentBill billEndDateOrig`,`TreatmentBill billStartDateOrig`) %>%
  group_by(Person) %>%
  mutate(Ajavahe = difftime(`TreatmentBill billEndDateOrig`,`TreatmentBill billStartDateOrig`,units="hours")) %>%
  unique() %>% select(Person,Ajavahe) %>% summarise(Keskmine_arve_aeg = mean(Ajavahe)) %>%
  select(Keskmine_arve_aeg)%>% ungroup() %>% pull() 

yl_vastused$Keskmine_raviarve_kestus_tundides <- ajavahed
######
#3)
arve_summa <- andmestik %>%
  group_by(Person,BillNr) %>% 
  select(Person,BillNr,Ravi_maksumus_eurodes) %>%
  summarise(Arve_summa_kokku = sum(Ravi_maksumus_eurodes)) %>%
  top_n(3,wt=Arve_summa_kokku)%>% ungroup() 

#Nendel, kellel on ainult 1 või 2 taset, siis lisan vastavalt 2 või 1 tühja rida sellele inimesele juurde.
kellel_2_arvet <- arve_summa %>% select(Person) %>% table() %>% melt(varnames = "Person") %>%
  filter(value==2) %>% mutate(BillNr = NA, Arve_summa_kokku = NA) %>% select(1,3,4)
kellel_1_arvet <- arve_summa %>% select(Person) %>% table() %>% melt(varnames = "Person") %>%
  filter(value==1) %>% mutate(BillNr = NA, Arve_summa_kokku = NA) %>% select(1,3,4)


#Nendel, kellel on 4 vaatlust (mingi tase on topelt), siis määran indikaatori ja lisan ainult 3 tagasi
kellel_4_arvet <- arve_summa %>% select(Person) %>% table() %>% melt(varnames = "Person") %>%
  filter(value==4)

nendel_kellel_enne_oli_4 <- arve_summa %>% filter(Person %in% kellel_4_arvet$Person) %>% group_by(Person) %>%
  mutate(Sisse_jatta = rep(c("jah","ei"),times=c(3,1))) %>% filter(Sisse_jatta=="jah") %>% select(-4) %>% ungroup()

#Eemaldan need vaatlused, millel on 4 top_n-i.
arve_summa <- arve_summa %>% filter(!(Person %in% kellel_4_arvet$Person))

#Lisan tagasi ilma 4-nda vaatluseta. Lisaks lisan juurde lisa read 2 ja 1 top_n Personile
arve_summa <- rbind(arve_summa,nendel_kellel_enne_oli_4,kellel_2_arvet,kellel_1_arvet,kellel_1_arvet)

veergude_arv <- ncol(yl_vastused)
abi_indeks <- 0
for(i in seq(1,6,2)){
  yl_vastused[,veergude_arv+i] <- arve_summa %>% group_by(Person)  %>%
    top_n(1,wt=Arve_summa_kokku)  %>% distinct(Person,.keep_all = T)  %>% ungroup() %>%
    select(Arve_summa_kokku) %>% pull()
  
    arve_nr <- arve_summa %>% group_by(Person) %>%
      top_n(1,wt=Arve_summa_kokku) %>% distinct(Person,.keep_all = T) %>% ungroup()%>%
      select(BillNr) %>% pull()
    
    inimese_id <- arve_summa %>% group_by(Person) %>%
      top_n(1,wt=Arve_summa_kokku) %>% unique() %>% ungroup()%>%
      select(Person) %>% pull()
  
  yl_vastused[,veergude_arv+i+1] <- andmestik %>% filter(BillNr %in% arve_nr & Person %in% inimese_id) %>%
    select(Person,BillNr,Nimetus) %>% group_by(Person,BillNr) %>% 
    summarise(Nimetus = paste(Nimetus,collapse= "; ")) %>% ungroup() %>% distinct(Person,.keep_all = T) %>%
    select(Nimetus)
    
  
  arve_summa <- arve_summa %>% group_by(Person)  %>%
    top_n(-3+i-abi_indeks,wt=Arve_summa_kokku) %>% ungroup()
  abi_indeks <- abi_indeks+1
}

colnames(yl_vastused) <- c("Inimene","Raviteenustele kulunud summa eurodes",
                                           "Raviarve keskmine kestvus tundides",
                                           "Kulukaim raviarve 1 eurodes","Kulukaima raviarve 1 sisalduvate raviteenuste nimed",
                                           "Kulukaim raviarve 2 eurodes","Kulukaima raviarve 2 sisalduvate raviteenuste nimed",
                                           "Kulukaim raviarve 3 eurodes","Kulukaima raviarve 3 sisalduvate raviteenuste nimed")

#################################################################
#Kirjutame tulemused ja korrastatud andmestikud exceli failidesse
#################################################################

write_xlsx(x=yl_vastused,path=paste(getwd(),"Vastused.xlsx",sep="/"))
write_xlsx(x=andmestik,path=paste(getwd(),"Korrastatud_andmestik.xlsx",sep="/"))

