# Run from the repository root: Rscript code/analysis.R  (data in data/, tikz output to report/)
# The hard-coded Gemeinde_id values below come from as.numeric(factor(Gemeinde)), so they depend on
# the alphabetical sort order of the municipality names. Pin it so the IDs are the same everywhere.
if (Sys.setlocale("LC_COLLATE", "en_US.UTF-8") == "") stop("Locale en_US.UTF-8 is required (see README)")
library(readr)
library(readxl)
library(foreign)
abholzungen <- read.csv(file="data/terrabrasilis_legal_amazon_area_states_municipalities.csv", skip = 1, head=FALSE, sep=";")
Wald <- read.csv(file = "data/terrabrasilis_legal_amazon_area_country.csv", skip = 1, head = FALSE, sep = ";")
projekte <- read_excel("data/fundo_amazônia_projekte_cashflows.xlsx")
library(tidyverse)
library(tidyr)
library(ggplot2)
library(sandwich)
library(dplyr)
library(plm)
library(fastDummies)
library(writexl)
library(geobr)
library(sf)
library(rio)
library(crul)
library(tikzDevice)
library(lmtest)
library(microbenchmark)
library(texreg)
library(xtable)






## Some aggregate deforestation values
Wald$V3 <- NULL
colnames(Wald)
names(Wald)[1] <- "Jahr"
names(Wald)[2] <- "Abholzungen"
Wald$Fund <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4971305, 43425436,
               22868042, 107782585, 81696238, 53650472, 37026632)
typeof(Wald$Abholzungen)
ggplot(data = Wald, mapping = aes(x = Jahr, y = Abholzungen)) + geom_point() + geom_line() 
lm(Abholzungen ~ Jahr + Fund, Wald)


tikz('report/simpleEx.tex',width=3.5,height=3.5)
ggplot(data = Wald, mapping = aes(x = Jahr, y = Abholzungen)) + geom_point() + geom_line() 
dev.off()




## Start looking at municipalities  
colnames(abholzungen)
names(abholzungen)[1] <- "Jahr"
names(abholzungen)[2] <- "Fläche"
names(abholzungen)[3] <- "Gemeinde"
names(abholzungen)[4] <- "Code_Gemeinde"
names(abholzungen)[5] <- "Bundesstaat"


### 3 Gemeinden have the same name but in different states 
abholzungen$Gemeinde[abholzungen$Code_Gemeinde == 2100873] = "ARAGUANÃ (MA)"
abholzungen$Gemeinde[abholzungen$Code_Gemeinde == 1703305] = "BOM JESUS DO TOCANTINS (TO)"
abholzungen$Gemeinde[abholzungen$Code_Gemeinde == 1505551] = "PAU D'ARCO (PA)"
abholzungen$Gemeinde[abholzungen$Code_Gemeinde == 2109239] = "PRESIDENTE MÉDICI (MA)"
abholzungen$Gemeinde[abholzungen$Code_Gemeinde == 5107206] = "RIO BRANCO (MT)"



# Have to change the format of "Fläche" (commas are a problem)
abholzungen$Fläche = gsub(",", ".", abholzungen$Fläche)
abholzungen$Fläche <- as.numeric(as.character(abholzungen$Fläche))
typeof(abholzungen$Fläche)
class(abholzungen$Fläche)
mode(abholzungen$Fläche)
table(index(abholzungen), useNA = "ifany")
abholzungen[,4] <- as.numeric(as.character(abholzungen[,4]))
sapply(abholzungen, class)
## Also need to change the format of "Bundesstaat"
abholzungen$Bundesstaat <- as.factor(as.character(abholzungen$Bundesstaat))
typeof(abholzungen$Bundesstaat)
class(abholzungen$Bundesstaat)

### Finding unbalances in the panel 
is.pbalanced(abholzungen)
abholzungen$unique_id <- paste(abholzungen$Gemeinde,abholzungen$Jahr)
abholzungen$duplicate = duplicated(abholzungen$unique_id)
subset(abholzungen, duplicate=="TRUE")
suppressMessages(library(tidyverse))
abholzungen <- 
  abholzungen %>% 
  group_by(Gemeinde, Jahr, Bundesstaat, Code_Gemeinde) %>% 
  select(-c(duplicate)) %>% 
  summarize(Fläche  = sum(Fläche, na.rm = TRUE))

is.pbalanced(abholzungen)
 



## Numeric municipality id for ID-time index
abholzungen$Gemeinde_id <- as.numeric(factor(abholzungen$Gemeinde)) 

## Balance the panel
abholzungen_balanced <- make.pbalanced(abholzungen, balance.type = c("fill"), index = c("Gemeinde_id", "Jahr"))
  
is.pbalanced(abholzungen_balanced)


## Missing municiplaity names 
subset(abholzungen_balanced, is.na(Gemeinde))
subset(abholzungen_balanced, Gemeinde_id == 2|Gemeinde_id==3|Gemeinde_id==7
       |Gemeinde_id == 9|Gemeinde_id == 10| Gemeinde_id == 11| Gemeinde_id == 18|Gemeinde_id==19
       | Gemeinde_id == 22| Gemeinde_id == 27| Gemeinde_id == 30|Gemeinde_id==31|Gemeinde_id == 32|
         Gemeinde_id == 33|Gemeinde_id==34|Gemeinde_id==35|Gemeinde_id==37|
         Gemeinde_id == 38|Gemeinde_id == 40|Gemeinde_id==44|Gemeinde_id == 46|Gemeinde_id == 47
       |Gemeinde_id == 49| Gemeinde_id == 50|Gemeinde_id==51|Gemeinde_id==53|Gemeinde_id == 58|Gemeinde_id == 59
       | Gemeinde_id == 63| Gemeinde_id == 64|Gemeinde_id == 65| Gemeinde_id == 66| Gemeinde_id == 67
       | Gemeinde_id == 70| Gemeinde_id == 77| Gemeinde_id == 79 | Gemeinde_id == 81| Gemeinde_id == 83
       | Gemeinde_id == 84 | Gemeinde_id == 85 | Gemeinde_id == 87 | Gemeinde_id == 88 | Gemeinde_id == 95
       | Gemeinde_id == 97 | Gemeinde_id == 99 | Gemeinde_id == 101 | Gemeinde_id == 104 | Gemeinde_id == 111
       | Gemeinde_id == 119 | Gemeinde_id == 121 | Gemeinde_id == 122 | Gemeinde_id == 129 | Gemeinde_id == 130
       | Gemeinde_id == 131 | Gemeinde_id == 134 | Gemeinde_id == 140 | Gemeinde_id == 141 | Gemeinde_id == 149 
       | Gemeinde_id == 151 | Gemeinde_id == 152 | Gemeinde_id == 156 | Gemeinde_id == 157 | Gemeinde_id == 161
       | Gemeinde_id == 162 | Gemeinde_id == 163 | Gemeinde_id == 170 | Gemeinde_id == 171 | Gemeinde_id == 174
       | Gemeinde_id == 175 | Gemeinde_id == 182 | Gemeinde_id == 183 | Gemeinde_id == 187 | Gemeinde_id == 189| Gemeinde_id == 192
       | Gemeinde_id == 192 | Gemeinde_id == 193 | Gemeinde_id == 196 
       | Gemeinde_id == 197 | Gemeinde_id == 198 | Gemeinde_id == 199 | Gemeinde_id == 200 | Gemeinde_id == 202 | Gemeinde_id == 203 | Gemeinde_id == 204
       | Gemeinde_id == 207 | Gemeinde_id == 212 | Gemeinde_id == 213 | Gemeinde_id == 215 | Gemeinde_id == 220
       | Gemeinde_id == 221 | Gemeinde_id == 223 | Gemeinde_id == 224 | Gemeinde_id == 229 | Gemeinde_id == 230
       | Gemeinde_id == 232 | Gemeinde_id == 235 | Gemeinde_id == 239 | Gemeinde_id == 240 | Gemeinde_id == 245
       | Gemeinde_id == 247 | Gemeinde_id == 248 | Gemeinde_id == 251 | Gemeinde_id == 252 | Gemeinde_id == 253 
       | Gemeinde_id == 255 | Gemeinde_id == 256 | Gemeinde_id == 257 | Gemeinde_id == 263 | Gemeinde_id == 271
       | Gemeinde_id == 272 | Gemeinde_id == 275 | Gemeinde_id == 277 | Gemeinde_id == 280
       | Gemeinde_id == 282 | Gemeinde_id == 288 | Gemeinde_id == 292 | Gemeinde_id == 294 | Gemeinde_id == 298
       | Gemeinde_id == 301 | Gemeinde_id == 305 | Gemeinde_id == 307 | Gemeinde_id == 308 | Gemeinde_id == 310
       | Gemeinde_id == 311 | Gemeinde_id == 312 | Gemeinde_id == 315 | Gemeinde_id == 316 | Gemeinde_id == 325
       | Gemeinde_id == 328 | Gemeinde_id == 329 | Gemeinde_id == 332 | Gemeinde_id == 333 
       | Gemeinde_id == 334 | Gemeinde_id == 338 | Gemeinde_id == 344 | Gemeinde_id == 347 | Gemeinde_id == 348
       | Gemeinde_id == 354 | Gemeinde_id == 357 | Gemeinde_id == 359
       | Gemeinde_id == 360 | Gemeinde_id == 362 | Gemeinde_id == 363 | Gemeinde_id == 365 | Gemeinde_id == 367
       | Gemeinde_id == 379 | Gemeinde_id == 380 | Gemeinde_id == 381 | Gemeinde_id == 385 | Gemeinde_id == 388
       | Gemeinde_id == 391 | Gemeinde_id == 401 | Gemeinde_id == 403 | Gemeinde_id == 405 | Gemeinde_id == 408
       | Gemeinde_id == 409 | Gemeinde_id == 410 | Gemeinde_id == 420 | Gemeinde_id == 422 | Gemeinde_id == 423| Gemeinde_id == 424
       | Gemeinde_id == 425 | Gemeinde_id == 426 | Gemeinde_id == 429 | Gemeinde_id == 430 | Gemeinde_id == 431
       | Gemeinde_id == 435 | Gemeinde_id == 436 | Gemeinde_id == 437 | Gemeinde_id == 438
       | Gemeinde_id == 441 | Gemeinde_id == 442 | Gemeinde_id == 443 | Gemeinde_id == 444 | Gemeinde_id == 451
       | Gemeinde_id == 452 | Gemeinde_id == 455 | Gemeinde_id == 458 | Gemeinde_id == 459 | Gemeinde_id == 464
       | Gemeinde_id == 465 | Gemeinde_id == 467 | Gemeinde_id == 468 | Gemeinde_id == 470 
       | Gemeinde_id == 471 | Gemeinde_id == 472 | Gemeinde_id == 474 | Gemeinde_id == 475 | Gemeinde_id == 478
       | Gemeinde_id == 482 | Gemeinde_id == 490 
       | Gemeinde_id == 493 | Gemeinde_id == 495 | Gemeinde_id == 498 | Gemeinde_id == 499
       | Gemeinde_id == 500 | Gemeinde_id == 507 | Gemeinde_id == 510 | Gemeinde_id == 511
       | Gemeinde_id == 513 | Gemeinde_id == 515 | Gemeinde_id == 516 | Gemeinde_id == 519 | Gemeinde_id == 520 | Gemeinde_id == 523
       | Gemeinde_id == 525 | Gemeinde_id == 526 | Gemeinde_id == 528 | Gemeinde_id == 529 | Gemeinde_id == 530 | Gemeinde_id == 531
       | Gemeinde_id == 532 | Gemeinde_id == 536 | Gemeinde_id == 539 | Gemeinde_id == 541 | Gemeinde_id == 548
       | Gemeinde_id == 549 | Gemeinde_id == 550 | Gemeinde_id == 551 | Gemeinde_id == 552 | Gemeinde_id == 556 
       | Gemeinde_id == 557 | Gemeinde_id == 558 | Gemeinde_id == 560 | Gemeinde_id == 565 | Gemeinde_id == 566 | Gemeinde_id == 567
       | Gemeinde_id == 568 | Gemeinde_id == 571 | Gemeinde_id == 578 | Gemeinde_id == 580 | Gemeinde_id == 583
       | Gemeinde_id == 586 | Gemeinde_id == 596 | Gemeinde_id == 597 | Gemeinde_id == 601 | Gemeinde_id == 602
       | Gemeinde_id == 604 | Gemeinde_id == 605 | Gemeinde_id == 607 | Gemeinde_id == 611 | Gemeinde_id == 612
       | Gemeinde_id == 614 | Gemeinde_id == 619 | Gemeinde_id == 621 | Gemeinde_id == 624 | Gemeinde_id == 626
       | Gemeinde_id == 627 | Gemeinde_id == 634 | Gemeinde_id == 636)

abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id==2] = "ABEL FIGUEIREDO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id == 3] = "ABREULÂNDIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id == 7] = "AFUÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id == 9] = "ÁGUA BOA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  10] = "AGUIARNÓPOLIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  11] = "ALCÂNTARA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  18] = "ALTO ALEGRE DO MARANHÃO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  19] = "ALTO ALEGRE DO PINDARÉ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  22] = "ALTO PARAGUAI"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  27] = "AMAPÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  30] = "AMATURÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  31] = "ANAJÁS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  32] = "ANAJATUBA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  33] = "ANAMÃ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  34] = "ANANÁS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  35] = "ANANINDEUA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  37] = "ANGICO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  38] = "ANORI"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  40] = "APICUM-AÇU"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  44] = "ARAGUAIANA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  46] = "ARAGUANÃ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  47] = "ARAGUANÃ (MA)"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  49] = "ARAME"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  50] = "ARAPOEMA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  51] = "ARAPUTANGA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  53] = "ARENÁPOLIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  58] = "AUGUSTINÓPOLIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  59] = "AUGUSTO CORRÊA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  63] = "AXIXÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  64] = "AXIXÁ DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  65] = "BACABAL"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  66] = "BACABEIRA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  67] = "BACURI"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  70] = "BANDEIRANTES DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  77] = "BARRA DO GARÇAS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  79] = "BELÉM"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  81] = "BENEVIDES"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  83] = "BEQUIMÃO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  84] = "BERNARDO DO MEARIM"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  85] = "BERNARDO SAYÃO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  87] = "BOA VISTA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  88] = "BOA VISTA DO GURUPI"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  95] = "BOM JESUS DO TOCANTINS (TO)"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  97] = "BONITO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  99] = "BRAGANÇA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  101] = "BRASILÂNDIA DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  104] = "BREJO DE AREIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  111] = "BURITI DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  119] = "CACHOEIRA DO ARARI"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  121] = "CACHOEIRA GRANDE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  122] = "CACHOEIRINHA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  129] = "CAMPO NOVO DO PARECIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  130] = "CAMPO VERDE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  131] = "CAMPOS DE JÚLIO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  134] = "CANARANA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  140] = "CAPANEMA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  141] = "CAPINZAL DO NORTE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  149] = "CARMOLÂNDIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  151] = "CARRASCO BONITO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  152] = "CARUTAPERA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  156] = "CASTANHEIRAS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  157] = "CEDRAL"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  161] = "CHAPADA DE AREIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  162] = "CHAPADA DOS GUIMARÃES"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  163] = "CHAVES"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  170] = "CODÓ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  171] = "COLARES"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  174] = "COLINAS DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  175] = "COLMÉIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  182] = "CONQUISTA D'OESTE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  183] = "COROATÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  187] = "COUTO MAGALHÃES"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  189] = "CUIABÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  192] = "CURIONÓPOLIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  193] = "CURRALINHO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  196] = "CURVELÂNDIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  197] = "CUTIAS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  198] = "DARCINÓPOLIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  199] = "DAVINÓPOLIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  200] = "DENISE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  202] = "DIVINÓPOLIS DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  203] = "DOIS IRMÃOS DO TOCANTIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  204] = "DOM AQUINO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  207] = "DUERÉ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  212] = "ESPERANTINA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  213] = "ESPERANTINÓPOLIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  215] = "ESTREITO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  220] = "FERREIRA GOMES"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  221] = "FIGUEIRÓPOLIS D'OESTE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  223] = "FONTE BOA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  224] = "FORMOSA DA SERRA NEGRA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  229] = "GLÓRIA D'OESTE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  230] = "GODOFREDO VIANA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  232] = "GOIANORTE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  235] = "GOVERNADOR EDINSON LOBÃO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  239] = "GOVERNADOR NEWTON BELLO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  240] = "GOVERNADOR NUNES FREIE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  245] = "GUARAÍ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  247] = "GUIMARÃES"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  248] = "GUIRATINGA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  251] = "ICATU"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  252] = "IGARAPÉ DO MEIO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  253] = "IGARAPÉ-AÇU"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  255] = "IMPERATRIZ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  256] = "INDIAVAÍ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  257] = "INHANGAPI"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  263] = "IRITUIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  271] = "ITAPIRANGA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  272] = "ITAPORÃ DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  275] = "ITAUBAL"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  277] = "ITIQUIRA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  280] = "JACIARA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  282] = "JAPURÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  288] = "JOÃO LISBOA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  292] = "JUARINA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  294] = "JUNCO DO MARANHÃO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  298] = "JUSCIMEIRA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  301] = "LAGO DA PEDRA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  303] = "LAGOA GRANDE DO MARANHÃO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  305] = "LAMBARI D'OESTE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  307] = "LIMA CAMPOS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  308] = "LIMOEIRO DO AJURU"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  310] = "LUCIARA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  311] = "LUÍS DOMINGUES"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  312] = "LUZINÓPOLIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  315] = "MÃE DO RIO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  316] = "MAGALHÃES BARATA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  325] = "MARACAÇUMÉ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  328] = "MARANHÃOZINHO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  329] = "MARAPANIM"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  332] = "MARIANÓPOLIS DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  333] = "MARITUBA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  334] = "MATINHA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  338] = "MAURILÂNDIA DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  344] = "MIRANORTE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  347] = "MIRINZAL"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  348] = "MOCAJUBA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  354] = "MONTE SANTO DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  356] = "MORROS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  357] = "MUANÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  359] = "MURICILÂNDIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  360] = "NAZARÉ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  362] = "NOBRES"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  363] = "NORMANDIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  365] = "NOSSA SENHORA DO LIVRAMENTO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  367] = "NOVA BRASILÂNDIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  379] = "NOVA NAZARÉ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  380] = "NOVA OLÍMPIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  381] = "NOVA OLINDA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  385] = "NOVA TIMBOTEUA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  388] = "NOVA XAVANTINA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  391] = "NOVO HORIZONTE DO NORTE" 
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  401] = "OURÉM"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  403] = "OURO PRETO DO OESTE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  405] = "PACARAIMA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  408] = "PALMEIRÂNDIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  409] = "PALMEIRANTE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  410] = "PALMEIRAS DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  420] = "PAULO RAMOS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  422] = "PEDRA PRETA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  423] = "PEDREIRAS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  424] = "PEDRO AFONSO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  425] = "PEDRO DO ROSÁRIO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  426] = "PEIXE-BOI"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  429] = "PEQUIZEIRO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  430] = "PERI MIRIM"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  431] = "PERITORÓ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  435] = "PINHEIRO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  436] = "PIRAPEMAS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  437] = "PIRAQUÊ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  438] = "PIUM"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  441] = "PLANALTO DA SERRA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  442] = "POÇÃO DE PEDRAS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  443] = "POCONÉ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  444] = "PONTA DE PEDRAS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  451] = "PORTO ESPERIDIÃO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  452] = "PORTO ESTRELA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  455] = "PORTO RICO DO MARANHÃO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  458] = "POXORÉU"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  459] = "PRACUÚBA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  464] = "PRESIDENTE JUSCELINO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  465] = "PRESIDENTE KENNEDY"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  467] = "PRESIDENTE MÉDICI (MA)"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  468] = "PRESIDENTE SARNEY"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  470] = "PRIMAVERA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  471] = "PRIMAVERA DE RONDÔNIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  472] = "QUATIPURU"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  474] = "RAPOSA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  475] = "RECURSOLÂNDIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  478] = "RIACHINHO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  482] = "RIO BRANCO (MT)"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  490] = "RONDONÓPOLIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  493] = "ROSÁRIO OESTE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  495] = "SALINÓPOLIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  498] = "SAMPAIO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  499] = "SANDOLÂNDIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  500] = "SANTA BÁRBARA DO PARÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  507] = "SANTA IZABEL DO PARÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  510] = "SANTA LUZIA DO PARÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  511] = "SANTA LUZIA DO PARUÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  513] = "SANTA MARIA DO PARÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  515] = "SANTA RITA DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  516] = "SANTA RITA DO TRIVELATO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  519] = "SANTA TEREZINHA DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  520] = "SANTANA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  523] = "SANTARÉM NOVO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  525] = "SANTO ANTÔNIO DO IÇÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  526] = "SANTO ANTÔNIO DO LESTE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  528] = "SANTO ANTÔNIO DO TAUÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  529] = "SANTO ANTÔNIO DOS LOPES"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  530] = "SÃO BENTO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  531] = "SÃO BENTO DO TOCANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  532] = "SÃO CAETANO DE ODIVELAS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  536] = "SÃO FELIPE D'OESTE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  539] = "SÃO FRANCISCO DO BREJÃO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  541] = "SÃO FRANCISCO DO PARÁ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  548] = "SÃO JOÃO DO CARÚ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  549] = "SÃO JOÃO DO PARAÍSO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  550] = "SÃO JOÃO DO SOTER"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  551] = "SÃO JOSÉ DE RIBAMAR"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  552] = "SÃO JOSÉ DO POVO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  556] = "SÃO JOSÉ DOS QUATRO MARCOS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  557] = "SÃO LUÍS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  558] = "SÃO LUÍS GONZAGA DO MARANHÃO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  560] = "SÃO MATEUS DO MARANHÃO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  565] = "SÃO PEDRO DA ÁGUA BRANCA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  566] = "SÃO PEDRO DA CIPA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  567] = "SÃO RAIMUNDO DO DOCA BEZERRA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  568] = "SÃO SEBASTIÃO DA BOA VISTA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  571] = "SAPUCAIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  578] = "SERRA DO NAVIO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  580] = "SERRANO DO MARANHÃO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  583] = "SÍTIO NOVO"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  586] = "SOURE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  596] = "TEIXEIRÓPOLIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  597] = "TERRA ALTA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  601] = "TIMBIRAS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  602] = "TOCANTINÓPOLIS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  604] = "TONANTINS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  605] = "TRACUATEUA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  607] = "TRIZIDELA DO VALE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  611] = "TURIAÇU"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  612] = "TURILÂNDIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  614] = "UIRAMUTÃ"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  619] = "URUCURITUBA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  621] = "VALE DE SÃO DOMINGOS"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  624] = "VARGEM GRANDE"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  626] = "VIANA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  627] = "VIGIA"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  634] = "VITÓRIA DO MEARIM"
abholzungen_balanced$Gemeinde[abholzungen_balanced$Gemeinde_id ==  636] = "WANDERLÂNDIA"



#### Some Gemeinde have too many NA observations 
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 636)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 634)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 619)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 607)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 605)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 601)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 586)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 583)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 580)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 571)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 568)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 567)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 566)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 560)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 558)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 552)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 550)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 536)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 530)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 528)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 526)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 519)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 515)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 511)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 510)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 499)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 498)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 493)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 482)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 475)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 474)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 472)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 467)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 465)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 455)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 444)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 443)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 442)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 441)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 438)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 430)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 429)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 423)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 422)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 410)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 409)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 408)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 388)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 381)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 365)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 363)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 357)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 356)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 354)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 347)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 344)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 338)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 334)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 333)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 328)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 316)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 315)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 312)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 308)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 301)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 298)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 294)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 292)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 280)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 277)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 275)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 257)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 256)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 251)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 248)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 247)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 240)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 239)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 235)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 229)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 221)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 215)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 213)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 207)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 204)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 203)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 202)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 200)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 198)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 196)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 189)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 175)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 174)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 171)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 161)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 157)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 151)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 149)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 130)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 129)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 122)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 121)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 119)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 111)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 101)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 95)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 85)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 84)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 83)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 81)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 77)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 70)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 67)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 65)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 64)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 63)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 59)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 53)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 51)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 47)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 44)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 40)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 35)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 31)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 22)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 18)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 11)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 9)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 7)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 3)
abholzungen_balanced <-subset(abholzungen_balanced, Gemeinde_id!= 2)















## Dimensions
is.pbalanced(abholzungen_balanced)
pdim(abholzungen_balanced) 



### Replacing  NA's with mean 
####################################################### DO NOT USE WITHOUT CHECKING!
for (i in which(sapply(abholzungen_balanced, is.numeric))) {
  for (j in which(is.na(abholzungen_balanced[, i]))) {
    abholzungen_balanced[j, i] <- mean(abholzungen_balanced[abholzungen_balanced[, "Gemeinde_id"] == abholzungen_balanced[j, "Gemeinde_id"], i],  na.rm = TRUE)
  }
}
##############################################################################################################



## Cashflows into Municipalities 
is.pbalanced(projekte)
pdim(projekte)

colnames(projekte)
names(projekte)[1] <- "Gemeinde"
names(projekte)[2] <- "Jahr"
names(projekte)[3]<- "Cashflow"
projekte$V4 <- NULL

projekte$Cashflow = gsub(",", ".", projekte$Cashflow)
projekte$Cashflow <- as.numeric(projekte$Cashflow)
typeof(projekte$Cashflow)
class(projekte$Cashflow)



### Balancing the projekte dataframe
projekte$Gemeinde_id <- paste(projekte$Gemeinde,projekte$Jahr) 
projekte$duplicate = duplicated(projekte$Gemeinde_id) # generate the duplicate variable
subset(projekte, duplicate=="TRUE")

projekte <-
  projekte %>% 
  group_by(Gemeinde,Jahr) %>% 
  select(-c(Gemeinde_id,duplicate)) %>% 
  summarize(Cashflow = sum(Cashflow, na.rm=TRUE))

projekte$Gemeinde_id <- as.numeric(factor(projekte$Gemeinde))

projekte_balanced <- make.pbalanced(projekte, balance.type = c("fill"), index = c("Gemeinde_id","Jahr"))
      
subset(projekte_balanced, is.na(Gemeinde))

subset(projekte_balanced, Gemeinde_id == 1)
projekte_balanced$Gemeinde[projekte_balanced$Gemeinde_id==1] = "ABAETETUBA"


is.pbalanced(projekte_balanced)
pdim(projekte_balanced)


##### Adding Treatment Dummies
projekte_treatments <- read_excel("data/projekte_treatments.xlsx")
names(projekte_treatments)[5] <- "Treatments"
names(projekte_treatments)[6] <- "Treated"



#### DiD Graph 
### Treatment happens in time period = 0 
projekte_treatments$JahrIndicator <- 0
for(n in 1:length(projekte_treatments$Treated)){ifelse(projekte_treatments$Treated[n] == 0, projekte_treatments$JahrIndicator[n] <- -1, ifelse(projekte_treatments$Treated[n-1] == 1, projekte_treatments$JahrIndicator[n] <- 1, projekte_treatments$JahrIndicator[n] <- 0))}


## Removing ID's to prevent unbalances or repeated values 
projekte_treatments$Gemeinde_id <- NULL
abholzungen_balanced$Gemeinde_id <- NULL
### Merging both data frames by Gemeinde and Jahr and checking for unbalances
abholzungen_merged <- merge(abholzungen_balanced, projekte_treatments, by = c("Gemeinde", "Jahr"), all = TRUE)
is.pbalanced(abholzungen_merged)
pdim(abholzungen_merged) 

abholzungen_merged <-subset(abholzungen_merged, Gemeinde != "SANTA LUZIA DO PARÁ")
abholzungen_merged <-subset(abholzungen_merged, Gemeinde != "POCONÉ")
abholzungen_merged <-subset(abholzungen_merged, Gemeinde != "NOSSA SENHORA DO LIVRAMENTO")
abholzungen_merged <-subset(abholzungen_merged, Gemeinde != "POCONÉ")
abholzungen_merged <-subset(abholzungen_merged, Gemeinde != "MÃE DO RIO")
abholzungen_merged <-subset(abholzungen_merged, Gemeinde != "CUIABÁ")
abholzungen_merged <-subset(abholzungen_merged, Gemeinde != "ABEL FIGUEIREDO")







table(abholzungen_merged$Jahr)
table(abholzungen_merged$Gemeinde)

subset(abholzungen_merged, is.na(Cashflow))
subset(abholzungen_merged, is.na(Fläche))

abholzungen_merged$Cashflow[is.na(abholzungen_merged$Cashflow)] <- 0



### Adding commodity production at the munucipal level
library(readxl)
landwirtschaft <- read_excel("data/Landwirtschaftlich verwendete Flächen.xlsx")
is.pbalanced(landwirtschaft)
pdim(landwirtschaft)


## Constant over time 
landwirtschaft <- landwirtschaft[rep(seq_len(nrow(landwirtschaft)), each = 13), ]
landwirtschaft$Jahr <- rep(c(2008,2009,2010,2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),times=637)
is.pbalanced(landwirtschaft)

typeof(landwirtschaft$FLÄCHE)
landwirtschaft$FLÄCHE <- as.numeric(landwirtschaft$FLÄCHE)


## Balancing the landwirtschaft df
landwirtschaft$id <- paste(landwirtschaft$GEMEINDE,landwirtschaft$Jahr) # concatenate to make unique ID
landwirtschaft$duplicate = duplicated(landwirtschaft$id) # generate the duplicate variable
subset(landwirtschaft, duplicate=="TRUE")

landwirtschaft <-
  landwirtschaft %>% 
  group_by(GEMEINDE,Jahr) %>% 
  select(-c(id,duplicate)) %>% 
  summarize(FLÄCHE = sum(FLÄCHE, na.rm=TRUE))


is.pbalanced(landwirtschaft)
pdim(landwirtschaft)

names(landwirtschaft)[1] <- "Gemeinde"
names(landwirtschaft)[3] <- "Verwendete_Fläche" 
  

### Merging landwirtschaft with main df
abholzungen_merged_again <- merge(abholzungen_merged, landwirtschaft, by = c("Gemeinde", "Jahr"), all = TRUE) ## Adding to dataframe

is.pbalanced(abholzungen_merged_again)
pdim(abholzungen_merged_again)


abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ABEL FIGUEIREDO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ABREULÂNDIA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "AFUÁ")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ÁGUA BOA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ALCÂNTARA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ALTAMIRA DO MARANHÃO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ALTO PARAGUAI")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ANAJÁS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ANANINDEUA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "APICUM-AÇU")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ARAGUAIANA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ARAPUTANGA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ARENÁPOLIS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "AUGUSTO CORRÊA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "AXIXÁ")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "AXIXÁ DO TOCANTINS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "BACABAL")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "BACURI")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "BANDEIRANTES DO TOCANTINS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "BARRA DO GARÇAS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "BARREIRINHAS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "BENEVIDES")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "BERNARDO DO MEARIM")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "BERNARDO SAYÃO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "BRASILÂNDIA DO TOCANTINS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "BURITI DO TOCANTINS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "CACHOEIRA DO ARARI")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "CACHOEIRA GRANDE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "CACHOEIRINHA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "CAMPO NOVO DO PARECIS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "CAMPO VERDE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "CARMOLÂNDIA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "CARRASCO BONITO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "CEDRAL")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "CHAPADA DE AREIA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "COLARES")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "COLINAS DO TOCANTINS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "COLMÉIA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "CUIABÁ")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "CURVELÂNDIA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "DARCINÓPOLIS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "DENISE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "DIVINÓPOLIS DO TOCANTINS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "DOIS IRMÃOS DO TOCANTINS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "DOM AQUINO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "DUERÉ")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ESPERANTINÓPOLIS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ESTREITO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "FIGUEIRÓPOLIS D'OESTE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "GLÓRIA D'OESTE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "GOVERNADOR EDISON LOBÃO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "GOVERNADOR NEWTON BELLO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "GOVERNADOR NUNES FREIRE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "GUIMARÃES")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "GUIRATINGA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ICATU")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "INDIAVAÍ")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "INHANGAPI")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ITAUBAL")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ITIQUIRA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "JACIARA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "JUARINA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "JUNCO DO MARANHÃO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "JUSCIMEIRA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "LAGO DA PEDRA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "LIMOEIRO DO AJURU")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "LUZINÓPOLIS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "MÃE DO RIO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "MAGALHÃES BARATA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "MARANHÃOZINHO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "MARITUBA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "MATINHA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "MAURILÂNDIA DO TOCANTINS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "MIRANORTE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "MIRINZAL")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "MONTE SANTO DO TOCANTINS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "MORROS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "MUANÁ")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "NORMANDIA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "NOSSA SENHORA DO LIVRAMENTO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "NOVA OLINDA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "NOVA XAVANTINA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "PALMEIRÂNDIA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "PALMEIRANTE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "PALMEIRAS DO TOCANTINS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "PEDRA PRETA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "PEDREIRAS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "PEQUIZEIRO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "PERI MIRIM")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "PIUM")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "PLANALTO DA SERRA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "POÇÃO DE PEDRAS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "POCONÉ")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "PONTA DE PEDRAS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "PORTO RICO DO MARANHÃO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "PRESIDENTE KENNEDY")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "QUATIPURU")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "RAPOSA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "RECURSOLÂNDIA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "ROSÁRIO OESTE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SAMPAIO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SANDOLÂNDIA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SANTA LUZIA DO PARÁ")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SANTA LUZIA DO PARUÁ")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SANTA RITA DO TOCANTINS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SANTA TEREZINHA DO TOCANTINS")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SANTO ANTÔNIO DO LESTE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SANTO ANTÔNIO DO TAUÁ")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SÃO BENTO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SÃO FELIPE D'OESTE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SÃO JOSÉ DO POVO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SÃO LUÍS GONZAGA DO MARANHÃO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SÃO MATEUS DO MARANHÃO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SÃO PEDRO DA CIPA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SÃO RAIMUNDO DO DOCA BEZERRA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SÃO SEBASTIÃO DA BOA VISTA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SAPUCAIA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SERRANO DO MARANHÃO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SÍTIO NOVO")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "SOURE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "TRACUATEUA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "TRIZIDELA DO VALE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "URUCURITUBA")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "VALE DO ACRE")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "VALE DO JURUÁ")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "VITÓRIA DO MEARIM")
abholzungen_merged_again <-subset(abholzungen_merged_again, Gemeinde != "WANDERLÂNDIA")








### Adding commodity prices -> time variant
Commodity_prices <- read_xlsx("data/Commodity_price.xlsx")
names(Commodity_prices)[1] <- "Jahr"


abholzungen_merged_again2 <- merge(abholzungen_merged_again, Commodity_prices, by = c("Jahr"), all = TRUE) ## Adding to dataframe

pdim(abholzungen_merged_again2)



## Adding data of the "amazon area" of each Gemeinde 
Fläche_im_Wald  <- read_excel("data/Gesammtfläche von Gemeinden.xls")
## Only interested in some columns 
Fläche_im_Wald$NM_REGIAO <- NULL
Fläche_im_Wald$NM_UF <- NULL
Fläche_im_Wald$NM_MUN...7 <- NULL
Fläche_im_Wald$CD_UF <- NULL
Fläche_im_Wald$SIGLA <- NULL
Fläche_im_Wald$CD_MUN <- NULL
Fläche_im_Wald$LAT_SEDE <- NULL
Fläche_im_Wald$LNG_SEDE <- NULL
Fläche_im_Wald$...13 <- NULL
Fläche_im_Wald$...14 <- NULL
Fläche_im_Wald$...15 <- NULL
Fläche_im_Wald$...16 <- NULL
Fläche_im_Wald$...17 <- NULL
Fläche_im_Wald$Gebiet_Regierung <- ifelse(Fläche_im_Wald$SEDE_AL == "true", 1, 0)
Fläche_im_Wald$SEDE_AL <- NULL

is.pbalanced(Fläche_im_Wald)
pdim(Fläche_im_Wald)

#INTERESTING COLUMNS 
colnames(Fläche_im_Wald)
names(Fläche_im_Wald)[1] <- "Gemeinde"
names(Fläche_im_Wald)[2] <- "Gesamtfläche"
names(Fläche_im_Wald)[3] <- "Anteil_Amazonas"
names(Fläche_im_Wald)[4] <- "Anteil_prozent"

### Doesn't change over Time 
Fläche_im_Wald_Jahre <- Fläche_im_Wald[rep(seq_len(nrow(Fläche_im_Wald)), each = 13), ]
Fläche_im_Wald_Jahre$Jahr <- rep(c(2008,2009,2010,2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),times=636)
is.pbalanced(Fläche_im_Wald_Jahre)

## Balancing the area of each Gemeinde before merging 
Fläche_im_Wald_Jahre$Gemeinde_id <- paste(Fläche_im_Wald_Jahre$Gemeinde,Fläche_im_Wald_Jahre$Jahr) 
Fläche_im_Wald_Jahre$duplicate = duplicated(Fläche_im_Wald_Jahre$Gemeinde_id)
subset(Fläche_im_Wald_Jahre, duplicate=="TRUE") 


Fläche_im_Wald_Jahre <-
  Fläche_im_Wald_Jahre %>% 
  group_by(Gemeinde,Jahr) %>% 
  select(-c(Gemeinde_id,duplicate)) %>% 
  summarize(Gesamtfläche, Anteil_Amazonas, Anteil_prozent  = sum(Gesamtfläche, Anteil_Amazonas, Anteil_prozent, na.rm = TRUE))

is.pbalanced(Fläche_im_Wald_Jahre)
pdim(Fläche_im_Wald_Jahre)


Fläche_im_Wald_Jahre$Prozent <-  Fläche_im_Wald_Jahre$Anteil_Amazonas / Fläche_im_Wald_Jahre$Gesamtfläche * 100
  
Fläche_im_Wald_Jahre$Anteil_prozent <- NULL


##Adding to dataframe
stillmerging_abholzungen <- merge(abholzungen_merged_again2, Fläche_im_Wald_Jahre, by = c("Gemeinde", "Jahr"), all = TRUE)

stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ABEL FIGUEIREDO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ABREULÂNDIA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "AFUÁ")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ÁGUA BOA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ALCÂNTARA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ALTAMIRA DO MARANHÃO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ALTO ALEGRE DO MARANHÃO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ALTO PARAGUAI")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ANAJÁS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ANANINDEUA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "APICUM-AÇU")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ARAGUAIANA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ARAPUTANGA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ARENÁPOLIS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "AUGUSTO CORRÊA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "AXIXÁ")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "AXIXÁ DO TOCANTINS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "BACABAL")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "BACURI")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "BANDEIRANTES DO TOCANTINS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "BARRA DO GARÇAS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "BARREIRINHAS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "BENEVIDES")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "BERNARDO DO MEARIM")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "BERNARDO SAYÃO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "BRASILÂNDIA DO TOCANTINS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "BEQUIMÃO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "BURITI DO TOCANTINS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "CACHOEIRA DO ARARI")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "CACHOEIRA GRANDE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "CACHOEIRINHA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "CAMPO NOVO DO PARECIS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "CAMPO VERDE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "CARMOLÂNDIA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "CARRASCO BONITO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "CEDRAL")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "CHAPADA DE AREIA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "COLARES")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "COLINAS DO TOCANTINS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "COLMÉIA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "CUIABÁ")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "CURVELÂNDIA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "DARCINÓPOLIS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "DENISE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "DIVINÓPOLIS DO TOCANTINS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "DOIS IRMÃOS DO TOCANTINS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "DOM AQUINO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "DUERÉ")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ESPERANTINÓPOLIS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ESTREITO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "FIGUEIRÓPOLIS D'OESTE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "GLÓRIA D'OESTE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "GOVERNADOR EDISON LOBÃO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "GOVERNADOR NEWTON BELLO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "GOVERNADOR NUNES FREIRE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "GUIMARÃES")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "GUIRATINGA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ICATU")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "INDIAVAÍ")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "INHANGAPI")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ITAUBAL")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ITIQUIRA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "JACIARA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "JUARINA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "JUNCO DO MARANHÃO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "JUSCIMEIRA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "LAJEADO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "LAGO DA PEDRA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "LIMOEIRO DO AJURU")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "LUZINÓPOLIS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "MÃE DO RIO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "MAGALHÃES BARATA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "MARANHÃOZINHO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "MARITUBA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "MATINHA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "MAURILÂNDIA DO TOCANTINS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "MIRANORTE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "MIRINZAL")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "MONTE SANTO DO TOCANTINS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "MORROS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "MUANÁ")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "NORMANDIA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "NOSSA SENHORA DO LIVRAMENTO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "NOVA OLINDA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "NOVA XAVANTINA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "PALMEIRÂNDIA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "PALMEIRANTE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "PALMEIRAS DO TOCANTINS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "PEDRA PRETA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "PEDREIRAS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "PEQUIZEIRO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "PERI MIRIM")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "PIUM")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "PLANALTO DA SERRA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "POÇÃO DE PEDRAS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "POCONÉ")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "PONTA DE PEDRAS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "PORTO RICO DO MARANHÃO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "PRESIDENTE KENNEDY")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "QUATIPURU")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "RAPOSA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "RECURSOLÂNDIA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "ROSÁRIO OESTE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SAMPAIO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SANDOLÂNDIA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SANTA CRUZ DO ARARI")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SANTA LUZIA DO PARÁ")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SANTA LUZIA DO PARUÁ")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SANTA RITA DO TOCANTINS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SANTA TEREZINHA DO TOCANTINS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SANTO ANTÔNIO DO LESTE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SANTO ANTÔNIO DO TAUÁ")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SÃO BENTO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SÃO FELIPE D'OESTE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SÃO JOÃO DO SOTER")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SÃO JOSÉ DO POVO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SÃO LUÍS GONZAGA DO MARANHÃO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SÃO MATEUS DO MARANHÃO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SÃO PEDRO DA CIPA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SÃO RAIMUNDO DO DOCA BEZERRA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SÃO SEBASTIÃO DA BOA VISTA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SAPUCAIA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SERRANO DO MARANHÃO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SÍTIO NOVO")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "SOURE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "TIMBIRAS")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "TRACUATEUA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "TRIZIDELA DO VALE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "URUCURITUBA")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "VALE DO ACRE")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "VALE DO JURUÁ")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "VITÓRIA DO MEARIM")
stillmerging_abholzungen <-subset(stillmerging_abholzungen, Gemeinde != "WANDERLÂNDIA")




## Checking for unbalances
is.pbalanced(stillmerging_abholzungen) ##Panel is balanced but with repeated values
pdim(stillmerging_abholzungen)


### Still want to add data about indigenous lands
indigenous_lands_PA <- read.dbf("data/TI_PA.dbf", as.is = FALSE)
indigenous_lands_PA <- indigenous_lands_PA[, -c(1:2)]
indigenous_lands_PA <- indigenous_lands_PA[, -c(6:14)]
indigenous_lands_PA <- indigenous_lands_PA[, -c(2)]


indigenous_lands_AM <- read.dbf("data/TI_AM.dbf", as.is = FALSE)
indigenous_lands_AM <- indigenous_lands_AM[, -c(1:2)]
indigenous_lands_AM <- indigenous_lands_AM[, -c(6:14)]
indigenous_lands_AM <- indigenous_lands_AM[, -c(2)]

indigenous_lands_AP <- read.dbf("data/TI_AP.dbf", as.is = FALSE)
indigenous_lands_AP <- indigenous_lands_AP[, -c(1:2)]
indigenous_lands_AP <- indigenous_lands_AP[, -c(6:14)]
indigenous_lands_AP <- indigenous_lands_AP[, -c(2)]

indigenous_lands_RR <- read.dbf("data/TI_RR.dbf", as.is = FALSE)
indigenous_lands_RR <- indigenous_lands_RR[, -c(1:2)]
indigenous_lands_RR <- indigenous_lands_RR[, -c(6:14)]
indigenous_lands_RR <- indigenous_lands_RR[, -c(2)]

indigenous_lands_RO <- read.dbf("data/TI_RO.dbf", as.is = FALSE)
indigenous_lands_RO <- indigenous_lands_RO[, -c(1:2)]
indigenous_lands_RO <- indigenous_lands_RO[, -c(6:14)]
indigenous_lands_RO <- indigenous_lands_RO[, -c(2)]

indigenous_lands_AC <- read.dbf("data/TI_AC.dbf", as.is = FALSE)
indigenous_lands_AC <- indigenous_lands_AC[, -c(1:2)]
indigenous_lands_AC <- indigenous_lands_AC[, -c(6:14)]
indigenous_lands_AC <- indigenous_lands_AC[, -c(2)]

indigenous_lands_MA <- read.dbf("data/TI_MA.dbf", as.is = FALSE)
indigenous_lands_MA <- indigenous_lands_MA[, -c(1:2)]
indigenous_lands_MA <- indigenous_lands_MA[, -c(6:14)]
indigenous_lands_MA <- indigenous_lands_MA[, -c(2)]

indigenous_lands_MT <- read.dbf("data/TI_MT.dbf", as.is = FALSE)
indigenous_lands_MT <- indigenous_lands_MT[, -c(1:2)]
indigenous_lands_MT <- indigenous_lands_MT[, -c(6:14)]
indigenous_lands_MT <- indigenous_lands_MT[, -c(2)]

indigenous_lands_TO <- read.dbf("data/TI_TO.dbf", as.is = FALSE)
indigenous_lands_TO <- indigenous_lands_TO[, -c(1:2)]
indigenous_lands_TO <- indigenous_lands_TO[, -c(6:14)]
indigenous_lands_TO <- indigenous_lands_TO[, -c(2)]


indigenous_lands <- do.call("rbind", list(indigenous_lands_AC, indigenous_lands_AM, indigenous_lands_AP,
                                          indigenous_lands_MA, indigenous_lands_MT, indigenous_lands_PA,
                                          indigenous_lands_RO, indigenous_lands_RR, indigenous_lands_TO))
## Names of the Gemeinden are all messed up because of brazilian characters 
indigenous_lands_organized <- read_xlsx("data/Final_Data_for_Indigenous_Lands.xlsx")


### New Data with proper names
indigenous_lands_organized <- indigenous_lands_organized[rep(seq_len(nrow(indigenous_lands_organized)), each = 13), ]
indigenous_lands_organized$Jahr <- rep(c(2008,2009,2010,2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),times=236)
names(indigenous_lands_organized)[1] <- "Gemeinde"
names(indigenous_lands_organized)[4] <- "Fläche_Indianer"
names(indigenous_lands_organized)[2] <- "Durchschnittliche_Homologation"
names(indigenous_lands_organized)[3] <- "Erste_Homologation"

## Balancing just in case
indigenous_lands_organized$Gemeinde_id <- paste(indigenous_lands_organized$Gemeinde,indigenous_lands_organized$Jahr) 
indigenous_lands_organized$duplicate = duplicated(indigenous_lands_organized$Gemeinde_id)
subset(indigenous_lands_organized, duplicate=="TRUE") 


indigenous_lands_organized <-
  indigenous_lands_organized %>% 
  group_by(Gemeinde,Jahr) %>% 
  select(-c(Gemeinde_id,duplicate)) %>% 
  summarize(Durchschnittliche_Homologation, Erste_Homologation, Fläche_Indianer  = sum(Durchschnittliche_Homologation, Erste_Homologation, Fläche_Indianer, na.rm = TRUE))

is.pbalanced(indigenous_lands_organized)
pdim(Fläche_im_Wald_Jahre) ### Balanced panel: ready to be merged

### Merging indigenous land to main dataframe
stillmerging_abholzungen_2 <- merge(stillmerging_abholzungen, indigenous_lands_organized, by = c("Gemeinde", "Jahr"), all = TRUE)
stillmerging_abholzungen_2$Fläche_Indianer[is.na(stillmerging_abholzungen_2$Fläche_Indianer)] <- 0

stillmerging_abholzungen_2$Gemeinde_id <- as.numeric(factor(stillmerging_abholzungen_2$Gemeinde)) 

which(is.na(stillmerging_abholzungen_2$Fläche))

stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 6)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 58)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 94)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 101)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 146)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 191)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 263)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 271)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 291)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 314)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 357)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 360)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 361)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 417)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 418)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 431)
stillmerging_abholzungen_2 <-subset(stillmerging_abholzungen_2, Gemeinde_id!= 476)



pdim(stillmerging_abholzungen_2)





### Defining panel data frame for Difference-in-Difference
abholzungen_did <- pdata.frame(stillmerging_abholzungen_2, index = c("Gemeinde", "Jahr"))
is.pbalanced(abholzungen_did)
pdim(abholzungen_did)
is.na(abholzungen_did$Gemeinde)
which(is.na(abholzungen_did$Gemeinde))
is.na(abholzungen_did$Jahr)
which(is.na(abholzungen_did$Jahr))
which(is.na(abholzungen_did$Fläche))



abholzungen_did$Jahr <- as.numeric(as.character(abholzungen_did$Jahr))


### Dependet variable as share of total area ++ normalized measure
abholzungen_did$Anteil_abholzung <- abholzungen_did$Fläche/abholzungen_did$Gesamtfläche*100
abholzungen_did$diff2 <- diff(abholzungen_did$Anteil_abholzung)



### Creating variable for agricultural activity in municipalities
abholzungen_did$agriculture <- ((abholzungen_did$Banana+abholzungen_did$Beef+abholzungen_did$Corn+abholzungen_did$Hard_Logs+
  abholzungen_did$Rice+abholzungen_did$Coffee+abholzungen_did$Rubber+abholzungen_did$Soybeans+abholzungen_did$Sugar)/9)*abholzungen_did$Verwendete_Fläche

### Trying to add time variant indigenous variable to the data set
abholzungen_did$Reservations <- (abholzungen_did$Jahr - abholzungen_did$Erste_Homologation) * abholzungen_did$Fläche_Indianer
abholzungen_did$Reservations[is.na(abholzungen_did$Reservations)] <- 0


##### Average for Treatment and control group

abholzungen_did[["Unit.Indicator"]][is.na(abholzungen_did[["Unit.Indicator"]])] <- 0
abholzungen_did$ControlFläche <- ifelse(abholzungen_did$Unit.Indicator == 0, abholzungen_did$Fläche, 0)
abholzungen_did$TreatedFläche <- ifelse(abholzungen_did$Unit.Indicator >0, abholzungen_did$Fläche, 0)

abholzungen_did$indicator2010 <- ifelse(abholzungen_did$Jahr == 2010 & abholzungen_did$Treated > 0, abholzungen_did$Gemeinde_id,0)
treated2010 <- abholzungen_did[abholzungen_did$Gemeinde_id %in% c(490, 484, 465, 316, 312, 305, 298, 296, 269, 237, 224, 151, 119, 102, 31, 10), ]
treated2010means <- aggregate(treated2010$Fläche, list(treated2010$Jahr), mean)
names(treated2010means)[1] <- "Jahr"
names(treated2010means)[2] <- "Fläche2010"

abholzungen_did$indicator2011 <- ifelse(abholzungen_did$Jahr == 2011 & abholzungen_did$Treated > 0, abholzungen_did$Gemeinde_id,0)
treated2011 <- abholzungen_did[abholzungen_did$Gemeinde_id %in% c(504, 481, 399, 367, 334, 280, 222, 184, 163, 71), ]
treated2011means <- aggregate(treated2011$Fläche, list(treated2011$Jahr), mean)
names(treated2011means)[1] <- "Jahr"
names(treated2011means)[2] <- "Fläche2011"

abholzungen_did$indicator2012 <- ifelse(abholzungen_did$Jahr == 2012 & abholzungen_did$Treated > 0, abholzungen_did$Gemeinde_id,0)
treated2012 <- abholzungen_did[abholzungen_did$Gemeinde_id %in% c(469, 330, 281, 48, 28), ]
treated2012means <- aggregate(treated2012$Fläche, list(treated2012$Jahr), mean)
names(treated2012means)[1] <- "Jahr"
names(treated2012means)[2] <- "Fläche2012"

abholzungen_did$indicator2013 <- ifelse(abholzungen_did$Jahr == 2013 & abholzungen_did$Treated > 0, abholzungen_did$Gemeinde_id,0)
treated2013 <- abholzungen_did[abholzungen_did$Gemeinde_id %in% c(502, 487, 265, 258, 177, 134, 55), ]
treated2013means <- aggregate(treated2013$Fläche, list(treated2013$Jahr), mean)
names(treated2013means)[1] <- "Jahr"
names(treated2013means)[2] <- "Fläche2013"


abholzungen_did$indicator2014 <- ifelse(abholzungen_did$Jahr == 2014 & abholzungen_did$Treated > 0, abholzungen_did$Gemeinde_id,0)
treated2014 <- abholzungen_did[abholzungen_did$Gemeinde_id %in% c(485, 326, 325, 310, 283, 257, 251, 250, 235, 234, 219, 172, 159, 154, 140, 138, 124, 78, 41, 9, 8), ]
treated2014means <- aggregate(treated2014$Fläche, list(treated2014$Jahr), mean)
names(treated2014means)[1] <- "Jahr"
names(treated2014means)[2] <- "Fläche2014"

abholzungen_did$indicator2015 <- ifelse(abholzungen_did$Jahr == 2015 & abholzungen_did$Treated > 0, abholzungen_did$Gemeinde_id,0)
treated2015 <- abholzungen_did[abholzungen_did$Gemeinde_id %in% c(474, 468, 396, 392, 373, 372, 342, 266, 255, 240, 232, 168, 153, 114, 83, 77, 67, 4), ]
treated2015means <- aggregate(treated2015$Fläche, list(treated2015$Jahr), mean)
names(treated2015means)[1] <- "Jahr"
names(treated2015means)[2] <- "Fläche2015"

abholzungen_did$indicator2016 <- ifelse(abholzungen_did$Jahr == 2016 & abholzungen_did$Treated > 0, abholzungen_did$Gemeinde_id,0)
treated2016 <- abholzungen_did[abholzungen_did$Gemeinde_id %in% c(423), ]
treated2016means <- aggregate(treated2016$Fläche, list(treated2016$Jahr), mean)
names(treated2016means)[1] <- "Jahr"
names(treated2016means)[2] <- "Fläche2016"

abholzungen_did$indicator2017 <- ifelse(abholzungen_did$Jahr == 2017 & abholzungen_did$Treated > 0, abholzungen_did$Gemeinde_id,0)
treated2017 <- abholzungen_did[abholzungen_did$Gemeinde_id %in% c(524, 522, 498, 496, 475, 449, 403, 259, 213, 205, 133, 106, 76, 17, 12), ]
treated2017means <- aggregate(treated2017$Fläche, list(treated2017$Jahr), mean)
names(treated2017means)[1] <- "Jahr"
names(treated2017means)[2] <- "Fläche2017"

abholzungen_did$indicator2018 <- ifelse(abholzungen_did$Jahr == 2018 & abholzungen_did$Treated > 0, abholzungen_did$Gemeinde_id,0)
treated2018 <- abholzungen_did[abholzungen_did$Gemeinde_id %in% c(520, 507, 506, 464, 460, 459, 442, 440, 427, 421, 397, 393, 381, 366, 358, 329, 327, 323, 320 , 318, 301, 279, 273, 256, 254, 239, 238, 230, 226, 223, 217, 209, 202, 198, 195, 150, 129, 125, 116, 113, 98, 93, 92, 74, 61, 51, 42, 32, 11, 3, 1), ]
treated2018means <- aggregate(treated2018$Fläche, list(treated2018$Jahr), mean)
names(treated2018means)[1] <- "Jahr"
names(treated2018means)[2] <- "Fläche2018"


is.na(abholzungen_did$ControlFläche) <- abholzungen_did$ControlFläche == 0
is.na(abholzungen_did$TreatedFläche) <- abholzungen_did$TreatedFläche == 0 

controlyearmeans <- aggregate(abholzungen_did$ControlFläche, list(abholzungen_did$Jahr), mean, na.rm = TRUE)
treatedyearmeans <- aggregate(abholzungen_did$TreatedFläche, list(abholzungen_did$Jahr), mean, na.rm = TRUE)
names(treatedyearmeans)[1] <- "Jahr"
names(treatedyearmeans)[2] <- "FlächeT"
names(controlyearmeans)[1] <- "Jahr"
names(controlyearmeans)[2]<- "FlächeC"
means <- merge(treatedyearmeans, controlyearmeans, by = "Jahr")
tikz('report/T&CGraph.tex',width=4.5,height=4)
ggplot(means, aes(Jahr)) + 
  geom_line(aes(y = FlächeT, colour = "Treatment")) + 
  geom_line(aes(y = FlächeC, colour = "Control"))
dev.off()

means <- cbind(treated2010means, treated2011means, treated2012means, treated2013means, treated2014means, treated2015means, treated2016means, treated2017means, treated2018means, treatedyearmeans, controlyearmeans)
means[3] <- NULL
means[4] <- NULL
means[5]<- NULL
means[6]<- NULL
means[7]<-NULL
means[8] <- NULL
means[9] <- NULL
means[10] <- NULL
means[11] <- NULL
means[12] <- NULL

for(t in 1:length(means$Jahr)){ifelse(means$Jahr[t] < 2010, means$Gruppe2010Jahr[t] <- -1, ifelse(means$Jahr[t] == 2010, means$Gruppe2010Jahr[t] <- 0, means$Gruppe2010Jahr[t] <- 1))}
for(t in 1:length(means$Jahr)){ifelse(means$Jahr[t] < 2011, means$Gruppe2011Jahr[t] <- -1, ifelse(means$Jahr[t] == 2011, means$Gruppe2011Jahr[t] <- 0, means$Gruppe2011Jahr[t] <- 1))}
for(t in 1:length(means$Jahr)){ifelse(means$Jahr[t] < 2012, means$Gruppe2012Jahr[t] <- -1, ifelse(means$Jahr[t] == 2012, means$Gruppe2012Jahr[t] <- 0, means$Gruppe2012Jahr[t] <- 1))}
for(t in 1:length(means$Jahr)){ifelse(means$Jahr[t] < 2013, means$Gruppe2013Jahr[t] <- -1, ifelse(means$Jahr[t] == 2013, means$Gruppe2013Jahr[t] <- 0, means$Gruppe2013Jahr[t] <- 1))}
for(t in 1:length(means$Jahr)){ifelse(means$Jahr[t] < 2014, means$Gruppe2014Jahr[t] <- -1, ifelse(means$Jahr[t] == 2014, means$Gruppe2014Jahr[t] <- 0, means$Gruppe2014Jahr[t] <- 1))}
for(t in 1:length(means$Jahr)){ifelse(means$Jahr[t] < 2015, means$Gruppe2015Jahr[t] <- -1, ifelse(means$Jahr[t] == 2015, means$Gruppe2015Jahr[t] <- 0, means$Gruppe2015Jahr[t] <- 1))}
for(t in 1:length(means$Jahr)){ifelse(means$Jahr[t] < 2016, means$Gruppe2016Jahr[t] <- -1, ifelse(means$Jahr[t] == 2016, means$Gruppe2016Jahr[t] <- 0, means$Gruppe2016Jahr[t] <- 1))}
for(t in 1:length(means$Jahr)){ifelse(means$Jahr[t] < 2017, means$Gruppe2017Jahr[t] <- -1, ifelse(means$Jahr[t] == 2017, means$Gruppe2017Jahr[t] <- 0, means$Gruppe2017Jahr[t] <- 1))}
for(t in 1:length(means$Jahr)){ifelse(means$Jahr[t] < 2018, means$Gruppe2018Jahr[t] <- -1, ifelse(means$Jahr[t] == 2018, means$Gruppe2018Jahr[t] <- 0, means$Gruppe2018Jahr[t] <- 1))}
means_long <- read_xlsx("data/updated_means.xlsx")


#### DID GRAPHIK ########### ########### ########### ########### ###########

means_long$post <- 0
for(i in 1:length(means_long$time_til)){ifelse(means_long$time_til[i] > -1, means_long$post[i] <- 1, means_long$post[i] <- 0)}


means_long <- means_long%>%
mutate(
  lead1 = case_when(time_til == -1 ~ 1, TRUE ~ 0),
  lead2 = case_when(time_til == -2 ~ 1, TRUE ~ 0),
  lead3 = case_when(time_til == -3 ~ 1, TRUE ~ 0),
  lead4 = case_when(time_til == -4 ~ 1, TRUE ~ 0),
  lead5 = case_when(time_til == -5 ~ 1, TRUE ~ 0),
  lead6 = case_when(time_til == -6 ~ 1, TRUE ~ 0),
  lead7 = case_when(time_til == -7 ~ 1, TRUE ~ 0),
  lead8 = case_when(time_til == -8 ~ 1, TRUE ~ 0),
  lead9 = case_when(time_til == -9 ~ 1, TRUE ~ 0),
  
  lag0 = case_when(time_til == 0 ~ 1, TRUE ~ 0),
  lag1 = case_when(time_til == 1 ~ 1, TRUE ~ 0),
  lag2 = case_when(time_til == 2 ~ 1, TRUE ~ 0),
  lag3 = case_when(time_til == 3 ~ 1, TRUE ~ 0),
  lag4 = case_when(time_til == 4 ~ 1, TRUE ~ 0),
  lag5 = case_when(time_til == 5 ~ 1, TRUE ~ 0),
  lag6 = case_when(time_til == 6 ~ 1, TRUE ~ 0),
  lag7 = case_when(time_til == 7 ~ 1, TRUE ~ 0),
  lag8 = case_when(time_til == 8 ~ 1, TRUE ~ 0),
  lag9 = case_when(time_til == 9 ~ 1, TRUE ~ 0),
  lag10 = case_when(time_til == 10 ~ 1, TRUE ~ 0)
)


graph_regression <- plm(Fläche_mean ~ lead1 + lead2 + lead3 + lead4 + lead5 + lead6 + lead7 + lead8 + lead9 + lag1 + 
                          lag2 + lag3 + lag4 + lag5 + lag6 + lag7 + lag8 + lag9 + lag10, data = means_long)
summary(graph_regression)

# order of the coefficients for the plot
plot_order <- c("lead9", "lead8", "lead7", 
                "lead6", "lead5", "lead4", "lead3", 
                "lead2", "lead1", "lag1", 
                "lag2", "lag3", "lag4", "lag5")

# grab the clustered standard errors
# and average coefficient estimates
# from the regression, label them accordingly
# add a zero'th lag for plotting purposes
leadslags_plot <- tibble(
  sd = c(graph_regression$cse[plot_order], 0),
  mean = c(coef(graph_regression)[plot_order], 0),
  label = c(-9,-8,-7,-6, -5, -4, -3, -2, -1, 1,2,3,4,5, 0)
)







# This version has a point-range at each
# estimated lead or lag
# comes down to stylistic preference at the
# end of the day!
leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd)) +
  geom_hline(yintercept = 0.035169444, color = "red") +
  geom_pointrange() +
  theme_minimal() +
  xlab("Years before and after Treatment") +
  ylab("Deforestation Rate") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 0,
             linetype = "dashed")

tikz('report/DiDGraphik.tex',width=3.5,height=3.5)
leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd)) +
  # this creates a red horizontal line
  geom_hline(yintercept = 17.44, color = "red") +
  geom_line() + 
  geom_point() +
  geom_ribbon(alpha = 0.2) +
  theme_minimal() +
  # Important to have informative axes labels!
  xlab("Years before and after Treatment") +
  ylab("Deforestation Rate") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
dev.off()

leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd)) +
  # this creates a red horizontal line
  geom_hline(yintercept = 17, color = "red") +
  geom_line() + 
  geom_point() +
  geom_ribbon(alpha = 0.2) +
  theme_minimal() +
  # Important to have informative axes labels!
  xlab("Years before and after castle doctrine expansion") +
  ylab("log(Homicide Rate)") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)


########### ########### ########### ########### ########### ########### ###########



ggplot(means, aes(Gruppe2018Jahr))+
  geom_line(aes(y = Fläche2018, colour = "Fläche 2018"))+
  geom_line(aes(y = FlächeC, colour = "Control"))


ggplot(means, aes(Gruppe2010Jahr))+
  geom_line(aes(y = Fläche2010, colour = "Fläche 2010"))+
  geom_line(aes(y = Fläche2011, colour = "Fläche 2011"))+
  geom_line(aes(y = Fläche2012, colour = "Fläche 2012"))+
  geom_line(aes(y = Fläche2013, colour = "Fläche 2013"))+
  geom_line(aes(y = Fläche2014, colour = "Fläche 2014"))+
  geom_line(aes(y = Fläche2015, colour = "Fläche 2015"))+
  geom_line(aes(y = Fläche2016, colour = "Fläche 2016"))+
  geom_line(aes(y = Fläche2017, colour = "Fläche 2017"))+
  geom_line(aes(y = Fläche2018, colour = "Fläche 2018"))



#### GGPLOT
ggplot(means, aes(Jahr)) + 
  geom_line(aes(y = FlächeT, colour = "Treatment")) + 
  geom_line(aes(y = FlächeC, colour = "Control"))+
  geom_line(aes(y=Fläche2010, colour = "Group2010"))+
  geom_line(aes(y=Fläche2011, colour = "Group2011"))+
  geom_line(aes(y=Fläche2012, colour = "Group2012"))+
  geom_line(aes(y=Fläche2013, colour = "Group2013"))+
  geom_line(aes(y=Fläche2014, colour = "Group2014"))+
  geom_line(aes(y=Fläche2015, colour = "Group2015"))+
  geom_line(aes(y=Fläche2016, colour = "Group2016"))+
  geom_line(aes(y=Fläche2017, colour = "Group2017"))+
  geom_line(aes(y=Fläche2018, colour = "Group2018"))

ggplot(means, aes(Jahr)) + 
  geom_line(aes(y = FlächeT, colour = "Treatment")) + 
  geom_line(aes(y = FlächeC, colour = "Control"))+
  geom_line(aes(y=Fläche2010, colour = "Group2010"))+
  geom_line(aes(y=Fläche2011, colour = "Group2011"))+
  geom_line(aes(y=Fläche2012, colour = "Group2012"))+
  geom_line(aes(y=Fläche2013, colour = "Group2013"))


ggplot(means, aes(Jahr)) + 
  geom_line(aes(y = FlächeT, colour = "Treatment")) + 
  geom_line(aes(y = FlächeC, colour = "Control"))+
  geom_line(aes(y=Fläche2014, colour = "Group2014"))+
  geom_line(aes(y=Fläche2015, colour = "Group2015"))+
  geom_line(aes(y=Fläche2016, colour = "Group2016"))+
  geom_line(aes(y=Fläche2017, colour = "Group2017"))+
  geom_line(aes(y=Fläche2018, colour = "Group2018"))


ggplot(means, aes(Jahr)) + 
  geom_line(aes(y = FlächeT, colour = "Treatment")) + 
  geom_line(aes(y = FlächeC, colour = "Control"))



tikz('report/Points.tex',width=3.5,height=3.5)
ggplot() + geom_point(data = abholzungen_did, mapping = aes(x = Jahr, y = Fläche))
dev.off()



tikz('report/GroupsGGplot.tex',width=3.5,height=3.5)
ggplot(means, aes(Jahr)) + 
  geom_line(aes(y = FlächeT, colour = "Treatment")) + 
  geom_line(aes(y = FlächeC, colour = "Control"))+
  geom_line(aes(y=Fläche2014, colour = "Group2014"))+
  geom_line(aes(y=Fläche2015, colour = "Group2015"))+
  geom_line(aes(y=Fläche2016, colour = "Group2016"))+
  geom_line(aes(y=Fläche2017, colour = "Group2017"))+
  geom_line(aes(y=Fläche2018, colour = "Group2018"))
dev.off()







ggplot() + geom_point(data = abholzungen_did, mapping = aes(x = Jahr, y = Fläche))
ggplot() + geom_point(data = abholzungen_did, mapping = aes(x = Jahr, y = Anteil_abholzung))
ggplot() + geom_point(data = abholzungen_did, mapping = aes(x = Jahr, y = Cashflow))
ggplot(abholzungen, aes(Jahr, Fläche, color = Bundesstaat, shape = Bundesstaat)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_color_grey() +
  scale_shape_manual(values = 1:9) +
  theme_light()


ggplot(abholzungen, aes(x = Jahr, y = Fläche)) +
  geom_bar(data=subset(abholzungen,Gemeinde=="ALTAMIRA"),stat="identity")+
  scale_color_grey() +
  theme_light() +
    ggtitle("Abholzungen in Altamira")






ggplot() + geom_point(data = abholzungen_did, mapping = aes(x = Cashflow, y = diff2))
ggplot() + geom_point(data = abholzungen_did, mapping = aes(x = Cashflow, y = Anteil_abholzung))
ggplot() + geom_point(data = abholzungen_did, mapping = aes(x = Fläche_Indianer, y = diff2))
ggplot() + geom_point(data = abholzungen_did, mapping = aes(x = Fläche_Indianer, y = Anteil_abholzung))



ggplot(as.data.frame(abholzungen_did), aes(Cashflow, Fläche, color = Gemeinde, shape = Gemeinde)) +
  geom_point()+
  geom_smooth(se = TRUE)+
  scale_color_grey()+
  scale_shape_manual(values=1:640)+
  theme_light()



### Descriptive Statistics
tapply(abholzungen_did$Fläche, abholzungen_did$Jahr, mean)
density <- density(abholzungen_did$Fläche)
plot(density, xlim = c(0,100))


boxplot(Fläche~Jahr,data=abholzungen_did, main="Municipal Deforestation Data",
        xlab="Year", ylab="Area")


treatedStatistics <- abholzungen_did[!is.na(abholzungen_did$Unit.Indicator), ]
controlStatistics <- abholzungen_did


treatedStatistics <- treatedStatistics[!is.na(treatedStatistics$Unit.Indicator),]
treatedStatistics$Banana <- NULL
treatedStatistics$Beef <- NULL
treatedStatistics$Hard_Logs<-NULL
treatedStatistics$Rice<-NULL
treatedStatistics$Coffee<-NULL
treatedStatistics$Rubber<-NULL
treatedStatistics$Soybeans<-NULL
treatedStatistics$Sugar<-NULL
treatedStatistics$Erste_Homologation <- NULL
treatedStatistics$Durchschnittliche_Homologation<-NULL
treatedStatistics$Anteil_Amazonas<-NULL
treatedStatistics$Anteil_prozent<-NULL
treatedStatistics$Reservations <- NULL
treatedStatistics$agriculture<-NULL
treatedStatistics$Code_Gemeinde<-NULL
treatedStatistics$JahrIndicator<-NULL
treatedStatistics$Treatments<-NULL
treatedStatistics$Unit.Indicator<-NULL
treatedStatistics$Treated<-NULL
treatedStatistics$Gemeinde_id<-NULL
treatedStatistics$diff2 <- NULL
treatedStatistics$Corn <- NULL
treatedStatistics$Prozent <- NULL
treatedStatistics$Gemeinde <- NULL
treatedStatistics$Jahr <- NULL
treatedStatistics$Anteil_abholzung <- NULL
treatedStatistics$Cashflow <- NULL
summary(treatedStatistics)

controlStatistics$Banana <- NULL
controlStatistics$Beef <- NULL
controlStatistics$Corn <- NULL
controlStatistics$Hard_Logs <- NULL
controlStatistics$Rice <- NULL
controlStatistics$Coffee <- NULL
controlStatistics$Rubber <- NULL
controlStatistics$Soybeans <- NULL
controlStatistics$Sugar <- NULL
controlStatistics$Reservations <- NULL
controlStatistics$agriculture <- NULL
controlStatistics$diff2 <- NULL
controlStatistics$Gemeinde_id <- NULL
controlStatistics$Code_Gemeinde <- NULL
controlStatistics$Anteil_abholzung <- NULL
controlStatistics$Gemeinde <- NULL
controlStatistics$Jahr <- NULL
controlStatistics$Cashflow <- NULL
controlStatistics$Treatments <- NULL
controlStatistics$Treated<- NULL
controlStatistics$Unit.Indicator <- NULL
controlStatistics$JahrIndicator <- NULL
controlStatistics$Anteil_Amazonas <- NULL
controlStatistics$Prozent <- NULL
controlStatistics$Durchschnittliche_Homologation <- NULL
controlStatistics$Erste_Homologation <- NULL
summary(controlStatistics)

hist(treatedStatistics$Gesamtfläche, main = "Size of treated Municipalities", xlab = "Area in square kilometers",xlim = c(0,150000), breaks = 50)
hist(abholzungen_did$Gesamtfläche, main = "Size of all Municipalities", xlab = "Area in square kilometers",xlim = c(0,150000), breaks = 50)
hist(treatedStatistics$Fläche, main = "Cleared areas in treated Municipalities", xlab = "Area in square kilometers", xlim = c(0,150), breaks = 75)
hist(abholzungen_did$Fläche,main = "Cleared areas in all Municipalities", xlab = "Area in square kilometers", xlim = c(0,150), breaks = 75)
hist(abholzungen_did$Gesamtfläche, main = "Size of all Municipalities", xlab = "Area in square kilometers",xlim = c(0,100000), breaks = 50)

mean(treatedStatistics$Gesamtfläche)
mean(abholzungen_did$Gesamtfläche, na.rm = TRUE)
print(xtable(summary(treatedStatistics), caption = "Treated municipalities"))
print(xtable(summary(controlStatistics), caption = "Entire sample"))

## Transformations 
abholzungen_did$lags <- lag(abholzungen_did$Fläche)
abholzungen_did$diff <- diff(abholzungen_did$Fläche)
abholzungen_did$betw <- Between(abholzungen_did$Fläche)
abholzungen_did$with <- Within(abholzungen_did$Fläche)
ggplot() + geom_point(data = abholzungen_did, mapping = aes(x = Cashflow, y = diff))


## First Difference Estimators
### Manually calculating first differences 

abholzungen_did$did <- abholzungen_did$Jahr*abholzungen_did$Treated

abholzungen_did$Jahr <- as.numeric(as.character(abholzungen_did$Jahr))
abholzungen_did$dJahr <- diff(abholzungen_did$Jahr)
abholzungen_did$dCashflow <- diff(abholzungen_did$Cashflow)
abholzungen_did$Cashflowlags <- lag(abholzungen_did$Cashflow)


abholzungen_did$Cashflow = gsub(",", ".", abholzungen_did$Cashflow)
abholzungen_did$Cashflow <- type.convert(abholzungen_did$Cashflow, numerals = "warn.loss", as.is = TRUE)

typeof(abholzungen_did$Cashflow)
class(abholzungen_did$Cashflow)
mode(abholzungen_did$Cashflow)

abholzungen_did$dCashflow <- diff(abholzungen_did$Cashflow)

abholzungen_did$Treated[is.na(abholzungen_did$Treated)] <- 0


## FD Estimate with plm on non-differenced and differenced data 
summary(plm(Fläche ~ Cashflowlags + factor(Jahr) + Cashflowlags*factor(Jahr), data = abholzungen_did, model = "fd"))
summary(plm(diff ~ dCashflow, data = abholzungen_did, model = "fd"))



####################################################################################################################
abholzungen_did2 <- abholzungen_did
abholzungen_did2$dJahr <- NULL
abholzungen_did2$Prozent <- NULL
abholzungen_did2$Fläche <- NULL
abholzungen_did2$diff <- NULL
abholzungen_did2$lags <- NULL
abholzungen_did2$betw <- NULL
abholzungen_did2$with <- NULL
abholzungen_did2$with <- NULL
abholzungen_did2$Verwendete_Fläche <- NULL
abholzungen_did2$Gesamtfläche <- NULL
abholzungen_did2$Erste_Homologation <- NULL
abholzungen_did2$Durchschnittliche_Homologation <- NULL
abholzungen_did2$Fläche_Indianer <- NULL
abholzungen_did2$Code_Gemeinde <- NULL
abholzungen_did2$Anteil_Amazonas <- NULL
abholzungen_did2$Cashflow <- NULL
abholzungen_did2$Anteil_abholzung <- NULL

abholzungen_did2$dCashflow <- abholzungen_did2$dCashflow / 100



form <- diff2 ~ dCashflow + Jahr + dCashflow*Jahr
wi <- plm(form, data = abholzungen_did2, model = "within") 
re <- plm(form, data = abholzungen_did2, model = "random") 
phtest(form, data = abholzungen_did2, method = "aux", vcov = vcovHC) 
                #### p value is significant -> use Fixed effects



####################################################################################################################




### Estimate the first fixed effects model
wi <- plm(diff ~ dCashflow + Verwendete_Fläche + Jahr, data = abholzungen_did, model = "within") 
          ### First model with significant coefficient 
    
re <- plm(diff ~ dCashflow + dCashflow*Jahr + Jahr, data = abholzungen_did, model = "random") 
try(summary(re))  # regressors are collinear; newer plm versions fail on the Wald test here



######## Eliminating Colinearity
abholzungen_did$Durchschnittliche_Homologation <- NULL
abholzungen_did$Prozent <- NULL
abholzungen_did$Fläche_Indianer <- NULL
abholzungen_did$Gesamtfläche <- NULL
abholzungen_did$Erste_Homologation <- NULL
abholzungen_did$Anteil_Amazonas <- NULL
abholzungen_did$Verwendete_Fläche <- NULL
abholzungen_did$Treatments <- NULL



## Estimating a normal ols 
lm<-lm(Fläche ~ factor(Jahr) + dCashflow + dCashflow*factor(Jahr) + Reservations + agriculture, data = abholzungen_did)
summary(lm)
      ### Treatment coefficient has larger magnitude and highly significant
        ### Interestingly the interaction term is positive and significant

## Estimating Fixed effects model 
FE <- plm(with ~ Cashflowlags + factor(Jahr) + Cashflowlags*factor(Jahr) +Reservations + agriculture + factor(Gemeinde)*Cashflowlags, data = abholzungen_did, model = "within")
summary(FE)
# Standard errors clustered by municipality
coeftest(FE, vcov = vcovHC(FE, type = "HC0", cluster = "group"))
      ### Using lags instead of difference leads to much higher significance
      ### Using the with variable leads to higher R squared
      ### normalized measure as a dependent variable leads to double the number of observations (?)


## Estimating Difference in Difference Model
didreg = lm(Fläche ~ Treated + Jahr + did + Reservations + agriculture, data = abholzungen_did)
summary(didreg)
clusterdidreg <- coeftest(didreg, vcov = vcovCL(didreg, cluster = ~Gemeinde, type = "HC0"))

## Estimating First Difference Model
FD <- plm(Fläche ~ factor(Jahr) + Cashflow + Cashflow*factor(Jahr) + Reservations + agriculture + factor(Gemeinde)*Cashflow, data = abholzungen_did, model = "fd")
summary(FD)

### Estimating DiD with other dependent variable 
didreg2 = lm(Anteil_abholzung ~ Treated + Jahr + did + Reservations + agriculture, data = abholzungen_did)
summary(didreg2)
clusterdidreg2 <- coeftest(didreg2, vcov = vcovCL(didreg2, cluster = ~Gemeinde, type = "HC0"))

# Reservations and agriculture are time-invariant, so the within model cannot identify them
# (older plm versions dropped them silently; newer ones fail)
didreg3 = plm(diff ~ Treated + Jahr + did, data = abholzungen_did)
summary(didreg3)
clusterdidreg3 <- coeftest(didreg3, vcov = vcovHC(didreg3, type = "HC0", cluster = "group"))


didreg4 = plm(diff2 ~ Treated + Jahr + did, data = abholzungen_did)
summary(didreg4)
clusterdidreg4 <- coeftest(didreg4, vcov = vcovHC(didreg4, type = "HC0", cluster = "group"))



FE1 <- plm(diff ~ Cashflow + factor(Jahr) + Cashflow*factor(Jahr) +Reservations + agriculture + factor(Gemeinde)*Cashflow, data = abholzungen_did, model = "within")
summary(FE1)

FE2 <- plm(Fläche ~ Cashflowlags + factor(Jahr) + Cashflowlags*factor(Jahr) +Reservations + agriculture + factor(Gemeinde)*Cashflowlags, data = abholzungen_did, model = "within")
summary(FE2)

FE3 <- plm(diff2 ~ Cashflowlags + factor(Jahr) + Cashflowlags*factor(Jahr) +Reservations + agriculture + factor(Gemeinde)*Cashflowlags, data = abholzungen_did, model = "within")
summary(FE3)

FE4 <- plm(with ~ Cashflowlags + factor(Jahr) + Cashflowlags*factor(Jahr) +Reservations + agriculture, data = abholzungen_did, model = "within")
summary(FE4)


#### Exporting tables to latex
screenreg(didreg)
texreg(didreg, dcolumn = TRUE, booktabs = TRUE,use.packages = FALSE, label = "tab:3", caption = "DiD model",float.pos = "h")

screenreg(didreg2)
texreg(didreg2, dcolumn = TRUE, booktabs = TRUE, use.packages = FALSE, label = "tab:4", caption = "DiD model 2", float.pos = "h" )

screenreg(didreg3)
texreg(didreg3, dcolumn = TRUE, booktabs = TRUE,use.packages = FALSE, label = "tab:3", caption = "DiD model",float.pos = "h")

screenreg(didreg4)
texreg(didreg4, dcolumn = TRUE, booktabs = TRUE,use.packages = FALSE, label = "tab:3", caption = "DiD model",float.pos = "h")

screenreg(FE4)
texreg(FE4, dcolumn = TRUE, booktabs = TRUE,use.packages = FALSE, label = "tab:3", caption = "DiD model",float.pos = "h")

screenreg(clusterdidreg)
texreg(clusterdidreg, dcolumn = TRUE, booktabs = TRUE,use.packages = FALSE, label = "tab:3", caption = "DiD model",float.pos = "h")

screenreg(clusterdidreg2)
texreg(clusterdidreg2, dcolumn = TRUE, booktabs = TRUE,use.packages = FALSE, label = "tab:3", caption = "DiD model",float.pos = "h")

screenreg(clusterdidreg3)
texreg(clusterdidreg3, dcolumn = TRUE, booktabs = TRUE,use.packages = FALSE, label = "tab:3", caption = "DiD model",float.pos = "h")

screenreg(clusterdidreg4)
texreg(clusterdidreg4, dcolumn = TRUE, booktabs = TRUE,use.packages = FALSE, label = "tab:3", caption = "DiD model",float.pos = "h")
