

require(readxl)
a = read_excel('_sessions/PlottingIII/prepare_data/t10-1-01.xlsx', sheet = 'Okt')
a = a[-c(1:8),c(1,3,4,11,12)]
a = a[-(77:81),]
a$continent = rep(c('Europa','Amerika','Afrika','Asien','Australien, Neuseeland, Ozeanien'),c(38,9,5,21,3))
a = a[-which(a[[1]] %in% c('Europa3','Amerika','Afrika','Asien','Australien, Neuseeland, Ozeanien'))
,]
a = a[,c(1,6,2,4,3,5)]
names(a) = c('Land','Region','Besucher_2018','Dauer_2018','Besucher_2019','Dauer_2019')

tourism = a %>% select(Land, Region, Besucher_2018, Besucher_2019) %>%
  rename("2018" = Besucher_2018,
         "2019" = Besucher_2019) %>%
  gather(Jahr, Besucher, -Land, -Region) %>%
  left_join(
    a %>% select(Land, Region, Dauer_2018, Dauer_2019) %>%
      rename("2018" = Dauer_2018,
             "2019" = Dauer_2019) %>%
      gather(Jahr, Dauer, -Land, -Region)
  )
  

# tourism %>% pivot_wider(names_from = Jahr, 
#                         values_from = c(Besucher, Dauer))

write_csv(tourism,'1_Data/Tourismus.csv')
write_csv(tourism,'_sessions/PlottingIII/1_Data/Tourismus.csv')


a = read_excel('_sessions/PlottingIII/prepare_data/je-d-21.03.03.xlsx')

tbl = as_tibble(t(a[c(1,3,4,5,6,21,30,43,61),-c(2:5)]))
names(tbl) = unlist(tbl[1,])
names(tbl)[1] = 'Code'
tbl = tbl[-1,]
names(tbl) = c('Code','Bevölkerung','Dichte','lo20','hi65','Erwerbsquote','BIP','Motorisierung','Äquivalenzeinkommen')

codes = xml2::read_html('https://de.wikipedia.org/wiki/ISO-3166-1-Kodierliste') %>%
  rvest::html_node(xpath='//*[@id="mw-content-text"]/div/table') %>%
  rvest::html_table(fill=T)
codes = codes[,c(1,3)]
names(codes) = c('Land','Code')

cnts = tbl %>% left_join(codes) %>% select(Land, everything()) %>% select(-Code)
cnts$Land[cnts$Land == 'Osterreich!Österreich'] = 'Österreich'
cnts$Land[cnts$Land == 'Danemark!Dänemark'] = 'Dänemark'
cnts$Land[cnts$Land == 'Turkei!Türkei'] = 'Türkei'
cnts$Land[cnts$Land == 'Russische Föderation'] = 'Russland'
cnts$Land[cnts$Land == 'Tschechien'] = 'Tschechische Republik'
cnts$Land[cnts$Land == 'Vereinigtes Königreich Großbritannien und Nordirland'] = 'Vereinigtes Königreich'

sum(cnts$Land %in% tourism$Land)
cnts$Land[!cnts$Land%in%tourism$Land]

cnts[cnts == '...'] = NA 
cnts = cnts %>% readr::type_convert()
imp <- mice(cnts[,-1])
cnts[,-1] = complete(imp)

write_csv(cnts,'1_Data/Europa.csv')

require(xlm2);require(rvest)

pop = read_html('https://de.wikipedia.org/wiki/Liste_von_Staaten_und_Territorien_nach_Bev%C3%B6lkerungsentwicklung') %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>% html_table(fill=TRUE)

bip = read_html('https://de.wikipedia.org/wiki/Liste_der_L%C3%A4nder_nach_Bruttoinlandsprodukt_pro_Kopf') %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table/tbody/tr/td[2]/table') %>% html_table(fill=TRUE)

dens = read_html('https://de.wikipedia.org/wiki/Liste_der_Staaten_der_Erde') %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>% html_table(fill=TRUE) 

quote = quote[-1,c(2,3)]
bip = bip[-1,c(2,3)]
dens = dens[,c(1,4,6)]

quote[[1]] = sapply(str_split(quote[[1]], '[:blank:]'),function(x) paste0(x[!duplicated(x)],collapse=' '))
bip[[1]] = sapply(str_split(bip[[1]], '[:blank:]'),function(x) paste0(x[!duplicated(x)],collapse=' '))
dens[[1]] = str_replace_all(dens[[1]],'[:punct:]|[:digit:]','')
mean(bip[[1]]%in%dens[[1]])

names(quote) = c('Land', 'Beschäftigungsquote')
names(bip) = c('Land', 'BIP')
names(dens) = c('Land', 'Bevölkerung','Dichte')

dens$Land = str_replace_all(dens$Land,'ohne[:print:]+','')
dens$Land = str_replace_all(dens$Land,'mit[:print:]+','')
dens$Dichte = str_replace_all(dens$Dichte,'Kern[:print:]+','')
dens$Dichte = str_replace_all(dens$Dichte,'ges[:print:]+','')
dens$Dichte = str_replace_all(dens$Dichte,'[:punct:]|[:blank:]','')
dens$Bevölkerung = str_replace_all(dens$Bevölkerung,'Kern[:print:]+','')
dens$Bevölkerung = str_replace_all(dens$Bevölkerung,'ges[:print:]+','')
dens$Bevölkerung = str_replace_all(dens$Bevölkerung,'[:punct:]','')

dens$Land[!dens$Land %in% bip$Land]

bip$Land[!bip$Land%in%dens$Land]
dens$Land[str_detect(dens$Land,'Zyp')]

bip$Land[bip$Land == 'Luxemburg Luxemburg[3]'] = 'Luxemburg'
bip$Land[bip$Land == 'Saudi-Arabien'] = 'SaudiArabien'
bip$Land[bip$Land == 'Turkei Türkei'] = 'Türkei'
bip$Land[bip$Land == 'Osterreich Österreich'] = 'Österreich'
bip$Land[bip$Land == 'Irland Irland[4]'] = 'Irland'
bip$Land[bip$Land == 'Frankreich Frankreichb'] = 'Frankreich'
bip$Land[bip$Land == 'Vereinigtes Konigreich Königreich'] = 'Vereinigtes Königreich'
bip$Land[bip$Land == 'Danemark Dänemark'] = 'Dänemark'
bip$Land[bip$Land == 'Weissrussland Weißrussland'] = 'Weißrussland'
bip$Land[bip$Land == 'China Volksrepublik Chinaa'] = 'Volksrepublik China'
bip$Land[bip$Land == 'Zypern Republik'] = 'Zypern'
bip$Land[bip$Land == 'Agypten Ägypten'] = 'Ägypten'


d = dens %>% inner_join(bip)
d = d[-1,]

 
d$Land[d$Land == 'Tschechien'] = 'Tschechische Republik'
d$Land[d$Land == 'Weißrussland'] = 'Weissrussland'
d$Land[d$Land == 'SaudiArabien'] = 'Saudi Arabien'
d$Land[d$Land == 'Korea Süd'] = 'Korea, Republik'
d$Land[d$Land == 'Neuseeland'] = 'Neuseeland und Ozeanien'
d$Land[d$Land == 'Südafrika'] = 'Südafrika, Republik'
d$Land[d$Land == 'Volksrepublik China'] = 'China, Volksrepublik'

tourism$Land[!(tourism$Land %in% d$Land)]
d$Land[!(d$Land %in% tourism$Land)][str_detect(d$Land[!(d$Land %in% tourism$Land)],'Kore')]

d= d%>%filter(Land %in% tourism$Land) %>% as_tibble() %>% readr::type_convert()

imp = mice(d[,-1])
d[,-1] = complete(imp)

write_csv(d,'1_Data/Länder.csv')
write_csv(d,'_sessions/PlottingIII/1_Data/Länder.csv')
