
require(tidyverse)

options(stringsAsFactors = F)

data = read_csv('_sessions/PlottingII/1_Data/crime_full.csv',col_names = F)
vars = readLines('_sessions/PlottingII/1_Data/vars.txt')

sel_crit = (ncol(data) - 17):ncol(data)
sel_meta = 1:5
sel_pred = (max(sel_meta) + 1) : (min(sel_crit) - 1)

vars = str_sub(vars, 4, nchar(vars))
vars = str_split(vars, ':')
vars_short = sapply(vars, `[[`, 1)
vars_long = sapply(vars, `[[`, 2)

names(data) = vars_short

data[data == '?'] = NA

data = data[,apply(data,2,function(x) mean(!is.na(x))) > .95]
data = data[apply(data,1,function(x) all(!is.na(x))),]

data = data %>% filter(state %in% c('PA','OH','MI','MA','TX','NJ','CA'))

paste0("'",names(crime),"'",',',collapse='')

var = c('communityname','state','population','householdsize',
        'racepctblack','racePctWhite','racePctAsian','racePctHisp',
        'agePct12t21','agePct12t29','agePct16t24','agePct65up','numbUrban',
        'pctUrban','medIncome','pctWWage','pctWFarmSelf','pctWInvInc',
        'pctWSocSec','pctWPubAsst','pctWRetire','medFamInc','perCapInc',
        'whitePerCap','blackPerCap','indianPerCap','AsianPerCap','OtherPerCap',
        'HispPerCap','NumUnderPov','PctPopUnderPov','PctLess9thGrade',
        'PctNotHSGrad','PctBSorMore','PctUnemployed','PctEmploy','PctEmplManu',
        'PctEmplProfServ','PctOccupManu','PctOccupMgmtProf','MalePctDivorce',
        'MalePctNevMarr','FemalePctDiv','TotalPctDiv','PersPerFam','PctFam2Par',
        'PctKids2Par','PctYoungKids2Par','PctTeen2Par','PctWorkMomYoungKids',
        'PctWorkMom','NumKidsBornNeverMar','PctKidsBornNeverMar','NumImmig',
        'PctImmigRecent','PctImmigRec5','PctImmigRec8','PctImmigRec10',
        'PctRecentImmig','PctRecImmig5','PctRecImmig8','PctRecImmig10',
        'PctSpeakEnglOnly','PctNotSpeakEnglWell','PctLargHouseFam',
        'PctLargHouseOccup','PersPerOccupHous','PersPerOwnOccHous',
        'PersPerRentOccHous','PctPersOwnOccup','PctPersDenseHous',
        'PctHousLess3BR','MedNumBR','HousVacant','PctHousOccup','PctHousOwnOcc',
        'PctVacantBoarded','PctVacMore6Mos','MedYrHousBuilt','PctHousNoPhone',
        'PctWOFullPlumb','OwnOccLowQuart','OwnOccMedVal','OwnOccHiQuart',
        'OwnOccQrange','RentLowQ','RentMedian','RentHighQ','RentQrange',
        'MedRent','MedRentPctHousInc','MedOwnCostPctInc','MedOwnCostPctIncNoMtg',
        'NumInShelters','NumStreet','PctForeignBorn','PctBornSameState',
        'PctSameHouse85','PctSameCity85','PctSameState85','LandArea','PopDens',
        'PctUsePubTrans','LemasPctOfficDrugUn','murders','robberies','assaults',
        'burglaries','larcenies','autoTheft','arsons')

var = c('communityname','state','population','householdsize',
        'pctUrban','medIncome','pctWSocSec','pctWRetire',
        'whitePerCap','blackPerCap','AsianPerCap',
        'HispPerCap','PctPopUnderPov','PctNotHSGrad','PctUnemployed','TotalPctDiv',
        'PersPerFam','PctWorkMom','NumImmig',
        'PctImmigRecent','PctNotSpeakEnglWell','RentMedian',
        'NumInShelters','NumStreet','PctForeignBorn','PctBornSameState',
        'LandArea','PopDens','PctUsePubTrans','murders','robberies','assaults',
        'burglaries','larcenies','autoTheft','arsons')

data = data %>% select(var)

sel = str_detect(names(data),'PerPop')

write_csv(data[,!sel], '_sessions/PlottingII/1_Data/crime.csv')


crime = read_csv('_sessions/PlottingII/1_Data/crime.csv')

# vector of crime variables
crime_vars = c("murders","robberies","assaults","burglaries","larcenies","autoTheft","arsons")

# transform to long
crime_long <- crime %>% 
  gather(crime_var, frequency, crime_vars)

ggplot(data = crime_long,
       mapping = aes(x = PctUsePubTrans, y = frequency)) + 
  geom_point() + 
  scale_y_log10() + 
  facet_wrap(~ crime_var) +
  theme(
    panel.background = element_rect(fill='white'),
    panel.grid.major = element_line(color = 'grey75',
                                    size = .25),
    panel.grid.minor = element_line(color = 'grey75',
                                    size = .1),
    strip.background = element_rect(fill='white'),
    strip.text = element_text(face='italic', size=12, hjust=1),
    axis.title.y = element_text(size=12,margin=margin(r = 10)),
    axis.title.x = element_text(size=12,margin=margin(t = 10)),
    panel.spacing = unit(1.1, units = "lines")
    
  ) + 
  labs(x = '% public transportation', y = 'Crime frequency')


crime_theme <- theme(
  panel.background = element_rect(fill = 'white'),
  panel.grid.major = element_line(color = 'grey75', size = .25),
  panel.grid.minor = element_line(color = 'grey75', size = .1),
  strip.background = element_rect(fill = 'white'),
  strip.text = element_text(face = 'italic', size = 12, hjust = 1),
  axis.title.x = element_text(size = 12, margin = margin(t = 10)),
  axis.title.y = element_text(size = 12, margin = margin(r = 10)),
  panel.spacing = unit(1.1, units = "lines")
)

ggplot(data = crime_long,
       mapping = aes(x = PctUsePubTrans, y = frequency,
                     color = state, size = population, alpha=.1)) + 
  geom_point() + 
  scale_y_log10() + 
  facet_wrap(~ crime_var) + 
  crime_theme


require(ggmap)
register_google('AIzaSyD3Qjgf-skqll06EBy3iZg2BvMzJMKP474')

world = as.numeric(geocode("World"))

USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=4), extent="normal")


locations <- c("Hoensbroek", "Johannesburg", "Barrow-in-Furness",
               "Hong Kong", "Singapore", "Tangail", "Maastricht", "Bendigo") %>%
  geocode()

world <- map_data("world")
ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "grey") +
  geom_point(data = locations, aes(lon, lat), colour = "red", size = 5) + 
  coord_map("ortho", orientation = c(30, 80, 0)) +
  theme_void()

