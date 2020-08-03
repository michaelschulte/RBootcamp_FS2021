require(tidyverse)
require(readxl)



kantone = xml2::read_html('https://de.wikipedia.org/wiki/Kanton_(Schweiz)') %>%
  rvest::html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  rvest::html_table()

k = str_replace(kantone$Kanton, 'Kanton ', '')
kantone$Kanton = str_sub(k, 1, (nchar(k) - 1) / 2)
names(kantone)[6] = 'Einwohner'



sheets = expand.grid(c('FS','HS'),
                     c('08', '09', '10', '11', '12','13', '14','15', '16', '17', '18', '19'))
sheets = paste0(sheets[[1]], sheets[[2]])
sheets = c('WS04','SS05','WS05','SS06','WS06','SS07','HS07', sheets)


res = list()
for(i in 1:length(sheets)){


  a = read_excel("1_Data/immatrikulationen.xlsx", skip = 5, sheet = sheets[i]) %>%
    slice(-1) %>% select(-2) %>% slice(1:8) %>%
    rename(Fakultät = "Schweizer Studierende") %>%
    mutate(Fakultät = str_replace_all(Fakultät, '[:digit:]', ''),
           Jahr = 2000 + as.numeric(str_extract(sheets[i], '[:digit:]+')),
           Semester = str_extract(sheets[i], '[:alpha:]+')) %>%
    mutate(Semester = case_when(
      Semester == 'SS' ~ 'FS',
      Semester == 'WS' ~ 'HS',
      TRUE ~ Semester
    )) %>% 
    pivot_longer(names_to = 'Abk.', 
                 values_to = 'Immatrikulationen',
                 cols = c(-Fakultät, -Jahr, -Semester)) %>%
    mutate(Immatrikulationen = as.numeric(Immatrikulationen)) %>%
    filter(!Abk. %in% c('Schweiz', 'Ausland','Total2')) 
  
  a = a %>%
    bind_rows(a %>% filter(Abk. == "OW/NW") %>% 
                mutate(Immatrikulationen = Immatrikulationen / 2,
                       Abk. = 'NW')) %>%
    bind_rows(a %>% filter(Abk. == "AI/AR") %>% 
                mutate(Immatrikulationen = Immatrikulationen / 2,
                       Abk. = 'AR')) %>%
    mutate(
      Immatrikulationen = case_when(
        Abk. %in% c("OW/NW","AI/AR") ~ Immatrikulationen / 2,
        TRUE ~ Immatrikulationen ),
      Abk. = case_when(
        Abk. == "OW/NW" ~ "OW",
        Abk. == "AI/AR" ~ "AI", 
        TRUE ~ Abk. ))
    
  
  res[[i]] = a %>%
    left_join(kantone %>% select(Abk., Kanton, Einwohner)) %>%
    mutate(Einwohner = as.numeric(str_replace_all(Einwohner, "'", ''))) %>%
    rename(Code = Abk.) %>%
    select(Fakultät, Code, Kanton, Einwohner, Jahr, Semester, Immatrikulationen)

  }

res = do.call(rbind, res) %>%
  left_join(can.template(2016) %>% select(-values), by = c('Code' = "name")) %>%
  rename(Id = bfs_nr) %>%
  select(Fakultät, Code, Id, Kanton, Einwohner, Jahr, Semester, Immatrikulationen) %>%
  filter(Jahr > 2005) 
  
write_csv(res, '1_Data/Immatrikulationen.csv')
write_csv(res, '_sessions/Intro2Stats/1_Data/Immatrikulationen.csv')


####### WORK WITH IMMATRIKULATION ------------

require(RSwissMaps)

immatrikulation = read_csv('1_Data/immatrikulationen.csv')

imm_canton_2006  = immatrikulation %>% 
  filter(Jahr == 2006) %>% 
  group_by(Id) %>% 
  summarize(Immatrikulationen = sum(Immatrikulationen))
i2006 = can.plot(imm_canton_2006$Id, imm_canton_2006$Immatrikulationen, 2016, 
                 color_continuous = viridis::cividis(2),lakes='none') + labs(title = '2006')

imm_canton_2016  = immatrikulation %>% 
  filter(Jahr == 2016) %>% 
  group_by(Id) %>% 
  summarize(Immatrikulationen = sum(Immatrikulationen))
i2016 = can.plot(imm_canton_2016$Id, imm_canton_2016$Immatrikulationen, 2016, 
                 color_continuous = viridis::cividis(2),lakes='none') + labs(title = '2006')

i2006 / i2016

a = chisq.test(rbind(imm_canton_2006$Immatrikulationen, imm_canton_2016$Immatrikulationen))

round(a$residuals)

imm_canton_2006  = immatrikulation %>% 
  filter(Jahr == 2006) %>% 
  group_by(Id) %>% 
  summarize(Immatrikulationen = sum(Immatrikulationen))

imm_canton_2016  = immatrikulation %>% 
  filter(Jahr == 2016) %>% 
  group_by(Id) %>% 
  summarize(Immatrikulationen = sum(Immatrikulationen))

means = colMeans(rbind(imm_canton_2006$Immatrikulationen, 
      imm_canton_2016$Immatrikulationen))
ps = means / sum(means)
n1 = sum(imm_canton_2006)
n2 = sum(imm_canton_2016)

set.seed(100)
distr = sapply(1:30000, function(x) {
  d = t(rmultinom(2, size = n1, prob = ps))
  e = t(colMeans(d) %o% rowMeans(d)) / mean(d)
  x2 = sum((d - e)**2 / e) 
  x2
  })


pdf('_sessions/Intro2Stats/image/verteiling.pdf',width=8,height=2.3)
par(mar=c(3,1,1,1))
h = hist(distr, plot = F, breaks = 100)
plot.new() ; plot.window(xlim = c(0,70), ylim = c(0, .06))
rect(h$mids - mean(diff(h$mids))*.45, 0, h$mids + mean(diff(h$mids))*.45, h$density, border=NA, lwd=10, col='#EA4B68')
mtext(seq(0,70,10),side=1,at=seq(0,70,10))
mtext(expression(italic(X)^2),side=1,cex=1.5,line=1.5)
dev.off()


d = rbind(imm_canton_2006$Immatrikulationen, 
          imm_canton_2016$Immatrikulationen)
e = t(colMeans(d) %o% rowMeans(d)) / mean(d)
x2 = sum((d - e)**2 / e) 

pdf('_sessions/Intro2Stats/image/verteiling2.pdf',width=8,height=2.3)
par(mar=c(3,1,1,1))
h = hist(distr, plot = F, breaks = 100)
plot.new() ; plot.window(xlim = c(0,70), ylim = c(0, .06))
rect(h$mids - mean(diff(h$mids))*.45, 0, h$mids + mean(diff(h$mids))*.45, h$density, border=NA, lwd=10, col='#EA4B68')
rect(52.25 - mean(diff(h$mids))*.45, 0, 52.25 + mean(diff(h$mids))*.45, h$density[94], border=NA, lwd=10, col='#606060')
lines(c(52.25,52.25),c(h$density[94] + .001, .03),lty=2)
#text(52.25,.035,labels=expression(italic(X)^2),cex=1.5)
mtext(seq(0,70,10),side=1,at=seq(0,70,10))
mtext(expression(italic(X)^2),side=1,cex=1.5,line=1.5)
dev.off()

pdf('_sessions/Intro2Stats/image/verteiling3.pdf',width=8,height=2.3)
par(mar=c(3,1,1,1))
h = hist(distr, plot = F, breaks = 100)
plot.new() ; plot.window(xlim = c(0,70), ylim = c(0, .06))
rect(h$mids - mean(diff(h$mids))*.45, 0, h$mids + mean(diff(h$mids))*.45, h$density, border=NA, lwd=10, col='#EA4B68')
rect(h$mids[93:103] - mean(diff(h$mids[93:103]))*.45, 0, h$mids[93:103] + mean(diff(h$mids[93:103]))*.45, h$density[93:103], border=NA, lwd=10, col='#606060')
lines(c(52.25,52.25),c(h$density[94] + .001, .03),lty=2)
#text(52.25,.035,labels=expression(italic(X)^2),cex=1.5)
mtext(seq(0,70,10),side=1,at=seq(0,70,10))
mtext(expression(italic(X)^2),side=1,cex=1.5,line=1.5)
dev.off()


pdf('_sessions/Intro2Stats/image/verteiling4.pdf',width=8,height=4)
par(mar=c(3,2,1,1))
h = hist(distr, plot = F, breaks = 100)
plot.new() ; plot.window(xlim = c(0,70), ylim = c(0, .16))
sapply(c(25),function(df) {
  x = seq(0,70,.01)
  y = dchisq(seq(0,70,.01),df = df)
  lines(x, y, xlim=c(0,75),col=ifelse(df == 25, '#EA4B68', '#606060'),lwd=ifelse(df == 25,10,5))
  #text(x[which.max(y)],max(y)+.01, paste0("df = ",df),font=2,col=ifelse(df == 25, '#EA4B68', '#606060'))
})
mtext(seq(0,70,10),side=1,at=seq(0,70,10))
lines(c(52.25,52.25),c(h$density[94] + .001, .03),lty=2)
mtext(expression(italic(Chi)^2),side=1,cex=1.5,line=1.5)
mtext('Density',side=2,cex=1.5)
dev.off()

pdf('_sessions/Intro2Stats/image/verteiling5.pdf',width=8,height=4)
par(mar=c(3,2,1,1))
h = hist(distr, plot = F, breaks = 100)
plot.new() ; plot.window(xlim = c(0,70), ylim = c(0, .16))
sapply(c(5,15,35,45,25),function(df) {
  x = seq(0,70,.01)
  y = dchisq(seq(0,70,.01),df = df)
  lines(x, y, xlim=c(0,75),col=ifelse(df == 25, '#EA4B68', '#606060'),lwd=ifelse(df == 25,10,5))
  text(x[which.max(y)],max(y)+.01, paste0("df = ",df),font=2,col=ifelse(df == 25, '#EA4B68', '#606060'))
  })
mtext(seq(0,70,10),side=1,at=seq(0,70,10))
mtext(expression(italic(Chi)^2),side=1,cex=1.5,line=1.5)
mtext('Density',side=2,cex=1.5)
dev.off()


png('_sessions/Intro2Stats/image/normal.png',width=6,height=6,unit='in',res=300)
par(mar=c(3,2,1,1))
h = hist(distr, plot = F, breaks = 100)
plot.new() ; plot.window(xlim = c(-5,5), ylim = c(0, .8))

x = seq(-5,5,.05) ; y = dnorm(x, -2, .5)
lines(x, y, lwd=10,  col ='#606060')
text(x[which.max(y)] + .5,max(y) - .04,labels = expression(paste(mu," = -2")),adj=0,cex=1.5,col='#606060')
text(x[which.max(y)] + .5,max(y) - .08,labels = expression(paste(sigma," = .5")),adj=0,cex=1.5,col='#606060')


x = seq(-5,5,.05) ; y = dnorm(x, 2, 2)
lines(x, y, lwd=10,  col ='#606060')
text(x[which.max(y)],max(y)+.1,labels = expression(paste(mu," = 2")),adj=0,cex=1.5,col='#606060')
text(x[which.max(y)],max(y)+.06,labels = expression(paste(sigma," = 2")),adj=0,cex=1.5,col='#606060')


x = seq(-5,5,.05) ; y = dnorm(x, 0, 1) 
lines(x, y, lwd=10,  col ='#EA4B68')
text(x[which.max(y)],max(y)+.1,labels = expression(paste(mu," = 0")),adj=0,cex=1.5,col='#EA4B68')
text(x[which.max(y)],max(y)+.06,labels = expression(paste(sigma," = 1")),adj=0,cex=1.5,col='#EA4B68')



mtext(seq(-5,5,1),side=1,at=seq(-5,5,1))
mtext(c('x','Dichte'),side=c(1,2),cex=1.5,line=c(1.5,0))
dev.off()


x = seq(0,10,.01)
expo = dexp(x)

x = seq(0,10,.01)
bimod = dnorm(x,2.5,1) + dnorm(x,7.5,1) 

nrep = 10000

ns = 1:20
expos = list()
for(i in 1:length(ns)){
  print(i)
  expos[[i]] = sapply(1:nrep, function(k) mean(sample(x, size = ns[i], prob = expo)))
  }


ns = 1:20
bimods = list()
for(i in 1:length(ns)){
  print(i)
  bimods[[i]] = sapply(1:nrep, function(k) mean(sample(x, size = ns[i], prob = bimod)))
  }



ggplot(tibble(x = x, y = bimod), aes(x = x, y = y)) + 
  geom_line(col = "#EA4B68", size=3) + 
  theme_minimal() +  xlim(0,10) + 
  labs(x = "x", y = 'Dichte') + 
  theme(axis.text = element_text(size=14), axis.text.y = element_blank(),
        axis.title = element_text(size=20)) + 
  ggsave(filename = '_sessions/Intro2Stats/image/clt_bimod.png',
         device = 'png', width=6, height= 3, dpi=300, units = 'in')


ggplot(tibble(x = x, y = expo), aes(x = x, y = y)) + 
  geom_line(col = "#EA4B68", size=3) + 
  theme_minimal() +  xlim(0,10) + 
  labs(x = "x", y = 'Dichte') + 
  theme(axis.text = element_text(size=14), axis.text.y = element_blank(),
        axis.title = element_text(size=20)) + 
  ggsave(filename = '_sessions/Intro2Stats/image/clt_expos.png',
         device = 'png', width=6, height= 3, dpi=300, units = 'in')


require(gganimate)
require(gifski)

bimods_tbl = tibble(val = unlist(bimods), ns = rep(ns, rep(nrep, length(ns))))
bimods_tbl_short = bimods_tbl %>% group_by(ns) %>% filter((1:n()) == 1) %>% ungroup()
p = ggplot(bimods_tbl, aes(x = val)) + 
  geom_histogram(col = "white", fill = "#606060", binwidth = .25) + 
  geom_text(data = bimods_tbl_short, mapping=aes(x = 5,y = 1800, label = paste0('Mittelwert aus ',ns,' Werten')), size = 8) +
  theme_minimal() +  xlim(0,10) + 
  labs(x = expression(bar(x)), y = 'Dichte') + 
  theme(axis.text = element_text(size=14), axis.text.y = element_blank(),
        axis.title = element_text(size=20))
anim = p + transition_states(ns,
                             transition_length = 0,
                             state_length = .5) #& patchwork::plot_annotation()
anim_save('_sessions/Intro2Stats/image/clt_bimods.gif', anim, res = 300, width = 6, height = 3, unit = 'in')

expos_tbl = tibble(val = unlist(expos), ns = rep(ns, rep(nrep, length(ns))))
expos_tbl_short = expos_tbl %>% group_by(ns) %>% filter((1:n()) == 1) %>% ungroup()
p = ggplot(expos_tbl, aes(x = val)) + 
  geom_histogram(col = "white", fill = "#606060", binwidth = .25) + 
  geom_text(data = expos_tbl_short, mapping=aes(x = 5,y = 1800*2.4, label = paste0('Mittelwert aus ',ns,' Werten')), size = 8) +
  theme_minimal() +  xlim(0,10) + 
  labs(x = expression(bar(x)), y = 'Dichte') + 
  theme(axis.text = element_text(size=14), axis.text.y = element_blank(),
        axis.title = element_text(size=20))
anim = p + transition_states(ns,
                      transition_length = 0,
                      state_length = .5) #& patchwork::plot_annotation()
anim_save('_sessions/Intro2Stats/image/clt_expos.gif', anim, res = 300, width = 6, height = 3, unit = 'in')




library(tidyverse) ; library(ggrepel)

# Lade Tourismus Daten
tour <- read_csv('1_Data/Tourismus.csv') 
europa <- read_csv('1_Data/Europa.csv') 

# Berechne Nächte per Region
d = tour %>%
  mutate(Nächte = Besucher * Dauer) %>%
  left_join(europa) %>% filter(!is.na(Äquivalenzeinkommen))

m = lm(log2(Nächte) ~ Äquivalenzeinkommen, data = d)
exp(coef(m))
coef(m)

intercepts = seq(6.2,7.2, length = 40)
slopes = seq(0, 0.0001, length = 40)
#pars = expand.grid(intercepts, slopes)
pars = cbind(intercepts, slopes)

mse = c()
for(i in 1:nrow(pars)){
  m$coefficients[1] = pars[i,1]
  m$coefficients[2] = pars[i,2]
  mse[i] = sum((predict(m) - log2(d$Nächte))**2)
  }
pars = pars[order(mse, decreasing = T),]
mse = mse[order(mse, decreasing = T)]

sel = diff(pars[,1])>=0 | diff(pars[,2])>=0
pars = pars[sel, ]
mse = mse[sel]

m = lm(log2(Nächte) ~ Äquivalenzeinkommen, data = d)

tbls = list()
for(i in 1:nrow(pars)){
  m$coefficients[1] = pars[i,1]
  m$coefficients[2] = pars[i,2]
  tbl = tibble(yhat = predict(m), mod = i, intercept = pars[i,1], slope = pars[i,2], mse = mse[i])
  tbl = bind_cols(d, tbl)
  tbls[[i]] = tbl
  }
tbl = do.call(rbind, tbls)
tbl_short = tbl[duplicated(tbl$intercept),]

p  = ggplot(tbl, 
       aes(x = Äquivalenzeinkommen, 
           y = Nächte,
           label = Land)) +
  scale_y_continuous(trans = 'log2', limits = c(16,14000)) +
  geom_segment(
    aes(x = Äquivalenzeinkommen, xend = Äquivalenzeinkommen,
        y = 2**yhat, yend = Nächte), linetype=3) +
  geom_text(data = tbl_short, aes(x = 17000, y = 12000, label = paste0("∑e^2 = ", round(mse,1))), size=4) +
  geom_point() + 
  geom_abline(data = tbl_short, aes(intercept = intercept, slope = slope), col = '#EA4B68', size = 1.5) +
  theme_bw()

require(gganimate)
anim = p + transition_states(mod,
                             transition_length = 1,
                             state_length = 0, wrap = FALSE) #& patchwork::plot_annotation()
#anim
anim_save('_sessions/LinearModelsI/image/regression_fit.gif', anim, res = 300, width = 3.6, height = 3.1, unit = 'in')








