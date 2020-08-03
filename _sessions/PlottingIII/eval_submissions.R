
require(googlesheets4)
library(googledrive)
library(tidyverse)

id = '13fEhfM5oMrD0XqsKWJRCB_bz3v0UgDSEoQ75A1QhPgc'

d = read_sheet(ss = id)

inds = c(0, rep(1:11, rep(3,11)))

res = c()  
for(i in 1:11){
  res[i] = mean(unlist(d[,which(inds == i)[1]]))
}
res

res = c()  
for(i in 1:11){
  res[i] = mean(unlist(d[,which(inds == i)[2]]))
}
res

res = c()  
for(i in 1:11){
  res[i] = mean(unlist(d[,which(inds == i)[3]]))
}
res

res
res_med = c()  
for(i in 1:11){
  res_med[i] = median(unlist(d[,which(inds == i)]))
  }

res_med = c()  
for(i in 1:11){
  res_med[i] = median(unlist(d[,which(inds == i)]))
  }


par(mar=c(10,3,1,1))
barplot(res,las=2,ylim=c(0,5),border='white')



res_med[2]
res_med[4]




names = c('Dozenten','61 6e 64 72 65 ','blutiger anfänger',
          'sissifus','SG','6d 61 75 72 75 73 ','Violin',
          'Topsecret','70 61 74 72 69 63 6b ','geiler scheiss',
          '73 61 6c 6f 6d 65 ')
names(res) = names

par(mar=c(10,3,1,1))
barplot(res,las=2,ylim=c(0,5),border='white')

