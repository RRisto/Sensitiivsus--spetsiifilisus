#eeldusandmed
#esimene jaotus
mean1=0
sd1=1
nr1=10000
#teine jaotus
mean2=2
sd2=1
nr2=10000
#esimene grupp ehk terved
values=rnorm(n=nr1, mean=mean1, sd=sd1)
values=data.frame(values)
#teine grupp ehk haiged
values2=rnorm(n=nr2, mean=mean2, sd=sd2)
values2=data.frame(values2)

#paneme mõlemad joonisele
library(ggplot2)
ggplot(values, aes(x=values))+
    geom_density(colour="green", fill="green", alpha=0.1)+
    geom_density(data=values2, aes(x=values2), 
                 colour="blue", fill="blue", alpha=0.1)

#paneme mõlemad joonisele ning märgime öra osa, mis on valepositiivsed
library(dplyr)
library(reshape2)
library(data.table)
data=cbind(values, values2)  %>% 
    melt  %>% 
    data.table

gg <- data[,list(x=density(value)$x, y=density(value)$y),by="variable"]

#leiame 95% kvintiili esimesest jaotusest
qnorm(0.95, mean=mean1)#teoreetiline piir
piir=quantile(values$values, c(0.95)) #päris lähedal

#minu variant, kust saab ära märkida valepositiivsed ja valenegatiivsed
ggplot(data, aes(x=value))+
    geom_density(aes(group=variable, fill=variable),colour=0, alpha=0.3)+
    geom_ribbon(data=subset(gg,variable=="values2" & x<=piir),
                aes(x=x,ymax=y),ymin=0,fill="red", alpha=0.2)+
    geom_ribbon(data=subset(gg,variable=="values" & x>=piir),
                aes(x=x,ymax=y),ymin=0,fill="yellow", alpha=0.2)+
    geom_vline(xintercept=piir, colour="blue", size=1)

ggplot(data, aes(x=value))+
    geom_density(aes(group=variable), alpha=0.3)+
    geom_ribbon(data=subset(gg,variable=="values2" & x<=piir),
                aes(x=x,ymax=y),ymin=0,fill="red", alpha=0.2)+
    geom_ribbon(data=subset(gg,variable=="values" & x>=piir),
                aes(x=x,ymax=y),ymin=0,fill="yellow", alpha=0.2)+
    geom_vline(xintercept=piir, colour="blue", size=1)

#leiame, mitu % on see teisest jaotusest (values2)
pnorm(piir, mean=mean2, sd=sd2) #teoreetiline
nrow(subset(values2, values2>piir))/nrow(values2)#päris lähedal
#õiged positiivsed (keso n haiged ja test seda ka näitab)
truepos=nrow(subset(values2, values2>piir))
#valepositiivsed, need, kes on terved, kuid test näitab haigena
falsepos=nrow(subset(values, values>piir))
#õiged negatiivsed (kes on terved ja ka test nii näitab)
trueneg=nrow(subset(values, values<=piir))
#valengeatiivsed (need, kes on haiged, kuid test näitab tervena)
falseneg=nrow(subset(values2, values2<=piir))
#sensitiivsus
truepos/(falseneg+truepos)
#spetsiifilsus
trueneg/(trueneg+falsepos)
#positive predictive value
truepos/(truepos+falsepos)
#negative predicitve value
trueneg/(trueneg+falseneg)

#teeme asjast funktsiooni, esimene funktsioon graafiku tegemiseks
andmetabel =function(mean1=4, mean2=6, sd1=1, sd2=1, nr1=10000, nr2=10000) {
    library(dplyr)
    #esimene grupp ehk terved
    terved=rnorm(n=nr1, mean=mean1, sd=sd1) %>% 
        data.frame %>%
        mutate(name="terved")
        
    #teine grupp ehk haiged
    haiged=rnorm(n=nr2, mean=mean2, sd=sd2) %>% 
        data.frame %>% 
        mutate(name="haiged")
    #paneme ühte tabelisse kokku
    library(data.table)
    data=rbind(terved, haiged)%>%
        data.table
    
    names(data)[1]=c("value")
    data
}
     
#teine funktsioon valenegatiivsete ja -positiivsete visualiseerimiseks
graafik=function(data=data, qt=0.95) {
    
    gg <- data[,list(x=density(value)$x, y=density(value)$y),by="name"]
    #piir, kus kohast tervete seas jääb qt% vaatlustest vasakule
    #ehk test nätab, et on terved
    sub=subset(data, name=="terved", select= c(value))
    piir=quantile(sub$value, c(qt)) 
    library(ggplot2)
    ggplot(data, aes(x=value))+
        geom_density(aes(group=name), alpha=0.4)+
        geom_ribbon(data=subset(gg,name=="haiged" & x<=piir),
                    aes(x=x,ymax=y, fill="valenegatiivsed"),ymin=0, alpha=0.4)+
        geom_ribbon(data=subset(gg,name=="terved" & x>=piir),
                    aes(x=x,ymax=y, fill="valepositiivsed"),ymin=0, alpha=0.4)+
        geom_vline(xintercept=piir, colour="blue", size=1)+
        scale_fill_discrete(name="")
}

#kolmas funktsioon spetsiifiluse, tundlikkuse ja määrade välja arvutamiseks
tabel=function(data=data, qt=0.95) {
    sub2=subset(data, name=="terved", select= c(value))
    piir=quantile(sub2$value, c(qt)) 
    
    #õiged positiivsed (kes on haiged ja test seda ka näitab)
    truepos=nrow(subset(data, name=="haiged" & value>piir))
    #valepositiivsed, need, kes on terved, kuid test näitab haigena
    falsepos=nrow(subset(data, name=="terved" & value>piir))
    #õiged negatiivsed (kes on terved ja ka test nii näitab)
    trueneg=nrow(subset(data, name=="terved" & value<=piir))
    #valengeatiivsed (need, kes on haiged, kuid test näitab tervena)
    falseneg=nrow(subset(data, name=="haiged" & value<=piir))
    näitaja=c("sensitiivsus (testi võimsus)", "spetsiifilisus", 
              "valepositiivsete määr (I tüüpi viga)", 
              "valenegatiivsete määr (II tüüpi viga)",
              "positive predictive value", 
            "negative predictive value")
    väärtus=c(round(truepos/(falseneg+truepos),3),
                round(trueneg/(trueneg+falsepos),3),
                1-round(trueneg/(trueneg+falsepos),3),
                1-round(truepos/(falseneg+truepos),3),
                round(truepos/(truepos+falsepos),3),
                round(trueneg/(trueneg+falseneg),3))
    data.frame(näitaja, väärtus)
}


#funktsioon mis teeb spec, sens, pos-pred value, neg-pred value tabeli

population=20000
spec=0.95
sens=0.638
ratio=0.5 #haigete osakaal

haige=c(round(population*ratio*sens, 0), round(population*ratio*(1-sens), 0))
terve=c(round(population*(1-ratio)*(1-spec), 0), round(population*(1-ratio)*spec, 0))
tulem=c("test positiivne", "test negatiivne")
test_summary=data.frame(tulem, haige, terve)
pospred=haige[1]/(haige[1]+terve[1])
negpred=terve[2]/(haige[2]+terve[2])


#teeme funktsiooniks
neg_pos_predictive=function(ratio, sensitivity, specificity) {
    population=20000
    haige=c(round(population*ratio*sensitivity, 0), round(population*ratio*(1-sensitivity), 0))
    terve=c(round(population*(1-ratio)*(1-specificity), 0), round(population*(1-ratio)*specificity, 0))
    tulem=c("test positiivne", "test negatiivne")
    test_summary=data.frame(tulem, haige, terve)
    pospred=haige[1]/(haige[1]+terve[1])
    negpred=terve[2]/(haige[2]+terve[2])
    return(data.frame(pospred, negpred))
}

ratiod=c(0.9, 0.7, 0.5, 0.3, 0.1)
vastus= data.frame(positive_pred=numeric(),
                   negative_pred=numeric())

for (i in 1:length(ratiod)) {
    vastus[i,]=neg_pos_predictive(ratio=ratiod[i], sensitivity = 0.8, specificity=0.95)
}

vastus$sensitivity=0.8
vastus$specificity=0.95
vastus$ratio=ratiod
#plotime
library(ggplot2)

ggplot(vastus, aes(x=ratio, y=positive_pred))+
    geom_line(colour="blue")+
    geom_line(aes(y=negative_pred), colour="red")+
    ylab("%")

#väikese levikuga korral
ratiod2=c(0.1, 0.07, 0.05, 0.03, 0.01)
vastus2= data.frame(positive_pred=numeric(),
                   negative_pred=numeric())

for (i in 1:length(ratiod2)) {
    vastus2[i,]=neg_pos_predictive(ratio=ratiod2[i], sensitivity = 0.8, specificity=0.95)
}

vastus2$sensitivity=0.8
vastus2$specificity=0.95
vastus2$ratio=ratiod2

ggplot(vastus2, aes(x=ratio, y=positive_pred))+
    geom_line(colour="blue")+
    geom_line(aes(y=negative_pred), colour="red")+
    ylab("%")

#paneme kokku väikese ja suure leviku 
ratiod2=c(0.999,0.99,0.98,0.97,0.96,0.95,0.94,0.93,0.92,0.91,0.9,0.8, 0.7, 
          0.6,0.5, 0.4,0.3,0.2,0.1, 0.07, 0.05, 0.03, 0.01, 0.001)
vastus2= data.frame(positive_pred=numeric(),
                    negative_pred=numeric())

for (i in 1:length(ratiod2)) {
    vastus2[i,]=neg_pos_predictive(ratio=ratiod2[i], sensitivity = 0.8, specificity=0.95)
}

vastus2$ratio=ratiod2
#reshabime
library(reshape2)
vastus2=melt(vastus2, id=c("ratio"))
#plotime
library(scales) 
ggplot(vastus2, aes(x=ratio, y=value))+
    geom_line(aes(colour=variable), size=1)+
    xlab("haigete osakaal populatsioonist")+
    scale_y_continuous(labels=percent)+
    scale_x_continuous(labels=percent)+
    theme_minimal()

