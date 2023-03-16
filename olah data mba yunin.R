##########Olah data Ca dan K##################
setwd("C:/Users/EBC KOMPUTER/Documents")
library(readxl)
data <- read_excel("profil air limbah batik.xlsx", 
                  sheet = "data spss")
View(data)
fix(data)
data$Lokasi<- as.factor(data$Kode)
library(tidyverse)
summary<- data%>%
  group_by(Kode) %>%
  summarise(mean.DO = mean(DO),se.DO = sd(DO),
            mean.pH = mean(pH),se.pH = sd(pH),
            mean.TSS = mean(TSS),se.DO = sd(TSS),            
            mean.Konduktivitas = mean(Konduktivitas),se.Konduktivitas = sd(Konduktivitas),
            mean.COD = mean(COD),se.COD= sd(COD),
            mean.Cr = mean(Cr),se.Cr = sd(Cr),
            mean.S = mean(S),se.S = sd(S),
            mean.Fenol = mean(Fenol),se.Fenol = sd(Fenol),
            mean.Lemak = mean(Lemak), se.Lemak = sd(Lemak),
            mean.Nitrat = mean(Nitrat), se.Nitrat = sd(Nitrat),
            mean.BOD = mean(BOD), se.BOD = sd(BOD))
sumarya
str(data)
write.csv(summary, "mean dan se limbah batik.csv")
str(data)


# Uji anova ---------------------------------------------------------------


#UJI ANOVA CA
#Compute the analysis of variance data DO
res.aov <- aov(DO ~ Lokasi, data = data)

#summary anova
summary(res.aov)

#Compute the analysis of variance data pH
res.aov <- aov(pH ~ Lokasi, data = data)

#summary anova
summary(res.aov)

#Compute the analysis of variance data TSS
res.aov <- aov(TSS ~ Lokasi, data = data)

#summary anova
summary(res.aov)

#Compute the analysis of variance data konduktivitas
res.aov <- aov(Konduktivitas ~ Lokasi, data = data)

#summary anova
summary(res.aov)

#Compute the analysis of variance data COD
res.aov <- aov(COD ~ Lokasi, data = data)

#summary anova
summary(res.aov)
#################################################################
#Compute the analysis of variance data Cr
res.aov <- aov(Cr ~ Lokasi, data = data)

#summary anova
summary(res.aov)


#post hoc dengan TukeyHSD data Cr
tukey <- TukeyHSD(res.aov)
tukey


#UJI TUKEY HSD Cr
library(multcompView)
cld <- multcompLetters4(res.aov, tukey)
cld
str(cld)

cld2 <- cld$Station["Letters"] 
cld2 <- as.data.frame(cld2)
print(cld2)

data <- add_column(sumCaK, cld2, .after = "se.Ca")
write.csv(data, "hasil tukey Ca.csv")
colnames(data)[4]  <- "TukeyCa" 



# Grafik Cr ---------------------------------------------------------------

####GRAFIK CR
library(ggsignif)
Cr <- ggplot(summary, aes(x=Kode, y=mean.Cr, fill=Kode, ymax=180, ymin=0)) + 
  geom_col(width = .4, position = position_dodge(.8), colour="black")+
  geom_errorbar(aes(ymin=mean.Cr-se.Cr, ymax=mean.Cr+se.Cr),width=.1,
                position=position_dodge(.8), color="black")+
  geom_signif(comparisons = list(c("C", "D")), annotations = "**", y_position = 55, tip_length =  0.05)+
  geom_signif(comparisons = list(c("A", "C")), annotations = "*", y_position = 45, tip_length =  0.05)+
 
  labs(x="Location", y="Cr (mg/L)", fill=NULL)+
  scale_fill_manual(values = c("lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue"))+
  scale_y_continuous(expand=expansion(0), limits = c(0,75), breaks = seq(0,75,15))+
  
  
  
  theme(
    plot.margin = unit(c(0.8, 0.7, 0.7, 0.7), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14, color = "black", family = "sans"), # family untuk atur jenis tulisan serif untuk TNR
    axis.title.y = element_text(margin=margin(t = 0, r=8, b=0, l=0)), # mengatur jarak teks sumbu y
    axis.text = element_text(size = 14, color = "black"),
    axis.text.y= element_text(size=14),
    axis.text.x = element_text(size=14),
    axis.ticks.x = element_blank(), legend.position = "none"
  )

Cr

ggsave("bar_cr.png", width=5, height =5, dpi=400)




#####################################################
#Compute the analysis of variance data S
res.aov <- aov(S ~ Lokasi, data = data)

#summary anova
summary(res.aov)

#Compute the analysis of variance data fenol
res.aov <- aov(Fenol ~ Lokasi, data = data)

#summary anova
summary(res.aov)

#post hoc dengan TukeyHSD data Fenol
tukey <- TukeyHSD(res.aov)
tukey

#Compute the analysis of variance data Lemak
res.aov <- aov(Lemak ~ Lokasi, data = data)

#summary anova
summary(res.aov)

#Compute the analysis of variance data N-NH3
res.aov <- aov(N-NH3 ~ Lokasi, data = data)

#summary anova
summary(res.aov)

#Compute the analysis of variance data BOD
res.aov <- aov(BOD ~ Lokasi, data = data)

#summary anova
summary(res.aov)

#menampilkan compact letter hasil tukey HSD
install.packages("multicompView")
library(multcompView)
cld <- multcompLetters4(res.aov, tukey)
cld
str(cld)

cld2 <- cld$Station["Letters"] 
cld2 <- as.data.frame(cld2)
print(cld2)

data <- add_column(sumCaK, cld2, .after = "se.Ca")
write.csv(data, "hasil tukey Ca.csv")
colnames(data)[4]  <- "TukeyCa" 

#UJI ANOVA K
#Compute the analysis of variance data ca
res.aov <- aov(K ~ Station, data = CaK)

#summary anova
summary(res.aov)

#post hoc dengan TukeyHSD data Ca
tukey <- TukeyHSD(res.aov)
tukey

#menampilkan compact letter hasil tukey HSD
install.packages("multicompView")
library(multcompView)
cld <- multcompLetters4(res.aov, tukey)
cld
str(cld)

cld2 <- cld$Station["Letters"] 
cld2 <- as.data.frame(cld2)
print(cld2)


#sisipkan kolumn baru setelah kolom se.K
data <- add_column(data, cld2, .after = "se.K")
#merubah nama kolum
colnames(data)[7]  <- "TukeyK" 
TukeyK <- c("A", "B", "C")

data <- data[,-7]
data <- add_column(data, TukeyK, .after="se.K")
write.csv(data, "hasil tukey Ca dan K.csv")
library(writexl)
writexl::write_xlsx(data, "data tukey ca dan K.xlsx")

##visualisasi data
grafikKCa <- read_excel("data tukey ca dan K.xlsx", 
                        +     sheet = "Sheet2")
str(grafikKCa)
grafikKCa$Station <- as.factor(grafikKCa$Station) 
grafikKCa <- mutate(grafikKCa, Minerals = case_when(
  Minerals == "Ca" ~ "Calcium",
  Minerals == "K" ~ "Potassium"))

grafik <- ggplot(data = grafikKCa,
                 aes(x = Station, y = mean, fill = Station)) +
  facet_grid(cols = vars(Minerals)) +
  geom_col(width = .2, position = position_dodge(.8), colour="black")+
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  scale_y_continuous(
    name = "Concentration (mg/L)",
    expand = expansion(mult = c(0, 0.1)),
    labels = scales::number_format(accuracy = 1)) +
  labs(title = "", x="Station",
       caption = "Means followed by a common letter are not significantly different according to the Tukey-test") +
  geom_text(aes(label=Tukey, y= mean+se+20)) +
  scale_fill_manual(values = c("midnightblue", "steelblue3", 'lightskyblue')) +
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    axis.title = element_text(size = 15, color = "black"),
    axis.text.x = element_text(size=13, color = "black"),
    axis.text.y = element_text(size=13, color = "black"),
    axis.title.x = element_text(size=15, vjust = 2, color = "black"),
    axis.title.y = element_text(size=15, vjust=1.5, color = "black")
  ) +
  theme(legend.title = element_blank(),
        legend.position = "none")

grafik
ggsave("grafikCaK2.jpg", width=5.6, height =5, dpi=700)



#######REGRASI KORELASI#################
###########################################################################
#############################uji korelasi dgn p value################################
###########################################################################

library("Hmisc")
library("Hmisc")

datacor <- CaK[,4:10]
res2 <- rcorr(as.matrix(datacor))
koef <- res2$r
koef <- as.data.frame(koef)
koef <- res2$r
View(koef)
koef <- as.data.frame(koef)
pvalue <- res2$P
pvalue <- as.data.frame(pvalue)

writexl::write_xlsx(koef, "koefisien korelasi CaK.xlsx")
writexl::write_xlsx(pvalue, "pvalue korelasi CaK.xlsx")
fix(datacor)
#plot korelasi
library("PerformanceAnalytics")
chart.Correlation(datacor, histogram=TRUE, pch=20)



###########################################################################
#############################regresi linier################################
###########################################################################
Kalium <-   lm(K ~ DO, data=CaK)
summary(Kalium)
library(ggplot2)

Kalium <- ggplot(data=CaK, aes(x=DO, y=K)) +
  geom_point()+
  geom_smooth(method="lm", se=0)+
  theme_classic()+
  theme(
    plot.margin = unit(c(1.5, 2, 0.7,  0.7), "cm"),
    text = element_text(size = 22),axis.text = element_text(size = 20),
  )+
  labs(x="Dissolved Oxygen(mg/L)", y="Potassium (µg/L)", fill=NULL)+
  scale_y_continuous(expand=expansion(0), limits = c(0,300), breaks = seq(0,300,50))+
  scale_x_continuous(expand=expansion(0), limits = c(0,10), breaks = seq(0,10,2))
Kalium
ggsave("reg_AmmoniumChl.png", width=9, height = 5, dpi=700)

####################################################################
#####################Bar graph wth dashed line#########################
library(readxl)
datasum <- read_excel("datasum.xlsx", sheet = "dashed")
View(datasum)
fix(datasum)
#bargraph
grafik <- ggplot(data = datasum,
                 aes(x = Location, y = K, fill = Location))+
  geom_col(width = 0.6, position = position_dodge(.7), colour="black")+  
  scale_y_continuous(
    name = "Concentration of Potassium (mg/L)",
    expand = expansion(mult = c(0, 0.065)),
    labels = scales::number_format(accuracy = 1),  breaks = seq(0,600,150)) +
  labs(title = "", x="")+
  scale_fill_manual(values = c("#77C3EC", "#77C3EC", "#77C3EC", "#77C3EC"))+
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    axis.title = element_text(size = 15, color = "black"),
    axis.text.x = element_text(size=11,color = "black"),
    axis.text.y = element_text(size=11, color = "black"),
    axis.title.x = element_text(size=15, vjust = -0.5, color = "black"),
    axis.title.y = element_text(size=15, vjust=1.5, color = "black")
  ) +
  theme(legend.title = element_blank(),
        legend.position = "none")

grafik

grafik + geom_hline(aes(yintercept=250.833), colour="#990000", linetype="dashed")+
  annotate("text", x = "", y = 252, label = "Padelegan", vjust = -0.5, 
           size=5)
ggsave("grafikperbandingan K.jpg", width=7, height =6, dpi=700)


#bargraph Ca
grafik <- ggplot(data = datasum,
                 aes(x = Location, y = Ca, fill = Location))+
  geom_col(width = 0.6, position = position_dodge(.7), colour="black")+  
  scale_y_continuous(
    name = "Concentration of Calcium (mg/L)",
    expand = expansion(mult = c(0, 0)),
    labels = scales::number_format(accuracy = 1), limits = c(0,600), breaks = seq(0,600,150)) +
  labs(title = "", x="")+
  scale_fill_manual(values = c("#77C3EC", "#77C3EC", "#77C3EC", "#77C3EC"))+
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    axis.title = element_text(size = 15, color = "black"),
    axis.text.x = element_text(size=11,color = "black"),
    axis.text.y = element_text(size=11, color = "black"),
    axis.title.x = element_text(size=15, vjust = -0.5, color = "black"),
    axis.title.y = element_text(size=15, vjust=1.5, color = "black")
  ) +
  theme(legend.title = element_blank(),
        legend.position = "none")

grafik

grafik + geom_hline(aes(yintercept=521.464), colour="#990000", linetype="dashed")+
  annotate("text", x = "", y = 522, label = "Padelegan", vjust = -0.5, 
           size=5)
ggsave("grafikperbandingan Ca.jpg", width=7, height =6, dpi=700)


# dendogram ---------------------------------------------------------------

library(pheatmap) ## for heatmap generation
library(tidyverse) ## for data wrangling
library(ggplotify) ## to convert pheatmap to ggplot2
library(heatmaply) ## for constructing interactive heatmap
require(RColorBrewer)
library(gtools)
library(viridis)

str(contoh)


data2 <-  data[ ,4:14]
rownames(data2) <- c("Ghedding 1","Ghedding 2","Jambangan 1","Jambangan 2",
                  "Tajung 1", "Tajung 2", "Mlogur 1", "Mlogur 2")
#buat yg kode
data3 <-data.pca
row.names(data3) <- data$Code 
data3 <- data3[,-1]
rownames(data3) <- c("A1", "A2", "B1", "B2", "C1", "C2", "D1", "D2")
pheatmap(data3, scale="column", border_color = NA,
         cellwidth = 30, cellheight = 20,
         color=viridis::viridis(20),
         angle_col = 45 )


##HEATMAP PAKAI NAMA LOKASI
pheatmap(data2, scale="column", border_color = NA,
         cellwidth = 30, cellheight = 20,
         color=viridis::viridis(20),
         angle_col = 45 )
#blue
pheatmap(data2, scale="column", border_color = "white",
         color=colorRampPalette(brewer.pal(8, "Blues"))(100),
         angle_col = 45 )
#manual
pheatmap(data2, scale="column", border_color = "white",
         color=colorRampPalette(c(c("#d53e4f", "#f46d43", "#fdae61", "#e6f598", "#abdda4", "#32CD32", "#228B22")))(50),
         cellwidth = 30, cellheight = 20,
         angle_col = 45 )

pheatmap(data2, scale="column", border_color = NA,
         color=rev(brewer.pal(9, "YlGnBu")),
         angle_col = 45 )

# Here is a fancy color palette inspired by http://www.colbyimaging.com/wiki/statistics/color-bars
cool = rainbow(50, start=rgb2hsv(col2rgb('blue'))[1], end=rgb2hsv(col2rgb('blue'))[1])
warm = rainbow(50, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(warm))
mypalette <- colorRampPalette(cols)(255)

pheatmap(data2, scale="column", border_color = "white",
         color=colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab"),
         angle_col = 45 )

########################PCA########################
library(readxl)
library(factoextra)
library(FactoMineR)

row.names(data) <- data$Code

data.pca<- data[,c(2,4:14)]

res.pca <- PCA(data.pca, graph = FALSE)

eig.val <- get_eigenvalue(res.pca)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

site<-c(rep("A",2),rep("B",2),rep("C",2),rep("D",2))

fviz_pca_biplot(res.pca, 
                col.ind = data.pca$Code, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Site")




fviz_pca_biplot(res.pca, 
                # Individuals
                #geom.ind = "point",
                fill.ind = data.pca$Code, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                repel = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "Set1",
                
                legend.title = list(fill = data.pca$Code, color = "Contrib",
                                    alpha = "Contrib")
)













library(factoextra)
pca <- datacor
fix(pca)
res.pca <- prcomp(pca, scale = TRUE)
res.pca
var <- get_pca_var(res.pca)
var
cos2 <- as.data.frame(cos2)
writexl::write_xlsx(cos2, "square cosine.xlsx")
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)


get_pca_var(res.pca)
ind <- get_pca_ind(res.pca)

ind$cos2
barpca <- fviz_eig(res.pca)
barpca
ggsave("bar_eigent.png", width=9, height = 5, dpi=400)
eig.val <- get_eigenvalue(res.pca)
eig.val
writexl::write_xlsx(eig.val, "eig.val.xlsx")

#ekstrak hasil pca
var <- get_pca_var(res.pca)
var
fviz_pca_var(res.pca, col.var = "black")


fviz_pca_var(res.pca,
             gradient.cols= grey.colors(4,start=0, end=0.3),
             repel = TRUE,     # Avoid text overlapping
             title=""
)
ggsave("pca_var.png", width=7, height = 7, dpi=400)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "Black", # Variables color
                col.ind = "black",  # Individuals color
                title = ""
)

ggsave("pca_biplot.png", width=7, height = 7, dpi=400)

var$contrib
kontibusi <- as.data.frame(var$contrib)
writexl::write_xlsx(kontibusi, "kontribusivariabelPCA.xlsx")
library("corrplot")
a <- corrplot(var$contrib, is.corr=F) 
a <- corrplot(var$contrib, method = 'color', order = 'alphabet', is.corr = F)

