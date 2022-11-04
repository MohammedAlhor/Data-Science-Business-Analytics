#inladen data
setwd("~/Data-Science-Business-Analytics/Data")
df <- read.csv("vwodata.csv")
df<-df[,-1]
pca.out<- prcomp(df)
plot(pca.out, type="l")
# plot neemt af, omdat de opeenvolgende principal componenten minder kunnen verklaren(er is minder over om te verklaren)
summary(pca.out)

#pca.out$
# op zoek naar de elleboog in de plot
# punt voor de ellboog kiezen, punt erna voegt niet veel toe.

biplot(pca.out, cex=c(0.5,1), col=c('grey','red'))



USelections2016 <- read_csv("USelections2016.csv")
USelections2016<-USelections2016[,-1]

pca.out2<- prcomp(USelections2016)
plot(pca.out2, type="l")
summary(pca.out2)

# als de eenheden niet hetzelfde zijn, gaan de varianties ook anders zijn
# gebruik in dit geval scale.=TRUE

pca.out2<- prcomp(USelections2016, scale.=TRUE)
biplot(pca.out2, cex=c(0.5,1), col=c('grey','red'))
