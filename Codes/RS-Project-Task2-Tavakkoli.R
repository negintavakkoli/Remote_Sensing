###################################################
#call needed libraries

x <- c("raster","rgdal","sp" , "ggplot2", "rasterVis", "ps", "RStoolbox", "dplyr","magrittr","sf", "devtools", "randomForest")
# install.packages(x, dependencies = TRUE)
lapply(x, library, character.only = TRUE)


########################################################################


#######################################################################
# Change Detection Part
########################################################################


#raster 2 pic of 2017 and 2021
img_2017<-raster("Raw input/L1C_T56JML_A012282_20171028T235244.tif","Raw input/L1C_T56JML_A012282_20171028T235244.tif","Raw input/L1C_T56JML_A012282_20171028T235244.tif", native=F)


img_2017 <- stack("Raw input/L1C_T56JML_A012282_20171028T235244.tif",
                  "Raw input/L1C_T56JML_A012282_20171028T235244.tif", 
                  "Raw input/L1C_T56JML_A012282_20171028T235244.tif",
                  native=F)
img_2021<-raster("Raw input/L1C_T56JML_A024180_20211022T235246.tif")

colors<-c("#549431", "#AA6039", "#2B4B6F")




##ordering labels
for (i in 1:length(unique(shp$classes))) {cat(paste0(i, " ", levels(as.factor(shp$classes))[i]), sep="\n")}

##Making data frame for 2017
smp <- extract(img_2017, shp, df = TRUE)

#making data frame for 2021
smp <- extract(img_2021, shp, df = TRUE)


#In case there are NA elements in the file
smp <- smp[complete.cases(smp), ]

smp$cl <- as.factor(shp$classes[match(smp$ID, seq(nrow(shp)))])
smp <- smp[-1]


write.csv(smp,'smp.csv')
# smp <-read.csv('smp.csv')
smp <- smp[-1]


sp <- aggregate( . ~ cl, data = smp, FUN = mean, na.rm = TRUE )







summary(smp$cl)


smp.size <- min(summary(as.factor(smp$cl)))

# sapply(smp, function(x) sum(is.na(x)))
# which(is.na(smp$cl))

rfmodel <- tuneRF(x = smp[-ncol(smp)],
                  y = smp$cl,
                  sampsize = smp.size,
                  strata = smp$cl,
                  ntree = 250,
                  importance = TRUE,
                  doBest = TRUE
)


rfmodel


varImpPlot(rfmodel)

saveRDS(rfmodel, "rfmodel.rds")
# save(rfmodel, file = "rfmodel.RData")





result_2017 <- predict(img_2017,
                       rfmodel,
                       filename = "2017_RF.tif",
                       overwrite = TRUE
)



result <- predict(img,
                  rfmodel,
                  filename = "2021_RF.tif",
                  overwrite = TRUE
)




plot(result, col=colors, legend=FALSE)
legend("topright", legend = levels(as.factor(shp$classes)), fill=colors, title="RF Model 2017", bg ="white")



plot(result, col=colors, legend=FALSE)
legend("topright", legend = levels(as.factor(shp$classes)), fill=colors, title="RF Model 2021", bg ="white")



#plot results
plot(result, col=colors, legend=FALSE)
legend("topright", legend = levels(as.factor(shp$classes)), fill=colors, title="land cover", bg ="white")





## read and convert classification outcome to vector and then count number of each class in the vector
raster_2017 = raster("2017_RF.tif")

vector_2017 = getValues(raster_2017,1,nrow(raster_2017))
# number of NA values
print(sum(is.na(vector_2017)))
print(table(vector_2017))


raster_2021 = raster("2021_RF.tif")

vector_2021 = getValues(raster_2021,1,nrow(raster_2021))
# number of NA values
print(sum(is.na(vector_2021)))
print(table(vector_2021))