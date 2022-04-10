###################################################
#call needed libraries

x <- c("raster","rgdal","sp" , "ggplot2", "rasterVis", "ps", "RStoolbox", "dplyr","magrittr","sf", "devtools", "randomForest")
# install.packages(x, dependencies = TRUE)
lapply(x, library, character.only = TRUE)



####################################################

setwd("D:/University/Remot Sensing/Final Project-Deforestaion")

#Stack 3 picture in Sep, Oct, and Nov 2017
img <- stack("Raw input/L1C_T56JML_A012282_20171028T235244.tif",
             "Raw input/L1C_T56JML_A003731_20171122T235232.tif", 
             "Raw input/L1C_T56JML_A011710_20170918T235241.tif",
             native=F)

shp<-shapefile("Task 1/Training Data/training-data.shp")
shp <- spTransform(shp, crs(img))






#Saving training file as pdf
pdf("training-data.pdf",         # File name
    width = 8, height = 7, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "srgb",    # Color model 
    paper = "A4" )         # Paper size

plotRGB(img, r = 1, g = 2, b = 3, stretch = "lin")
plot(shp, col="red", add=TRUE)

levels(as.factor(shp$classes))
dev.off()


##ordering labels
for (i in 1:length(unique(shp$classes))) {cat(paste0(i, " ", levels(as.factor(shp$classes))[i]), sep="\n")}

##Making data frame
smp <- extract(img, shp, df = TRUE)


#In case there are NA elements in the file
smp <- smp[complete.cases(smp), ]

smp$cl <- as.factor(shp$classes[match(smp$ID, seq(nrow(shp)))])
smp <- smp[-1]


write.csv(smp,'smp.csv')
# smp <-read.csv('smp.csv')
smp <- smp[-1]


sp <- aggregate( . ~ cl, data = smp, FUN = mean, na.rm = TRUE )

##Plot mean value of each band
pdf("line-mean.pdf")
plot(0,
     ylim = c(min(sp[2:ncol(sp)]), max(sp[2:ncol(sp)])), 
     xlim = c(1, ncol(smp)-1), 
     type = 'n', 
     xlab = "bands", 
     ylab = "reflectance [% * 100]"
)


colors<-c("#549431", "#AA6039", "#2B4B6F")



for (i in 1:nrow(sp)){
  lines(as.numeric(sp[i, -1]), 
        lwd = 2, 
        col = colors[i]
  )
}


grid()

# add a legend
legend(as.character(sp$cl),
       x = "topleft",
       col = colors,
       lwd = 2,
       bty = "n"
)
dev.off()



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


result <- predict(img,
                  rfmodel,
                  filename = "classification_RF.tif",
                  overwrite = TRUE
)




result_2017 <- predict(img_2017,
                       rfmodel,
                       filename = "2017_RF.tif",
                       overwrite = TRUE
)

plot(result, col=colors, legend=FALSE)
legend("topright", legend = levels(as.factor(shp$classes)), fill=colors, title="RF Model 2017", bg ="white")




pdf("result.pdf", paper = "A4")
plot(result, col=colors, legend=FALSE)
legend("topright", legend = levels(as.factor(shp$classes)), fill=colors, title="land cover", bg ="white")
dev.off()

###############################################################################################
#Validation
###############################################################################################
classification <- raster("classification_RF.tif")


# create 25 test samples per class
samplesperclass <- 25

smp.test <- sampleStratified(x = classification,
                             size = samplesperclass,
                             na.rm = TRUE,
                             sp = TRUE)
# smp.test$classification_RF
# smp.test$cell
smp.test$classification_RF

smp.test <- smp.test[sample(nrow(smp.test)), ]
smp.test <- smp.test[, -c(1, 2)]
smp.test$ID <- 1:nrow(smp.test)
smp.test


plot(classification, 
     axes = FALSE, 
     box = FALSE,
     col = colors
)
points(smp.test)
shapefile(smp.test,
          filename = "validation_RF.shp",
          overwrite = TRUE
)

##########################################################
#Labeling the validation data 
##########################################################

img.classified <- raster("classification_RF.tif")
shp.valid <- shapefile("validation_RF.shp")
shp.train <- shapefile("training-data.shp")


level_order <- seq(1:9)
reference <- as.factor(shp.valid$validclass)
reference

predicted <- as.factor(extract(img.classified, shp.valid))
predicted


accmat <- table("pred" = predicted, "ref" = reference)
accmat
for (i in 1:length(unique(shp$classes))) {cat(paste0(i, " ", levels(as.factor(shp$classes))[i]), sep="\n")}
UA <- diag(accmat) / rowSums(accmat) * 100
UA



# Producer's accuracy
PA <- diag(accmat) / colSums(accmat) * 100
PA

# Overall accuracy
OA <- sum(diag(accmat)) / sum(accmat) * 100
OA


a = c(levels(as.factor(shp.train$classes)), "Sum", "PA")
a


length(a)
accmat.ext <- addmargins(accmat)
accmat.ext <- rbind(accmat.ext, "Users" = c(PA, NA))
accmat.ext <- cbind(accmat.ext, "Producers" = c(UA, NA, OA))
colnames(accmat.ext) <- c(levels(as.factor(shp.train$classes)), "Sum", "PA")
rownames(accmat.ext) <- c(levels(as.factor(shp.train$classes)), "Sum", "UA")
accmat.ext <- round(accmat.ext, digits = 1)
dimnames(accmat.ext) <- list("Prediction" = colnames(accmat.ext),
                             "Reference" = rownames(accmat.ext))
class(accmat.ext) <- "table"
accmat.ext
write.csv(accmat.ext, "accuracy.csv")
write.csv2(accmat.ext, "accuracy2.csv")




sign <- binom.test(x = sum(diag(accmat)),
                   n = sum(accmat),
                   alternative = c("two.sided"),
                   conf.level = 0.95
)

pvalue <- sign$p.value
pvalue

CI95 <- sign$conf.int[1:2]
CI95


kappa <- function(m) {
  N <- sum(m)
  No <- sum(diag(m))
  Ne <- 1 / N * sum(colSums(m) * rowSums(m))
  return( (No - Ne) / (N - Ne) )
}

kappacoefficient <- kappa(accmat)
kappacoefficient


