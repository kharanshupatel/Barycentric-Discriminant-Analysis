---
title: "Kharanshu_Patel-BADA"
author: "Kharanshu Patel"
date: "11/10/2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ExPosition)
library(corrplot)
library(ggplot2)
library(InPosition)
library(dplyr)
library(PTCA4CATA)
library(data4PCCAR)
library(classInt)
library(Hmisc)
library(psych)
library(TInPosition)
library(RColorBrewer)
library(plotly)
library(heatmaply)
library(TExPosition)
```
# Method : Barycentric Discriminant Analysis

Barycentric discriminant analysis is a robust version of discriminant analysis that is used like discriminant analysis when multiple measurements describe a set of observations in which each observation belongs to one category or group from a set of a-priori defined categories. 

The goal of BADA is to combine the measurements to create new variables called components or discriminant variables that best separate the categories. These discriminant variables are also used to assign the original observations or
“new” observations to the a-priori defined categories.

BADA combines the original variables to create new variables that best separate the groups and that can also be used to optimally assign old or new observations to these categories. The quality of the performance is evaluated by cross-validation techniques that estimate the performance of the classification model for new observations.

# Dataset : oasis_cross-sectional

The Open Access Series of Imaging Studies (OASIS) is a project aimed at making MRI data sets of the brain freely available to the scientific community.
It is a cross-sectional MRI Data in Young, Middle Aged, Nondemented and Demented Older Adults.
This set consists of a cross-sectional collection of 216 subjects (rows) aged 33 to 96. The subjects include both men and women. 
It measures 216 subjects (rows) on 4 quanitiative variables (columns). The 5 qualitative variables have been used to interpret the data. 

Measures included in the study:

Age: Age (years)

Gender: Male or Female

Education: Years of Education - Recoded into categories from 1 (highest) to 5 (lowest)

SES: Socioeconomic status classified into categories from 1 (highest) to 5 (lowest)

MMSE: Mini-Mental State Examination score – range from 0 (worst) to 30 (best)

CDR: Clinical Dementia Rating - (0 = no dementia, 0.5 = very mild AD, 1 = mild AD, 2 = moderate AD)

ASF : Atlas scaling factor (unitless). Computed scaling factor that transforms native-space brain and skull to the atlas target (i.e. the determinant of the transform matrix)

eTIV: Estimated total intracranial volume (cm3)

nWBV: Normalized whole brain volume, expressed as a percent of all voxels in the atlas-masked image that are labeled as gray or white matter by the automated tissue segmentation process 

```{r data_set}

library(readxl)
oasis_cross_sectional <- read_excel("oasis_cross_sectional.xlsx")

head(oasis_cross_sectional)

oasis.dummy <- oasis_cross_sectional[c(6,8,9)]

# Recode Variable Educ
# Used dummy values 50 and 100 for the sake of recode

table(oasis_cross_sectional$Educ)
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 1] <- "50" 
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 5] <- "1" 
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 50] <- "5" 

oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 2] <- "100" 
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 4] <- "2" 
oasis_cross_sectional$Educ[oasis_cross_sectional$Educ == 100] <- "4" 

```
# BADA - Design Colors
```{r BADA - Colors}
COLPAL <- list()

COLPAL$rows$CDR$vec <- oasis_cross_sectional$CDR
COLPAL$rows$MF$vec <- oasis_cross_sectional$`M/F`
COLPAL$rows$Educ$vec <- oasis_cross_sectional$Educ
COLPAL$rows$SES$vec <- oasis_cross_sectional$SES

COLPAL$rows$CDR$mat <- makeNominalData(as.matrix(COLPAL$rows$CDR$vec))
COLPAL$rows$MF$mat <- makeNominalData(as.matrix(COLPAL$rows$MF$vec))
COLPAL$rows$Educ$mat <- makeNominalData(as.matrix(COLPAL$rows$Educ$vec))
COLPAL$rows$SES$mat <- makeNominalData(as.matrix(COLPAL$rows$SES$vec))


# Reorder the columns in ascending order
COLPAL$rows$CDR$mat <- COLPAL$rows$CDR$mat[,order(colnames(COLPAL$rows$CDR$mat))]
COLPAL$rows$Educ$mat <- COLPAL$rows$Educ$mat[,order(colnames(COLPAL$rows$Educ$mat))]
COLPAL$rows$SES$mat <- COLPAL$rows$SES$mat[,order(colnames(COLPAL$rows$SES$mat))]
COLPAL$rows$MF$mat <- COLPAL$rows$MF$mat[,order(colnames(COLPAL$rows$MF$mat))]

COLPAL$rows$CDR$labels <- colnames(COLPAL$rows$CDR$mat)
COLPAL$rows$Educ$labels <- colnames(COLPAL$rows$Educ$mat)
COLPAL$rows$SES$labels <- colnames(COLPAL$rows$SES$mat)
COLPAL$rows$MF$labels <- unique(oasis_cross_sectional$`M/F`)

## CDR original --> 
COLPAL$rows$CDR$color_groups <- c("#037520","#daeb20","#f08300","#780000")
COLPAL$rows$Educ$color_groups <-c("#03022f","#03a6a8","#84026a","#f802ab","#d40108")
COLPAL$rows$SES$color_groups <- c("#013600","#f08300","#282be2","#f802ab","#ff0200")
COLPAL$rows$MF$color_groups <- c("#fc33e2", "#282be2")

#Next we need to create a vector of the group colors.
#We'll take one group at a time and trade the group names for the color names...

#First, copy the group names
COLPAL$rows$CDR$color_obs <- as.matrix(COLPAL$rows$CDR$vec)
COLPAL$rows$Educ$color_obs <- as.matrix(COLPAL$rows$Educ$vec)
COLPAL$rows$SES$color_obs <- as.matrix(COLPAL$rows$SES$vec)
COLPAL$rows$MF$color_obs <- as.matrix(COLPAL$rows$MF$vec)

#Then, for each group, replace the group name with the respective group color

COLPAL$rows$CDR$color_obs[which(COLPAL$rows$CDR$vec == "0")] <- 
COLPAL$rows$CDR$color_groups[1]
COLPAL$rows$CDR$color_obs[which(COLPAL$rows$CDR$vec == "0.5")] <- COLPAL$rows$CDR$color_groups[2]
COLPAL$rows$CDR$color_obs[which(COLPAL$rows$CDR$vec == "1")] <-
COLPAL$rows$CDR$color_groups[3]
COLPAL$rows$CDR$color_obs[which(COLPAL$rows$CDR$vec == "2")] <-
COLPAL$rows$CDR$color_groups[4]

COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "1")] <- 
COLPAL$rows$Educ$color_groups[1] #darkblue
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "2")] <- 
COLPAL$rows$Educ$color_groups[2] #cyan
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "3")] <- 
COLPAL$rows$Educ$color_groups[3] #magenta
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "4")] <- 
COLPAL$rows$Educ$color_groups[4] #pink
COLPAL$rows$Educ$color_obs[which(COLPAL$rows$Educ$vec == "5")] <- 
COLPAL$rows$Educ$color_groups[5] #red

COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "1")] <- 
COLPAL$rows$SES$color_groups[1] #darkgreen
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "2")] <- 
COLPAL$rows$SES$color_groups[2] #olivegreen
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "3")] <- 
COLPAL$rows$SES$color_groups[3] #yellow
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "4")] <- 
COLPAL$rows$SES$color_groups[4] #lightpink
COLPAL$rows$SES$color_obs[which(COLPAL$rows$SES$vec == "5")] <- 
COLPAL$rows$SES$color_groups[5] #red

COLPAL$rows$MF$color_obs[which(COLPAL$rows$MF$vec == "F")] <- 
COLPAL$rows$MF$color_groups[1]
COLPAL$rows$MF$color_obs[which(COLPAL$rows$MF$vec == "M")] <- 
COLPAL$rows$MF$color_groups[2]

col4bada <- c("darkgreen","darkblue","darkred")

oasis.design <- oasis_cross_sectional$SES

oasis.design <- makeNominalData(as.matrix(oasis.design))

oasis.design <- oasis.design[,order(colnames(oasis.design))]

colnames(oasis.design) <- c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")

```
# BADA
```{r BADA}

resBADA <- tepBADA(DATA = oasis.dummy,
                   scale = 'SS1', center = TRUE,
                   DESIGN = oasis_cross_sectional$SES,
                   make_design_nominal = TRUE,
                   group.masses = NULL,
                   weights = NULL, graphs =  FALSE,
                   k = 0)

Fk <- resBADA$TExPosition.Data$fi
Fi <- resBADA$TExPosition.Data$fii
Fj <- resBADA$TExPosition.Data$fj

```

# BADA - Inferences

```{r BADA - Inferences}
resBADA.inf <- tepBADA.inference.battery(DATA = oasis.dummy,
                   scale = 'SS1', center = TRUE,
                   DESIGN = oasis_cross_sectional$SES,
                   make_design_nominal = TRUE,
                   group.masses = NULL,
                   weights = NULL,
                   graphs = FALSE,
                   k = 2,
                   test.iters = 100,
                   critical.value = 2)

```


# BADA - PLOTTING GRAPHS

### BADA - Heat Map

```{r BADA - Heat Map}

col4heat <- c("red","green")

heatMapIJ.WE <- makeggHeatMap4CT(cor(oasis.dummy, oasis.design),
colorAttributes = col4heat,
fontSize.x = 15
) + ggtitle('Heat Map')

print(heatMapIJ.WE)

```

### BADA - Scree Plot BADA + Inference

```{r BADA - Scree Plot BADA}

# Create Function for Scree Plot
PlotScreeFix <- function (ev, p.ev = NULL, max.ev = NULL, alpha = 0.05, col.ns = "#006D2C", 
          col.sig = "#54278F", title = "Explained Variance per Dimension", 
          plotKaiser = FALSE, color4Kaiser = "darkorchid4", lwd4Kaiser = 2.5) 
{
  val.tau = (100 * ev/sum(ev))
  Top.y = ceiling(max(val.tau) * 0.1) * 10
  if (!is.null(max.ev)) {
    ev = ev * (max.ev/ev[1])
  }
  par(mar = c(5, 6, 4, 4))
  plot(x = seq(1, length(val.tau)), y = val.tau, xlab = "Dimensions", 
       ylab = "Percentage of Explained Variance", main = title, 
       type = "l", col = col.ns, lwd = 1, xlim = c(1, length(val.tau)), 
       ylim = c(0, Top.y))
  points(x = seq(1, length(val.tau)), y = val.tau, pch = 16, 
         cex = 1, col = col.ns, lwd = 2.5)
  if (!is.null(p.ev)) {
    signi.vp = which(p.ev < alpha)
    lines(x = seq(1, max(signi.vp)), y = val.tau[1:max(signi.vp)], 
          type = "l", col = col.sig, lwd = 1.5)
    points(x = signi.vp, y = val.tau[signi.vp], 
           pch = 16, cex = 1.5, col = col.sig, lwd = 3.5)
    }
  par(new = TRUE)
  par(mar = c(5, 6, 4, 4) + 0.5)
  le.max.vp = Top.y * (ev[1]/val.tau[1])
  plot(ev, ann = FALSE, axes = FALSE, type = "n", ylim = c(0, 
                                                           le.max.vp))
  if (plotKaiser) {
    abline(h = sum(ev)/length(ev), col = color4Kaiser, lwd = lwd4Kaiser)
  }
  mtext("Inertia Extracted by the Components", side = 4, line = 3)
  axis(4)
}

# Plot the Scree
PlotScreeFix(ev = resBADA$TExPosition.Data$eigs,
          p.ev = NULL, 
          alpha = 0.05,
          col.ns = "#006D2C", col.sig = "#54278F",
          title = "Explained Variance per Dimension",
          plotKaiser = TRUE)
       
```

### BADA - Graphs for the J set

```{r BADA - J set Graphs}

baseMap.j <- PTCA4CATA::createFactorMap(Fj,
                            col.points   = col4bada,
                            alpha.points =  1,
                            cex = 4,
                            col.labels   = col4bada)
# A graph for the J-set
aggMap.j <- baseMap.j$zeMap_background + baseMap.j$zeMap_dots + baseMap.j$zeMap_text

print(aggMap.j)

# Add Lines
zeLines <- ggplot2::annotate("segment", x = c(0), y = c(0),
                    xend = Fj[,1],
                    yend = Fj[,2],
                    color = col4bada,
                    alpha = .7,
                    arrow = arrow(length = unit(.3, "cm") ) )

# Create the map by adding background, labels, and arrows:
aggMap.j.arrows <- baseMap.j$zeMap_background +
                                      zeLines + baseMap.j$zeMap_text

print(aggMap.j.arrows)
```

### BADA - Graphs for the I set
```{r BADA - I set Graphs}

# Create the base map
baseMap.i <- PTCA4CATA::createFactorMap(Fi,
                                        col.points   = COLPAL$rows$SES$color_obs,
                                        alpha.points =  .6)

# Plain map with color for the I-set
aggMap.i <- baseMap.i$zeMap_background + baseMap.i$zeMap_dots

print(aggMap.i)

# Color for the means
col4Means <- COLPAL$rows$SES$color_groups

rownames(Fk) <- c("Class 3", "Class 1", "Class 2", "Class 4", "Class 5")

# create the map for the means
MapGroup    <- PTCA4CATA::createFactorMap(Fk,
                                axis1 = 1, axis2 = 2,
                                constraints = baseMap.i$constraints,
                                title = NULL,
                                col.points = col4Means,
                                display.points = TRUE,
                                pch = 19, cex = 6.5,
                                display.labels = TRUE,
                                alpha.points = 1,
                                alpha.labels = 1,
                                col.labels = col4Means,
                                text.cex = 5,
                                font.face = "bold",
                                font.family = "sans",
                                col.axes = "darkorchid",
                                alpha.axes = 0.2,
                                width.axes = 1.1,
                            col.background = adjustcolor("lavender",
                                                      alpha.f = 0.2),
                            force = 1, segment.size = 0)

# The map with observations and group means
aggMap.i.withMeans <- aggMap.i+
  MapGroup$zeMap_dots + MapGroup$zeMap_text

print(aggMap.i.withMeans)

## BADA - Confusion matrices - Confidence Interval Plots
fixedCM   <- resBADA.inf$Inference.Data$loo.data$fixed.confuse
looedCM   <- resBADA.inf$Inference.Data$loo.data$loo.confuse

# Create Confidence Interval Plots
BootCube <- resBADA.inf$Inference.Data$boot.data$fi.boot.data$boots
dimnames(BootCube)[[2]] <- c("Dimension 1","Dimension 2")
GraphElli <- MakeCIEllipses(BootCube[,1:2,],
                  names.of.factors = c("Dimension 1","Dimension 2"),
                  col = col4Means,
                  p.level = .95
)

# create the I-map with Observations, means and confidence intervals
aggMap.i.withCI <-  aggMap.i +  GraphElli + MapGroup$zeMap_dots + MapGroup$zeMap_text

print(aggMap.i.withCI)

# Create 75% Tolerance interval polygons
GraphTI.Hull.90 <- MakeToleranceIntervals(Fi,
                                  as.factor(oasis_cross_sectional$SES),
                                  names.of.factors = c("Dim1","Dim2"),
                                  col = col4Means,
                                  line.size = .5, line.type = 3,
                                  alpha.ellipse = .2,
                                  alpha.line = .4,
                                  p.level = .75, 
                                  type = 'hull' 
                                  )

aggMap.i.withHull <- aggMap.i +
  GraphTI.Hull.90 + MapGroup$zeMap_dots +
  MapGroup$zeMap_text +  MapGroup$zeMap_dots

print(aggMap.i.withHull)
```

## BADA - Contribution Bars I-set

```{r BADA - Contribution Bars I-set}

## Signed Contributions
signed.ctrI <- resBADA$TExPosition.Data$ci * sign(resBADA$TExPosition.Data$fi)
rownames(signed.ctrI) <- c("Class 3", "Class 1", "Class 2", "Class 4", "Class 5")
signed.ctrI <- signed.ctrI[order(rownames(signed.ctrI)),]

## Contribution 1
ctrI.s.1 <- PrettyBarPlot2(signed.ctrI[,1],
                         threshold = 1 / NROW(signed.ctrI),
                         font.size = 5,
                         color4bar = COLPAL$rows$SES$color_groups, 
                         main = 'OASIS BADA:  I set - Variable Contribution 1 (Signed)',
                         ylab = 'Contributions',
                         ylim = c(1.2*min(signed.ctrI), 1.2*max(signed.ctrI))
)
print(ctrI.s.1)

## Contribution 2 
ctrI.s.2 <- PrettyBarPlot2(signed.ctrI[,2],
                           threshold = 1 / NROW(signed.ctrI),
                           font.size = 5,
                           color4bar = COLPAL$rows$SES$color_groups, 
                           main = 'OASIS BADA: I set - Variable Contribution 2 (Signed)',
                           ylab = 'Contributions',
                           ylim = c(1.2*min(signed.ctrI), 1.2*max(signed.ctrI))
)
print(ctrI.s.2)

## Contribution 3 
ctrI.s.3 <- PrettyBarPlot2(signed.ctrI[,3],
                           threshold = 1 / NROW(signed.ctrI),
                           font.size = 5,
                           color4bar = COLPAL$rows$SES$color_groups, 
                           main = 'OASIS BADA: I set - Variable Contribution 3 (Signed)',
                           ylab = 'Contributions',
                           ylim = c(1.2*min(signed.ctrI), 1.2*max(signed.ctrI))
)
print(ctrI.s.3)

```
## BADA - Contribution Bars J-set

```{r BADA - Contribution Bars J-set}

## Signed Contributions
signed.ctrJ <- resBADA$TExPosition.Data$cj * sign(resBADA$TExPosition.Data$fj)

## Contribution 1
ctrJ.s.1 <- PrettyBarPlot2(signed.ctrJ[,1],
                         threshold = 1 / NROW(signed.ctrJ),
                         font.size = 5,
                         color4bar = gplots::col2hex(col4bada), 
                         main = 'OASIS BADA:  J set - Variable Contribution 1 (Signed)',
                         ylab = 'Contributions',
                         ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)

print(ctrJ.s.1)

## Contribution 2
ctrJ.s.2 <- PrettyBarPlot2(signed.ctrJ[,2],
                         threshold = 1 / NROW(signed.ctrJ),
                         font.size = 5,
                         color4bar = gplots::col2hex(col4bada), 
                         main = 'OASIS BADA:  J set - Variable Contribution 2 (Signed)',
                         ylab = 'Contributions',
                         ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)

print(ctrJ.s.2)

## Contribution 3
ctrJ.s.3 <- PrettyBarPlot2(signed.ctrJ[,3],
                         threshold = 1 / NROW(signed.ctrJ),
                         font.size = 5,
                         color4bar = gplots::col2hex(col4bada), 
                         main = 'OASIS BADA:  J set - Variable Contribution 3 (Signed)',
                         ylab = 'Contributions',
                         ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)

print(ctrJ.s.3)

```


## BADA - Bootstrap Ratios I-set

```{r BADA - Bootstrap Ratios I-set}

BR.i <- resBADA.inf$Inference.Data$boot.data$fi.boot.data$tests$boot.ratios
rownames(BR.i) <- c("Class 3", "Class 1", "Class 2", "Class 4", "Class 5")
BR.i <- BR.i[order(rownames(BR.i)),]

laDim = 1
i.BR1 <- PrettyBarPlot2(BR.i[,laDim],
                        threshold = 2,
                        font.size = 5,
                   color4bar = COLPAL$rows$SES$color_groups,
                   main = paste0(
                     'BADA on the Oasis: I-set Bootstrap ratio Dimension ',laDim),
                  ylab = 'Bootstrap ratios'
                  )
print(i.BR1)

laDim = 2
i.BR2 <- PrettyBarPlot2(BR.i[,laDim],
                        threshold = 2,
                        font.size = 5,
                   color4bar = COLPAL$rows$SES$color_groups,
                   main = paste0(
                     'BADA on the Oasis: I-set Bootstrap ratio Dimension ',laDim),
                  ylab = 'Bootstrap ratios'
                  )
print(i.BR2)
```

## BADA - Bootstrap J-set

```{r BADA - Bootstrap J-set}

BR.j <- resBADA.inf$Inference.Data$boot.data$fj.boot.data$tests$boot.ratios

laDim = 1
j.BR1 <- PrettyBarPlot2(BR.j[,laDim],
                        threshold = 2,
                        font.size = 5,
                   color4bar = gplots::col2hex(col4bada),
                   main = paste0(
                     'BADA on the Oasis: J-set Bootstrap ratio Dimension ',laDim),
                  ylab = 'Bootstrap ratios'
                  )
print(j.BR1)

laDim = 2
j.BR2 <- PrettyBarPlot2(BR.j[,laDim],
                        threshold = 2,
                        font.size = 5,
                   color4bar = gplots::col2hex(col4bada),
                   main = paste0(
                     'BADA on the Oasis: J-set Bootstrap ratio Dimension ',laDim),
                  ylab = 'Bootstrap ratios'
                  )
print(j.BR2)

```
