library(easyCODA)

## Examples
# Data frame fish has sex, habitat and mass in first columns, 
# then morphometric data in remaining columns
data(fish)
sex     <- fish[,1]
habitat <- fish[,2]
mass    <- fish[,3]
fishm   <- as.matrix(fish[,4:29])
# Convert to compositional data matrix
fishm   <- fishm / apply(fishm, 1, sum)
# Compute logarithm of mass and interaction of sex (F/M) and habitat (L/P) categories
logmass <- log(mass)
sexhab  <- 2*(sex-1)+habitat
sexhab.names <- c("FL","FP","ML","MP")
rownames(fishm) <- sexhab.names[sexhab]
# Create dummy variables for sexhab and create matrix of covariates
sexhab.Z <- DUMMY(sexhab, catnames=sexhab.names)
vars     <- cbind(logmass, sexhab.Z)
# Perform RDA on centred logratios
require(ca)
fish.RDA <- RDA(CLR(fishm), cov=vars)
# Plot results 
# (for more options see Appendix of Compositional Data Analysis in Practice)
PLOT.RDA(fish.RDA, map="contribution", rescale=0.05, indcat=2:5, 
         colrows=rainbow(4, start=0.1, end=0.8)[sexhab], cexs=c(0.8,0.8,1))


# reference : https://pmassicotte.github.io/stats-denmark-2019/07_rda.html#/
# 'vegan' package
library(vegan)

data(varespec)
data(varechem)
## Common but bad way: use all variables you happen to have in your
## environmental data matrix
vare.cca <- cca(varespec, varechem)
vare.cca
plot(vare.cca)
## Formula interface and a better model
vare.cca <- cca(varespec ~ Al + P*(K + Baresoil), data=varechem)
vare.cca
plot(vare.cca)
## Partialling out and negative components of variance
cca(varespec ~ Ca, varechem)
cca(varespec ~ Ca + Condition(pH), varechem)
## RDA
data(dune)
data(dune.env)
dune.Manure <- rda(dune ~ Manure, dune.env)
plot(dune.Manure) 

# reference : https://popgen.nescent.org/2018-03-27_RDA_GEA.html


# reference : https://r.qcbs.ca/workshop10/book-en/redundancy-analysis.html

data1 <- read.csv("C:/Users/User/Desktop/논문데이터/redundancy analysis 1 average.csv",sep=",",header=T)
data2 <- read.csv("C:/Users/User/Desktop/논문데이터/redundancy analysis 2 average.csv",sep=",",header=T) 

species_model <- rda(data1[,-c(1,2,3)] ~ BOD + COD + TN + TP + TOC +
                       SS + EC + pH + DO + Temperature + Turbidity +
                       Transparency + Chla + LowWaterLevel + Inflow +
                       Discharge + Reservoir, data=data2)
summary(species_model)

plot(species_model)
# Type 1 scaling : shows similarities between objects in the response matrix 
ordiplot(species_model, scaling = 1, type = "text")
# Type 2 scaling : shows the effects of explanatory variables
ordiplot(species_model, scaling = 2, type = "text", frame = FALSE)


# Custom triplot code!

# Type 1

## extract % explained by the first 2 axes
perc <- round(100*(summary(species_model)$cont$importance[2, 1:2]), 1)

## extract scores - these are coordinates in the RDA space
sc_si <- scores(species_model, display="sites", choices=c(1,2), scaling=1)
sc_sp <- scores(species_model, display="species", choices=c(1,2), scaling=1)
sc_bp <- scores(species_model, display="bp", choices=c(1,2), scaling=1)

## Custom triplot, step by step

# Set up a blank plot with scaling, axes, and labels (part1)
plot(species_model,
     scaling = 1, # set scaling type 
     type = "none", # this excludes the plotting of any points from the results
     frame = FALSE,
     # set axis limits
     xlim = c(-70,10), 
     ylim = c(-20,60),
     # label the plot (title, and axes)
     main = "RDA result (shows similarities between objects in the algae species)",
     xlab = paste0("RDA1 (", perc[1], "%)"), 
     ylab = paste0("RDA2 (", perc[2], "%)") 
)

# add points for site scores
points(sc_si, 
       pch = 21, # set shape (here, circle with a fill colour)
       col = "black", # outline colour
       bg = "steelblue", # fill colour
       cex = 1.2) # size
# add arrows for effects of the explanatory variables
arrows(0,0, # start them from (0,0)
       sc_bp[,1]*100, sc_bp[,2]*100, # end them at the score value
       col = "brown", 
       lwd = 1)
# add text labels for arrows
text(x = (sc_bp[,1] - 0.05)*100, # adjust text coordinate to avoid overlap with arrow tip
     y = (sc_bp[,2] - 0.05)*100, 
     labels = rownames(sc_bp), 
     col = "red", 
     cex = 1, 
     font = 2)
# add points for species scores
points(sc_sp, 
       pch = 22, # set shape (here, square with a fill colour)
       col = "black",
       bg = "green", 
       cex = 1.2)
# add text labels for species abbreviations
text((sc_sp + c(0.03, 0.09)), # adjust text coordinates to avoid overlap with points 
     labels = rownames(sc_sp), 
     col = "navyblue", 
     font = 2, # bold
     cex = 1)


# Type 2

## extract % explained by the first 2 axes
perc <- round(100*(summary(species_model)$cont$importance[2, 1:2]), 1)

## extract scores - these are coordinates in the RDA space
sc_si <- scores(species_model, display="sites", choices=c(1,2), scaling=2)
sc_sp <- scores(species_model, display="species", choices=c(1,2), scaling=2)
sc_bp <- scores(species_model, display="bp", choices=c(1,2), scaling=2)

## Custom triplot, step by step

# Set up a blank plot with scaling, axes, and labels
plot(species_model,
     scaling = 2, # set scaling type 
     type = "none", # this excludes the plotting of any points from the results
     frame = FALSE,
     # set axis limits
     xlim = c(-1,1), 
     ylim = c(-1,1),
     # label the plot (title, and axes)
     main = "RDA result (shows the effects of measurement variables)",
     xlab = paste0("RDA1 (", perc[1], "%)"), 
     ylab = paste0("RDA2 (", perc[2], "%)") 
)
# add points for site scores
points(sc_si, 
       pch = 21, # set shape (here, circle with a fill colour)
       col = "black", # outline colour
       bg = "steelblue", # fill colour
       cex = 1.2) # size
# add points for species scores
points(sc_sp, 
       pch = 22, # set shape (here, square with a fill colour)
       col = "black",
       bg = "green", 
       cex = 1.2)
# add text labels for species abbreviations
text((sc_sp + c(0.03, 0.09)), # adjust text coordinates to avoid overlap with points 
     labels = rownames(sc_sp), 
     col = "navyblue", 
     font = 2, # bold
     cex = 1)
# add arrows for effects of the explanatory variables
arrows(0,0, # start them from (0,0)
       sc_bp[,1]*1.5, sc_bp[,2]*1.5, # end them at the score value
       col = "brown", 
       lwd = 1)
# add text labels for arrows
text(x = (sc_bp[,1] - 0.05)*1.5, # adjust text coordinate to avoid overlap with arrow tip
     y = (sc_bp[,2] - 0.05)*1.5, 
     labels = rownames(sc_bp), 
     col = "red", 
     cex = 1, 
     font = 2)
