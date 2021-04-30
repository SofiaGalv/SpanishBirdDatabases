
### CART TREES --------------------------------------------------------------

library(rpart.plot)
library(rpart)

## For building CART trees, we used the databases that gather the occurrences grouped by species.

  ##MNCN CART tree

model <- rpart(Observaciones ~ Weight + Wingspan + Occurrence.in.Sp + 
                 Amenazada + Wetland + Farmland  + Cliff + Forest + Scrubland +
                 Agroforest, data = MNCN2, method = "anova" )
print(model)
par(xpd = NA)
rpart.plot(model, type = 3, digits = 3, fallen.leaves = TRUE)

names(model) <- c("Weight", "Windspan", "Geographical distribution", 
                  "Endangered", "Wetlands", "Farmland", "Cliffs", "Forest", 
                  "Scrubland", "Agroforest")

labels = c("Weight", "Windspan", "Geographical distribution", "Endangered", 
           "Wetlands", "Farmland", "Cliffs", "Forest", "Scrubland", 
           "Agroforest", at = c(1:6), las = 2)
modellabels = c("Weight", "Windspan", "Geographical distribution", "Endangered",
                "Wetlands", "Farmland", "Cliffs", "Forest", "Scrubland", 
                "Agroforest")

  #EBD CART tree

model <- rpart(Observaciones ~ Weight + Wingspan + Occurrence.in.Sp + 
                 Endangered + Wetland + Farmland  + Cliff + Forest + Scrubland +
                 Agroforest, data = EBD2, method = "anova" )
par(xpd = NA)
rpart.plot(model, type = 3, digits = 3, fallen.leaves = TRUE)

  #SEO CART tree

model <- rpart(Observaciones ~ Weight + Wingspan + Occurrence.in.Sp + 
                 Endangered + Wetland + Farmland  + Cliff + Forest + Scrubland +
                 Agroforest, data = SEO2, method = "anova" )
par(xpd = NA)
rpart.plot(model, type = 3, digits = 3, fallen.leaves = TRUE)

  #eBird CART tree

model <- rpart(Observaciones ~ Weight + Wingspan + Occurrence.in.Sp + 
                 Endangered + Wetland + Farmland  + Cliff + Forest + Scrubland +
                 Agroforest, data = ebird2, method = "anova" )
par(xpd = NA)
rpart.plot(model, type = 3, digits = 3, fallen.leaves = TRUE)

  #AVIS CART tree

model <- rpart(Observaciones ~ Weight + Wingspan + Occurrence.in.Sp + 
                 Endangered + Wetland + Farmland  + Cliff + Forest + Scrubland +
                 Agroforest, data = AVIS, method = "anova" )
par(xpd = NA)
rpart.plot(model, type = 3, digits = 3, fallen.leaves = TRUE)
