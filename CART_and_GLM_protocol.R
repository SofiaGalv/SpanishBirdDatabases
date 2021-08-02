################################################################################
###################### GLM AND CART TREES PROTOTOL #############################
################################################################################

###### CART TREES ##############################################################

library(rpart.plot)
library(rpart)

## For building CART trees, we used the databases that gather the occurrences grouped by species.

  ##MNCN CART tree

CART_MNCN <- rpart(Observaciones ~ Weight + Wingspan + Occurrence.in.Sp + 
                 Amenazada + Wetland + Farmland  + Cliff + Forest + Scrubland +
                 Agroforest, data = MNCN2, method = "anova" )
print(CART_MNCN)
par(xpd = NA)
rpart.plot(CART_MNCN, type = 3, digits = 3, fallen.leaves = TRUE)

  #EBD CART tree

CART_EBD <- rpart(Observaciones ~ Weight + Wingspan + Occurrence.in.Sp + 
                 Endangered + Wetland + Farmland  + Cliff + Forest + Scrubland +
                 Agroforest, data = EBD2, method = "anova" )
par(xpd = NA)
rpart.plot(CART_EBD, type = 3, digits = 3, fallen.leaves = TRUE)

  #SEO CART tree

CART_SEO <- rpart(Observaciones ~ Weight + Wingspan + Occurrence.in.Sp + 
                 Endangered + Wetland + Farmland  + Cliff + Forest + Scrubland +
                 Agroforest, data = SEO2, method = "anova" )
par(xpd = NA)
rpart.plot(CART_SEO, type = 3, digits = 3, fallen.leaves = TRUE)

  #eBird CART tree

CART_EBIRD <- rpart(Observaciones ~ Weight + Wingspan + Occurrence.in.Sp + 
                 Endangered + Wetland + Farmland  + Cliff + Forest + Scrubland +
                 Agroforest, data = ebird2, method = "anova" )
par(xpd = NA)
rpart.plot(CART_EBIRD, type = 3, digits = 3, fallen.leaves = TRUE)

  #AVIS CART tree

CART_AVIS <- rpart(Observaciones ~ Weight + Wingspan + Occurrence.in.Sp + 
                 Endangered + Wetland + Farmland  + Cliff + Forest + Scrubland +
                 Agroforest, data = AVIS, method = "anova" )
par(xpd = NA)
rpart.plot(CART_AVIS, type = 3, digits = 3, fallen.leaves = TRUE)


###### GENERALIZED LINEAR MODELS FOR THE NUMBER OF SPECIES OBSERVATIONS ########

## 1. Test the correlation between weight and wingspan.
cor(x = damico$Weight, y = damico$Wingspan)
plot(damico$Weight ~ damico$Wingspan) #r = 0.82

## 2. Perform GLMs

# MNCN database
datos <- MNCN2
model_MNCN <- glm(datos$Observaciones ~ datos$Weight +
                    datos$Occurrence.in.Sp +
                    datos$Endangered + datos$Wetland +
                    datos$Accesible,
                  family = poisson())
summary(modelo_MNCN)

#EBD database
datos <- EBD2
model_EBD <- glm(datos$Observaciones ~ datos$Weight +
                   datos$Occurrence.in.Sp +
                   datos$Endangered + datos$Wetland +
                   datos$Accesible,
                 family = poisson())
summary(model_EBD)

#SEO database
datos <- SEO2
model_SEO <- glm(datos$Observaciones ~ datos$Weight +
                   datos$Occurrence.in.Sp +
                   datos$Endangered + datos$Wetland +
                   datos$Accesible,
                 family = poisson())
summary(model_SEO)

#ebird database
datos <- ebird2
modelo2 <- glm(datos$Observaciones ~ datos$Weight +
                 datos$Occurrence.in.Sp +
                 datos$Endangered + datos$Wetland +
                 datos$Accesible,
               family=poisson())
summary(model_eBird)

# GLM for AVIS database
datos <-AVIS
model_AVIS <- glm(datos$Observaciones ~ datos$Weight +
                    datos$Occurrence.in.Sp +
                    datos$Endangered + datos$Wetland +
                    datos$Accesible,
                  family = poisson())
summary(model_AVIS)