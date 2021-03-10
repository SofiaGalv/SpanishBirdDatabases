
### STATISTICAL ANALYSIS ------------------------------------------------------

### 1. Statistical comparison between databases for continuous variables 

## ANOVA test for the three variables

  #Geographical distribution ANOVA

data <- c(MNCN$Occurrence.in.Sp, EBD$Occurrence.in.Sp, IEET$Occurrence.in.Sp,
          SEO$Occurrence.in.Sp, ebird$Occurrence.in.Sp, avis_final)
categories <- as.factor(c(rep(1,nrow(MNCN)), rep(2, nrow(EBD)), 
                          rep(3, nrow(IEET)), rep(4,nrow(SEO)), 
                          rep(5, nrow(ebird)), rep(6, length(avis_final))))

PC.data <- data.frame(data, categories)
str(PC.data)
res1 <- lm(PC.data$data ~ PC.data$categories)
anova(res1)

  #Weight ANOVA

data <- c(MNCN$Weight, EBD$Weight, IEET$Weight, SEO$Weight, ebird$Weight,
          avis_final_Weight)
categories <- as.factor(c(rep(1,nrow(MNCN)), rep(2, nrow(EBD)), 
                          rep(3, nrow(IEET)), rep(4, nrow(SEO)), 
                          rep(5,nrow(ebird)), rep(6, length(avis_final_Weight))))

PC.data <- data.frame(data, categories)
str(PC.data)
res1 <- lm(PC.data$data ~ PC.data$categories)
anova(res1)

  #Windspan ANOVA

data <- c(MNCN$Wingspan, EBD$Wingspan, IEET$Wingspan, SEO$Wingspan, 
          ebird$Wingspan, avis_final_wingspan)
categories <- as.factor(c(rep(1,nrow(MNCN)), rep(2, nrow(EBD)), 
                          rep(3, nrow(IEET)), rep(4, nrow(SEO)), 
                          rep(5,nrow(ebird)), rep(6, length(avis_final_wingspan))))

PC.data <- data.frame(data, categories)
str(PC.data)
res1 <- lm(PC.data$data ~ PC.data$categories)
anova(res1)


## Bonferroni post-hoc tests.
  # Select six random samples of 1000 observations per database. 
  # Repeat this step six times to obtain the samples for each variable studied.

Select_rows <- sample(1:nrow(MNCN), 1000)
Sample1_MNCN <- MNCN[Select_rows, ]

Select_rows <- sample(1:nrow(EBD), 1000)
Sample1_EBD <- EBD[Select_rows, ]

Select_rows <- sample(1:nrow(IEET), 1000)
Sample1_IEET <- IEET[Select_rowss, ]

Select_rows <- sample(1:nrow(SEO), 1000)
Sample1_SEO <- SEO[Select_rows, ]

Select_rows <- sample(1:nrow(ebird), 1000)
Sample1_ebird <- ebird[Select_rows, ]

Select_rows <- sample(1:length(avis_final), 1000)
Sample1_AVIS <- avis_final[Select_rows]

# Perform Bonferroni post-hoc tests (repeat six times per variable, with each 
## group of 6 random samples of the databases).

  #Bonferroni tests with "Geographical distribution" variable.

occurrences <- c(Sample1_MNCN$Occurrence.in.Sp, Sample1_EBD$Occurrence.in.Sp, 
               Sample1_IEET$Occurrence.in.Sp, Sample1_SEO$Occurrence.in.Sp, 
               Sample1_ebird$Occurrence.in.Sp, Sample1_AVIS)
cases <- c(rep("Sample1_MNCN", length(Sample1_MNCN$Occurrence.in.Sp)), 
           rep("Sample1_EBD", length(Sample1_EBD$Occurrence.in.Sp)),
           rep("Sample1_IEET", length(Sample1_IEET$Occurrence.in.Sp)),
           rep("Sample1_SEO", length(Sample1_SEO$Occurrence.in.Sp)),
           rep("Sample1_ebird", length(Sample1_ebird$Occurrence.in.Sp)),
           rep("Sample1_AVIS", length(Sample1_AVIS)))
pairwise.t.test(occurrences, cases, p.adj ="bonferroni")

  #Bonferroni tests with "Weight" variable.

avis_final_Weight <- rep(AVIS$Weight,AVIS$Observaciones)

Select_rows <- sample(1:nrow(MNCN), 1000)
Sample1_MNCN <- MNCN[Select_rows, ]

Select_rows <- sample(1:nrow(EBD), 1000)
Sample1_EBD <- EBD[Select_rows, ]

Select_rows <- sample(1:nrow(IEET), 1000)
Sample1_IEET <- IEET[Select_rows, ]

Select_rows <- sample(1:nrow(SEO), 1000)
Sample1_SEO <- SEO[Select_rows, ]

Select_rows <- sample(1:nrow(ebird), 1000)
Sample1_ebird <- ebird[Select_rows, ]

Select_rows <- sample (1:length(avis_final_Weight), 1000)
Sample1_AVIS <- avis_final_Weight[Select_rows]

weight <- c(Sample1_MNCN$Weight, Sample1_EBD$Weight, Sample1_IEET$Weight,
           Sample1_SEO$Weight, Sample1_ebird$Weight, Sample1_AVIS)
cases <- c(rep("Sample1_MNCN", length(Sample1_MNCN$Weight)), 
          rep("Sample1_EBD", length(Sample1_EBD$Weight)),
          rep("Sample1_IEET", length(Sample1_IEET$Weight)),
          rep("Sample1_SEO", length(Sample1_SEO$Weight)),
          rep("Sample1_ebird", length(Sample1_ebird$Weight)),
          rep("Sample1_AVIS", length(Sample1_AVIS)))
pairwise.t.test(weight, cases, p.adj ="bonferroni")

  #Bonferroni tests with "Wingspan" variable.

avis_final_wingspan <- rep (AVIS$Wingspan,AVIS$Observaciones)

Select_rows <- sample(1:nrow(MNCN), 1000)
Sample1_MNCN <- MNCN[Select_rows, ]

Select_rows <- sample(1:nrow(EBD), 1000)
Sample1_EBD <- EBD[Select_rows, ]

Select_rows <- sample(1:nrow(IEET), 1000)
Sample1_IEET <- IEET[Select_rows, ]

Select_rows <- sample(1:nrow(SEO), 1000)
Sample1_SEO <- SEO[Select_rows, ]

Select_rows <- sample(1:nrow(ebird), 1000)
Sample1_ebird <- ebird[Select_rows, ]

Select_rows <- sample(1:length(avis_final_wingspan), 1000)
Sample1_AVIS <- avis_final_wingspan[Select_rows]

wingspan <- c(muestra1_MNCN$Wingspan, muestra1_EBD$Wingspan,
               muestra1_IEET$Wingspan, muestra1_SEO$Wingspan, 
                muestra1_ebird$Wingspan, muestra1_AVIS)

cases <- c( rep("Sample1_MNCN", length(Sample1_MNCN$Wingspan)), 
           rep("Sample1_EBD", length(Sample1_EBD$Wingspan)),
           rep("Sample1_IEET", length(Sample1_IEET$Wingspan)),
           rep("Sample1_SEO", length(Sample1_SEO$Wingspan)),
           rep("Sample1_ebird", length(Sample1_ebird$Wingspan)),
           rep("Sample1_AVIS", length(Sample1_AVIS)))
pairwise.t.test(wingspan, cases, p.adj ="bonferroni")


### 2. Statistical analysis for categorical variables

## We create three ".txt" files with the contingency tables to perform 
## Chi-Square tests: two for "Threat" variable (one with EBD database and one
## without it, dividing the databases into professional and citizen), and one 
## for "Breeding habitats" variables.

  # Chi-Square test for "Threat" variable, including EBD database.

Threat <- read.table("Threat.txt", header = T, sep = ",")
chisq.test(Threat, correct = F)

  # Chi-Square test for "Threat" variable, excluding EBD database.

Threat <- read.table("Threat2.txt", header = T, sep = ",")
chisq.test(Amenaza, correct = F)

  # Chi-Square test for "Breeding habitats" variables

Habitat <- read.table("Habitat.txt", header = T, sep = ",")
chisq.test(Habitat, correct = F)


### 3. GLM for the number of observations of species.

## We exclude wingspan variable (as it is correlated with weight (r = 0.82).

  # Correlation test between Weight and Wingspan

cor(x = table_DAmico$Weight, y = table_DAmico$Wingspan)
plot(able_DAmico$Weight ~ able_DAmico$Wingspan)

  # GLM for MNCN database

datos <- MNCN2
modelo1 <- step(glm(datos$Observaciones ~ datos$Weight + datos$Wingspan + 
                 datos$Occurrence.in.Sp + datos$Status.in.Sp + datos$Wetland +
                 datos$Farmland + datos$Agroforest + datos$Forest + 
                 datos$Scrubland + datos$Cliff, family = quasipoisson())) 

modelo2 <- step(glm(datos$Observaciones ~ datos$Weight + datos$Occurrence.in.Sp +
                    datos$Amenazada + datos$Wetland + datos$Accesible, 
                    family = poisson()))
summary(modelo1)
summary(modelo2)
  
  #GLM for EBD database

datos <- EBD2
modelo1<- step(glm(datos$Observaciones ~ datos$Weight + datos$Wingspan + 
                datos$Occurrence.in.Sp + datos$Status.in.Sp + datos$Wetland +
                datos$Farmland + datos$Agroforest + datos$Forest + 
                datos$Scrubland + datos$Cliff, family = quasipoisson())) 

modelo2 <- step(glm(datos$Observaciones ~ datos$Weight + datos$Occurrence.in.Sp + 
                    datos$Amenazada + datos$Wetland + datos$Accesible, 
                    family = poisson())) 
summary(modelo1)
summary(modelo2)

  #GLM for SEO database

datos <- SEO2
modelo1 <- step(glm(datos$Observaciones ~ datos$Weight + datos$Wingspan + 
                  datos$Occurrence.in.Sp + datos$Status.in.Sp + datos$Wetland +
                  datos$Farmland + datos$Agroforest + datos$Forest + 
                  datos$Scrubland + datos$Cliff, family = poisson())) 

modelo2 <- step(glm(datos$Observaciones ~ datos$Weight + datos$Occurrence.in.Sp + 
                    datos$Amenazada + datos$Wetland + datos$Accesible, 
                    family = poisson())) 
summary(modelo1)
summary(modelo2)

  #GLM for ebird database

datos <- ebird2
modelo1 <- step(glm(datos$Observaciones ~ datos$Weight + datos$Wingspan + 
                  datos$Occurrence.in.Sp + datos$Status.in.Sp + datos$Wetland +
                  datos$Farmland + datos$Agroforest + datos$Forest + 
                  datos$Scrubland + datos$Cliff, family=poisson())) 

modelo2 <- step(glm(datos$Observaciones ~ datos$Weight + datos$Occurrence.in.Sp + 
                    datos$Amenazada + datos$Wetland + datos$Accesible, 
                    family=poisson())) 

summary(modelo1)
summary(modelo2)
  
  # GLM for AVIS database

datos <-AVIS
modelo1 <- step(glm(AVIS$Observaciones ~ AVIS$Weight + AVIS$Wingspan + 
                      AVIS$Occurrence.in.Sp + AVIS$Status.in.Sp + AVIS$Wetland +
                      AVIS$Farmland + AVIS$Agroforest + AVIS$Forest + 
                      AVIS$Scrubland + AVIS$Cliff, family = poisson()))
modelo2 <- step(glm(datos$Observaciones ~ datos$Weight + datos$Occurrence.in.Sp + 
                    datos$Amenazada + datos$Wetland + datos$Accesible, 
                    family = poisson()))
summary(modelo1)
summary(modelo2)
