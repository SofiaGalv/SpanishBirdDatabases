##########################################################################
######################### HISTOGRAMS PROTOCOL ############################
##########################################################################

library(car)
library(Hmisc)

## 1. First, we tested the normality and homocedasticity (homogeneity of 
# variance) of our samples. For Levene test, we took 1000 random subsamples,
# as the function only allow for a maximum of 5000 data.

# For geographical distribution:

  # Random samples of 1000 observations

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

Select_rows <- sample(1:nrow(AVIS), 1000) 
Sample1_AVIS <- AVIS[Select_rows]

  # Test the normality of the samples. 

shapiro.test(Sample1_MNCN$Occurrence.in.Sp)
shapiro.test(Sample1_EBD$Occurrence.in.Sp)
shapiro.test(Sample1_IEET$Occurrence.in.Sp)
shapiro.test(Sample1_SEO$Occurrence.in.Sp)
shapiro.test(Sample1_ebird$Occurrence.in.Sp)
shapiro.test(Sample1_AVIS$Ocurrence.in.Sp)
      # None of the samples follow a normal distribution.

  # Levene test to test the homocedasticity (equal variances)

Occurrences <- c(Sample1_MNCN$Occurrence.in.Sp, Sample1_EBD$Occurrence.in.Sp,
                 Sample1_IEET$Occurrence.in.Sp, Sample1_SEO$Occurrence.in.Sp,
                 Sample1_ebird$Occurrence.in.Sp, Sample1_AVIS$Ocurrence.in.Sp)

Ddbb <- c(rep("MNCN",1000), rep("EBD",1000), rep("IEET",1000),
          rep("SEO",1000), rep("eBird",1000), rep("AVIS", 1000))

Geodistribution <- data.frame(Occurrences,Ddbb)                          

leveneTest(Occurrences ~ Ddbb, Geodistribution)
class(Geodistribution$Ddbb)
      # The test reveals a p-value smaller than 0.05, indicating that there 
      # is significant difference between the group variances in the databases.

# For weight:

  # Test the normality of the samples.

shapiro.test(Sample1_MNCN$LogWeight)
shapiro.test(Sample1_EBD$Weight)
shapiro.test(Sample1_IEET$Weight)
shapiro.test(Sample1_SEO$Weight)
shapiro.test(Sample1_ebird$Weight)
shapiro.test(Sample1_AVIS$Weight)
      # None of the samples follow a normal distribution

  # Levene test to test the homocedasticity (equal variances)

Weight <- c(Sample1_MNCN$Weight, Sample1_EBD$Weight,
            Sample1_IEET$Weight, Sample1_SEO$Weight,
            Sample1_ebird$Weight, Sample1_AVIS$Weight)

Ddbb <- c(rep("MNCN",1000), rep("EBD",1000), rep("IEET",1000),
          rep("SEO",1000), rep("eBird",1000), rep("AVIS", 1000))

Weight_df <- data.frame(Weight,Ddbb)                          
Weight_df$Ddbb <- as.factor(Weight_df$Ddbb)
leveneTest(Weight ~ Ddbb, Weight_df)
      # The test reveals a p-value smaller than 0.05, indicating that there 
      # is significant difference between the group variances in the databases.

# For wingspan:

  # Test the normality of the samples.

shapiro.test(Sample1_MNCN$Wingspan)
shapiro.test(Sample1_EBD$Wingspan)
shapiro.test(Sample1_IEET$Wingspan)
shapiro.test(Sample1_SEO$Wingspan)
shapiro.test(Sample1_ebird$Wingspan)
shapiro.test(Sample1_AVIS$Wingspan)
    # None of the samples follow a normal distribution

  # Levene test to test the homocedasticity (equal variances)

Wingspan <- c(Sample1_MNCN$Wingspan, Sample1_EBD$Wingspan,
              Sample1_IEET$Wingspan, Sample1_SEO$Wingspan,
              Sample1_ebird$Wingspan, Sample1_AVIS$Wingspan)

Ddbb <- c(rep("MNCN",1000), rep("EBD",1000), rep("IEET",1000),
          rep("SEO",1000), rep("eBird",1000), rep("AVIS", 1000))
Wingspan_df$Ddbb <- as.factor(Wingspan_df$Ddbb)
Wingspan_df <- data.frame(Wingspan,Ddbb)
leveneTest(Wingspan ~ Ddbb, Wingspan_df, center = "median")
hartleyTest(Wingspan ~ Ddbb, Wingspan_df, center = "median")
      # The test reveals a p-value smaller than 0.05, indicating that there 
      # is significant difference between the group variances in the databases.

## HISTOGRAMS

  # Making high resolution plots (300ppi): (1)Geographical distribution, 
  # (2) Weight, (3) Wingspan, (4) log(weight), (5) log(wigspan).

# MNCN
tiff("MNCNGeodistirbution.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(MNCN$Occurrence.in.Sp, main = "",
     xlab="Geographical distirbution (%)", 
     border="bisque4", 
     col="bisque3",
     ylim=c(0,400),
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

tiff("MNCNWeight.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(log(MNCN$Weight), main = "",
     xlab="Weight (grams)", 
     border="bisque4", 
     xlim = c(1.5,10),
     col="bisque3",
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()
?hist

tiff("MNCNWingspan.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(log(MNCN$Wingspan), main = "",
     xlab="Wingspan (centimetres)", 
     border="bisque4", 
     col="bisque3", xlim = c(2,6),
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

# EBD
tiff("EBDGeodistribution.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(EBD$Occurrence.in.Sp, main = "",
     xlab="Geographical distirbution (%)", 
     border="bisque4", 
     ylim=c(0,2500),
     col="bisque3", xlim = c(0,100),
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()


tiff("EBDWeight.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(log (EBD$Weight), main = "",
     xlab="Weight (grams)", 
     border="bisque4", xlim = c(2,10), 
     col="bisque3",
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

tiff("EBDWingspan.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(log(EBD$Wingspan), main = "",
     xlab="Wingspan (centimetres)", 
     border="bisque4", 
     col="bisque3", xlim = c(2,6),
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

# IEET
tiff("IEETGeodistribution.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(IEET$Occurrence.in.Sp, main = "",
     xlab="Geographical distirbution (%)", 
     border="bisque4",
     col="bisque3",
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

tiff("IEETWeight.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(log(IEET$Weight), main = "",
     xlab="Weight (grams)", 
     border="bisque4",
     col="bisque3", xlim = c(1.5,10),
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

tiff("IEETWingpspan.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(log(IEET$Wingspan), main = "",
     xlab="Wingspan (centimetres)", 
     border="bisque4", 
     col="bisque3", xlim = c(2,6),
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

# SEO
tiff("SEOGeodistribution.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(SEO$Occurrence.in.Sp, main = "",
     xlab="Geographical distirbution (%)", 
     border="bisque4",
     col="bisque3",
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

tiff("SEOWeight.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(log(SEO$Weight), main = "",
     xlab="Weight (grams)", 
     border="bisque4",
     col="bisque3", xlim = c(1.5,10),
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

tiff("SEOWingspan.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(log(SEO$Wingspan), main = "",
     xlab="Wingspan (centimetres)", 
     border="bisque4", 
     col="bisque3", xlim = c(2,6),
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

# eBird
tiff("eBirdGeodistribution.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(ebird$Occurrence.in.Sp, main = "",
     xlab="Geographical distirbution (%)", 
     border="bisque4",
     col="bisque3",
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

tiff("eBirdWeight.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(log(ebird$Weight), main = "",
     xlab="Weight (grams)", 
     border="bisque4",
     col="bisque3", xlim = c(1.5,10),
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

tiff("eBirdWingspan.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(log(ebird$Wingspan), main = "",
     xlab="Wingspan (centimetres)", 
     border="bisque4", 
     col="bisque3", xlim = c(2,6),
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

# AVIS
tiff("AVISGeodistribution.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(AVIS$Occurrence.in.Sp, main = "",
     xlab="Geographical distirbution (%)", 
     border="bisque4",
     col="bisque3",
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

tiff("AVISWeight.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(log(AVIS$Weight), main = "",
     xlab="Weight (grams)", 
     border="bisque4",
     col="bisque3", xlim = c(1.5,10),
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()

tiff("AVISWingspan.tiff", width = 7, height = 5, units = 'in', res = 300)
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 1, font.lab = 1, font.axis = 1)
hist(log(AVIS$Wingspan), main = "",
     xlab="Wingspan (centimetres)", 
     border="bisque4", 
     col="bisque3", xlim = c(2,6),
     breaks = 20)
minor.tick(nx = 4, ny = 2)
dev.off()
