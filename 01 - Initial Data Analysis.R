library(tidyverse)
library(logistf)

# Set Working Directory
setwd("C:\\Users\\Claire\\Dropbox\\GHC Open Source Day\\data")

dat.abduct <- read.csv("Hackathon Attempted Abduction Data Set.csv") %>%
	as_tibble(.)
dat.missing <- read.csv("Hackathon Missing Children Data Set.csv") %>%
	as_tibble(.)
	
dat.abduct[dat.abduct == ""] <- NA	
dat.missing[dat.missing == ""] <- NA
	
################# Start Cleaning Data for Regression

dat.var <- c("Sex", "Race", "Vehicle.Style", "Vehicle.Color")
dat.missing.clean <- dat.missing %>%
	select_(.dots = dat.var) %>%
	na.omit(.)

dat.var2 <- c("Child.Gender.1", "Child.Race.1", "Vehicle.Type", "Vehicle.Color")	
dat.abduct.clean <- dat.abduct %>%
	select_(.dots = dat.var2) %>%
	na.omit(.)
	
dat.missing.clean <- cbind(1, dat.missing.clean)
colnames(dat.missing.clean) <- c("Missing", "Sex", "Race", "Vehicle.Type", "Vehicle.Color")

dat.abduct.clean <- cbind(0, dat.abduct.clean)
colnames(dat.abduct.clean) <- c("Missing", "Sex", "Race", "Vehicle.Type", "Vehicle.Color")

################# Start Cleaning Data for Visualization
dat.abduct.viz <- dat.abduct

child.var1 <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Child.Race.1", "Child.Gender.1", "Child.Approximate.Age.1")

child.var2 <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Child.Race.2", "Child.Gender.2", "Child.Approximate.Age.2") 

child.var3 <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Child.Race.3", "Child.Gender.3", "Child.Approximate.Age.3")

child.var4 <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Child.Race.4", "Child.Gender.4", "Child.Approximate.Age.4")

child.var5 <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Child.Race.5", "Child.Gender.5", "Child.Approximate.Age.5")

child.var6 <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Child.Race.6", "Child.Gender.6", "Child.Approximate.Age.6")

adult.var1 <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Offender.Race.1", "Offender.Gender.1", "Offender.Perceived.Age.1")

adult.var2 <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Offender.Race.2", "Offender.Gender.2", "Offender.Perceived.Age.2")

adult.var3 <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Offender.Race.3", "Offender.Gender.3", "Offender.Perceived.Age.3")

dat.child.viz1 <- dat.abduct.viz %>%
	select_(.dots = child.var1) %>%
	na.omit(.)
colnames(dat.child.viz1) <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Child.Race", "Child.Gender", "Child.Approximate.Age")
	
dat.child.viz2 <- dat.abduct.viz %>%
	select_(.dots = child.var2) %>%
	na.omit(.)
colnames(dat.child.viz2) <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Child.Race", "Child.Gender", "Child.Approximate.Age")
	
dat.child.viz3 <- dat.abduct.viz %>%
	select_(.dots = child.var3) %>%
	na.omit(.)
colnames(dat.child.viz3) <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Child.Race", "Child.Gender", "Child.Approximate.Age")
	
dat.child.viz4 <- dat.abduct.viz %>%
	select_(.dots = child.var4) %>%
	na.omit(.)
colnames(dat.child.viz4) <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Child.Race", "Child.Gender", "Child.Approximate.Age")
	
dat.child.viz5 <- dat.abduct.viz %>%
	select_(.dots = child.var5) %>%
	na.omit(.)
colnames(dat.child.viz5) <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Child.Race", "Child.Gender", "Child.Approximate.Age")	

dat.child.viz6 <- dat.abduct.viz %>%
	select_(.dots = child.var6) %>%
	na.omit(.)
colnames(dat.child.viz6) <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Child.Race", "Child.Gender", "Child.Approximate.Age")
	
dat.child.viz <- rbind(dat.child.viz1, dat.child.viz2, dat.child.viz3, dat.child.viz4, dat.child.viz5, dat.child.viz6)
	
dat.offender.viz1 <- dat.abduct.viz %>%
	select_(.dots = adult.var1) %>%
	na.omit(.)
colnames(dat.offender.viz1) <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Offender.Race", "Offender.Gender", "Offender.Perceived.Age")

dat.offender.viz2 <- dat.abduct.viz %>%
	select_(.dots = adult.var2) %>%
	na.omit(.)
colnames(dat.offender.viz2) <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Offender.Race", "Offender.Gender", "Offender.Perceived.Age")
	
dat.offender.viz3 <- dat.abduct.viz %>%
	select_(.dots = adult.var3) %>%
	na.omit(.)
colnames(dat.offender.viz3) <- c("Case.Number", "Incident.City", "Incident.State", "Incident.Zip", "Vehicle.Type", "Vehicle.Color", "Offender.Race", "Offender.Gender", "Offender.Perceived.Age")

dat.offender.viz <- rbind(dat.offender.viz1, dat.offender.viz2, dat.offender.viz3)

################## Save Data
write.csv(dat.child.viz, file = "child_data.csv")
write.csv(dat.offender.viz, file = "offender_data.csv")
write.csv(full.dat, file = "missing_compare.csv")

