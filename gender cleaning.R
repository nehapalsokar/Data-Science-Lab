

# load packages
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(wesanderson)
set.seed(1000)

# load dataset
mental.health <- read.csv("mental-health.csv", header = TRUE, stringsAsFactors = TRUE)

# check structure
#str(mental.health)

# create new variable names
new.names <- c("self.employed", "num.employees", "tech.company", "tech.role", "mental.health.coverage", "mental.health.options", "mental.health.formally.discussed", "mental.health.resources", "anonymity.protected", "medical.leave", "mental.health.negative", "physical.health.negative", "mental.health.comfort.coworker", "mental.health.comfort.supervisor", "mental.health.taken.seriously", "coworker.negative.consequences", "private.med.coverage", "resources", "reveal.diagnosis.clients.or.business", "revealed.negative.consequences.CB", "reveal.diagnosis.coworkers", "revealed.negative.consequences.CW", "productivity.effected", "percentage", "previous.employer", "prevemp.mental.health.coverage", "prevemp.mental.health.options", "prevemp.mental.health.formally.discussed", "prevemp.mental.health.resources", "prevemp.anonymity.protected", "prevemp.mental.health.negative",
               "prevemp.physical.health.negative", "prevemp.mental.health.coworker", "prevemp.mental.health.comfort.supervisor", "prevemp.mental.health.taken.seriously", "prevemp.coworker.negative.consequences", "mention.phsyical.issue.interview", "why.whynot.physical", "mention.mental.health.interview", "why.whynot.mental", "career.hurt", "viewed.negatively.by.coworkers", "share.with.family", "observed.poor.handling", "observations.lead.less.likely.to.reveal", "family.history", "ever.had.mental.disorder", "currently.have.mental.disorder", "if.yes.what", "if.maybe.what", "medical.prof.diagnosis", "what.conditions", "sought.prof.treatment", "treatment.affects.work", "no.treatment.affects.work", "age", "gender", "country.live", "US.state", "country.work", "state.work", "work.position", "remotely"  )

# change names
colnames(mental.health) <- new.names
# check
#str(mental.health)

# what does gender variable look like?
head(table(mental.health$gender))

tail(table(mental.health$gender))

# ok, we have some issues with gender that need to be cleaned up - see next section below

# convert some factor variables to character
mental.health$why.whynot.physical <- as.character(mental.health$why.whynot.physical)
mental.health$why.whynot.mental <- as.character(mental.health$why.whynot.mental)
mental.health$gender <- as.character(mental.health$gender)
mental.health$work.position <- as.character(mental.health$work.position)

# convert boolean to factor
mental.health$self.employed <- as.factor(mental.health$self.employed)
levels(mental.health$self.employed) <- c("No", "Yes")
mental.health$tech.role <- as.factor(mental.health$tech.role)
levels(mental.health$tech.role) <- c("No", "Yes")

# let's try to standardize responses
mental.health[mental.health$gender == "Male", "gender"] <- "M"
mental.health[mental.health$gender == "male", "gender"] <- "M"
mental.health[mental.health$gender == "MALE", "gender"] <- "M"
mental.health[mental.health$gender == "Man", "gender"] <- "M"
mental.health[mental.health$gender == "man", "gender"] <- "M"
mental.health[mental.health$gender == "m", "gender"] <- "M"
mental.health[mental.health$gender == "man ", "gender"] <- "M"
mental.health[mental.health$gender == "Dude", "gender"] <- "M"
mental.health[mental.health$gender == "mail", "gender"] <- "M"
mental.health[mental.health$gender == "M|", "gender"] <- "M"
mental.health[mental.health$gender == "Cis male", "gender"] <- "M"
mental.health[mental.health$gender == "Male (cis)", "gender"] <- "M"
mental.health[mental.health$gender == "Cis Male", "gender"] <- "M"
mental.health[mental.health$gender == "cis male", "gender"] <- "M"
mental.health[mental.health$gender == "cisdude", "gender"] <- "M"
mental.health[mental.health$gender == "cis man", "gender"] <- "M"
mental.health[mental.health$gender == "Male.", "gender"] <- "M"
mental.health[mental.health$gender == "Male ", "gender"] <- "M"
mental.health[mental.health$gender == "male ", "gender"] <- "M"
mental.health[mental.health$gender == "Malr", "gender"] <- "M"
mental.health[841,"gender"] <- "M"

mental.health[mental.health$gender == "Female", "gender"] <- "F"
mental.health[mental.health$gender == "Female ", "gender"] <- "F"
mental.health[mental.health$gender == " Female", "gender"] <- "F"
mental.health[mental.health$gender == "female", "gender"] <- "F"
mental.health[mental.health$gender == "female ", "gender"] <- "F"
mental.health[mental.health$gender == "Woman", "gender"] <- "F"
mental.health[mental.health$gender == "woman", "gender"] <- "F"
mental.health[mental.health$gender == "f", "gender"] <- "F"
mental.health[mental.health$gender == "Cis female", "gender"] <- "F"
mental.health[mental.health$gender == "Cis female ", "gender"] <- "F"
mental.health[mental.health$gender == "Cisgender Female", "gender"] <- "F"
mental.health[mental.health$gender == "Cis-woman", "gender"] <- "F"
mental.health[mental.health$gender == "fem", "gender"] <- "F"
mental.health[1091, "gender"] <- "F"
mental.health[17, "gender"] <- "F"

# gender queer (GQ)
mental.health[mental.health$gender == "Agender", "gender"] <- "GQ"
mental.health[mental.health$gender == "Androgynous", "gender"] <- "GQ"
mental.health[mental.health$gender == "Bigender", "gender"] <- "GQ"
mental.health[mental.health$gender == "Female or Multi-Gender Femme", "gender"] <- "GQ"
mental.health[mental.health$gender == "female-bodied; no feelings about gender", "gender"] <- "GQ"
mental.health[mental.health$gender == "Fluid", "gender"] <- "GQ"
mental.health[mental.health$gender == "fm", "gender"] <- "GQ"
mental.health[mental.health$gender == "GenderFluid", "gender"] <- "GQ"
mental.health[mental.health$gender == "GenderFluid (born female)", "gender"] <- "GQ"
mental.health[mental.health$gender == "Genderflux demi-girl", "gender"] <- "GQ"
mental.health[mental.health$gender == "genderqueer", "gender"] <- "GQ"
mental.health[mental.health$gender == "Genderqueer", "gender"] <- "GQ"
mental.health[mental.health$gender == "fm", "gender"] <- "GQ"
mental.health[mental.health$gender == "genderqueer woman", "gender"] <- "GQ"
mental.health[mental.health$gender == "human", "gender"] <- "GQ"
mental.health[mental.health$gender == "Human", "gender"] <- "GQ"
mental.health[mental.health$gender == "Unicorn", "gender"] <- "GQ"
mental.health[mental.health$gender == "Male/genderqueer", "gender"] <- "GQ"
mental.health[mental.health$gender == "nb masculine", "gender"] <- "GQ"
mental.health[mental.health$gender == "non-binary", "gender"] <- "GQ"
mental.health[mental.health$gender == "Nonbinary", "gender"] <- "GQ"
mental.health[mental.health$gender == "AFAB", "gender"] <- "GQ"

# transgender (TG)
mental.health[mental.health$gender == "Male (trans, FtM)", "gender"] <- "TG"
mental.health[mental.health$gender == "Transgender woman", "gender"] <- "TG"

# see what's left
index <- which(mental.health$gender != "M" & mental.health$gender != "F" & mental.health$gender != "GQ" & mental.health$gender != "TG")

mental.health[index, "gender"]

# create vector of final gender values to fill in based on index
last.genders <- c("F", "TG", "GQ", "GQ", "F", "GQ", "GQ", "GQ", "M", "Refused", "GQ", "GQ", "GQ", "TG", "GQ", NA)

# fill in remaining values
mental.health[index, "gender"] <- last.genders

# check gender
table(mental.health$gender)
barplot(table(mental.health$gender))

