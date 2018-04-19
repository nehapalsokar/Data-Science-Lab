#What percentage of employees reach out for help to the employer, given that the employer is
#providing benefits to those who are suffering from mental illness?

data<-read.csv("MentalHealthCleanedDataset.csv")
names(data) = tolower(names(data))

# create new variable names
new.names <- c("self.employed", "num.employees", "tech.company", "tech.role", "mental.health.coverage", "mental.health.options", "mental.health.formally.discussed", "mental.health.resources", "anonymity.protected", "medical.leave", "mental.health.negative", "physical.health.negative", "mental.health.comfort.coworker", "mental.health.comfort.supervisor", "mental.health.taken.seriously", "coworker.negative.consequences", "private.med.coverage", "resources", "reveal.diagnosis.clients.or.business", "revealed.negative.consequences.CB", "reveal.diagnosis.coworkers", "revealed.negative.consequences.CW", "productivity.effected", "percentage", "previous.employer", "prevemp.mental.health.coverage", "prevemp.mental.health.options", "prevemp.mental.health.formally.discussed", "prevemp.mental.health.resources", "prevemp.anonymity.protected", "prevemp.mental.health.negative",
               "prevemp.physical.health.negative", "prevemp.mental.health.coworker", "prevemp.mental.health.comfort.supervisor", "prevemp.mental.health.taken.seriously", "prevemp.coworker.negative.consequences", "mention.phsyical.issue.interview", "why.whynot.physical", "mention.mental.health.interview", "why.whynot.mental", "career.hurt", "viewed.negatively.by.coworkers", "share.with.family", "observed.poor.handling", "observations.lead.less.likely.to.reveal", "family.history", "ever.had.mental.disorder", "currently.have.mental.disorder", "if.yes.what", "if.maybe.what", "medical.prof.diagnosis", "what.conditions", "sought.prof.treatment", "treatment.affects.work", "no.treatment.affects.work", "age", "gender", "country.live", "US.state", "country.work", "state.work", "work.position", "remotely"  )

colnames(data) <- new.names
str(data)
Yes_cov<-subset(data, data$mental.health.coverage == "Yes") 
View(Yes_cov)


ggplot(Yes_cov, aes(x = mental.health.comfort.supervisor,y=percentage, fill=mental.health.comfort.supervisor)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ggtitle("people reaching out to employer when mental benefits=yes")+
  xlab("people reaching out to employer")


#When employer doesnt offer benefits
No_cov<-subset(data, data$mental.health.coverage == "No") 
View(No_cov)

ggplot(No_cov, aes(x = mental.health.comfort.supervisor,y=percentage, fill=mental.health.comfort.supervisor)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ggtitle("people reaching out to employer when mental benefits=No")+
  xlab("people reaching out to employer")

#Does employee know about mental health care available under employer-provided coverage  
Awareness<-subset(data, data$mental.health.options=="Yes"|data$mental.health.options=="No"|data$mental.health.options=="N/A"|data$mental.health.options=="I am not sure")

ggplot(Awareness , aes(x = mental.health.options,y=frequency, fill=mental.health.options)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  ggtitle("Does employee know about mental health care available under employer-provided coverage") +
  xlab("Does employee know about mental health care available under employer-provided coverage")


chisq.test(table(all$self.employed, all$currently.have.mental.disorder))

#logistic regression
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

data$mental.health.comfort.supervisor2 <- relevel(data$mental.health.comfort.supervisor, ref = "Yes")
test <- multinom(data$mental.health.comfort.supervisor ~ data$mental.health.coverage + data$mental.health.options, data = data)
summary(test)

levels(data$mental.health.comfort.supervisor2)


#1. Model execution output shows some iteration history and includes the final negative log-likelihood 1231.900910.This value is multiplied by two as shown in the model summary as the Residual Deviance. 
