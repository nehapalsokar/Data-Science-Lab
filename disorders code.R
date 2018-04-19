require(dplyr)
require(ggplot2)
require(tidyr)
dat <- read.csv("MentalHealthCleanedDataset.csv", header = TRUE, stringsAsFactors = TRUE)
head(names(dat))
names(dat) = tolower(names(dat))
dat2 = rename(dat, 
              mdis_now = current.mental.disorder,
              mdis_past = have.you.had.a.mental.health.disorder.in.the.past.,
              mdis_diagnosed = have.you.been.diagnosed.with.a.mental.health.condition.by.a.medical.professional.,
              sex = gender,
              country = what.country.do.you.live.in.,
              mdis_cat = if.yes..what.condition.s..have.you.been.diagnosed.with.)


# Save all mdis variables in a new df
dat2 %>% select(contains('mdis'), age, sex, country) %>% str()
disorder_vars = dat2 %>% select(contains('mdis'))

names(disorder_vars)

tmp = dat2 %>% separate(mdis_cat, 
                        sep = '\\|',
                        c('mdis_1', 'mdis_2', 'mdis_3', 'mdis_4', 'mdis_5',
                          'mdis_6', 'mdis_7', 'mdis_8', 'mdis_9'),
                        fill = 'right')
# Turn those new variables into factors
tmp2 = tmp %>% select(matches('mdis_[1-9]')) %>% mutate_all(.funs = 'as.factor')
tmp2 %>% select(matches('mdis_[1-9]')) %>% str()

# add these new variables to the disorder_vars df
disorder_vars = cbind(disorder_vars, tmp2)

# Order the factor levels of mdis_1 by frequency
tmp = table(disorder_vars$mdis_1) 
disorder_vars$mdis_1 = factor(disorder_vars$mdis_1, 
                              levels = names(tmp[order(tmp, decreasing = TRUE)])) 

# shorten the factor levels (to plot below)
# split at first '(' and only take the first par
#levels(disorder_vars$mdis_1) = substr(levels(disorder_vars$mdis_1), 1, 15)
#strsplit(levels(disorder_vars$mdis_1), split = '\\(')

levels(disorder_vars$mdis_1) = sapply(strsplit(levels(disorder_vars$mdis_1), split = "\\("), `[`, 1)

subdat = subset(disorder_vars, mdis_1 != "I haven\'t been formally diagnosed, so I felt uncomfortable answering, but Social Anxiety and Depression.")
subdat = subset(subdat, !is.na(mdis_1))


colnames(subdat)[13] <- 'mdis_1'
summary(subdat$mdis_1)
plot.mdis_1 <- subdat %>%
  group_by(mdis_1) %>%
  summarize(count = n()) %>%
  top_n(n=10, wt=count) %>%
  arrange(desc(count))

summary(plot.mdis_1)
ggplot(plot.mdis_1, aes(x = reorder(mdis_1, count), y= count, fill=mdis_1 )) + 
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = c("red","red","red","red","red","red","red","red","red","red")) + 
  ggtitle('Top 10 Mental Health Disorders') +
  xlab('Disorder') +
  ylab('Count') + 
  theme(legend.position="none") +
  coord_flip()

#ggplot(data = plot.mdis_1, aes(x = plot.mdis_1)) +
 # geom_bar(fill = 'red', alpha = .75) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))
