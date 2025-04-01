library(foreign)
library(tidyverse)

#dat <- as.data.frame(read.spss("/Users/gregorymatthews/Dropbox/social_dominance/HierarchyNoDS 02.16.sav"))
dat <- as.data.frame(read.spss("/Users/noral/OneDrive/Desktop/CDSC/social dominance new/HierarchyNoDS 02.16.sav"))
#dat <- as.data.frame(read.spss("C:/Users/gigip/OneDrive/Desktop/Loyola Chicago/CDSC/social_dominance/HierarchyNoDS 02.16.sav"))
dat %>% select(cismen_1:cismen_4, cismen_competence) %>% View()

#1-4: Competence: These get averaged together to get a competence score
#5-8: Warmth: These get averaged together for a warmth score. 
#cluster these: small <- dat %>% select(cismen_competence:crohns_warmth)


# get rid of extra words in 1 and 5 rating 
  # cols1to8 is includes each column for given group
getnames <- dat %>% select(cismen_1:crohns_8)
colnametochange  <-  colnames(getnames)
cols1to8 <- dat %>% mutate(across(all_of(colnametochange), ~ as.numeric(substr(., 1, 1)))) %>% select(cismen_1:crohns_8)

# small is the means for scores 1-4 and 5-8 for each group
  # clean aggregates to be the overall mean for competence and warmth for each group
small <- dat %>% mutate(across(all_of(colnametochange), ~ as.numeric(substr(., 1, 1)))) %>% select(cismen_competence:crohns_warmth)

clean <- as.data.frame(matrix(apply(small, 2, mean, na.rm = TRUE) , ncol = 2, byrow = TRUE))
names(clean) <- c("comp", "warmth")
clean$var <- names(apply(small, 2, mean, na.rm = TRUE))[seq(1,92,2)]
clean %>% mutate(var = str_remove(var,"_competence")) %>%  ggplot(aes(x = warmth, y= comp)) + geom_point() + geom_label(aes(label = var))

greg <- clean %>% mutate(var = str_remove(var,"_competence")) 

#density ellipse?  
#fill the ellipses with alpha = 0.5.  Put points on top.  
# need to figure out how many clusters to do
set.seed(1234)
greg$clust <- kmeans(greg[,1:2], 4, iter.max = 1000, nstart = 10)$cluster
greg %>%  ggplot(aes(x = warmth, y= comp, color = as.factor(clust))) + geom_point() +
  xlim(1,5) + ylim(1,5) + stat_ellipse(type = "norm")

greg %>% arrange(clust)


# figure out number of clusters
# Use map_dbl to run many models with varying value of k (centers)
# tot_withinss <- map_dbl(2:10,  function(k){
#   model <- kmeans(x = greg, centers = k)
#   return(model$tot.withinss)
# })

tot_withinss <- c()
for (k in 1:10){
  model <- kmeans(x = greg[,1:2], centers = k)
  tot_withinss[k] <- model$tot.withinss
}

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)
# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point()+
  scale_x_continuous(breaks = 1:10) + theme_bw()

#FOUR clusters.


# boxplots for 1-4 and 5-8 for each group

# pivot longer
boxes <- cols1to8 %>% pivot_longer(cols = cismen_1:crohns_8, names_to = "group", values_to = "score") %>% drop_na()
boxes1 <- boxes %>% group_by(group, score) %>% summarize(count = n()) %>% 
  mutate(score = factor(score, levels = c(5, 4, 3, 2, 1)))

# all group names
groups <- greg %>% select(var) %>% list()

# cismen boxplots
boxes1 %>% filter(grepl('cismen', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))

# transwomen boxplots
boxes1 %>% filter(grepl('transwomen', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))

# nonbinary boxplots
boxes1 %>% filter(grepl('nonbinary', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# bisexual boxplots
boxes1 %>% filter(grepl('bisexual', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# gay boxplots
boxes1 %>% filter(grepl('gay', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# black boxplots
boxes1 %>% filter(grepl('black', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# asian boxplots
boxes1 %>% filter(grepl('asian', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# undocumented boxplots
boxes1 %>% filter(grepl('undocumented', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# midage boxplots
boxes1 %>% filter(grepl('midage', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# addict boxplots
boxes1 %>% filter(grepl('addict', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# anxiety boxplots
boxes1 %>% filter(grepl('anxiety', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# ocd boxplots
boxes1 %>% filter(grepl('ocd', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# bipolar boxplots
boxes1 %>% filter(grepl('bipolar', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# intellectual boxplots
boxes1 %>% filter(grepl('intellectual', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# criminal boxplots
boxes1 %>% filter(grepl('criminals', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# facialdeform boxplots
boxes1 %>% filter(grepl('facialdeform', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# prosthetic boxplots
boxes1 %>% filter(grepl('prosthetics', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# nondisabled boxplots
boxes1 %>% filter(grepl('nondisabled', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# deaf boxplots
boxes1 %>% filter(grepl('deaf', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# diabetes boxplots
boxes1 %>% filter(grepl('diabetes', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# autism boxplots
boxes1 %>% filter(grepl('autism', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# cancer boxplots
boxes1 %>% filter(grepl('cancer', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# muscdystrophy boxplots
boxes1 %>% filter(grepl('muscdystrophy', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# ciswomen boxplots
boxes1 %>% filter(grepl('ciswomen', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))



# transmen boxplots
boxes1 %>% filter(grepl('transmen', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))



# heterosexual boxplots
boxes1 %>% filter(grepl('heterosexual', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))



# lesbian boxplots
boxes1 %>% filter(grepl('lesbian', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# white boxplots
boxes1 %>% filter(grepl('white', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# latinxhispanic boxplots
boxes1 %>% filter(grepl('latinxhispanic', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# immigrants boxplots
boxes1 %>% filter(grepl('immigrants', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# elderly boxplots
boxes1 %>% filter(grepl('elderly', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# youngadults boxplots
boxes1 %>% filter(grepl('youngadults', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))



# schizophrenia boxplots
boxes1 %>% filter(grepl('schizophrenia', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# homeless boxplots
boxes1 %>% filter(grepl('homeless', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# depression boxplots
boxes1 %>% filter(grepl('depression', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))



# Alzheimer boxplots
boxes1 %>% filter(grepl('alzheimer', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))



# sociopaths boxplots
boxes1 %>% filter(grepl('sociopaths', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))



# amputees boxplots
boxes1 %>% filter(grepl('amputees', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# hivaids boxplots
boxes1 %>% filter(grepl('hivaids', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))



# wheelchair boxplots
boxes1 %>% filter(grepl('wheelchair', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# cochlearimplants boxplots
boxes1 %>% filter(grepl('cochlearimplants', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# blind boxplots
boxes1 %>% filter(grepl('blind', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# autoimmune boxplots
boxes1 %>% filter(grepl('autoimmune', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# adhd boxplots
boxes1 %>% filter(grepl('adhd', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))



# epilepsy boxplots
boxes1 %>% filter(grepl('epilepsy', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))


# crohns boxplots
boxes1 %>% filter(grepl('crohns', group)) %>% 
  ggplot(aes(x = group, y = count, fill = as.factor(score))) + 
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))

# i want to loop through all groups but not working rn
for (g in length(groups)){
  boxes %>% filter(grepl(groups[g], group)) %>% ggplot(aes(y = score)) + geom_boxplot() + facet_wrap(~ group, ncol =4)
}