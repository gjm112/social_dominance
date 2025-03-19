library(foreign)
library(tidyverse)

#dat <- as.data.frame(read.spss("/Users/gregorymatthews/Dropbox/social_dominance/HierarchyNoDS 02.16.sav"))
# dat <- as.data.frame(read.spss("/Users/noral/OneDrive/Desktop/CDSC/social dominance new/HierarchyNoDS 02.16.sav"))
dat <- as.data.frame(read.spss("C:/Users/gigip/OneDrive/Desktop/Loyola Chicago/CDSC/social_dominance/HierarchyNoDS 02.16.sav"))
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

# need to figure out how many clusters to do
greg$clust <- kmeans(greg[,1:2], 5)$cluster
greg %>%  ggplot(aes(x = warmth, y= comp, color = as.factor(clust))) + geom_point() +
  xlim(1,5) + ylim(1,5) + stat_ellipse(type = "norm")

greg %>% arrange(clust)


# figure out number of clusters
# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = greg, centers = k)
  model$tot.withinss
})
# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)
# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point()+
  scale_x_continuous(breaks = 1:10)



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

# i want to loop through all groups but not working rn
for (g in length(groups)){
  boxes %>% filter(grepl(groups[g], group)) %>% ggplot(aes(y = score)) + geom_boxplot() + facet_wrap(~ group, ncol =4)
}