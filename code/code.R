library(foreign)
library(tidyverse)

#dat <- as.data.frame(read.spss("/Users/gregorymatthews/Dropbox/social_dominance/HierarchyNoDS 02.16.sav"))
dat <- as.data.frame(read.spss("/Users/noral/OneDrive/Desktop/CDSC/social dominance new/HierarchyNoDS 02.16.sav"))
#dat <- as.data.frame(read.spss("C:/Users/gigip/OneDrive/Desktop/Loyola Chicago/CDSC/social_dominance/HierarchyNoDS 02.16.sav"))
dat %>% View()

#1-4: Competence: These get averaged together to get a competence score
#5-8: Warmth: These get averaged together for a warmth score. 
#cluster these: small <- dat %>% select(cismen_competence:crohns_warmth)


#colnametochange <- colnames(dat[32:596])

small <- dat %>% select(cismen_competence:crohns_warmth)
colnametochange  <-  colnames(small)
newdat <- dat %>% mutate(across(all_of(colnametochange), ~ as.numeric(substr(., 1, 1)))) %>% select(cismen_competence:crohns_warmth)


#apply(small, 2, mean, na.rm = TRUE)
clean <- as.data.frame(matrix(apply(small, 2, mean, na.rm = TRUE) , ncol = 2, byrow = TRUE))
names(clean) <- c("comp", "warmth")
clean$var <- names(apply(small, 2, mean, na.rm = TRUE))[seq(1,92,2)]

clean %>% mutate(var = str_remove(var,"_competence")) %>%  ggplot(aes(x = warmth, y= comp)) + geom_point() + geom_label(aes(label = var))

greg <- clean %>% mutate(var = str_remove(var,"_competence")) 

greg$clust <- kmeans(greg[,1:2], 4)$cluster
greg %>%  ggplot(aes(x = warmth, y= comp, color = as.factor(clust))) + geom_point() +
  xlim(1,5) + ylim(1,5) + stat_ellipse(type = "norm")



