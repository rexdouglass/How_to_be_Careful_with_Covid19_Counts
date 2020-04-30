```{r}

#There's a miscode of New York City and New York state that creates an issue
lhs_long_clean_confirmed_wide <-
  lhs_long_clean %>%
  filter(!is.na(wikidata_id)) %>% #the NA_NA_NA obs were screwing with us
  dplyr::select(dataset, gid_geonameid_wikidata_id, date_asdate, confirmed) %>%
  filter(!is.na(gid_geonameid_wikidata_id) & !is.na(date_asdate)) %>%
  filter(!is.na(confirmed) & confirmed>0 ) %>%
  group_by(dataset, gid_geonameid_wikidata_id, date_asdate) %>%
  summarise(confirmed= max(confirmed, na.rm=T)) %>%
  ungroup() %>%
  pivot_wider(names_from = dataset, values_from = confirmed ) %>%
  mutate_if(is.numeric, list(~na_if(abs(.), Inf))) %>% distinct()
dim(lhs_long_clean_confirmed_wide) #154,499 #161,952

temp <- lhs_long_clean_confirmed_wide %>% dplyr::select(-gid_geonameid_wikidata_id,-date_asdate) %>% as.matrix()
condition <- ncol(temp) - (is.na(temp) %>% rowSums() ) >1
cor1 <- temp[condition,] %>% cor(use="pairwise.complete.obs")
library(ggcorrplot) #install.packages("ggcorrplot")
ggcorrplot(cor1, hc.order = F, type = "lower", lab = TRUE, colors=c("red","white","green"), method = "circle")

#install.packages('heatmaply');
library(heatmaply)
heatmaply(cor1)
hist(cor1[lower.tri(cor1)])

cor_list <- list()
for(q in lhs_long_clean_confirmed_wide$date_asdate %>% unique() %>% sort()  ){
  print(q)
  try({
    cor1 <- NULL
    temp <- lhs_long_clean_confirmed_wide %>% filter(date_asdate==q) %>% dplyr::select(-gid_geonameid_wikidata_id,-date_asdate) %>% as.matrix()
    condition <- ncol(temp) - (is.na(temp) %>% rowSums() ) >1
    cor1 <- temp[condition,] %>% cor(use="pairwise.complete.obs")
    cor_list[[ as.character(q)]] <- cor1[lower.tri(cor1)]
  })
}

plot(sapply(cor_list, mean, na.rm=T))

temp2 <-  lhs_long_clean %>%
  filter(!is.na(wikidata_id)) %>% #the NA_NA_NA obs were screwing with us
  dplyr::select(dataset, gid_geonameid_wikidata_id, date_asdate, confirmed) %>%
  filter(!is.na(gid_geonameid_wikidata_id) & !is.na(date_asdate)) %>%
  filter(!is.na(confirmed) & confirmed>0 ) %>%
  group_by(gid_geonameid_wikidata_id, date_asdate) %>%
  summarise(confirmed_var = var(confirmed, na.rm=T), n_count=n()) %>%
  ungroup() %>%
  mutate_if(is.numeric, list(~na_if(abs(.), Inf))) %>% distinct() %>%
  filter(!is.na(confirmed_var) & n_count>1)

temp2 %>%
  ggplot(aes(x=date_asdate, y=confirmed_var)) +
  geom_point()

temp2 %>%
  ggplot(aes(x=date_asdate %>% as.factor(), y=confirmed_var %>% sqrt() )) +
  geom_boxplot() + ylim(0,1000)

temp2 %>%
  ggplot(aes(x=date_asdate , y=confirmed_var %>% sqrt() )) +
  geom_point() +
  geom_smooth() +
  ylim(0,1000)

#There 3 outliers at least, new york because of a join error, the U.S. because cumulative is off by like 40 k when it gets up to a million

test2 <- lhs_long_clean %>% left_join(temp2)


```
