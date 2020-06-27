## If the Korean annotation is broken, please change the encoding to UTF-8 . (file -> reopen with Encoding -> utf-8 check)


###############################################################################

#범주형 데이터 k-medoids 군집분석

df <- read.csv("hn13-18_all_labeled.csv")  # 데이터를 저장한 디렉토리 주소와 함께 입력해주세요. 예: C:/hn13-18_all_labeled.csv

# 6~65줄까지 드래그 하여 실행해주세요
library(dplyr)
library(cluster)
library(ggplot2)
set.seed(20200612)
cat_df=df%>%select(incm, cfam, marri_1, D_1_1,LQ_1EQL,educ,BO1,BD1_11,BS1_1,sex, Ms, age)

col_name=c(paste(colnames(cat_df)))
n=length(col_name)

cat_df=cat_df %>%
  filter(cat_df$age>=20  &!is.na(cat_df$incm) & !is.na(cat_df$cfam) & !is.na(cat_df$marri_1) &!is.na(cat_df$D_1_1) & !is.na(cat_df$LQ_1EQL) & !is.na(cat_df$educ) & !is.na(cat_df$BO1) & !is.na(cat_df$BD1_11) &!is.na(cat_df$BS1_1) &!is.na(cat_df$sex) &!is.na(cat_df$Ms) &!is.na(cat_df$age))

cat_df=cat_df%>% filter(cat_df$D_1_1!=8 & cat_df$D_1_1!=9 & cat_df$LQ_1EQL!=8 & cat_df$LQ_1EQL!=9 & cat_df$educ!=99 & cat_df$BO1!=8 & cat_df$BO1!=9 & cat_df$BD1_11!=8 & cat_df$BD1_11!=9 & cat_df$BS1_1!=8 & cat_df$BS1_1!=9)

col_name[11]

cat_df=cat_df[1:11]


cat_df_0 = cat_df%>% filter(cat_df$Ms==0)
cat_df_1 = cat_df%>% filter(cat_df$Ms==1)

cat_df_0=cat_df_0[sample(nrow(cat_df_0), 1000),]
cat_df_1=cat_df_1[sample(nrow(cat_df_1), 1000),]


cat_df = rbind(cat_df_0, cat_df_1)




cat_df$incm = factor(cat_df$incm)
cat_df$cfam = factor(cat_df$cfam)
cat_df$marri_1 = factor(cat_df$marri_1)
cat_df$D_1_1 = factor(cat_df$D_1_1)
cat_df$LQ_1EQL = factor(cat_df$LQ_1EQL)
cat_df$educ = factor(cat_df$educ)
cat_df$BO1 = factor(cat_df$BO1)
cat_df$BD1_11 = factor(cat_df$BD1_11)
cat_df$BS1_1 = factor(cat_df$BS1_1)
cat_df$sex = factor(cat_df$sex)
cat_df$Ms = factor(cat_df$Ms)




gower_distance <- daisy(cat_df, metric = c("gower"))
class(gower_distance)


sil_width <- c(NA)
for(i in 2:8) {
  pam_fit <- pam(gower_distance, diss = TRUE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width) # 실루엣 그래프를 확인합니다. 

##########################클러스터 개수가 2개인 경우
k <- 2
pam_fit <- pam(gower_distance, diss = TRUE, k)
pam_results <- cat_df %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))



pam_results
pam_fit

pam_fit$clustering

labeled_df=mutate(cat_df,cluster = pam_fit$clustering)

labeled_df$cluster = factor(labeled_df$cluster)

labeled_df_1=labeled_df%>%filter(labeled_df$cluster==1)
labeled_df_2=labeled_df%>%filter(labeled_df$cluster==2)


attr = c('incm', 'cfam', 'marri_1', 'D_1_1','LQ_1EQL','educ','BO1','BD1_11','BS1_1','sex', 'Ms')

for(a in attr){
  print(a)
  print(c(mean(as.numeric(as.character(labeled_df_1[[a]]))), mean(as.numeric(as.character(labeled_df_2[[a]])))))
} #각 attribute의 평균값들을 확인합니다. 







for(a in attr){
  barplot(table(labeled_df_1[a]), xlab=paste(a, 1), ylab='count')
  barplot(table(labeled_df_2[a]), xlab=paste(a, 2), ylab='count')
  
} #bar plot으로 시각화된 자료를 확인합니다. 

labeled_df_1$incm



for(i in (1:11)){
  print(col_name[i])
  table <- as.data.frame.matrix(table(labeled_df[,i], labeled_df$cluster))
  print(chisq.test(labeled_df[,i], labeled_df$cluster))
  
} #카이제곱 독립성검정 결과를 확인합니다. 



###########################클러스터 개수가 3인 경우

k <- 3
pam_fit <- pam(gower_distance, diss = TRUE, k)
pam_results <- cat_df %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))



pam_results
pam_fit


pam_fit$clustering

labeled_df=mutate(cat_df,cluster = pam_fit$clustering)

labeled_df$cluster = factor(labeled_df$cluster)

labeled_df_1=labeled_df%>%filter(labeled_df$cluster==1)
labeled_df_2=labeled_df%>%filter(labeled_df$cluster==2)
labeled_df_3=labeled_df%>%filter(labeled_df$cluster==3)

attr = c('incm', 'cfam', 'marri_1', 'D_1_1','LQ_1EQL','educ','BO1','BD1_11','BS1_1','sex', 'Ms')

for(a in attr){
  print(a)
  print(c(mean(as.numeric(as.character(labeled_df_1[[a]]))), mean(as.numeric(as.character(labeled_df_2[[a]]))), mean(as.numeric(as.character(labeled_df_3[[a]])))))
} #각 attribute의 평균값을 확인합니다. 



str(labeled_df)





for(a in attr){
  barplot(table(labeled_df_1[a]), xlab=paste(a, 1), ylab='count')
  barplot(table(labeled_df_2[a]), xlab=paste(a, 2), ylab='count')
  barplot(table(labeled_df_3[a]), xlab=paste(a, 3), ylab='count')
} # bar plot으로 시각화된 결과를 확인합니다. 
