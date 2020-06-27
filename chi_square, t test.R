df <- read.csv("hn13-18_all_labeled.csv")

library(dplyr)

df_model=df%>% select(incm, ho_incm, cfam, marri_1, D_1_1, D_2_1, LQ1_sb, LQ_1EQL, LQ_2EQL, LQ_3EQL, LQ_4EQL, LQ_5EQL, educ, EC1_1, BO1, BO1_1, BO2_1, BD1, BD1_11,  BP1,  BS1_1,  BE5_1, LK_EDU, LK_LB_CO,sex, age,EC_wht_23, HE_BMI, Ms)



col_name=c(paste(colnames(df_model)))
n=length(col_name)

for(i in 1:n){
  print(col_name[i])
  print(table(is.na(df_model[col_name[i]])))
}

df_model=df_model %>%
  filter(df_model$age>=20  &!is.na(df_model[col_name[1]]) & !is.na(df_model[col_name[2]]) & !is.na(df_model[col_name[3]]) & !is.na(df_model[col_name[4]]) & !is.na(df_model[col_name[5]]) & !is.na(df_model[col_name[6]]) & !is.na(df_model[col_name[7]]) & !is.na(df_model[col_name[8]]) & !is.na(df_model[col_name[9]]) & !is.na(df_model[col_name[10]]) & !is.na(df_model[col_name[11]]) & !is.na(df_model[col_name[12]]) & !is.na(df_model[col_name[13]]) & !is.na(df_model[col_name[14]]) & !is.na(df_model[col_name[15]]) & !is.na(df_model[col_name[16]]) & !is.na(df_model[col_name[17]]) & !is.na(df_model[col_name[18]]) & !is.na(df_model[col_name[19]]) & !is.na(df_model[col_name[20]]) & !is.na(df_model[col_name[21]]) & !is.na(df_model[col_name[22]]) & !is.na(df_model[col_name[23]]) & !is.na(df_model[col_name[24]]) & !is.na(df_model[col_name[25]]) & !is.na(df_model[col_name[26]]) & !is.na(df_model[col_name[27]]) & !is.na(df_model[col_name[28]]) & !is.na(df_model[col_name[29]])  )




df_model=df_model%>% filter(df_model[col_name[1]]!=8 & df_model[col_name[1]]!=9 & df_model[col_name[2]]!=8 & df_model[col_name[2]]!=9 & df_model[col_name[3]]!=8 & df_model[col_name[3]]!=9 & df_model[col_name[4]]!=8 & df_model[col_name[4]]!=9 & df_model[col_name[5]]!=88 & df_model[col_name[5]]!=99 & df_model[col_name[6]]!=8 & df_model[col_name[6]]!=9  & df_model[col_name[7]]!=8 & df_model[col_name[7]]!=9 & df_model[col_name[8]]!=8 & df_model[col_name[8]]!=9 & df_model[col_name[9]]!=8 & df_model[col_name[9]]!=9 & df_model[col_name[10]]!=8 & df_model[col_name[10]]!=9 & df_model[col_name[11]]!=8 & df_model[col_name[11]]!=9 & df_model[col_name[12]]!=8 & df_model[col_name[12]]!=9 & df_model[col_name[13]]!=8 & df_model[col_name[13]]!=9 & df_model[col_name[14]]!=88 & df_model[col_name[14]]!=99 & df_model[col_name[15]]!=8 & df_model[col_name[15]]!=9 & df_model[col_name[16]]!=8 & df_model[col_name[16]]!=9 & df_model[col_name[17]]!=8 & df_model[col_name[17]]!=9 & df_model[col_name[18]]!=88 & df_model[col_name[18]]!=99 & df_model[col_name[19]]!=88 & df_model[col_name[19]]!=99 & df_model[col_name[20]]!=8 & df_model[col_name[20]]!=9 & df_model[col_name[21]]!=8 & df_model[col_name[21]]!=9 & df_model[col_name[22]]!=8 & df_model[col_name[22]]!=9 & df_model[col_name[23]]!=8 & df_model[col_name[23]]!=9 & df_model[col_name[24]]!=8 & df_model[col_name[24]]!=9& df_model[col_name[26]]!=888 & df_model[col_name[26]]!=999  )

col_name[29]



df_model$incm = factor(df_model$incm)
df_model$ho_incm = factor(df_model$ho_incm)
df_model$cfam = factor(df_model$cfam)
df_model$marri_1 = factor(df_model$marri_1)
df_model$D_1_1 = factor(df_model$D_1_1)
df_model$D_2_1 = factor(df_model$D_2_1)
df_model$LQ1_sb = factor(df_model$LQ1_sb)
df_model$LQ_1EQL = factor(df_model$LQ_1EQL)
df_model$LQ_2EQL = factor(df_model$LQ_2EQL)
df_model$LQ_3EQL = factor(df_model$LQ_3EQL)
df_model$LQ_4EQL = factor(df_model$LQ_4EQL)
df_model$LQ_5EQL = factor(df_model$LQ_5EQL)
df_model$educ = factor(df_model$educ)
df_model$EC1_1 = factor(df_model$EC1_1)
df_model$BO1 = factor(df_model$BO1)
df_model$BO1_1 = factor(df_model$BO1_1)
df_model$BO2_1 = factor(df_model$BO2_1)
df_model$BD1 = factor(df_model$BD1)
df_model$BD1_11 = factor(df_model$BD1_11)
df_model$BP1 = factor(df_model$BP1)
df_model$BS1_1 = factor(df_model$BS1_1)
df_model$BE5_1 = factor(df_model$BE5_1)
df_model$LK_EDU = factor(df_model$LK_EDU)
df_model$LK_LB_CO = factor(df_model$LK_LB_CO)
df_model$sex = factor(df_model$sex)
df_model$Ms = factor(df_model$Ms)







for(i in 1:25)
{
  print(col_name[i])
  table <- as.data.frame.matrix(table(df_model[,i], df_model$Ms))
  print(chisq.test(df_model[,i], df_model$Ms))
  
}



for(i in 26:28)
{
  print(col_name[i])
  a=(df_model %>% filter(df_model$Ms==0))[,i]
  b=(df_model %>% filter(df_model$Ms==1))[,i]
  print(var.test(a,b))
}

