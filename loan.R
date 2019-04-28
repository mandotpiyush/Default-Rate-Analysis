
list.of.packages <- c("dplyr", "tidyr", "ggplot2", "corrplot", "gridExtra", "choroplethr", "choroplethrMaps")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(choroplethr)
library(gridExtra)

loan <- read.csv("loan.csv", header = T, stringsAsFactors = F)
str(loan, list.len=ncol(loan))

#Check for NA values
na_pc <- round(100*colSums(is.na(loan))/nrow(loan),2)
na_pc <- na_pc[na_pc < 20]
sel_cols = names(na_pc)
str(loan %>% select(sel_cols))
loan1 <- loan %>% select(sel_cols)
#Looking at the columns stil following columns can be removed
# id
# member_id
# emp_title
# url
# desc
# title
loan1 <- loan1 %>% select(-c('id', 'member_id', 'emp_title', 'url', 'desc', 'title'))
summary(as.data.frame(loan1))

#Looking at summary we can further eliminate the following columns:
# collections_12_mths_ex_med - values 0 or NA
# policy_code - all values 1
# acc_now_delinq - all values 0
# chargeoff_within_12_mths
# delinq_amnt
# pub_rec_bankruptcies
# tax_liens
# recoveries
# collection_recovery_fee
# total_rec_late_fee
# out_prncp
# out_prncp_inv

loan1 <- loan1 %>% select(-c('collections_12_mths_ex_med', 'policy_code', 'acc_now_delinq', 
                             'chargeoff_within_12_mths', 'delinq_amnt', 'pub_rec_bankruptcies', 
                             'tax_liens', 'recoveries', 'collection_recovery_fee', 'total_rec_late_fee',
                             'out_prncp', 'out_prncp_inv'))
loan1 <- as.data.frame(unclass(loan1))
summary(loan1)


# As per the business context the following variables will not be important. 
# This is because these are consumers behaviour history and looks irrelavant.
# The loan financing company should look into customer demographics & loan product details.
# Removing columns: delinq_2yrs,earliest_cr_line,inq_last_6mths,open_acc,pub_rec,revol_bal,revol_util
# total_acc, initial_list_status, total_pymnt, total_pymnt_inv, total_rec_prncp, total_rec_int, last_pymnt_d
# last_pymnt_amnt,next_pymnt_d,last_credit_pull_d,application_type

fin_behavior_cols <- c('delinq_2yrs','earliest_cr_line','inq_last_6mths','open_acc','pub_rec','revol_bal','revol_util',
                       'total_acc', 'initial_list_status', 'total_pymnt', 'total_pymnt_inv', 'total_rec_prncp', 'total_rec_int', 'last_pymnt_d',
                       'last_pymnt_amnt','next_pymnt_d','last_credit_pull_d','application_type')
loan1 <- loan1 %>% select(-fin_behavior_cols)
summary(as.data.frame(loan1))

# We'll also remove the following variables
# pymnt_plan - only 1 factor and meaningless
# zip_code, addr_state - Not useful for understanding & too many levels

loan1 <- loan1 %>% select(-c('zip_code', 'addr_state', 'pymnt_plan'))
summary(as.data.frame(loan1))


#Further Data Cleaning
loan1$int_rate <- as.numeric(gsub("%", "", as.character(loan1$int_rate)))


corr <- cor(loan1 %>% select(colnames(loan1)[sapply(loan1, class) != "factor"]))
corrplot(corr, method=c("number"), type="upper")

#Numerical variables loan_amount, funded_amnt, funded_amnt_inv, installment are highly correlated
#We can take only loan_amount for investigation and disregard others
loan1 <- loan1 %>% select(-c('funded_amnt', 'funded_amnt_inv', 'installment'))

#subgrade has too many level to analyze. Hence dropping it
loan1 <- loan1 %>% select(-c('sub_grade'))

#create new column for default
loan1$defaulted <- as.factor(ifelse(loan$loan_status=='Charged Off', 1, 0))

#Create derived features
loan1$issue_d_mon <- factor(sapply(strsplit(as.character(loan1$issue_d), "-"), function(x) x[1]),
                            levels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                                     'Jul', 'Aug', 'Sep', 'Oct','Nov','Dec'))
loan1$issue_d_year <- as.factor(sapply(strsplit(as.character(loan1$issue_d), "-"), function(x) paste0('20',x[2])))

#drop unnessary column
loan1 <- loan1 %>% select(-c('issue_d', 'loan_status'))

loan1$emp_length <- as.character(loan1$emp_length)
loan1$emp_length <- gsub(" years", "", loan1$emp_length)
loan1$emp_length <- gsub(" year", "", loan1$emp_length)
loan1$emp_length <- gsub("\\+", "", loan1$emp_length)
loan1$emp_length <- gsub("<", "", loan1$emp_length)
loan1$emp_length <- as.numeric(loan1$emp_length)
loan1 <- loan1[!is.na(loan1$emp_length),]


numeric_columns <- colnames(loan1)[sapply(loan1, class) != "factor"]
cat_columns <- colnames(loan1)[sapply(loan1, class) == "factor"]

#Numeric univariate
numeric_summary <- summary(loan1 %>% select(numeric_columns))

#Categorical univariate plot function
categorical_univar_plot <- function(df, var) {
  p <- (loan1 %>% group_by_at(c(var)) %>% 
          summarise(count = n() ) %>% ungroup() %>% 
          ggplot(aes_string(var, 'count', fill=var)) + 
          geom_col() + 
          geom_text(aes(label=count), vjust=-1, size=3)+
          theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
          ggtitle(paste0(var, " distribution")))
  return(p)
}

#Lets look at the default rate
loan1 %>% group_by(defaulted) %>% 
  summarise(count = n() ) %>% ungroup() %>% 
  mutate(count_pc = 100*count/sum(count)) %>% 
  ggplot(aes(defaulted, count_pc, fill=defaulted)) + 
  geom_col() + 
  geom_text(aes(label=paste(round(count_pc,2), "%")), vjust=-1)

#bivariate analysis of categorical variable with default percentage.
categorical_bivar_plot <- function(df, var) {
  p <- (df %>% select_at(c(var, 'defaulted')) %>% group_by_at(c(var)) %>% 
          summarise(mean=mean(as.numeric(as.character(defaulted)))) %>% 
          ungroup %>% 
          mutate(default_percentage=round(100*mean,2)) %>% 
          ggplot(aes_string(var, 'default_percentage', fill=var)) + 
          geom_col(position="identity") + 
          geom_text(aes(label=paste(round(default_percentage,2), "%")), vjust=-1, size=3)+
          theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
          ggtitle(paste0("default percentage vs ", var)))
  return(p)
}

#numeric_box_plot function
numeric_box_plot <- function(df, var) {
  p <- (df %>% ggplot(aes_string('defaulted', var, fill='defaulted')) + 
          geom_boxplot() + 
          ggtitle(paste0("Boxplot ", var)))
  return(p)
}		

#numeric box plots
numeric_box_plots = list()
for (i in 1:length(numeric_columns)){
  numeric_box_plots[[i]] <- numeric_box_plot(loan1, numeric_columns[i])
}

#ecdf_plot function
ecdf_plot <- function(df, var) {
  p <- (df %>% ggplot(aes_string(var, color='defaulted')) + 
          stat_ecdf(geom="step") + 
          ggtitle(paste0("ECDF ", var)))
  return(p)
}

#ecdf_plots for numerical vars
ecdf_plots = list()
for (i in 1:length(numeric_columns)){
  ecdf_plots[[i]] <- ecdf_plot(loan1, numeric_columns[i])
}


#income is a very skewed dist. Create a new var
loan1$annual_inc_log <- log(loan1$annual_inc)
numeric_columns[5] <- 'annual_inc_log'

numeric_box_plots[[length(numeric_columns)]] <- numeric_box_plot(loan1, numeric_columns[length(numeric_columns)])
ecdf_plots[[length(numeric_columns)]] <- ecdf_plot(loan1, numeric_columns[length(numeric_columns)])

#Write a function for binning
loan1$loan_amnt_binned<-factor(ifelse(loan1$loan_amnt<=5000,"Small",
                                      ifelse(loan1$loan_amnt>5000 & loan1$loan_amnt<=15000,"Medium",
                                             ifelse(loan1$loan_amnt>15000 & loan1$loan_amnt<=25000,"High","VeryHigh"))),
                               levels=c('Small', 'Medium', 'High', 'VeryHigh'))


loan1$int_rate_binned<-factor(ifelse(loan1$int_rate<=10,"Low_rate",
                                     ifelse(loan1$int_rate>10 & loan1$int_rate<=15,"Medium_rate","High_rate")),
                              levels=c('Low_rate', 'Medium_rate', 'High_rate'))


loan1$annual_inc_binned<-factor(ifelse(loan1$annual_inc<=50000,"Small",
                                       ifelse(loan1$annual_inc>50000 & loan1$annual_inc<=100000,"Medium",
                                              ifelse(loan1$annual_inc>100000 & loan1$annual_inc<=150000,"High","VeryHigh"))),
                                levels=c('Small', 'Medium', 'High', 'VeryHigh'))

loan1$bin_dti<-factor(ifelse(loan1$dti<=10,"Low_dti",
                             ifelse(loan1$dti>10 & loan1$dti<=20,"Medium_dti","High_dti")),
                      levels=c('Low_dti','Medium_dti','High_dti'))


loan1$emp_length <- factor(ifelse(loan1$emp_length<=1,"freshers",
                                  ifelse(loan1$emp_length>1&loan1$emp_length<=3,"junior",
                                         ifelse(loan1$emp_length>3&loan1$emp_length<=7,"senior","expert"))),
                           levels=c('freshers', 'junior', 'senior', 'expert'))

product_pc <- prop.table(table(loan1$purpose))
product_pc_filtered <- product_pc[product_pc < 0.05]
product_pc_filtered_names <- names(product_pc_filtered)

loan1$purpose <- as.character(loan1$purpose)
loan1$purpose <- factor(ifelse(loan1$purpose %in% product_pc_filtered_names,"All Others",loan1$purpose),
                        levels=c('debt_consolidation','credit_card','home_improvement',
                                 'major_purchase','other','All Others'))

cat_columns <- colnames(loan1)[sapply(loan1, class) == "factor"]

#categorical univariate plots
categorical_univar_plots = list()
for (i in 1:length(cat_columns)){
  categorical_univar_plots[[i]] <- categorical_univar_plot(loan1, cat_columns[i])
}

#categorical bivariate plots
categorical_bivar_plots = list()
for (i in 1:length(cat_columns)){
  categorical_bivar_plots[[i]] <- categorical_bivar_plot(loan1, cat_columns[i])
}

#categorical trivariate plots
categorical_trivar_plots = list()
for (i in 1:length(cat_columns)){
  categorical_trivar_plots[[i]] <- categorical_trivar_plot(loan1, 'purpose', cat_columns[i], 3)
}

#Write a function for variable importance
var_importance <- function(df, var) {
  dp <- data.frame(df%>%select_at(c(var, 'defaulted')) %>% 
                     group_by_at(c(var)) %>% 
                     summarise(default_pc=mean(as.numeric(as.character(defaulted)))))[,2]
  return(max(dp)-min(dp))
}

#list the importance of all variables
cat_vars_importance <- function(df,cat_columns) {
  categorical_imp = c()
  for (i in 1:length(cat_columns)){
    categorical_imp[i] <- round(100*var_importance(df, cat_columns[i]),2)
  }
  df_imp <- data.frame(cbind(cat_columns, categorical_imp)) 
  colnames(df_imp) <- c("column", "importance")
  df_imp$importance <- as.numeric(as.character(df_imp$importance))
  df_imp <- data.frame(df_imp%>% 
                         filter(column != 'defaulted') %>% 
                         arrange(desc(importance)))
  df_imp$column <- factor(df_imp$column, levels=as.character(df_imp[,1]))
  return(df_imp)
}

cat_vars_importance_all_plot <- cat_vars_importance(loan1, cat_columns) %>%
  ggplot(aes(column, importance, fill=column)) +
  geom_col(position="identity") +
  geom_text(aes(label=paste(importance, "%")), vjust=-1, size=3)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ggtitle("Ovarall Variable Importance in default% variation")

#For trivariate analysis lets look at following few feature across few products
trivariate_features = c('grade', 'home_ownership', 'int_rate_binned', 'term', 
                        'loan_amnt_binned', 'annual_inc_binned', 'issue_d_year')

top_products = c('debt_consolidation', 'credit_card', 'home_improvement', 'major_purchase')

trivariate_plots = list()
for(i in 1:length(top_products)){
  trivariate_sub_plots = list()
  for(j in 1:length(trivariate_features)){
    trivariate_sub_plots[[j]] <- categorical_bivar_plot(loan1 %>% filter(purpose==top_products[i]),
                                                        trivariate_features[j])
  }
  trivariate_plots[[i]] <- trivariate_sub_plots
}

#trivariate plot for debt_consolidation
grid.arrange(grobs=trivariate_plots[[1]], ncol=3, top=top_products[1])
grid.arrange(grobs=trivariate_plots[[2]], ncol=3, top=top_products[2])

cat_vars_importance_product_plots = list()
products <- names(table(loan1$purpose))
for (i in 1:length(products)){
  cat_vars_importance_product_plots[[i]] <- 
    cat_vars_importance(loan1 %>% filter(purpose==products[i]), 
                        cat_columns[!cat_columns %in% 'purpose']) %>%
    ggplot(aes(column, importance, fill=column)) +
    geom_col(position="identity") +
    geom_text(aes(label=paste(importance, "%")), vjust=-1, size=3)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    ggtitle(paste0("Variable Importance for: ", products[i], " in default% variation"))
}

#Map plot for state-wise volume
locVol_df <- select(loan, addr_state)
locVol_df <- locVol_df %>% na.omit() %>% group_by(addr_state) %>% 
  dplyr::summarise(value = n())
x <- sapply(locVol_df$addr_state, function(x) state.name[grep(x,state.abb)[1]])				
names(x) <- NULL
locVol_df$region <- tolower(x)
state_choropleth(locVol_df, title = "Loan Volume by State", num_colors = 9)

#Map plot for state-wise Loan Amount
locAmt_df <- select(loan, addr_state, loan_amnt)
locAmt_df$loan_amnt <- as.numeric(locAmt_df$loan_amnt)
locAmt_df <- locAmt_df %>% na.omit() %>% group_by(addr_state) %>% 
  dplyr::summarise(value = sum(loan_amnt, na.rm = TRUE))
x <- sapply(locVol_df$addr_state, function(x) state.name[grep(x,state.abb)[1]])				
names(x) <- NULL
locAmt_df$region <- tolower(x)	
state_choropleth(locAmt_df, title = "Loan Amount by State", num_colors = 9)

