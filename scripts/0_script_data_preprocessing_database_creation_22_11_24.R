########################################
########################################
#####This script contains the steps used in the preprocessing of the LEO data used in the ML models. The code containned here is not required to run the examples present in the other scripts present in this folder
########################################
########################################

########
########
##Libraries
library(dplyr)
library(lubridate)
########
########

####################
####################
###Creating a database with the monthly means of each individual welfare indicator and the mean overall welfare score
####################
####################

######
###Reading welfare data and merging
######

files<-dir("data/wellfare_indicators/")

files<-files[-grep(".zip",files)]

welfare.db<-NULL
for(f in files){
  
  print(paste("importing",f,sep=" "))
  
  tmp.db<-read.table(paste0("data/wellfare_indicators/",f),h=T,sep=",")
  
  welfare.db<-rbind(tmp.db,welfare.db)
  
}

###Defining IDs as factors
welfare.db$idMisuraPrimaria<-as.factor(welfare.db$idMisuraPrimaria)

welfare.db$idLuogo<-as.factor(welfare.db$idLuogo)

######
###Reading ID data and merging
######

files<-dir("data/unique_ID_data/")

files<-files[-grep(".zip",files)]

id.db<-NULL
for(f in files){
  
  print(paste("importing",f,sep=" "))
  
  tmp.db<-read.table(paste0("data/unique_ID_data/",files[1]),h=T,sep=",")
  
  id.db<-rbind(id.db,tmp.db)
  
}


######
###Reading personal data and merging
######

files<-dir("../../data/personal_data/")

files<-files[-grep(".zip",files)]

pers.db<-NULL
for(f in files){
  
  print(paste("importing",f,sep=" "))
  
  tmp.db<-read.table(paste0("../../data/personal_data/",files[1]),h=T,sep=",")
  
  
  pers.db<-rbind(pers.db,tmp.db)
  
}


###Defining IDs as factors
pers.db$idMisuraPrimaria<-as.factor(pers.db$idMisuraPrimaria)

pers.db$idAnimale<-as.factor(pers.db$idAnimale)


#######
##Matching IDs
#######

welfare.db<-welfare.db[!duplicated(welfare.db),]

welfare.db.noNA<- welfare.db[complete.cases(welfare.db[, 13:17]), ]

welfare.month<-welfare.db.noNA %>%  group_by(idMisuraPrimaria, anno, mese) %>%
  summarize(mean_repro = mean(IbaDimIndicatoreDiRegolaritaRiproduttiva),
            mean_long=mean(IbaParIndicatoreDiLongevita),
            mean_mast=mean(IbaSccIndicatoreDiDisturbiDellaMammella),
            mean_keto=mean(IbaKetIndicatoreDiRischioDiChetosiSubclinica),
            mean_acid=mean(IbaAciIndicatoreDiRischioDiAcidosiSubclinica),
            min_day=min(giorno),
            month=min(mese),
            year=min(anno),
            .groups = 'drop') %>% as.data.frame()

###########
##Checking number of welfare classes and distribution
###########

##Number of good records
good.wel<-rowSums(welfare.month[,c("mean_repro", "mean_long", "mean_mast", "mean_keto", "mean_acid")]<10)

##Number of bad records
bad.wel<-rowSums(welfare.month[,c("mean_repro", "mean_long", "mean_mast", "mean_keto", "mean_acid")]>20)

##Number of intermediary records
int.wel<-(5-(good.wel+bad.wel))

welfare.month$Welfare_value<-as.numeric(paste0(bad.wel,int.wel,good.wel))

welfare.month$Welfare_class<-ifelse(welfare.month$Welfare_value<10,"G",ifelse(welfare.month$Welfare_value>10 & welfare.month$Welfare_value<=50,"I","B"))

write.table(welfare.month, file="monthly_measures/Welfare_monthly_21_10_24.txt", col.names=T,row.names=F,quote=F,sep="\t")


####################
####################
###Creating a database with the records for each one of the features used in the predictive models
####################
####################

##Defining the function to be used to merge the individual files with the measures
merge_db<-function(dir_path){
  
  k<-0
  
  files<-dir(dir_path)
  
  files<-files[-grep(".zip",files)]
  
  out.db<-NULL
  for(f in files){
    
    k<-k+1
    
    print(paste("Reading file ",k,"from ", length((files))))
    
    tmp.db<-data.table::fread(paste0(dir_path,f))
    
    
    out.db<-rbind(out.db,tmp.db)
    
  }
  
  out.db<-as.data.frame(out.db)
  
  return(out.db)
}

#######
####Monthly welfare
#######

welfare.db<-read.table("Welfare_monthly_21_10_24.txt",h=T,sep="\t")

welfare.db$idMisuraPrimaria<-as.character(welfare.db$idMisuraPrimaria)
welfare.db$idLuogo<-as.character(welfare.db$idLuogo)

###########
####Importing data for all features and filtering based on welfare animals
###########

dir_folders<-dir("hackathon_LEO/data")

for(d in dir_folders){
  
  print(paste("Starting analysis for:",d))
  
  tmp.path<-paste("data/",d,"/",sep="")
  
  tmp.read<-merge_db(dir_path=tmp.path)
  
  tmp.read$idAnimale<-as.character(tmp.read$idAnimale)
  
  tmp.read$idMisuraPrimaria<-as.character(tmp.read$idMisuraPrimaria)
  
  tmp.read<-tmp.read[which(tmp.read$idAnimale%in%welfare.db$idLuogo | tmp.read$idMisuraPrimaria%in%welfare.db$idLuogo),]
  
  tmp.path.out<-paste("feature_db/",gsub("_data","",d),"_21_10_24.txt",sep="")
  
  write.table(tmp.read, file=tmp.path.out, col.names=T,row.names=F,quote=F,sep="\t")
  
  
}



####################
####################
###Creating the database witht he monthly means of the records for each one of the features used in the predictive models
####################
####################

#####
##Acetone
####

acetone.db<-read.table("feature_db/acetone_21_10_24.txt",h=T,sep="\t")

head(acetone.db)

mean.month.acetona <- acetone.db %>%
  group_by(idAnimale, mese, anno) %>%
  summarise(
    mean_measure = mean(valoreMisura, na.rm = TRUE),
    count_measure = n()
  ) %>%
  ungroup() %>% as.data.frame()

write.table(mean.month.acetona, file="monthly_measures/acetona_month_mean_30_10.24.txt", col.names=T, row.names=F, quote=F,sep="\t")


#####
##Conductivity
####

conductivity.db<-read.table("feature_db/average_conductivity_21_10_24.txt",h=T,sep="\t")

head(conductivity.db)

mean.month.conductivity <- conductivity.db %>%
  group_by(idAnimale, mese, anno) %>%
  summarise(
    mean_measure = mean(ConducibilitaMedia, na.rm = TRUE),
    count_measure = n()
  ) %>%
  ungroup() %>% as.data.frame()

head(mean.month.conductivity)

write.table(mean.month.conductivity, file="monthly_measures/Milk_conductivity_month_mean_30_10.24.txt", col.names=T, row.names=F, quote=F,sep="\t")


#####
##BHBA
####

bhba.db<-read.table("feature_db/BHBA_21_10_24.txt",h=T,sep="\t")

head(bhba.db)

mean.month.bhba <- bhba.db %>%
  group_by(idAnimale, mese, anno) %>%
  summarise(
    mean_measure = mean(valoreMisura, na.rm = TRUE),
    count_measure = n()
  ) %>%
  ungroup() %>% as.data.frame()

head(mean.month.bhba)

write.table(mean.month.bhba, file="monthly_measures/bhba_month_mean_30_10.24.txt", col.names=T, row.names=F, quote=F,sep="\t")

#####
##Fat
####

fat.db<-read.table("feature_db/fat_21_10_24.txt",h=T,sep="\t")

head(fat.db)

mean.month.fat <- fat.db %>%
  group_by(idAnimale, mese, anno) %>%
  summarise(
    mean_measure = mean(valoreMisura, na.rm = TRUE),
    count_measure = n()
  ) %>%
  ungroup() %>% as.data.frame()

head(mean.month.fat)

write.table(mean.month.fat, file="monthly_measures/fat_month_mean_30_10.24.txt", col.names=T, row.names=F, quote=F,sep="\t")

#####
##Lactose
####

lactose.db<-read.table("feature_db/lactose_21_10_24.txt",h=T,sep="\t")

head(lactose.db)

mean.month.lactose <- lactose.db %>%
  group_by(idAnimale, mese, anno) %>%
  summarise(
    mean_measure = mean(valoreMisura, na.rm = TRUE),
    count_measure = n()
  ) %>%
  ungroup() %>% as.data.frame()

head(mean.month.lactose)

write.table(mean.month.lactose, file="monthly_measures/lactose_month_mean_30_10.24.txt", col.names=T, row.names=F, quote=F,sep="\t")

#####
##Milk conductivity
####

cond.db<-read.table("feature_db/milk_cond_21_10_24.txt",h=T,sep="\t")

head(cond.db)

mean.month.cond <- cond.db %>%
  group_by(idAnimale, mese, anno) %>%
  summarise(
    mean_measure = mean(valoreMisura, na.rm = TRUE),
    count_measure = n()
  ) %>%
  ungroup() %>% as.data.frame()

head(mean.month.cond)

write.table(mean.month.cond, file="monthly_measures/milk_cond_month_mean_30_10.24.txt", col.names=T, row.names=F, quote=F,sep="\t")

#####
##Protein
####

protein.db<-read.table("feature_db/protein_21_10_24.txt",h=T,sep="\t")

head(protein.db)

mean.month.protein <- protein.db %>%
  group_by(idAnimale, mese, anno) %>%
  summarise(
    mean_measure = mean(valoreMisura, na.rm = TRUE),
    count_measure = n()
  ) %>%
  ungroup() %>% as.data.frame()

head(mean.month.protein)

write.table(mean.month.protein, file="monthly_measures/protein_month_mean_30_10.24.txt", col.names=T, row.names=F, quote=F,sep="\t")

#####
##SCC
####

scc.db<-read.table("feature_db/SCC_21_10_24.txt",h=T,sep="\t")

head(scc.db)

mean.month.scc <- scc.db %>%
  group_by(idAnimale, mese, anno) %>%
  summarise(
    mean_measure = mean(valoreMisura, na.rm = TRUE),
    count_measure = n()
  ) %>%
  ungroup() %>% as.data.frame()

head(mean.month.scc)

write.table(mean.month.scc, file="monthly_measures/SCC_month_mean_30_10.24.txt", col.names=T, row.names=F, quote=F,sep="\t")

#####
##Urea
####

urea.db<-read.table("feature_db/urea_21_10_24.txt",h=T,sep="\t")

head(urea.db)

mean.month.urea <- urea.db %>%
  group_by(idAnimale, mese, anno) %>%
  summarise(
    mean_measure = mean(valoreMisura, na.rm = TRUE),
    count_measure = n()
  ) %>%
  ungroup() %>% as.data.frame()

head(mean.month.urea)

write.table(mean.month.urea, file="monthly_measures/urea_month_mean_30_10.24.txt", col.names=T, row.names=F, quote=F,sep="\t")


####################
####################
###Creating the database with the monthly means of the records for each one of the features and the welfare indicators that were used in the predictive models
####################
####################

#######
####Monthly welfare
#######

welfare.db<-read.table("monthly_measures/Welfare_monthly_21_10_24.txt",h=T,sep="\t")

colnames(welfare.db)[2]<-"idAnimale"

welfare.db$idAnimale<-as.character(welfare.db$idAnimale)

##Convert month and year to Date objects for comparison
welfare.db <- welfare.db %>%
  mutate(date1 = make_date(anno, mese, 1))

##Importing individual database and calculating the difference (in weeks) between the date of the measure and the date when the welfare indicator was calculates. This procedure was done to garantee that the mean values of the fetures in the previous month of the welfare indicator was used in the ML models.
files<-dir("monthly_measures/")

out.list<-list()
for(f in files){
  
  print(paste("Starting analysis for:", f))
  
  tmp.path<-paste("monthly_measures/",f,sep="")
  
  tmp.db<-read.table(tmp.path,h=T,sep="\t")
  
  tmp.db$idAnimale<-as.character(tmp.db$idAnimale)
  
  tmp.db <- tmp.db %>%
    mutate(date2 = make_date(anno, mese, 1))
  
  result <- welfare.db %>%
    left_join(tmp.db, by = "idAnimale", suffix = c(".df1", ".df2")) %>%
    filter(date2 < date1) %>%
    group_by(idAnimale, date1) %>%
    slice(which.max(date2)) %>%
    ungroup() %>%
    select(idAnimale, date1, date2)
  
  
  result$diff_week<-as.numeric(difftime(result$date1, result$date2, units = "weeks"))
  
  
  result <- result %>%
    left_join(welfare.db %>% select(idAnimale, Welfare_value,date1, Welfare_class), 
              by = c("idAnimale", "date1"))
  
  
  result <- result %>%
    left_join(tmp.db %>% select(idAnimale, mean_measure,date2, count_measure), 
              by = c("idAnimale", "date2")) %>% as.data.frame()
  
  colnames(result)[which(colnames(result)=="mean_measure")]<-paste("mean_",gsub("_month_mean_30_10.24.txt","",f),sep="")
  
  colnames(result)[which(colnames(result)=="count_measure")]<-paste("count_",gsub("_month_mean_30_10.24.txt","",f),sep="")
  
  out.list[[gsub("_month_mean_30_10.24.txt","",f)]]<-result
  
}


## Assigning common columns
common_columns <- c("idAnimale", "date1", "date2", "diff_week", "Welfare_value", "Welfare_class")

##Creating a new data frame with all unique combinations of the common columns across all data frames in the list
unique_combinations <- do.call(rbind, lapply(out.list, function(df) df[common_columns]))
unique_combinations <- unique(unique_combinations)

##Starting with this data frame to merge all unique columns from each data frame in the list
result_df <- unique_combinations

result_df$id2<-paste0(result_df$idAnimale,"_",result_df$date2)

##Retrieving the mean values of the features
for(i in seq_along(out.list)) {
  
  print(names(out.list[i]))
  
  # Get the name of the data frame and its unique columns (i.e., those not in common_columns)
  df <- out.list[[i]]
  
  df$id2<-paste0(df$idAnimale,"_",df$date2)
  
  result_df[,paste0("mean","_",names(out.list[i]))]<-df[match(result_df$id2, df$id2),paste0("mean","_",names(out.list[i]))]
  
  result_df[,paste0("count","_",names(out.list[i]))]<-df[match(result_df$id2, df$id2),paste0("count","_",names(out.list[i]))]
  
  
}

##Filtering the data frame to obtain a version with only the reruired columns
result_df_filter<-result_df[,c("idAnimale","date1","date2",
                               "diff_week","Welfare_value",
                               "Welfare_class","mean_acetona",
                               "mean_bhba","mean_casein",
                               "mean_daily_milking","mean_dscc",
                               "mean_fat","mean_lactose",
                               "mean_milk_cond",
                               "mean_Milk_conductivity", "mean_protein",
                               "mean_SCC","mean_SFA",
                               "mean_UFA","mean_urea")]

##This step was performed to check the percentage of missing data for each column included in the data frame
for(i in 1:ncol(result_df_filter)){
  
  print(colnames(result_df_filter)[i])
  
  print(length(which(is.na(result_df_filter[,i])))/nrow(result_df_filter))
  
}

##Checking the number of observations withou missign data
dim(na.omit(result_df_filter_2))

##Filtering only complete cases
pred.db<-na.omit(result_df_filter_2)

##Saving the database used to create the train and test sets. These sets were created randomly spliting the actual database in 70% and 30% of the observations, respectively. In order to assure replicability, the function set.seed() was used with the seed 123 during the randomly spliting process.
write.table(pred.db, file="Prediction_db_WelfareScore_noNA_30_20_24.txt", col.names=T, row.names=F,quote=F,sep="\t")
