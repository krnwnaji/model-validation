
# [README]
# 
# This script created by: Kurniawan
# date: 26/06/2023
# Title: Model data validation counter
# 
# Input:
# - Shapefile that already intersect between label and model
# - filename format used by this script Grid_PB_SJ_06J6001300
# Notes:
# - please modify 'change this section' to meet files atribute
# 
# Question could email to : kurniawan.wicaksono@sinarmasforestry.com


# applying packages
library(dplyr)
library(sf)
library(tidyverse)
library(stringr)



# User Parameter ----------------------------------------------------------
################################# Change this parameter ###################

# define path
input_path <- "Input/"
output_plot_path <- "Output/Plot/"
output_summ_path <- "Output/"

# define format
model_true <- "1"
model_false <- "0"
DV_true <- "weed"
DV_false <- "no weed"



# Script for data validation ----------------------------------------------

st=format(Sys.time(), "%Y%m%d_%H%M")

# Running plot
dirs <- list.dirs("Input/",full.names = TRUE, recursive = FALSE)
dirs

for (i in dirs){
  tryCatch(
    #try to do this
    {
      input <- list.files(i,pattern = "*.shp$")
      print(paste0("processing data from ", input))
      # select petak if the format is Grid_PB_SJ_06J6001300
      split <- strsplit(input,"_")[[1]][4]
      petak <- sub("*.shp","",split)
      filename <- sub("*.shp","",input)
      if (dir.exists(output_plot_path) == TRUE){
        print("dir exist, continue process")
      } else {
        dir.create(output_plot_path,recursive = FALSE)
      }
      # read input data
      r_input <- read_sf(paste0(i,"/",input)) %>% 
        as_tibble()
      # create data frame to define class
      df <- r_input %>% 
        mutate(
          class = case_when(
            weed == model_true & tolower(DV) == DV_true ~ "TP",
            weed == model_false & tolower(DV) == DV_true ~ "FN",
            weed == model_true & tolower(DV) == DV_false ~ "FP",
            weed == model_false & tolower(DV) == DV_false ~ "TN"
          )
        )
      # calculate each class
      summ <- df %>% 
        group_by(Plot) %>% 
        summarise(
          TP = sum(class=='TP', na.rm = TRUE),
          FN = sum(class=='FN', na.rm = TRUE),
          FP = sum(class=='FP', na.rm = TRUE),
          TN = sum(class=='TN', na.rm = TRUE),
        ) %>% 
        mutate(
          petakid = petak
        ) %>% drop_na() # remove missing value
      # create variable on each class
      TP <- summ %>% select("TP")
      FN <- summ %>% select("FN")
      FP <- summ %>% select("FP")
      TN <- summ %>% select("TN")
      # summarise total grid
      gridPlot <- df %>% 
        group_by(Plot) %>% 
        summarise(
          n = n()
        ) %>% drop_na()
      gridPlot
      # define formulas
      accuracy = (TP+TN)/(TP+TN+FP+FN)
      precission = (TP)/(TP+FP)
      recall = TP/(TP+FN)
      f1score = 2 * ((precission*recall)/(precission+recall))
      
      
      # create new table by desired output
      plot.result <- tibble(
        plot = summ$Plot,
        petak = summ$petakid,
        `total grid` = nrow(df),
        `total subgrid` = gridPlot$n,
        TP = TP,
        FP = FP,
        FN = FN,
        TN = TN,
        `accuracy %`=round(accuracy*100,2),
        `Precision %`=round(precission*100,2),
        `recall %`=round(recall*100,2),
        `f1score %`=round(f1score*100,2),
        `coverage issues %`=round(((TP+FN)/`total subgrid`)*100,2)
      )
      
      # print result
      matrix.plot <- as.matrix(plot.result)
      table <- as.data.frame(matrix.plot)
      print("Table successfully Created")
      print(table)
      
      # export to CSV
      write <- write.table(
        table,
        file=paste0(output_plot_path,'result_', filename,'.csv'), 
        row.names = FALSE, 
        quote = FALSE,
        append = FALSE,
        sep = ","
      )
      
      print("all process done")
      
    },
    #if an error occurs, tell me the error
    error=function(e) {
      message('An Error Occurred')
      print(e)
    },
    #if a warning occurs, tell me the warning
    warning=function(w) {
      message('A Warning Occurred')
      print(w)
      return(NA)
    }
  )
  
}

# combine all plots
plot_files <- list.files(output_plot_path,full.names = TRUE) %>% 
  lapply(read.csv,sep = ",") %>% 
  bind_rows()

write.table(
  plot_files,
  file=paste(output_summ_path,st,"_summary.csv",sep = ""), 
  row.names = FALSE, 
  quote = FALSE,
  append = FALSE,
  sep = ","
)

# create summarize for all plots
all_summ <- plot_files %>% 
  group_by(petak) %>% 
  summarise(
    total_subs = sum(total.subgrid),
    TP = sum(TP),
    FP = sum(FP),
    FN = sum(FN),
    TN = sum(TN)
  ) %>% 
  mutate(
    accuracy = round(((TP+TN)/(TP+TN+FP+FN))*100,2),
    precission = round(((TP)/(TP+FP))*100,2),
    recall = round((TP/(TP+FN))*100,2),
    f1score = round((2 * ((precission*recall)/(precission+recall))),2),
    `coverage issues %`=round(((TP+FN)/total_subs)*100,2)
  )
all_summ

write.table(
  all_summ,
  file=paste(output_summ_path,st,"_summary_petak.csv",sep = ""), 
  row.names = FALSE, 
  quote = FALSE,
  append = FALSE,
  sep = ","
)




