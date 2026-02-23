
#librarian::shelf(staRdom, quiet = TRUE)
library(staRdom, quietly = TRUE)
options(scipen = 999)

choose_directory <- 
  function(
    caption = 'Select data directory') {
    if (exists('utils::choose.dir')) {
      choose.dir(caption = caption) 
    } else {
      tk_choose.dir(caption = caption)
    }
  }

convert_aqualog <- function(){
  library(tcltk)
  library(utils)
  
  # choose folder where corrected data will be stored.
  cat("\nchoose destination folder")
  # destination_folder <- "~/Projects/Ch3_Calcium/processed-data"
  destination_folder <- choose_directory()
  
  x <- "yes"
  while (x == "yes") {
    # choose folder where original aqualog data are stored.
    cat("\nchoose folder where original aqualog data are stored.")
    main_folder <- choose_directory()
    
    cat("\nchoose name for project")
    proj_name <- readLines(n = 1)
    
    abs_files <- list.files(main_folder,pattern = "*ABS.dat")
    
    dir.create(paste(destination_folder,"/abs/", proj_name,sep = ""))
    dir.create(paste(destination_folder,"/eem/", proj_name,sep = ""))
    abs_cor_folder <- paste(destination_folder,"/abs/", proj_name, sep = "")
    eem_cor_folder <- paste(destination_folder,"/eem/", proj_name, sep = "")
    
    
    
    for (i in 1:length(abs_files)) {
      abs_temp <- read.table(paste(main_folder,"/", abs_files[i], sep = ""), sep = "\t")
      abs_temp <- subset(abs_temp,!is.na(V2))
      write.table(abs_temp,paste(abs_cor_folder,"/",gsub("ABS.dat",".csv",abs_files[i],fixed = TRUE), sep = ""), row.names = FALSE, col.names = FALSE, sep = ",")
    }
    
    
    sem_files <- list.files(main_folder, pattern = "*SEM.dat")
    
    for (i in 1:length(sem_files)) {
      abs_temp <- read.table(paste(main_folder,"/", sem_files[i], sep = ""), sep = "\t")
      abs_col <- ncol(abs_temp)
      abs_temp2 <- abs_temp[,2:abs_col]
      order_indices <- order(as.numeric(abs_temp2[1, ]))
      abs_temp2 <- abs_temp2[, order_indices]
      abs_temp3 <- cbind.data.frame(abs_temp[,1], abs_temp2)
      write.table(abs_temp3,paste(eem_cor_folder,"/",gsub("SEM.dat",".csv", sem_files[i], fixed = TRUE), sep = ""), row.names = FALSE, col.names = FALSE, sep = ",")
    }
    
    
    bem_files <- list.files(main_folder, pattern = "*BEM.dat")
    # use this to create a new blank file for every *BEM.dat file. Note - since the blank sample is associated/applied to each EEM, the blank files with the *BEM.dat extension are just repeated for each sample you collected 
    # for (i in 1:length(bem_files)) {
    #   bem_temp <- read.table(paste(main_folder,"/", bem_files[1],sep = ""), sep = "\t")
    #   bem_col <- ncol(bem_temp)
    #   bem_temp2 <- bem_temp[,2:bem_col]
    #   bem_temp2 <- bem_temp2[,order(bem_temp2[1,])]
    #   bem_temp3 <- cbind.data.frame(bem_temp[,1], bem_temp2)
    #   write.table(bem_temp3,paste(eem_cor_folder,"/",gsub("BEM.dat","_blank.csv", bem_files[i], fixed = TRUE), sep = ""), row.names = FALSE, col.names = FALSE, sep = ",")
    # }
    
    # if you don't want to create a ton of repeated blank files, use the R script instead
    bem_files <- list.files(main_folder, pattern = "*BEM.dat")
    bem_temp <- read.table(paste(main_folder, "/", bem_files[1], sep = ""), sep = "\t")
    bem_col <- ncol(bem_temp)
    bem_temp2 <- bem_temp[, 2:bem_col]
    order_indices <- order(as.numeric(bem_temp2[1, ]))
    bem_temp2 <- bem_temp2[, order_indices]
    bem_temp3 <- cbind.data.frame(bem_temp[, 1], bem_temp2)
    write.table(bem_temp3, paste(eem_cor_folder, "/", gsub("BEM.dat", "_blank.csv", bem_files[i], fixed = TRUE), sep = ""), row.names = FALSE, col.names = FALSE, sep = ",")
    
    print("\nCorrect Another Folder? (yes or no)")
    x<-readLines(n=1)
    print(x)  
    
  }
}


#### Re-format eem_metatemplate function ####

eem_metatemplate <- function(eem_list = NULL, absorbance = NULL){
  if(!is.null(eem_list)){
    t1 <- data.frame(sample = eem_names(eem_list), eem_location = lapply(eem_list,function(eem) eem$location) %>% unlist(),stringsAsFactors = FALSE)
  }
  if(!is.null(absorbance)){
    loc <- attr(absorbance,"location")
    t2 <- data.frame(sample = colnames(absorbance) %>% .[. != "wavelength"], abs_location = loc, stringsAsFactors = FALSE)
    if(exists("t1")) t1 <- full_join(t1,t2,by="sample") else t1 <- t2
  }
  t1
}