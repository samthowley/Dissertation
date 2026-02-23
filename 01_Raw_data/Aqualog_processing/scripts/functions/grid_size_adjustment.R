adjust_fluorescence <- function(data, cores = parallel::detectCores(logical = FALSE),  emission_range, excitation_range, emission_step, excitation_step) 
  {
  library('parallel')
  library('tidyverse')
  library('doParallel')
  library('foreach')
  
  cl <- parallel::makeCluster(spec = min(cores, length(data)), type = "PSOCK")
  doParallel::registerDoParallel(cl)
  
  
  eem_list <- 
    foreach::foreach(i = 1:length(data), 
                     .export = ls(globalenv()),
                     .packages = c('tidyverse', 'doParallel', 'foreach')) %dopar% {
      
      # data <- eem_list
      eem <- data[[i]]
      # if(type == 1 | type == TRUE){
      x <- 
        eem$x %>%
        data.frame() %>%
        `colnames<-`(eem$ex) %>%
        `rownames<-`(eem$em) %>% 
        mutate(em = eem$em) %>%
        gather("ex","z",-em) %>%
        mutate_all(as.numeric)
      
      x2 <- x %>%
        filter(!is.na(z))
      
      # adjust fluoresence 
       x3 <- MBA::mba.points(xyz = x2 %>% 
                              select(em,ex,z),
                            xy.est = expand.grid(em = seq(emission_range[1], emission_range[2], 
                                                          by = emission_step), 
                                                 ex = seq(excitation_range[1], excitation_range[2], 
                                                          by = excitation_step)),
                            verbose = TRUE, extend = FALSE) 
       
       eem$x <- 
         x3$xyz.est[,3] %>% 
         matrix(nrow = 221, ncol = 188) 
       
       eem$em <- 
         seq(emission_range[1], emission_range[2], 
             by = emission_step)
       
       eem$ex <- 
         seq(excitation_range[1], excitation_range[2], 
             by = excitation_step)
       
       eem

    # }
  }
  stopCluster(cl)
  
  class(eem_list) <- "eemlist"
  eem_list
}
 