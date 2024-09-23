## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidyft)

# make copies
copy(iris) -> a
copy(mtcars) -> b

# before
class(a)
class(b)

# convert codes
lapply(ls(),get) %>%
  lapply(setDT) %>% 
  invisible()

# after
class(a)
class(b)

## -----------------------------------------------------------------------------
rm(list = ls())

library(tidyft)
# make a large data.frame
iris[rep(1:nrow(iris),1e4),] -> dt
# size: 1500000 rows, 5 columns
dim(dt)
# save as fst table
as_fst(dt) -> ft
# remove the data.frame from RAM
rm(dt)

# inspect the fst table of large iris
ft
summary_fst(ft)

# list the variables in the environment
ls() # only the ft exists

## -----------------------------------------------------------------------------
ft %>% 
  slice_fst(5555:6666)  # get 5555 to 6666 row

## -----------------------------------------------------------------------------

sys_time_print({
  res =  ft %>% 
   select_fst(Species,Sepal.Length,Sepal.Width) %>% 
   rename(group = Species,sl = Sepal.Length,sw = Sepal.Width) %>% 
   arrange(group,sl) %>% 
   filter(sl > 5) %>% 
   distinct(sl,.keep_all = TRUE) %>% 
   summarise(sw = max(sw),by = group)
})

res
  

## -----------------------------------------------------------------------------

rm(list = ls())

library(profvis)
library(data.table)
library(dplyr)
library(dtplyr)
library(tidyft)


# make a large data.frame
iris[rep(1:nrow(iris),1e4),] -> dt
# size: 1500000 rows, 5 columns
dim(dt)
# save as fst table
as_fst(dt) -> ft
# remove the data.frame from RAM
rm(dt)
  

profvis({
  
  res1 = ft %>% 
    select_fst(Species,Sepal.Length,Sepal.Width,Petal.Length) %>% 
    dplyr::select(-Petal.Length) %>% 
    dplyr::rename(group = Species,sl = Sepal.Length,sw = Sepal.Width) %>% 
    dplyr::arrange(group,sl) %>% 
    dplyr::filter(sl > 5) %>% 
    dplyr::distinct(sl,.keep_all = TRUE) %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(sw = max(sw))
  
  res2 = ft %>% 
    select_fst(Species,Sepal.Length,Sepal.Width,Petal.Length) %>% 
    lazy_dt() %>% 
    dplyr::as_tibble() %>% 
    dplyr::select(-Petal.Length) %>% 
    dplyr::rename(group = Species,sl = Sepal.Length,sw = Sepal.Width) %>% 
    dplyr::arrange(group,sl) %>% 
    dplyr::filter(sl > 5) %>% 
    dplyr::distinct(sl,.keep_all = TRUE) %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(sw = max(sw)) %>% 
    as.data.table()
  
  res3 = ft[,c("Species","Sepal.Length","Sepal.Width","Petal.Length")] %>%  
    setDT() %>%
    .[,.SD,.SDcols = -"Petal.Length"] %>% 
    setnames(old =c("Species","Sepal.Length","Sepal.Width"),
             new = c("group","sl","sw")) %>% 
    setorder(group,sl) %>% 
    .[sl>5] %>% unique(by = "sl") %>% 
    .[,.(sw = max(sw)),by = group]
  
  
  res4 =  ft %>% 
    tidyft::select_fst(Species,Sepal.Length,Sepal.Width,Petal.Length) %>% 
    tidyft::select(-Petal.Length) %>% 
    tidyft::rename(group = Species,sl = Sepal.Length,sw = Sepal.Width) %>% 
    tidyft::arrange(group,sl) %>% 
    tidyft::filter(sl > 5) %>% 
    tidyft::distinct(sl,.keep_all = TRUE) %>% 
    tidyft::summarise(sw = max(sw),by = group)
  
  
})

setequal(res1,res2)
setequal(res2,res3)
setequal(res3,res4)


## -----------------------------------------------------------------------------
sessionInfo()

