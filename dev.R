
library(pacman)
p_load(devtools,usethis,roxygen2,pkgdown,badger)
p_load(fst,stringr,data.table,tidyft)

document()
#install(upgrade = "never",dependencies = F,quick = T)
install(upgrade = "never",dependencies = F)
#install(quick = T)
.rs.restartR()
rm(list = ls())
library(tidyft)

options(pkgdown.internet = F)
build_site()

submit_cran()


lapply(ls(),get) %>%
  lapply(setDT)


library(reprex)

reprex({
  library(tidyft)
  # get first 5 rows of iris
  as.data.table(iris)[1:5] -> a
  #show
  a
  # if you select
  a %>% select(1:3)
  # you lose the unselected columns forever
  a
}, venue = "R") -> res


library(pryr)
a = as.data.table(iris)
mem_change(a[,one:=1] -> b)
mem_change(a[,one:=1][] -> c)
address(a);address(b);address(c)

rm(list = ls())

library(profvis)
library(dplyr)
library(tidyft)
as.data.frame(starwars) -> starwars
starwars[sample.int(1:nrow(starwars),1e6,replace = T),] -> starwars

profvis({
  starwars %>%
    dplyr::as_tibble() %>%
    dplyr::select(name, dplyr::ends_with("color")) %>%
    dplyr::arrange(hair_color,skin_color,eye_color) -> a

  starwars %>%
    tidyft::setDT() %>%
    tidyft::select("name|color$") %>%
    tidyft::arrange(hair_color,skin_color,eye_color) -> b


})

all.equal(a,b)



rm(list = ls())

library(profvis)
library(dplyr)
library(tidyft)
as.data.frame(starwars) -> starwars
starwars[sample.int(1:nrow(starwars),1e6,replace = T),] -> starwars
copy(starwars) -> dat1
copy(starwars) -> dat2
copy(starwars) -> dat3

profvis({
  a = dat1 %>%
    dplyr::as_tibble() %>%
    dplyr::select(name, dplyr::ends_with("color")) %>%
    dplyr::arrange(hair_color,skin_color,eye_color)

  b = setorder(setDT(dat2)[,.SD,.SDcols = patterns("name|color$")],
           hair_color,skin_color,eye_color)

  c = dat3 %>%
    tidyft::setDT() %>%
    tidyft::select("name|color$") %>%
    tidyft::arrange(hair_color,skin_color,eye_color)


})

all.equal(a,b)
all.equal(b,c)

