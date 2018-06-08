library(tidyverse)

data<-read_csv2("E:/Projects/worldcup/matches_logo2.csv")

#just to put some dummy data being users' predictions / final outcomes
data<-data %>% 
  mutate(x=1,y=0,z=NA,a=2,b=1,c=1)


impute <- function(x,y,z,a,b,c,group_phase)
{
  if((group_phase==0) && (!is.na(x)) && (!is.na(y)) && (x!=y))
  {
    if (x>y) z<-1
    else z<-2
  }
  else z<-z 
  return(z)
}


scoring <- function(x,y,z,a,b,c,group_phase)
{
  if((!is.na(x)) && (!is.na(y)))
  {
    if ((x==a) && (y==b))
    { score <- 3 }
    else if ((x-y)==(a-b))
    { score <- 2 }
    else if ((x-y)*(a-b)>0)
    { score <- 1 }
    else score <- 0
  }  
  else score <- 0
  
    if(group_phase==0 && (!is.na(z)))
    { 
      if (z==c)
      { score_add <- 1 }
      else
      { score_add <- 0 }
      score=score+score_add 
    }
  return(score)
}

data<-data %>% 
  rowwise %>% 
  mutate(z=impute(x,y,z,a,b,c,group_phase)) %>% 
  mutate(score=scoring(x,y,z,a,b,c,group_phase))


# scoring(
#   x=2,
#   y=2,
#   z=2,
#   a=1,
#   b=1,
#   c=1,
#   group_phase=0
# )
