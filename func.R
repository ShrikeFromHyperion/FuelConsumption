func.class_tr <- function(x,a,c)
{
  b <- (a+c)/2
  if(x <= a)
    res <- 0 
  if(a < x && x <= b)
    res <- (x-a)/(b-a)
  if(b < x && x <= c)
    res <- (c-x)/(c-b)
  if(x > c)
    res <- 0
  return(res)
}

func.class_s <- function(x,a,c)
{
  b <- (a+c)/2
  if(x <= a)
    res <- 0 
  if(a < x && x <= b)
    res <- 2*(((x-a)/(c-a))^2)
  if(b < x && x <= c)
    res <- 1-2*(((x-c)/(c-a))^2)
  if(x > c)
    res <- 1
  return(res)
}

func.class_pi <- function(x,a,c)
{
  b <- (a+c)/2
  if(x <= c)
  {
    a1 <- c-b
    b1 <- c-(b/2)
    c1 <- c
    if(x <= a1)
      res <- 0 
    if(a1 < x && x <= b1)
      res <- 2*(((x-a1)/(c1-a1))^2)
    if(b1 < x && x <= c1)
      res <- 1-2*(((x-c1)/(c1-a1))^2)
    if(x > c1)
      res <- 1
  } else
  {
    a2 <- c
    b2 <- c+(b/2)
    c2 <- c+b
    if(x <= a2)
      res <- 0 
    if(a2 < x && x <= b2)
      res <- 2*(((x-a2)/(c2-a2))^2)
    if(b2 < x && x <= c2)
      res <- 1-2*(((x-c2)/(c2-a2))^2)
    if(x > c2)
      res <- 1
    res <- 1 - res
  }
  return(res)
}

func.class_lz <- function(x,a,c)
{
  if(a >= x)
    res <- 1
  if(x > a && x <= c)
    res <- (c-x)/(c-a)
  if(x > c)
    res <- 0
  return(res)
}

func.class_ls <- function(x,a,c)
{
  if(x <= a)
    res <- 0
  if(a < x && x <= c)
    res <- (x-a)/(c-a)
  if(x > c)
    res <- 1
  return(res)
}


