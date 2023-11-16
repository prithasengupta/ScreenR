print("this is the file that'll run tests")



test.QQ <- function(y, x) {
    cor <- cor.test(x, y)
    return(cor$p.value)
}

#categorical to quantitative--------
cp_test=function(data, dep, indep, sig=0.05){
  # data (dataframe) 
  # dep (str): dependent variable
  # indep (str): independent variable
  numeric_var=ifelse(is.numeric(data[dep]), dep, indep)
  cate_var=ifelse(is.numeric(data[dep]), indep, dep)
  result0=aov(data[[numeric_var]] ~ data[[cate_var]])
  result1=unlist(summary(result0))[9]
  return(result1)
}

#example
data("mtcars")
mtcars$cyl=as.character(mtcars$cyl)
x=cp_test(mtcars, 'mpg', 'cyl')
print(x)

#categorical vs categorical

chitest <- function(data, dep, indep, sig=0.05)
{
  data[dep] <- as.factor(data[dep])
  data[indep] <- as.factor(data[indep])
  chistat <- chisq.test(data[dep], data[indep], correct=FALSE)
  return(chistat$p.value)
  
}

