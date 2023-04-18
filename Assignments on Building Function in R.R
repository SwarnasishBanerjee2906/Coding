#==============================================================================================
# Assignments:
#==============================================================================================

#SUGGESTION 1:
#Often we are not required the graphs for all the numeric variables. Try to improve the code
#by adding an additional parameter "variable" that can take a vector of variable index and 
#return the graphs for only those variables.
#
#Example: Graphs(Boston, c(1,3,4))
#Will generate the graphics for only the numerical variables among the variables 1,3 & 4
#in the data Boston

graphs1=function(data,variable)
{
  if (!is.data.frame(data))
    stop("The given object is not a dataframe")
  for (name in variable)
  {
    if (is.numeric(data[,name]))
    {
      png(paste("Boxplot & Histogram -",name,".png"))
      par(mfrow=c(2,1))
      par(mar=c(2,2,2,2))
      boxplot(data[,name],horizontal=T,xlab=name,
              main=paste("Boxplot of ",name),col="coral")
      hist(data[,name], main = paste("Histogram of ",name),
           xlab = name, col = "skyblue2")
      dev.off()
    }
  } 
}

#SUGGESTION 2:
#Improve the code in suggestion 1 such that if the argument variable is ignored then it will
#return the graphs of all the numeric variables in the data by default.

graphs2=function(data,variable=names(data))
{
  if (!is.data.frame(data))
    stop("The given object is not a dataframe")
  for (name in variable)
  {
    if (is.numeric(data[,name]))
    {
      png(paste("Boxplot & Histogram -",name,".png"))
      par(mfrow=c(2,1))
      par(mar=c(2,2,2,2))
      boxplot(data[,name],horizontal=T,xlab=name,
              main=paste("Boxplot of ",name),col="coral")
      hist(data[,name], main = paste("Histogram of ",name),
           xlab = name, col = "skyblue2")
      dev.off()
    }
  } 
}

#SUGGESTION 3:
#We ignored the cateorical variables in our discussion. Make some improvement in your codes
#in suggestion 2 such that the function will take the argument "data" and "variable" and will
#return boxplots & histograms for the numerical variables and barplots and pie charts for
#the categorical variables.
#
#Example:
#Graphs(mtcars)
#will get the necessary graphics for all numeric variables and categorical variables in the
#data

graphs3=function(data,variable=names(data))
{
  if (!is.data.frame(data))
    stop("The given object is not a dataframe")
  for (name in variable)
  {
    if (is.numeric(data[,name]))
    {
      png(paste("Boxplot & Histogram -",name,".png"))
      par(mfrow=c(2,1))
      par(mar=c(2,2,2,2))
      boxplot(data[,name],horizontal=T,xlab=name,
              main=paste("Boxplot of ",name),col="coral")
      hist(data[,name], main = paste("Histogram of ",name),
           xlab = name, col = "skyblue2")
      dev.off()
    }
    if (is.factor(data[,name]))
    {
      png(paste("Barplot -",name,".png"))
      par(mfrow=c(2,1))
      par(mar=c(2,2,2,2))
      barplot(table(data[,name]),xlab=name,ylab="no. of cars",
              main=paste("Barchart of ",name),col=c("maroon","gold"))
      dev.off()
    }  
  } 
}

#SUGGESTION 4:
#Probably you need not want to mess up your working directory with so many image files...
#Create an additional argument for the function "dir" (directory), such that the function
#exports all the files to your specified folder (which need not necessaryly be your working
#directory).
#
#Example:
#Graphs(Boston, Variable = c(1,3,4), dir = ".../Praxis/LearntSometingNew/Graphs")
#will generate the necessary graphics for the variables 1, 3 and 4 in the specified location
#in your system i.e. ".../Praxis/LearntSometingNew/Graphs"

graphs4=function(data,variable=names(data),dir)
{
  setwd(dir) 
  if (!is.data.frame(data))
    stop("The given object is not a dataframe")
  for (name in variable)
  {
    if (is.numeric(data[,name]))
    {
      png(paste("Boxplot & Histogram -",name,".png"))
      par(mfrow=c(2,1))
      par(mar=c(2,2,2,2))
      boxplot(data[,name],horizontal=T,xlab=name,
              main=paste("Boxplot of ",name),col="coral")
      hist(data[,name], main = paste("Histogram of ",name),
           xlab = name, col = "skyblue2")
      dev.off()
    }
    if (is.factor(data[,name]))
    {
      png(paste("Barplot -",name,".png"))
      par(mfrow=c(2,1))
      par(mar=c(2,2,2,2))
      barplot(table(data[,name]),xlab=name,ylab="no. of cars",
              main=paste("Barchart of ",name),col=c("maroon","gold"))
      dev.off()
    }  
  } 
}

#SUGGESTION 5:
#Some more modification:
#The function will create a folder with the name of the Dataset where 
#all the graphs will be exported from R studio

graphs5=function(data,variable=names(data),folder_dir)
{
  if (file.exists(folder_dir))
  {
    setwd(folder_dir)
  }else
  {
    dir.create(folder_dir)
    setwd(folder_dir)
  }
  if (!is.data.frame(data))
    stop("The given object is not a dataframe")
  for (name in variable)
  {
    if (is.numeric(data[,name]))
    {
      png(paste("Boxplot & Histogram -",name,".png"))
      par(mfrow=c(2,1))
      par(mar=c(2,2,2,2))
      boxplot(data[,name],horizontal=T,xlab=name,
              main=paste("Boxplot of ",name),col="coral")
      hist(data[,name], main = paste("Histogram of ",name),
           xlab = name, col = "skyblue2")
      dev.off()
    }
    if (is.factor(data[,name]))
    {
      png(paste("Barplot -",name,".png"))
      par(mfrow=c(2,1))
      par(mar=c(2,2,2,2))
      barplot(table(data[,name]),xlab=name,ylab="no. of cars",
              main=paste("Barchart of ",name),col=c("maroon","gold"))
      dev.off()
    }  
  } 
}

