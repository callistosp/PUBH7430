FEV <- read.table("path/to/fev.txt", ## Replace with your local path
                  header=FALSE,
                  row.names=NULL,
                  col.names=c("subjID","height","age","init.height","init.age","logFEV"))

FEV$intage <- as.integer(FEV$age) ## Treat age as an integer

FEV$logFEV[1484] <- NA ## Eliminate an outlier
## Create a wide dataset
FEVW <- reshape(FEV,v.names=c("height","age","logFEV"),idvar="subjID",timevar="intage",direction="wide")

source("https://github.com/jwolfson/PUBH7430/raw/master/CorHeatmap.R")

### Do a scatterplot matrix
cor.mat <- round( cor(FEVW[,c("logFEV.6","logFEV.9","logFEV.12","logFEV.15","logFEV.18")],use="pairwise"), 2)

CorHeatmap(cor.mat)

