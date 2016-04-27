library("sqldf")
library("ggplot2")
library("pastecs")

library("wmtsa")
library("kza")


x <- read.csv("testtask.csv")
x$is_towards_stress <- as.character(x$is_towards_stress)
x$arc_date <- as.Date(x$arc_date)
newTr <- sqldf("SELECT arc_date,COUNT(DISTINCT timeseries_id) as t_count FROM x WHERE is_towards_stress = 'TRUE' GROUP BY arc_date")


FindPeak <- function(dat, SearchFrac=0.02){
  Data <- as.vector(dat[,2])
  Wave <- wavCWT(Data)
  WaveTree <- wavCWTTree(Wave)
  WavePeaks <- wavCWTPeaks(WaveTree, snr.min=1)
  WavePeaks_Times <- attr(WavePeaks, which="peaks")[,"iendtime"]
  
  NewPeakTimes <- c()
  dRange <- round(SearchFrac*length(Data))
  for(i in 1:length(WavePeaks_Times)){
    NewRange <- max(c(WavePeaks_Times[i]-dRange, 1)):min(c(WavePeaks_Times[i]+dRange, length(Data)))
    NewPeakTimes[i] <- which.max(Data[NewRange])+NewRange[1]-1
  }
  
  return(matrix(c(dat$arc_date[NewPeakTimes], Data[NewPeakTimes]), ncol=2, dimnames=list(NULL, c("PeakIndices", "Peaks"))))
}


pdf("plots.pdf")
newTr$t_count <- kz(newTr$t_count,m=3)
plot(newTr$arc_date, as.vector(newTr$t_count), type="l",xlab = "Date", ylab = "# of timeseries")
extreme <- FindPeak(newTr)
points(extreme, col="red", pch=20)
dev.off()
