generate_runs_plot <- function(runs_list=c("DR3", "AR4", "N1", "AR3", "DR2")) {
  generate_runs_list <- function(runs_list) {
    wektor <- c()
    wektor[1] = 894
    #first generating the vector
    for (run in runs_list) {
      run_length <- as.numeric(substr(run, nchar(run), nchar(run)))
      direction <- substr(run,1,1)
      direction <- ifelse(direction == "D", 1, ifelse(direction == "A", -1,0))
      fatigue <- 0
      for (idx in 1:run_length){
        wektor <- c(wektor, wektor[length(wektor)] + direction * (runif(1, min=100, max = 200)) - sign(direction) * fatigue)
        fatigue <- fatigue+30
      }
      #now generating label positions
    }
    previous <- 1
    label_positions <- generate_label_positions(runs_list, wektor, d=1)
    return(list(wektor, label_positions, labels=runs_list))
  }

  generate_label_positions <- function(runs_list, wektor, d = 3){ # d is the distance from segment/point
    iseven <- function(x) x %% 2 == 0
    position <- 1
    lista_wynikow <- list()
    for (run in runs_list) {
      run_length <- as.numeric(substr(run,nchar(run),nchar(run)))
      direction <- substr(run,1,1)
      ## teraz znajduje punkt odlegly od ODCINKA (i1, x1), (i2, x2) o d
      if (!iseven(run_length)){
        i1 <- position + (run_length %/% 2)
        i2 <- position + (run_length %/% 2) + 1
        print(c(i1, i2))
        x1 <- wektor[i1]
        x2 <- wektor[i2]
        C <- (x1+x2)/2 - 1/(x2-x1) * (i1 + 1/2)
        if (direction == 'D'){
          d1 <- (i1+1/2)-0.40
          d2 <- (x1+x2)/2 + 50
        } else {
          d1 <- (i1+1/2)-0.40
          d2 <- (x1+x2)/2 - 50
        }
        if (direction == 'N'){
          d1 <- (i1+i2)/2
          d2 <- x1 + 50
        }
        lista_wynikow <- c(lista_wynikow, list(c(d1[1], d2[1])))
      }
      if (iseven(run_length)){
        i <- position + run_length %/% 2
        x <- wektor[i]
        if (direction == 'D'){
          d1 <- i- 0.4
          d2 <- x + 50
        } else {
          d1 <- i + 0.4
          d2 <- x + 50
        }
        if (direction == "N"){
          d1 <- i
          d2 <- wektor[i] + 50
        }
        lista_wynikow <- c(lista_wynikow, list(c(d1, d2)))
      }
      position <- position + run_length
    }
    return(lista_wynikow)
  }

  runsy <- generate_runs_list(runs_list)
  wektor <- runsy[[1]]
  label_positions <- runsy[[2]]
  labels <- runsy[[3]]
  #jpeg(file="figure1", height=200, width=800))
  plot(wektor, ylim=c(min(wektor), max(wektor)), xlab="beat number", ylab="RR interval [ms]")
  ## tutaj rysujemy pierwszy segment - przerywany, HR rosnie
  if (substr(runs_list[[1]],1,1)=="D"){
    wektor0 <- wektor[1] + 150
    typ_linii <- 3
  } else {
    wektor0 <- wektor[1] - 150
    typ_linii <- 1
  }
  wektor0X <- -0.5

  segments(wektor0X, wektor0, 1, wektor[1] , lty = typ_linii, lwd = 3)
  for (beat in 2:length(wektor)) {
    if (wektor[beat]>wektor[beat-1]) {
      segments(beat-1, wektor[beat-1], beat, wektor[beat], lty=1, lwd=3, col="black")
    }
  }

  for (beat in 2:length(wektor)){
    if (wektor[beat]<wektor[beat-1]){
      segments(beat-1, wektor[beat-1], beat, wektor[beat], lty=3, lwd=3, col="black")
    }
  }

  points(wektor, pch=21,col="black", bg="white")

  for (beat in 2:(length(wektor)-1)) {
    if ((wektor[beat]<wektor[beat-1] | wektor[beat]==wektor[beat-1]) & (wektor[beat]<wektor[beat+1])){
      points(beat, wektor[beat], pch=21, col="black", bg="black", cex=1.4)
    }
  }

  for (beat in 2:(length(wektor)-1)) {
    if ((wektor[beat]>wektor[beat-1] | wektor[beat]==wektor[beat-1]) & (wektor[beat]>wektor[beat+1])) {
      points(beat, wektor[beat], pch=21, col="black", bg="gray50", cex=1.4)
    }
  }

  if (wektor[1]>wektor[2]) {
    points(1, wektor[1], pch=21, col="black", bg="gray50", cex=1.4)} else {points(1, wektor[1], pch=21, col="black", bg="black", cex=1.4)}

  if (substr(runs_list[[length(runs_list)]], 1, 1) == "D"){
    wektorLast <- wektor[length(wektor)] - 150
    typ_linii <- 3
  } else {
    wektorLast <- wektor[length(wektor)]+150
    typ_linii <- 1
  }
  wektorLastX <- length(wektor) + 1

  segments(wektorLastX, wektorLast, length(wektor), wektor[length(wektor)] , lty = typ_linii, lwd=3)
  for (label_idx in 1:length(label_positions)) {
    text(label_positions[[label_idx]][1],label_positions[[label_idx]][2], labels[label_idx])
  }

  if (substr(runs_list[[length(runs_list)]],1,1)=="D") {
    points(length(wektor), wektor[length(wektor)], pch=21, col="black", bg="gray50", cex=1.4)
  } else {
    points(length(wektor), wektor[length(wektor)], pch=21, col="black", bg="black", cex=1.4)}
}

png(filename="figure1.png", height = 1200, width = 2800, res = 300)
generate_runs_plot()
dev.off()
