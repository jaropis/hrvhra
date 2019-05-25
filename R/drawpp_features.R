#' function which returns the Poincare plot perpendicular line
#' @param rr vector with RR intervals
#' @param centroid the x coordinate of the centroid
#' @param centroid_offset how far the centroid should be from the perpendicular line
perp_line <- function(centroid, rr, centroid_offset){
  x <- seq(min(rr)-centroid_offset * 3, max(rr)+centroid_offset * 3, by=0.1)
  return(list(x, -1*x + 2 * centroid))
}

#' function which returns the Poincare plot parallel line
#' @param rr vector with RR intervals
#' @param centroid the x coordinate of the centroid
#' @param centroid_offset how far the centroid should be from the paralell line
parall_line <- function(centroid, centroid_offset, rr){
  x <- seq(min(rr)- 3 * centroid_offset, max(rr)+3*centroid_offset, by=0.1)
  return(list(x, x - 2 * centroid_offset))
}

#' function which returns the coordinates of the projection of a point on l2
#' @param (x, y) coordinates of the point to be projected
#' @param centroid_offset centroid offset
perp_point <- function(x, y, centroid_offset){
  return(c(x, y, (x+y) / 2 + centroid_offset, (x+y) / 2 - centroid_offset))
}

paral_point <- function(x, y, centroid){
  return(c(x, y, centroid - (y-x)/2, (y-x) / 2 + centroid))
}

#' function checking whether the segments perpendicular to l_1 are not too close from one another
#'
check_perp_point <- function(perp_point, existing_points, centroid_offset, scale){
  condition_length <- FALSE
  segment_length <- sqrt((perp_point[1] - perp_point[3])^2 + (perp_point[2] - perp_point[4])^2)
  if (segment_length > centroid_offset)
    condition_length <- TRUE

  condition_close <- TRUE
  # if this is the first point on the list just return if it is far enough
  if (length(existing_points) == 0){
    return(condition_length)
  }

  for (point in existing_points){
    distance <- sqrt((perp_point[3] - point[3])^2 + (perp_point[4] - point[4])^2)
    condition_close <- condition_close & (distance > centroid_offset * scale)
  }

  return(condition_length & condition_close)
}

check_paral_point <- function(paral_point, existing_points, centroid_offset, scale){
  condition_length <- FALSE
  segment_length <- sqrt((paral_point[1] - paral_point[3])^2 + (paral_point[2] - paral_point[4])^2)
  if (segment_length > centroid_offset)
    condition_length <- TRUE

  condition_close <- TRUE
  # if this is the first point on the list just return if it is far enough
  if (length(existing_points) == 0){
    return(condition_length)
  }

  for (point in existing_points){
    distance <- sqrt((paral_point[3] - point[3])^2 + (paral_point[4] - point[4])^2)
    condition_close <- condition_close & (distance > centroid_offset * scale)
  }

  return(condition_length & condition_close)
}

#' Function returns both x and the coordinate y of a point with coordinate x,
#' which lies on the line defined by points x1, y1, x2, y2

get_point_on_line <- function(x, x1, y1, x2, y2){
  a = (y2-y1)/(x2-x1)
  b = 1/2 * ((y2 + y1) - a * (x2 + x1))
  return(c(x, a * x + b))
}
#' Drawing reference features of the Poincare plot
#'
#' This function draws the Poincare plot in two subplots with the characteristic
#' object, i.e. the identity line, perpendicular line, centroid and a few select
#' distances to the characteristic lines
#'
#' @inheritParams preparepp
#' @param vname variable name - this will be used for construction \code{xlab} and \code{ylab} for the Poincare plot
#' @param \\dots Additional arguments passed on to \code{plot}
#' @export
#' @importFrom graphics abline
#'
#' @examples
#' drawpp_features(RR$RR, RR$flags)

drawpp_features <- function(rr, annotations, vname = "RR", centroid_offset = 15,
                            n_points_1 = 10, n_points_2 = 5, scale_1 = 0.5, scale_2 = 0.4){
  assert_that(is.character(vname), msg = "vname needs to be a string")
  pp <- preparepp(rr, annotations)
  png(filename = "figure.png", width = 3200, height = 1800, res = 300)
  oldpar <- par(mfrow=c(1,2))
  #first plot
  plot(pp[, 2] ~ pp[, 1], xlab=parse(text = paste(vname, "[i](ms)")),  ylab = parse(text = paste(vname, "[i+1](ms)")),
       pch=21, bg="gray70", col="black", xlim = c(range(rr)[1]-1.5 * centroid_offset, range(rr)[2]+ 1.5 * centroid_offset),
       ylim = c(range(rr)[1]-1.5 * centroid_offset, range(rr)[2]+1.5 * centroid_offset))
  abline(0, 1)
  points(mean(rr) + centroid_offset, mean(rr) - centroid_offset, pch=21, bg="black", col="black", cex=2)
  perp <- perp_line(mean(rr), rr, mean(rr))
  lines(perp[[1]], perp[[2]], lty = 2, lwd=2)
  parall <- parall_line(mean(rr), centroid_offset, rr)
  lines(parall[[1]], parall[[2]], lty = 2, lwd=2)

  # tu dodaj wiecej punktow w petli
  set.seed(1)
  lista_punktow <- list()
  while (length(lista_punktow) < n_points_1){
    drawn_point_idx <- sample(1:length(pp[, 1]), 1)
    drawn_point <- perp_point(pp[, 1][drawn_point_idx], pp[, 2][drawn_point_idx], centroid_offset)
    is_good <- check_perp_point(drawn_point, existing_points = lista_punktow,
                                centroid_offset = centroid_offset,scale = scale_1)
    if (is_good){
      lista_punktow <- c(list(drawn_point), lista_punktow)
    }
  }

  for (punkt in lista_punktow){
    segments(punkt[1], punkt[2], punkt[3], punkt[4], lwd=3)
  }
  for (punkt in lista_punktow){
    points(punkt[1], punkt[2], pch=21, bg="black", col="black")
  }
  x_text <- range(rr)[2] + 1/12*diff(range(rr))
  text(x_text, x_text - 2 * centroid_offset - 1/12*diff(range(rr)), expression(l[1]), cex = 1.5)

  x_text <- range(rr)[1] + 1/5*diff(range(rr))
  text(x_text, -x_text + 2 * mean(rr) + 1/12*diff(range(rr)), expression(l[2]), cex = 1.5)

  x_text <- range(rr)[1]
  y_text <- x_text
  text(x_text - 1/20 * diff(range(rr)), y_text + 1/20 * diff(range(rr)), "Id", cex = 1.5)

  x_start_arrow <- range(rr)[1] + 9/12 * diff(range(rr))
  y_start_arrow <- range(rr)[2] - 10/12 * diff(range(rr))
  endarrow <- get_point_on_line(mean(rr) + 1.5*centroid_offset, x_start_arrow, y_start_arrow, mean(rr) + centroid_offset, mean(rr) - centroid_offset)
  arrows(x_start_arrow, y_start_arrow, endarrow[1], endarrow[2], lwd = 3, angle = 10)
  text(x_start_arrow, y_start_arrow - 0.8*centroid_offset, "centroid", cex = 1.3)

  #second plot
  plot(pp[, 2] ~ pp[, 1], xlab=parse(text = paste(vname, "[i](ms)")),  ylab = parse(text = paste(vname, "[i+1](ms)")),
       pch=21, bg="gray70", col="black", xlim = c(range(rr)[1]-1.5 * centroid_offset, range(rr)[2]+ 1.5 * centroid_offset),
       ylim = c(range(rr)[1]-1.5 * centroid_offset, range(rr)[2]+1.5 * centroid_offset))
  #points(mean(rr) + centroid_offset, mean(rr) - centroid_offset, pch=21, bg="black", col="black", cex=2)
  abline(0, 1)
  lines(perp[[1]], perp[[2]], lty = 2, lwd=2)
  lines(parall[[1]], parall[[2]], lty = 2, lwd=2)
  set.seed(1)
  lista_punktow <- list()
  while (length(lista_punktow) < n_points_2) {
    drawn_point_idx <- sample(1:length(pp[, 1]), 1)
    drawn_point <- paral_point(pp[, 1][drawn_point_idx], pp[, 2][drawn_point_idx],
                               centroid = mean(pp[, 1]))
    is_good <- check_paral_point(drawn_point, existing_points = lista_punktow,
                                centroid_offset = centroid_offset, scale = scale_2)
    if (is_good){
      lista_punktow <- c(list(drawn_point), lista_punktow)
    }
  }
  for (punkt in lista_punktow){
    segments(punkt[1], punkt[2], punkt[3], punkt[4], lwd=3)
  }
  for (punkt in lista_punktow){
    points(punkt[1], punkt[2], pch=21, bg="black", col="black")
  }
  x_text <- range(rr)[2] + 1/12*diff(range(rr))
  text(x_text, x_text - 2 * centroid_offset - 1/12*diff(range(rr)), expression(l[1]), cex = 1.5)

  x_text <- range(rr)[1] + 1/5*diff(range(rr))
  text(x_text, -x_text + 2 * mean(rr) + 1/12*diff(range(rr)), expression(l[2]), cex = 1.5)

  x_text <- range(rr)[1]
  y_text <- x_text
  text(x_text - 1/20 * diff(range(rr)), y_text + 1/20 * diff(range(rr)), "Id", cex = 1.5)
  dev.off()
}
