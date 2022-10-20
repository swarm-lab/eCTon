#' @export
read_mat_stack <- function(file, name = "reconstruction3D") {
  if (shiny::isRunning()) {
    pbapply::pboptions(type = "shiny")
  } else {
    pbapply::pboptions(type = if (interactive()) "timer" else "none")
  }

  if (!grepl(".mat", file))
    stop("This is not a .mat file.")

  pbapply::pbapply(raveio::load_h5(file, name, ram = TRUE, quiet = FALSE), 3,
                   function(x) {
                     dim(x) <- c(dim(x), 1)
                     image(x)
                   }, simplify = FALSE)
}


#' @export
read_stack <- function(file) {
  Rvision::readMulti(file)
}


#' @export
find_container <- function(stack, lo = ceiling(length(stack) / 2) - 10,
                           up = ceiling(length(stack) / 2) + 10) {
  if (shiny::isRunning()) {
    pbapply::pboptions(type = "shiny")
  } else {
    pbapply::pboptions(type = if (interactive()) "timer" else "none")
  }

  pbapply::pbsapply(stack[lo:up], function(x) {
    im <- Rvision::changeBitDepth(x, "8U", 255 / max(x))
    circ <- Rvision::houghCircles(im, "ALT", 1.5, 200, 300, 0.7)
    if (nrow(circ) > 0) {
      circ[1, ]
    } else {
      c(id = NA, x = NA, y = NA, radius = NA, votes = NA)
    }
  }) -> circs

  Rfast::rowMedians(circs, na.rm = TRUE)[2:4] -> circ
  c("x", "y", "radius") -> names(circ)
  circ
}


#' @export
make_container_mask <- function(nrow, ncol, circ, scale = 0.9) {
  Rvision::zeros(nrow, ncol, 1, "32F") -> mask
  Rvision::drawCircle(mask, circ[1], circ[2], circ[3] * scale, "#010101", -1)
  mask
}


#' @export
find_usable_slices <- function(stack, mask) {
  if (shiny::isRunning()) {
    pbapply::pboptions(type = "shiny")
  } else {
    pbapply::pboptions(type = if (interactive()) "timer" else "none")
  }

  imask <- (mask * -1) + 2
  pbapply::pbsapply(stack, function(x) {
    Rvision::sum(x * imask)
  }) -> test

  d <- sqrt(abs(median(test) - test))
  k <- kmeans(d, quantile(d, c(0.25, 0.5, 0.75)))
  r <- rle(k$cluster)
  c(start = sum(r$lengths[1:(which.max(r$lengths) - 1)]) + 1,
    end = sum(r$lengths[1:which.max(r$lengths)]))
}


#' @export
unbend_light <- function(stack, start_end, circ) {
  if (shiny::isRunning()) {
    pbapply::pboptions(type = "shiny")
  } else {
    pbapply::pboptions(type = if (interactive()) "timer" else "none")
  }

  nc <- ncol(stack[[1]])
  nr <- nrow(stack[[1]])

  pbapply::pbsapply(start_end[1]:start_end[2], function(i) {
    h <- Rvision::improfile(stack[[i]], c(1, nc), c(circ[2], circ[2]))
    v <- Rvision::improfile(stack[[i]], c(circ[1], circ[1]), c(1, nr))
    h <- cbind(h, D = 1 * circ[3] - sqrt((h[, 1] - circ[1]) ^ 2 + (h[, 2] - circ[2]) ^ 2))
    v <- cbind(v, D = 1 * circ[3] - sqrt((v[, 1] - circ[1]) ^ 2 + (v[, 2] - circ[2]) ^ 2))
    dt <- as.data.frame(rbind(h, v))
    dt <- dt[dt$D > 0, ]
    dt <- dt[dt$D >= dt$D[which.max(dt$I)], ]

    mod <- robustbase::nlrob(I ~ (max(I) * exp(-B * D)) + 0,
                             data = dt, maxit = 5000, weights = 1 / sqrt(D),
                             start = list(B = max(dt$I) / 5),
                             control = nls.control(warnOnly = TRUE,
                                                   minFactor = 1 / 10240))

    c(SLICE = i, I = max(dt$I), coef(mod), SHIFT = 0)
  }) -> out

  t(out)
}


#' @export
find_global_treshold <- function(stack, circ, mods, mask, n = 5000,
                                 methods = c("Otsu", "RenyiEntropy")) {
  if (shiny::isRunning()) {
    pbapply::pboptions(type = "shiny")
  } else {
    pbapply::pboptions(type = if (interactive()) "timer" else "none")
  }

  nc <- ncol(stack[[1]])
  nr <- nrow(stack[[1]])
  D <- circ[3] - sqrt((matrix(1:nc, ncol = nc, nrow = nr, byrow = TRUE) - circ[1]) ^ 2 +
                        (matrix(1:nr, ncol = nc, nrow = nr) - circ[2]) ^ 2)
  dim(D) <- c(dim(D), 1)

  nz <- Rvision::findNonZero(mask)

  pbapply::pbsapply(1:nrow(mods), function(i) {
    id <- sample(1:nrow(nz), n)
    tmp <- Rvision::cloneImage(stack[[mods[i, 1]]])
    BASE <- (mods[i, 2] * exp(-mods[i, 3] * D)) + mods[i, 4]
    Rvision::subtract(tmp, Rvision::image(BASE), "self")
    Rvision::pget(tmp, nz[id, 1], nz[id, 2])
  }) -> smp

  dim(smp) <- c(dim(smp), 1)
  smp <- Rvision::image(smp)

  sapply(methods, function(method) {
    max(smp)[1] * Rvision::autothreshold((255 * smp) / max(smp)[1], method, mask = smp > 0) / 255
  }) -> ths

  mean(ths)
}


#' @export
make_clean_stack <- function(stack, mask, mods, circ, th = Inf) {
  if (shiny::isRunning()) {
    pbapply::pboptions(type = "shiny")
  } else {
    pbapply::pboptions(type = if (interactive()) "timer" else "none")
  }

  nc <- ncol(stack[[1]])
  nr <- nrow(stack[[1]])
  D <- circ[3] - sqrt((matrix(1:nc, ncol = nc, nrow = nr, byrow = TRUE) - circ[1]) ^ 2 +
                        (matrix(1:nr, ncol = nc, nrow = nr) - circ[2]) ^ 2)
  dim(D) <- c(dim(D), 1)

  pbapply::pblapply(1:nrow(mods), function(i) {
    tmp <- Rvision::cloneImage(stack[[mods[i, 1]]])
    BASE <- (mods[i, 2] * exp(-mods[i, 3] * D)) + mods[i, 4]
    Rvision::subtract(tmp, image(BASE), "self")
    Rvision::multiply(tmp, mask, "self")
    Rvision::multiply(tmp, (tmp > 0) / 255, "self")
    Rvision::multiply(tmp, (tmp >= th) / 255, "self")
    tmp
  })
}
