
readland.gem.tps<-function (file, specID = c("None", "ID", "imageID"), readcurves = FALSE, 
          warnmsg = T) 
{
  ignore.case = TRUE
  specID <- match.arg(specID)
  tpsfile <- scan(file = file, what = "char", sep = "\n", quiet = TRUE)
  lmdata <- grep("LM=", tpsfile, ignore.case)
  if (length(lmdata != 0)) {
    nland <- as.numeric(sub("LM=", "", tpsfile[lmdata], ignore.case))
    k <- 2
  }
  if (length(lmdata) == 0) {
    lmdata <- grep("LM3=", tpsfile, ignore.case)
    nland <- as.numeric(sub("LM3=", "", tpsfile[lmdata], 
                            ignore.case))
    k <- 3
  }
  if (any(nland == 0)) {
    stop("No landmark data for some specimens.")
  }
  n <- nspecs <- length(lmdata)
  if (max(nland) - min(nland) != 0) {
    stop("Number of landmarks not the same for all specimens.")
  }
  p <- nland[1]
  imscale <- as.numeric(sub("SCALE=", "", tpsfile[grep("SCALE", 
                                                       tpsfile, ignore.case)], ignore.case))
  if (is.null(imscale)) {
    imscale = array(1, nspecs)
  }
  if (warnmsg == T) {
    if (length(imscale) != nspecs) {
      print("Not all specimens have scale. Assuming landmarks have been previously scaled.")
    }
  }
  if (length(imscale) != nspecs) {
    imscale = array(1, nspecs)
  }
  crvs <- grep("CURVES=", tpsfile, ignore.case)
  if (length(crvs) > 0) {
    if (readcurves == TRUE && length(crvs) == 0) {
      stop("No CURVES= field present in file")
    }
    ncurve <- as.numeric(sub("CURVES=", "", tpsfile[crvs], 
                             ignore.case))
    ncurvepts <- as.numeric(sub("POINTS=", "", tpsfile[grep("POINTS=", 
                                                            tpsfile, ignore.case)], ignore.case))
    if (max(ncurve) - min(ncurve) != 0) {
      stop("Number of curves not the same for all specimens.")
    }
    if (warnmsg == T && readcurves == T) {
      print(paste("Landmarks 1:", p, " are fixed landmarks.", 
                  sep = ""))
      print(paste("Landmarks ", p + 1, ":", p + sum(ncurvepts[1:ncurve[1]]), 
                  " are semilandmarks.", sep = ""))
    }
    p <- nland[1] + sum(ncurvepts[1:ncurve[1]])
  }
  tmp <- tpsfile[-(grep("=", tpsfile))]
  options(warn = -1)
  tmp <- matrix(as.numeric(unlist(strsplit(tmp, "\\s+"))), 
                ncol = k, byrow = T)
  if (warnmsg == T) {
    if (sum(which(is.na(tmp) == TRUE)) > 0) {
      print("NOTE.  Missing data identified.")
    }
  }
  coords <- aperm(array(t(tmp), c(k, p, n)), c(2, 1, 3))
  imscale <- aperm(array(rep(imscale, p * k), c(n, k, p)), 
                   c(3, 2, 1))
  coords <- coords * imscale
  if (readcurves == F) {
    coords <- coords[1:nland, , ]
    if (n == 1) 
      coords <- array(coords, c(nland, k, n))
  }
  if (specID == "None") {
    if (warnmsg == T) {
      print("No Specimen names extracted")
    }
  }
  if (specID == "imageID") {
    imageID <- (sub("IMAGE=", "", tpsfile[grep("IMAGE=", tpsfile, ignore.case)], ignore.case))
    if (length(imageID) != 0) {
      imageID <- sub("Museum of Zoology, University of Michigan - ", "", imageID, ignore.case)
      imageID <- sub("Museum of Zoology, University of Michigan -", "", imageID, ignore.case)
      imageID <- sub("Museum Victoria - ", "", imageID, ignore.case)
      imageID <- sub(" - 1cm Bar", "", imageID, ignore.case)
      imageID <- sub(" - 2cm Bar", "", imageID, ignore.case)
      imageID <- sub(" - 5cm Bar", "", imageID, ignore.case)
      imageID <- sub(" - 10cm Bar", "", imageID, ignore.case)
      imageID <- sub(" - 20cm Bar", "", imageID, ignore.case)
      imageID <- sub(".jpg", "", imageID, ignore.case)
      imageID <- sub(".tif", "", imageID, ignore.case)
      imageID <- sub(".bmp", "", imageID, ignore.case)
      imageID <- sub(".tiff", "", imageID, ignore.case)
      imageID <- sub(".jpeg", "", imageID, ignore.case)
      imageID <- sub(".jpe", "", imageID, ignore.case)
      imageID <- sub(".png", "", imageID, ignore.case)
      
      dimnames(coords)[[3]] <- as.list(imageID)
      if (warnmsg == T) {
        print("Specimen names extracted from line IMAGE=")
      }
    }
    if (length(imageID) == 0) {
      if (warnmsg == T) {
        print("No name given under IMAGE=. Specimen names not extracted")
      }
    }
  }
  if (specID == "ID") {
    ID <- sub("ID=", "", tpsfile[grep("ID=", tpsfile, ignore.case)], 
              ignore.case)
    if (length(ID) == 0) {
      if (warnmsg == T) {
        print("No name given under ID=. Specimen names not extracted")
      }
    }
    if (length(ID) != 0) {
      dimnames(coords)[[3]] <- as.list(ID)
      if (warnmsg == T) {
        print("Specimen names extracted from line ID=")
      }
    }
  }
  return(coords = coords)
}
