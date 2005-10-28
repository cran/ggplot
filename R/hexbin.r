# Hexgaons Grob
# Minor changes to functions from hexbin to allow output directly to grobs.
# 
# @alias hexpolygonGrob
# @keyword hplot
# @keyword internal 
hexagonsGrob <- function(dat, style = c("colorscale", "centroids", "lattice",    "nested.lattice", "nested.centroids", "constant.col"), use.count = TRUE, cell.at = NULL, minarea = 0.05, maxarea = 0.8, check.erosion = TRUE, mincnt = 1, maxcnt = max(dat@count), trans = NULL, colorcut = seq(0, 1, length = 17), density = NULL, border = NULL, pen = NULL,colramp = function(n) {LinGray(n, beg = 90, end = 15)}, def.unit = "native", verbose = getOption("verbose")) {
    if (!is(dat, "hexbin")) 
        stop("first argument must be a hexbin object")
    style <- match.arg(style)
    if (minarea <= 0) 
        stop("hexagons cannot have a zero area, change minarea")
    if (maxarea > 1) 
        warning("maxarea > 1, hexagons may overplot")
    if (use.count) {
        cnt <- dat@count
    }
    else {
        cnt <- cell.at
        if (is.null(cnt)) {
            if (is.null(dat@cAtt)) 
                stop("Cell attribute cAtt is null")
            else cnt <- dat@cAtt
        }
    }
    xbins <- dat@xbins
    shape <- dat@shape
    tmp <- hcell2xy(dat, check.erosion = check.erosion)
    good <- mincnt <= cnt & cnt <= maxcnt
    xnew <- tmp$x[good]
    ynew <- tmp$y[good]
    cnt <- cnt[good]
    sx <- xbins/diff(dat@xbnds)
    sy <- (xbins * shape)/diff(dat@ybnds)
    switch(style, centroids = , lattice = , constant.col = , 
        colorscale = {
            if (is.null(trans)) {
                if (min(cnt, na.rm = TRUE) < 0) {
                  pcnt <- cnt + min(cnt)
                  rcnt <- {
                    if (maxcnt == mincnt) 
                      rep.int(1, length(cnt))
                    else (pcnt - mincnt)/(maxcnt - mincnt)
                  }
                }
                else rcnt <- {
                  if (maxcnt == mincnt) 
                    rep.int(1, length(cnt))
                  else (cnt - mincnt)/(maxcnt - mincnt)
                }
            }
            else {
                rcnt <- (trans(cnt) - trans(mincnt))/(trans(maxcnt) - 
                  trans(mincnt))
                if (any(is.na(rcnt))) 
                  stop("bad count transformation")
            }
            area <- minarea + rcnt * (maxarea - minarea)
        }, nested.lattice = , nested.centroids = {
            diffarea <- maxarea - minarea
            step <- 10^floor(log10(cnt))
            f <- (cnt/step - 1)/9
            area <- minarea + f * diffarea
            area <- pmax(area, minarea)
        })
    area <- pmin(area, maxarea)
    radius <- sqrt(area)
    switch(style, centroids = , constant.col = , lattice = {
        if (is.null(pen)) 
            pen <- rep.int(1, length(cnt))
        else if (length(pen) == length(cnt)) 
            break
        else if (length(pen) == 1) 
            pen <- rep.int(pen, length(cnt))
        else stop("'pen' has wrong length")
    }, nested.lattice = , nested.centroids = {
        if (!is.null(pen) && length(dim(pen)) == 2) {
            dp <- dim(pen)
            lgMcnt <- ceiling(log10(max(cnt)))
            if (dp[1] != length(cnt) && dp[1] != lgMcnt) {
                stop("pen is not of right dimension")
            }
            if (dp[1] == lgMcnt) {
                ind <- ceiling(log10(bin@count))
                ind[ind == 0] <- 1
                pen <- pen[ind, ]
            }
            else break
        }
        else {
            pen <- floor(log10(cnt)) + 2
            pen <- cbind(pen, pen + 10)
        }
    }, colorscale = {
        nc <- length(colorcut)
        if (colorcut[1] > colorcut[nc]) {
            colorcut[1] <- colorcut[1] + 1e-06
            colorcut[nc] <- colorcut[nc] - 1e-06
        }
        else {
            colorcut[1] <- colorcut[1] - 1e-06
            colorcut[nc] <- colorcut[nc] + 1e-06
        }
        colgrp <- cut(rcnt, colorcut, labels = FALSE)
        if (any(is.na(colgrp))) 
            colgrp <- ifelse(is.na(colgrp), 0, colgrp)
        clrs <- colramp(length(colorcut) - 1)
        pen <- clrs[colgrp]
    })
    inner <- 0.5
    outer <- (2 * inner)/sqrt(3)
    dx <- inner/sx
    dy <- outer/(2 * sy)
    rad <- sqrt(dx^2 + dy^2)
    hexC <- hexcoords(dx, dy, sep = NULL)
    switch(style, constant.col = , colorscale = {
        return(hexpolygonGrob(xnew, ynew, hexC, density = density, fill = pen, 
            border = if (!is.null(border)) border else pen))
				}
        
    , nested.lattice = , nested.centroids = {
        hexpolygonGrob(xnew, ynew, hexC, density = density, fill = if (is.null(border) || 
            border) 
            1
        else pen[, 1], border = pen[, 1])
    })
    if (style == "centroids" || style == "nested.centroids") {
        xcm <- dat@xcm[good]
        ycm <- dat@ycm[good]
        k <- sqrt(3)/2
        cosx <- c(1, k, 0.5, 0, -0.5, -k, -1, -k, -0.5, 0, 0.5, 
            k, 1)/sx
        siny <- c(0, 0.5, k, 1, k, 0.5, 0, -0.5, -k, -1, -k, 
            -0.5, 0)/sy
        dx <- sx * (xcm - xnew)
        dy <- sy * (ycm - ynew)
        dlen <- sqrt(dx^2 + dy^2)
        cost <- ifelse(dlen > 0, dx/dlen, 0)
        tk <- (6 * acos(cost))/pi
        tk <- round(ifelse(dy < 0, 12 - tk, tk)) + 1
        hrad <- ifelse(tk%%2 == 1, inner, outer)
        fr <- pmin(hrad * (1 - radius), dlen)
        xnew <- xnew + fr * cosx[tk]
        ynew <- ynew + fr * siny[tk]
    }
    n <- length(radius)
    if (verbose) 
        cat("length = ", length(pen), "\n", "pen = ", pen + 1, 
            "\n")
    n6 <- rep.int(6:6, n)
    pltx <- rep.int(hexC$x, n) * rep.int(radius, n6) + rep.int(xnew, 
        n6)
    plty <- rep.int(hexC$y, n) * rep.int(radius, n6) + rep.int(ynew, 
        n6)
    switch(style, 
			centroids = , lattice = 
				polygonGrob(pltx, plty, default.units = def.unit, id = NULL, id.lengths = n6, gp = gpar(fill = pen, col = border))
    , nested.lattice = , nested.centroids = 
        polygonGrob(pltx, plty, default.units = def.unit, id = NULL, id.lengths = n6, gp = gpar(fill = pen[, 2], col = if (!is.null(border)) border else pen[, 2]))
    )
}

hexpolygonGrob <- function (x, y, hexC = hexcoords(dx, dy, n = 1), dx, dy = NULL, 
    fill = 1, border = 0, hUnit = "native", ...) 
{
    n <- length(x)
    stopifnot(length(y) == n)
    stopifnot(is.list(hexC) && is.numeric(hexC$x) && is.numeric(hexC$y))
    if (hexC$no.sep) {
        n6 <- rep.int(6:6, n)
        if (!is.null(hUnit)) {
            polygonGrob(x = unit(rep.int(hexC$x, n) + rep.int(x, 
                n6), hUnit), y = unit(rep.int(hexC$y, n) + rep.int(y, 
                n6), hUnit), id.lengths = n6, gp = gpar(col = border, 
                fill = fill))
        }
        else {
            polygonGrob(x = rep.int(hexC$x, n) + rep.int(x, 
                n6), y = rep.int(hexC$y, n) + rep.int(y, n6), 
                id.lengths = n6, gp = gpar(col = border, fill = fill))
        }
    }
    else {
        n7 <- rep.int(7:7, n)
        polygon(x = rep.int(hexC$x, n) + rep.int(x, n7), y = rep.int(hexC$y, 
            n) + rep.int(y, n7), ...)
    }
}