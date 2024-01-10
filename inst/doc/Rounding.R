## -----------------------------------------------------------------------------
op <- options(digits = 15)
##' maybe add to package -- it's really useful here :
formatF <- function(x, ...) noquote(format(x, scientific=FALSE, drop0trailing=TRUE, ...))
formatF(rbind(           # R <= 3.6.x :
round(55.5, 0),          # 56 <- correct
round(55.55, 1),         # 55.5
round(55.555, 2),        # 55.55
round(55.5555, 3),       # 55.556 <- correct
round(55.55555, 4),      # 55.5555
round(55.555555, 5),     # 55.55555
round(55.5555555, 6),    # 55.555555
round(55.55555555, 7),   # 55.5555556 <- correct
round(55.555555555, 8),  # 55.55555555
round(55.5555555555, 9), # 55.555555555
round(55.55555555555,10),# 55.5555555556 <- correct
round(55.555555555555,11)# 55.55555555556 <- correct
))

## -----------------------------------------------------------------------------
require(round)
formatF(rbind(
  roundX(55.5, 0, "r1.C"),
  roundX(55.55, 1, "r1.C"),
  roundX(55.555, 2, "r1.C"),
  roundX(55.5555, 3, "r1.C"),
  roundX(55.55555, 4, "r1.C"),
  roundX(55.555555, 5, "r1.C"),
  roundX(55.5555555, 6, "r1.C"),
  roundX(55.55555555, 7, "r1.C"),
  roundX(55.555555555, 8, "r1.C"),
  roundX(55.5555555555, 9, "r1.C"),
  roundX(55.55555555555,10, "r1.C"),
  roundX(55.555555555555,11, "r1.C")))

## ----str-round----------------------------------------------------------------
str(round)

## -----------------------------------------------------------------------------
roundX(c(-2,2), digits = .Machine$integer.max, version = "r0.C")

## ----integer-x-large-digs-----------------------------------------------------
i <- c(-1,1)* 2^(33:16)
stopifnot(i == floor(i)) # are integer valued
roundAll(i, digits = 300, versions = c("r0.C", "r1.C"))

## ----small-x-large-digs-------------------------------------------------------
e <- 5.555555555555555555555e-308
d <- 312:305 ; names(d) <- paste0("d=", d)
roundAll(e, d, versions = c("r0.C", "r1.C", "r2.C"))

## ----JO-ex-sprintf------------------------------------------------------------
x <- 9.18665
format(x, digits = 5) #  "9.1867"
sprintf("%.4f", x)    #  "9.1867"
## but -- showing now
roundAll(x, 4)

## but note that
print(x, digits=18)

## ----JO-ex-diff---------------------------------------------------------------
print(rbind(9.1866, 9.18665, 9.1867), digits=18)
(dx <- c(9.1866, 9.1867) - x) # [1] -4.99999999998835e-05  4.99999999998835e-05
diff(abs(dx)) # is zero !
options(digits=7) # revert to (typical) default

## ----roundAll-x---------------------------------------------------------------
roundAll(x, 4)

## ----round_r3-def-------------------------------------------------------------
round_r3

## ----roundAll-55.5------------------------------------------------------------
op <- options(digits = 15, width = 2*3*23)
formatF(rbind(
 d0 = roundAll(55.5, 0)
,d1 = roundAll(55.55, 1)
,d2 = roundAll(55.555, 2)
,d3 = roundAll(55.5555, 3)
,d4 = roundAll(55.55555, 4)
,d5 = roundAll(55.555555, 5)
,d6 = roundAll(55.5555555, 6)
,d7 = roundAll(55.55555555, 7)
,d8 = roundAll(55.555555555, 8)
,d9 = roundAll(55.5555555555, 9)
,d10= roundAll(55.55555555555,10))); options(op)

## ----echo=FALSE---------------------------------------------------------------
print(sessionInfo(), locale=FALSE)

