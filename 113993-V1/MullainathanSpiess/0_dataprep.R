## AHS 2011 data cleaning for JEP
# Jann Spiess, March 2017

# Requires tnewhouse.csv from
# http://www2.census.gov/programs-surveys/ahs/2011/AHS%202011%20National%20and%20Metropolitan%20PUF%20v1.4%20CSV.zip
# (AHS 2011 public-use data)


# Read in data
newhouse <- read.csv(file="tnewhouse.csv", quote="\"'")
newhousepeek <- read.csv(file="tnewhouse.csv", nrow=10)
newhouse[, sapply(newhousepeek, is.factor)] <- lapply(newhouse[, sapply(newhousepeek, is.factor)], as.factor)

# Make some factors factors, numeric numeric
newhouse$CELLAR <- as.factor(newhouse$CELLAR)
newhouse$FPLWK <- as.factor(newhouse$FPLWK)
newhouse$TYPE <- as.factor(newhouse$TYPE)
newhouse$NUMTLT <- as.numeric(as.character(newhouse$NUMTLT))
newhouse$NUMCOLD <- as.numeric(as.character(newhouse$NUMCOLD))
newhouse$BUSPER <- as.numeric(as.character(newhouse$BUSPER))
newhouse$NUMDRY <- as.numeric(as.character(newhouse$NUMDRY))
newhouse$NUMSEW <- as.numeric(as.character(newhouse$NUMSEW))

# Delete rows with zero/missing value, and get the log value
newhouse <- newhouse[newhouse$VALUE > 0,]
newhouse$LOGVALUE <- log(newhouse$VALUE)

# Choose variables of interest
allvars <- names(newhouse)
charvar <- intersect(allvars,c("ACCESS", "ACCESSB", "AGERES", "AIR", "AIRSYS", "ANYCAR", "ANYPNT", "ANYRUG", "APTFL", "ASSTSERV", "ATBSUN", "BATHS", "BDCARP", "BEDRMS", "BEDX", "BUILT", "BURNER", "BUSIN", "BUSPER", "CELLAR", "CLIMB", "CONDO", "COOK", "DENS", "DINING", "DIRAC", "DISH", "DISPL", "DRSHOP", "DRY", "DSTEAM", "DSTOVE", "EAIRC", "EBAR", "ECNTAIR", "EDISHWR", "EDRYER", "EFRIDGE", "EHEATUT", "ELEV", "ENOEAPP", "EOTEAPP", "ETRSHCP", "EWASHR", "EXCLUS", "FAMRM", "FLOODPLN", "FLOORS", "FLOT", "FPINS", "FPLWK", "FRPL", "FRPLI", "FRSTOC", "GARAGE", "HALB", "HALFB", "HOTPIP", "INCP", "KEXCLU", "KITCH", "KITCHEN", "LAT70S", "LAUNDY", "LIVING", "LOT", "LVCARP", "MOBILTYP", "MOPERM", "MOSALL", "MOSAPR", "MOSAUG", "MOSDEC", "MOSFEB", "MOSJAN", "MOSJUL", "MOSJUN", "MOSMAR", "MOSMAY", "MOSNOV", "MOSOCT", "MOSSEP", "NEWC", "NOOTHRM", "NUMAIR", "NUNIT2", "NUNITS", "OARSYS", "OBEDRM", "ODIN", "ODIRAC", "OKITCH", "OLIVIN", "OOTHRM", "OTBUP", "OTHFN", "OTHLQ", "OTHLQ1", "OTHLQ2", "OTHRUN", "OVEN", "OWNHERE", "OWNLOT", "PERSERV", "PHONE", "PLUGS", "PORCH", "PREOCC", "PUBSEW", "RECRM", "REFR", "ROOMS", "ROOMSA", "ROOMSB", "SEWDIS", "SEWDISTP", "SEWDUS", "SHARAT", "SHARFR", "SINHV", "SINK", "SINVV", "SOTHV", "SRECV", "SRENTV", "SSELV", "STEAM", "STORG", "TELAV", "TELHH", "TENURE", "TIMSHR", "TRASH", "TUB", "TYPE", "UNITSF", "VACANC2", "VACANC2B", "VACANCY", "WASH", "WATER", "WATERD", "WELLDIS", "WELLDIS2", "WELDUS", "WELL", "WFPROP", "WHNGET", "WINTERELSP", "WINTERKESP", "WINTERNONE", "WINTEROVEN", "WINTERWOOD", "YRRND"))
qualvar <- intersect(allvars,c("BADSTEP", "BDSTPQ", "BLDMNT", "BLEAK", "BSINK", "CRACKS", "DFIRE", "DISAS", "DORREP", "EBOARD", "EBROKE", "ECRUMB", "EGOOD", "EHOLER", "ELEVWK", "EMISSR", "EMISSW", "EROACH", "ESAGR", "ESLOPW", "EVROD", "EXPOSE", "FREEZE", "GRDMNT", "HOLES", "HOWH", "IFBLOW", "IFCOLD", "IFDRY", "IFSEW", "IFTLT", "ILEAK", "IMAINT", "LEAK", "LTSOK", "LTSOK1", "LTSOK2", "M12ROACH", "M12ROD", "M3ROD", "MAJR1", "MAJR2", "MAJR3", "MICE", "MINR1", "MINR2", "MINR3", "NLEAK1", "NLEAK2", "NOTSUR", "NOWIRE", "NUMBLOW", "NUMCOLD", "NUMDRY", "NUMSEW", "NUMTLT", "OMAINT", "OTHCLD", "OTLEAK", "PILEAK", "PLEAK", "PLUMB", "RAILOK", "RAILOK1", "RAILOK2", "RATFREQ", "RATS", "RLEAK", "ROACHFRQ", "TALWIR", "TASB", "TOILET", "TRADON", "TREP", "TWATER", "WATERS", "WHYCD1", "WHYCD2", "WHYCD3", "WHYCD4", "WHYCD5", "WLEAK", "WTRHRL", "ZADEQ"))
geovar <- c("REGION","METRO","METRO3","NATLFLAG")
keepvars <- unique(c("LOGVALUE",geovar,charvar,qualvar))

# Limit to owner-occupied units with non-missing value and square-footage from metropolitan sample
ahs <- newhouse[ newhouse$TENURE=="1" &
                 newhouse$VACANCY == -6 &
                   newhouse$NATLFLAG == 2,
                  keepvars]

# Collapse very rare factor levels by hand so that don't have to delete full variable
ahs$SEWDIS[as.numeric(as.character(ahs$SEWDIS)) > 0] <- 1
ahs$WATER[as.numeric(as.character(ahs$WATER)) == 6] <- 7
ahs$ELEV[as.numeric(as.character(ahs$ELEV)) == 3] <- -6
ahs$ELEVWK[as.numeric(as.character(ahs$ELEVWK)) == 2] <- -6

# Take samples
set.seed(20170313)
ahs$holdout <- as.logical(1:nrow(ahs) %in% sample(nrow(ahs), nrow(ahs) - 10000))

# Create folds for lasso barcode plot
ahs$lassofolds <- as.factor(ceiling(10 * sample(nrow(ahs)) / nrow(ahs)))

# Introduce missingness dummies
withmiss <- c()
for(contvar in names(ahs)[(!sapply(ahs, is.factor)) & sapply(ahs, is.numeric)]) {
  if(sum(ahs[, contvar] < 0) > 0) {
    ahs[, paste0(contvar, "MISS")] <- as.factor(pmin(ahs[, contvar], 0))
    ahs[ahs[, contvar] < 0, contvar] <- mean(ahs[!ahs$holdout & ahs[, contvar] > 0, contvar], na.rm=T)
    withmiss <- c(withmiss, contvar)
  }
}

# Collapse missingness that does not have enough variation
for(varname in names(ahs)[sapply(ahs, is.factor)]) {
  thiscol <- as.numeric(as.character(ahs[,varname]))
  if(min(table(thiscol[thiscol < 0])) < 30) {
    ahs[,varname] <- as.numeric(as.character(ahs[,varname]))
    ahs[thiscol < 0,varname] <- -1
    ahs[,varname] <- as.factor(ahs[,varname])
  }
}

# Delete variables that are constant on full training set
for(varname in setdiff(names(ahs), "holdout")) {
  if(length(unique(ahs[!ahs$holdout, varname])) == 1) {
    ahs <- ahs[, !(names(ahs) %in% paste0(varname, c("", "IMPUTED")))]
    print(paste0("Deleted ",varname))
  }}

# Delete variables that have new levels on holdout
for(varname in names(ahs)[sapply(ahs, is.factor)]) {
  if((! varname %in% c("holdout","lassofolds")) & length(setdiff(unique(ahs[ahs$holdout, varname]),
                                                                 unique(ahs[!ahs$holdout, varname]))) > 0) {
    print(paste0(varname," misses ",setdiff(unique(ahs[ahs$holdout, varname]),
                                            unique(ahs[!ahs$holdout, varname]))))
    ahs <- ahs[, !(names(ahs) == varname)]
  }
}

# Flatten factors
ahs <- droplevels(ahs)

# Choose variables of interest
downtoearth <- function(RHS,survivors) {
  keepthem <- intersect(c(RHS,paste0(RHS,"MISS")),survivors)
  return(keepthem)
}
vars <- downtoearth(c(charvar,qualvar,geovar),names(ahs))

getformula <- function(varlist,LHS="LOGVALUE") {
  return(paste(LHS, paste(varlist,collapse=" + "), sep = " ~ "))
}

saveRDS(list(df=ahs,vars=vars,getformula=getformula),
        file="ahs2011forjep.rdata")