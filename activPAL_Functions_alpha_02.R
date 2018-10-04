################ Header ###############################################################################
# Program : activPal_Functions_alpha.R
#
# Reproduction of the matlab-program "CPAHR_activPAL_software_version_0.0.8"
#
# Input:  15s epoch.csv  file from activPAL
#
# Author: Jan Behrens           DATE: 
# Version:  a_2.0                 DATE: 09.08.2017
#           a_3.0                 DATE: 26.09.2017
#           a_3.1                 DATE: 03.11.2017
#           a_3.2                 DATE: 01.12.2017
#           a_3.3                 DATE: 08.01.2018
#           a_3.4                 DATE: 02.02.2018
#           a_3.5                 DATE: 16.02.2018
#           a_3.6                 DATE: 26.02.2018
#           a_3.7                 DATE: 03.05.2018
#           a_4.2                 DATE: 11.07.2018
#           a_4.3                 DATE: 16.07.2018
# Change:   a_2.0 - adding function to create summary 
#                 - ActivPal data Name edited for output csv files
#           a_3.0 - Adding  function For Bout statistics
#           a_3.1 - NW-File to delete from Analysis
#                 - Domain-File to classify Bouts to Domains
#                 - calculating general statistics for mastersheet
#                 - creating Mastersheet
#           a_3.2 - creating Average Day Individual File
#                 - Added Abs_SED_H variable from original Mastersheet
#                 - Different number of Domains between individuals possible (not within one ID)
#           a_3.3 - for NonWearTimes and DomainTimes each one file for all IDs
#                 - Option to define the separator and decimal symbol for the csv-files
#           a_3.4 -  Schlafzeiten k?nnen nun angepasst werden, sodass sie dem jeweiligen Tag bez?glich 
#                   eines spezifischen Zeitpunktes f?r den Tageswechsel zugeordnet werden
#                 - Zeiteinheit f?r Aktivit?sdauern setzbar (Minuten, Stunden)
#           a_3.5 - more variability
#                 - Repair for all warnings
#                 - Rename output variables and parameters
#           a_3.5 - optional input of xlxs-files for Domains and Nonweartimes
#                 - Domain-file not needed anymore for run NULL possible value
#           a_4.2 - When no Sleep Bout found NAs 
#                 - When Sleep Ends before Day Ends NAs
#                 - When one sedentary Bout over multiple days, days in between are deleted
#                 - simple Bugfixes
#           a_4.3 - ID readable as numeric and character 0001, 001, 01, 1 must be read as numeric because of xlsx-format
#                 - BugFixed: Functions are working for datasets with 1 Day of measurement          
###########################################################################################################;

################ Set Global Options ##############################

Sys.setenv(TZ="GMT")
options(tz="GMT")

####### Load or Install used packages #####################

#library("dplyr")
package.test <- require("dplyr")
if(!package.test)
{
  install.packages("dplyr", dependencies = TRUE)
  require("dplyr")
}

xlsxPackageTest <- require("xlsx")
if(!xlsxPackageTest)
{
  install.packages("xlsx", dependencies = TRUE)
  require("xlsx")
}

################ Analyse Functions ##################

######### SupportFile Functions ##################

xlsxToTableFormat <- function(xlsx.path, sheetIndex=1)
  # read Supportfiles in xlsx-Format
{
  x <- read.xlsx(file=xlsx.path, sheetIndex = sheetIndex, colClasses = "character",as.data.frame = TRUE)
  x[,2] <- as.Date(as.character(x[,2]),tz=getOption("tz"))
  if(ncol(x)>=5)
  {
    for(j in seq(from=3 , to=(ncol(x)-2) , by=3))
    {
      temp.list3 <- strsplit(as.character(x[,j]),"\ ")
      temp.list4 <- strsplit(as.character(x[,j+1]),"\ ")
      for(i in 1:nrow(x))
      {
        x[i,j] <- as.POSIXct(paste(as.character(x[i,2]),as.character(temp.list3[[i]][2]),sep=" "),tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
        x[i,j+1] <- as.POSIXct(paste(as.character(x[i,2]),as.character(temp.list4[[i]][2]),sep=" "),tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
      }
    }
  }
  else
  {
    x <- NULL
  }
  return(x)
}

#### Domain file ############
create_DomainCSV <- function(path=NULL,accdata=NULL, separator=",", dec=".")
  # Creates the Support File for the Domains, seperated by id. When path is given writes a csv.file when accdata is given
  # Dates are created from ActivPaldata
{
  if(is.null(accdata))
  {
    dates <- c(2016-04-14,2016-04-14,2016-04-14,2016-04-14,2016-04-14,2016-04-14,2016-04-14)
    start1 <- rep.int("00:00:00",times = length(dates))
    end1 <- rep.int("00:00:00",times = length(dates))
    DomainName1 <- rep.int("Domain1",times = length(dates))
    start2 <- rep.int("00:00:00",times = length(dates))
    end2 <- rep.int("00:00:00",times = length(dates))
    DomainName2 <- rep.int("Domain2",times = length(dates))
    start3 <- rep.int("00:00:00",times = length(dates))
    end3 <- rep.int("00:00:00",times = length(dates))
    DomainName3 <- rep.int("Domain3",times = length(dates))
    start4 <- rep.int("00:00:00",times = length(dates))
    end4 <- rep.int("00:00:00",times = length(dates))
    DomainName4 <- rep.int("Domain4",times = length(dates))
    start5 <- rep.int("00:00:00",times = length(dates))
    end5 <- rep.int("00:00:00",times = length(dates))
    DomainName5 <- rep.int("Domain5",times = length(dates))
  }
  else
  {
    n_tab <- read.csv(accdata, header=TRUE, sep=",")
    is_v1 <-  all(is.na(n_tab[,10])) & all(is.na(n_tab[,11])) # if version == 1 then uniaxial and variable is TRUE
    
    if(is_v1) # no sample data for v1
    {
      n_date <- as.character(n_tab[,1])
      n_date <- substring(n_date, 2)
      n_date <- strptime(n_date,format="%y-%m-%d", tz=getOption("tz"))
    }
    else # tested
    {
      temp_val <- n_tab[,1]
      n_date <- as.POSIXct((temp_val + 693960 - 719529)*86400, origin = "1970-01-01", tz=getOption("tz"))
      n_date <- as.Date(n_date, tz=getOption("tz"))
    }
    n_date <- unique(n_date)
    dates <- n_date
    start1 <- rep.int("00:00:00",times = length(dates))
    end1 <- rep.int("00:00:00",times = length(dates))
    DomainName1 <- rep.int("Domain1",times = length(dates))
    start2 <- rep.int("00:00:00",times = length(dates))
    end2 <- rep.int("00:00:00",times = length(dates))
    DomainName2 <- rep.int("Domain2",times = length(dates))
    start3 <- rep.int("00:00:00",times = length(dates))
    end3 <- rep.int("00:00:00",times = length(dates))
    DomainName3 <- rep.int("Domain3",times = length(dates))
    start4 <- rep.int("00:00:00",times = length(dates))
    end4 <- rep.int("00:00:00",times = length(dates))
    DomainName4 <- rep.int("Domain4",times = length(dates))
    start5 <- rep.int("00:00:00",times = length(dates))
    end5 <- rep.int("00:00:00",times = length(dates))
    DomainName5 <- rep.int("Domain5",times = length(dates))
  }
  
  DomainCsv <- cbind.data.frame(dates, start1, end1,DomainName1, start2, end2,DomainName2, start3, end3, 
                                DomainName3,start4, end4,DomainName4, start5, end5,DomainName5)
  colnames(DomainCsv) <- (c("dates", "start1", "end1","DomainName1", "start2", "end2","DomainName2", "start3", "end3", 
                            "DomainName3","start4", "end4","DomainName4", "start5", "end5","DomainName5"))
  if(!is.null(path))
  {
    write.table(DomainCsv,file = path, row.names = FALSE, sep=separator, dec=dec)
  }
  return(DomainCsv)
}

readDomains.Idsep <- function(path, separator=",", dec=".")
  # Function for Reading Domain-supportfiles which are sepperated by id
{
  DomainCSV <- read.table(path, header=TRUE, sep=separator, dec=dec)
  DomainSeq <- seq(from=2,to=(ncol(DomainCSV)-2),by=3)
  for(i in DomainSeq)
  {
    date <- vector("logical", length=nrow(DomainCSV))
    date2 <- vector("logical", length=nrow(DomainCSV))
    for(j in 1:nrow(DomainCSV))
    {
      date[j] <- paste(DomainCSV[j,1], DomainCSV[j,i],sep=" ")
      date2[j] <- paste(DomainCSV[j,1],DomainCSV[j,i+1],sep = " ")
    }
    DomainCSV[,i] <- as.POSIXct(date, tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
    DomainCSV[,i+1] <- as.POSIXct(date2, tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
  }
  return(DomainCSV)
}

readDomains.joined <- function(path, separator=",", dec=".")
  # Function for Reading the  Domain-supportfile for alle ids joined
{
  joined.Domains <- read.table(path, header=TRUE, sep=separator, dec=dec)
  DomainSeq <- seq(from=3,to=(ncol(joined.Domains)-2),by=3)
  for(i in DomainSeq)
  {
    date <- vector("logical", length=nrow(joined.Domains))
    date2 <- vector("logical", length=nrow(joined.Domains))
    for(j in 1:nrow(joined.Domains))
    {
      date[j] <- paste(as.character(joined.Domains[j,2]), joined.Domains[j,i],sep=" ")
      date2[j] <- paste(as.character(joined.Domains[j,2]),joined.Domains[j,i+1],sep = " ")
    }
    #joined.Domains[,i] <- as.POSIXct(date, tz=getOption("tz"), format="%d.%m.%Y %H:%M:%S")
    #joined.Domains[,i+1] <- as.POSIXct(date2, tz=getOption("tz"), format="%d.%m.%Y %H:%M:%S")
    #joined.Domains[,2] <- as.character(as.Date(joined.Domains[,i], format="%Y-%m-%d", tz=getOption("tz")))
    #print(getOption("tz"))
    joined.Domains[,i] <- as.POSIXct(date, tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
    joined.Domains[,i+1] <- as.POSIXct(date2, tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
  }
  return(joined.Domains)
}

getDomainsforID <- function(path=NULL, separated.csv=TRUE, JoinedDomains=NULL, id=NULL, separator=",", dec=".", is.xlsx=FALSE, sheetIndex=1)
  # Function for receiving Domain-informations for specific id from joined Domain File
{
  Domains <- NULL
  if(is.xlsx)
  {
    Domains <- getDomainsforID.XLSX(xlsx.path=path, separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=id, sheetIndex=sheetIndex)
  }
  else
  {
    # getDomainsforID(path) replace in methods with more parameters
    if(is.null(path))
    {
      if(is.null(JoinedDomains))
      {
        Domains <- NULL
      }
      if(separated.csv)
      {
        Domains <- NULL
      }
    }
    else
    {
      if(separated.csv)
      {
        Domains <- readDomains.Idsep(path, separator=separator, dec=dec)
      }
      else
      {
        if(is.null(JoinedDomains))
        {
          JoinedDomains <- readDomains.joined(path, separator=separator, dec=dec)
          JoinedDomains[,1] <- as.character(JoinedDomains[,1])
        }
        Domains <- JoinedDomains[which(JoinedDomains[,1]==id),2:ncol(JoinedDomains)]
        if(nrow(Domains)<1)
        {
          Domains <- NULL
        }
      }
    }
  }
  return(Domains)
}

getDomainsforID.XLSX <- function(xlsx.path=NULL, separated.csv=TRUE, JoinedDomains=NULL, id=NULL, sheetIndex=1)
  # Function for receiving Domain-informations for specific id from joined Domain File, for Domain-Files in xlsx-format
{
  Domains <- NULL
  # getDomainsforID(path) replace in methods with more parameters
  if(is.null(xlsx.path))
  {
    if(is.null(JoinedDomains))
    {
      Domains <- NULL
    }
    if(separated.csv)
    {
      Domains <- NULL
    }
  }
  else
  {
    if(separated.csv)
    {
      Domains <- xlsxToTableFormat(filepath=xlsx.path, sheetIndex=sheetIndex)
    }
    else
    {
      if(is.null(JoinedDomains))
      {
        JoinedDomains <- xlsxToTableFormat(filepath=xlsx.path, sheetIndex=sheetIndex)
      }
      
      if(!is.null(JoinedDomains))
      {
        Domains <- JoinedDomains[which(JoinedDomains[,1]==id),2:ncol(JoinedDomains)]
      }
      else
      {
        Domains <-NULL
      }
      
      if(nrow(Domains)<1)
      {
        Domains <- NULL
      }
    }
  }
  return(Domains)
}


#### NW file ############

createNWcsv <- function(path=NULL,accdata=NULL, separator=",", dec=".")
  # Creates the Support File for the non-weartime, seperated by id. When path is given writes a csv.file when accdata is given
  # Dates are created from ActivPaldata
{
  if(is.null(accdata))
  {
    dates <- c(2016-04-14,2016-04-14,2016-04-14,2016-04-14,2016-04-14,2016-04-14,2016-04-14)
  }
  else
  {
    n_tab <- read.csv(accdata, header=TRUE, sep=",")
    is_v1 <-  all(is.na(n_tab[,10])) & all(is.na(n_tab[,11])) # if version == 1 then uniaxial and variable is TRUE
    
    if(is_v1) # no sample data for v1
    {
      n_date <- as.character(n_tab[,1])
      n_date <- substring(n_date, 2)
      n_date <- strptime(n_date,format="%y-%m-%d", tz=getOption("tz"))
    }
    else # tested
    {
      temp_val <- n_tab[,1]
      n_date <- as.POSIXct((temp_val + 693960 - 719529)*86400, origin = "1970-01-01", tz=getOption("tz"))
      n_date <- as.Date(n_date, tz=getOption("tz"))
    }
    n_date <- unique(n_date)
    dates <- n_date
  }
  
  nw_start1 <- rep.int("00:00:00",times=length(dates))
  nw_end1 <- rep.int("00:00:00",times=length(dates))
  nw_reason1 <- rep.int("reason1",times=length(dates))
  nw_start2 <- rep.int("00:00:00",times=length(dates))
  nw_end2 <- rep.int("00:00:00",times=length(dates))
  nw_reason2 <- rep.int("reason2",times=length(dates))
  nw_start3 <- rep.int("00:00:00",times=length(dates))
  nw_end3 <- rep.int("00:00:00",times=length(dates))
  nw_reason3 <- rep.int("reason3",times=length(dates))
  NWCsv <- cbind.data.frame(dates,nw_start1,nw_end1,nw_reason1,nw_start2,nw_end2,nw_reason2,nw_start3,nw_end3,nw_reason3)
  colnames(NWCsv) <- c("dates","nw_start1","nw_end1","nw_reason1","nw_start2","nw_end2","nw_reason2","nw_start3","nw_end3","nw_reason3")
  if(!is.null(path))
  {
    write.table(NWCsv,file = path, row.names = FALSE, sep=separator, dec=dec)
  }
  return(NWCsv)
}

get_NWforID.XLSX <- function(xlsx.path,separated.csv=FALSE, JoinedNWs=NULL, id=NULL, sheetIndex=1)
  # Function for receiving NonWear-informations for specific id from joined Domain File, for NonWear-Files in xlsx-format
{
  NonWears <- NULL
  
  if(separated.csv)
  {
    NonWears <- xlsxToTableFormat(xlsx.path=xlsx.path, sheetIndex=sheetIndex)
  }
  else
  {
    if(is.null(JoinedNWs))
    {
      JoinedNWs <- xlsxToTableFormat(xlsx.path=xlsx.path, sheetIndex=sheetIndex)
    }
    if(!is.null(JoinedNWs))
    {
      NonWears <- JoinedNWs[which(JoinedNWs[,1]==id),2:ncol(JoinedNWs)]
    }
    else
    {
      NonWears <- NULL
    }
    if(nrow(NonWears)<1)
    {
      NonWears <- NULL
    }
  }
  return(NonWears)
}

getNWtable <- function(SED_ACT_ALL, NW.csv.Path,separated.csv=TRUE, JoinedNWs=NULL, id=NULL, separator=",", dec=".", is.xlsx=FALSE, sheetIndex=1)
  # create Table with information of the nonwear-times per day
{
  NW.tab <- get_NWforID(NW.csv.Path,separated.csv=separated.csv, JoinedNWs=JoinedNWs, id=id, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
  dte.all <- as.character(as.Date(SED_ACT_ALL[,1], tz=getOption("tz"),format="%Y-%m-%d"))
  dte.NW <- as.character(as.Date(NW.tab[,1], tz=getOption("tz"),format="%Y-%m-%d"))
  unq.dates2 <- unique(dte.all)
  unq.dates <- unique(dte.NW)
  if(length(unq.dates)<1)
  {
    return(NULL)
  }
  if(length(unq.dates) > length(unique(dte.all)))
  {
    datedel <- NULL
    for(i in 1:length(unq.dates))
    {
      if(length(which(unique(dte.all)==unq.dates[i]))<1)
      {
        datedel <- c(datedel,i)
      }
    }
    NW.tab <- NW.tab[-datedel,]
    unq.dates <- unq.dates[-datedel]
  }
  for(day in 1:length(unq.dates))
  {
    day.tab <- SED_ACT_ALL[which(dte.all == unq.dates[day]),]
    start.bout <- as.POSIXct(paste(unq.dates[day],day.tab[,2],sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
    fin.bout <- as.POSIXct(paste(unq.dates[day],day.tab[,3],sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
    reason.vec <- NW.tab[which(dte.NW == unq.dates[day]),seq(from=4,to=ncol(NW.tab),by=3)]
    reason.vec <- as.character(reason.vec)
    del.vec <- which(reason.vec == "-")
    NW.seq <- seq(from=2,to=(ncol(NW.tab)-2),by=3)
    null.nws <-  which(is.na(NW.tab[which(dte.NW == unq.dates[day]),NW.seq]))
    if(length(null.nws)>0)
    {
      NW.seq <- NW.seq[-null.nws]
    }
    alt.NWtime <- 0
    if(length(del.vec >0))
    {
      NW.seq <- NW.seq[-del.vec,]
    }
    del.com <- NULL
    for(i in NW.seq)
    {
      
      del.NW1 <- which(start.bout > as.POSIXct(NW.tab[which(dte.NW == unq.dates[day]),i], tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S") 
                       & start.bout < as.POSIXct(NW.tab[which(dte.NW == unq.dates[day]),(i+1)], tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S"))
      del.NW2 <- which(fin.bout > as.POSIXct(NW.tab[which(dte.NW == unq.dates[day]),i], tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S") 
                       & fin.bout < as.POSIXct(NW.tab[which(dte.NW == unq.dates[day]),(i+1)], tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S"))
      del.NW3 <- which(start.bout < as.POSIXct(NW.tab[which(dte.NW == unq.dates[day]),i], tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S") 
                       & fin.bout > as.POSIXct(NW.tab[which(dte.NW == unq.dates[day]),(i+1)], tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S"))
      if(is.null(del.com))
      {
        del.com <- c(del.NW1,del.NW2,del.NW3)
      }
      else
      {
        del.com <- c(del.com,del.NW1,del.NW2,del.NW3)
      }
      alt.NWtime <- alt.NWtime + difftime(NW.tab[which(dte.NW == unq.dates[day]),(i+1)],NW.tab[which(dte.NW == unq.dates[day]),i],units = "mins")
    }
    del.com <- unique(del.com)
    NW.bouts <- day.tab[del.com,]
    NW.time <- sum(NW.bouts[,5])/60
    alt.NWtime <- as.numeric(alt.NWtime)/60
    if(alt.NWtime > NW.time)
    {
      NW.time <- alt.NWtime
    }
    nonNW.time <- 24-NW.time
    if(day == 1)
    {
      NW.outVec <- c(day,NW.time,nonNW.time)
    }
    else
    {
      temp.vec <- c(day,NW.time,nonNW.time)
      NW.outVec <- rbind(NW.outVec,temp.vec )
    }
  }
  rownames(NW.outVec) <- NULL
  if(is.null(nrow(NW.outVec)))
  {
    names(NW.outVec) <- c("Day","NWHrs","TotalHrs-NW")
  }
  else
  {
    colnames(NW.outVec) <- c("Day","NWHrs","TotalHrs-NW")
  }
  
  return(NW.outVec)
}

NUll.NWtab <- function(SED_ACT_ALL)
  # create empty non-wear-table if no nonwear events where given for the id
{
  dte.all <- as.character(as.Date(SED_ACT_ALL[,1], tz=getOption("tz"),format="%Y-%m-%d"))
  unq.dates <- unique(dte.all)
  Day <- seq(from=1, to=length(unq.dates), by=1)
  NWHrs <- rep.int(0,times = length(unq.dates))
  tempNam <- rep.int(24,times = length(unq.dates))
  NW.tab <- cbind(Day,NWHrs,tempNam)
  colnames(NW.tab) <- c("Day","NWHrs","TotalHrs-NW")
  return(NW.tab)
}

read_NWcsv <- function(path, separator=",", dec=".")
  # Function for Reading nonWear-supportfiles which are sepperated by id
{
  NWCSV <- read.table(path, header=TRUE, sep=separator, dec=dec)
  NWSeq <- seq(from=2,to=(ncol(NWCSV)-2),by=3)
  for(i in NWSeq)
  {
    date <- vector("logical", length=nrow(NWCSV))
    date2 <- vector("logical", length=nrow(NWCSV))
    for(j in 1:nrow(NWCSV))
    {
      date[j] <- paste(NWCSV[j,1], NWCSV[j,i],sep=" ")
      date2[j] <- paste(NWCSV[j,1],NWCSV[j,i+1],sep = " ")
    }
    NWCSV[,i] <- as.POSIXct(date, tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
    NWCSV[,i+1] <- as.POSIXct(date2, tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
  }
  return(NWCSV)
}

readNWs.joined <- function(path, separator=",", dec=".")
  # Function for Reading the  nonwear-supportfile for alle ids joined
{
  joined.NWs <- read.table(path, header=TRUE, sep=separator, dec=dec)
  NWSeq <- seq(from=3,to=(ncol(joined.NWs)-2),by=3)
  for(i in NWSeq)
  {
    date <- vector("logical", length=nrow(joined.NWs))
    date2 <- vector("logical", length=nrow(joined.NWs))
    for(j in 1:nrow(joined.NWs))
    {
      date[j] <- paste(as.character(joined.NWs[j,2]), joined.NWs[j,i],sep=" ")
      date2[j] <- paste(as.character(joined.NWs[j,2]),joined.NWs[j,i+1],sep = " ")
    }
    joined.NWs[,i] <- as.POSIXct(date, tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
    joined.NWs[,i+1] <- as.POSIXct(date2, tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
  }
  return(joined.NWs)
}

get_NWforID <- function(path,separated.csv=TRUE, JoinedNWs=NULL, id=NULL, separator=",", dec=".",is.xlsx=FALSE, sheetIndex=1)
  # Function for receiving Domain-informations for specific id from joined nonwear File
{
  NonWears <- NULL
  if(is.null(JoinedNWs))
  {
    if(is.xlsx)
    {
      NonWears <- get_NWforID.XLSX(path,separated.csv=separated.csv, JoinedNWs=JoinedNWs, id=id, sheetIndex=sheetIndex)
    }
    else
    {
      if(separated.csv)
      {
        NonWears <- read_NWcsv(path, separator=separator, dec=dec)
      }
      else
      {
        JoinedNWs <- readNWs.joined(path, separator=separator, dec=dec)
      }
      NonWears <- JoinedNWs[which(JoinedNWs[,1]==id),2:ncol(JoinedNWs)]
    }
  }
  else
  {
    NonWears <- JoinedNWs[which(JoinedNWs[,1]==id),2:ncol(JoinedNWs)]
  }
  return(NonWears)
}

#### Slp file ############

createSleepCsv <- function(path=NULL,accdata=NULL, separator=",", dec=".", type="xlsx")
{
  if(is.null(accdata))
  {
    dates <- c(2016-04-14,2016-04-14,2016-04-14,2016-04-14,2016-04-14,2016-04-14,2016-04-14)
  }
  else
  {
    n_tab <- read.csv(accdata, header=TRUE, sep=",")
    is_v1 <-  all(is.na(n_tab[,10])) & all(is.na(n_tab[,11])) # if version == 1 then uniaxial and variable is TRUE
    
    if(is_v1) # no sample data for v1
    {
      n_date <- as.character(n_tab[,1])
      n_date <- substring(n_date, 2)
      n_date <- strptime(n_date,format="%y-%m-%d", tz=getOption("tz"))
    }
    else # tested
    {
      temp_val <- n_tab[,1]
      n_date <- as.POSIXct((temp_val + 693960 - 719529)*86400, origin = "1970-01-01", tz=getOption("tz"))
      n_date <- as.Date(n_date, tz=getOption("tz"))
    }
    n_date <- unique(n_date)
    dates <- n_date
  }
  
  slp_start <- rep.int("23:00:00",times=length(dates))
  slp_end <- rep.int("05:00:00",times=length(dates))
  Interrups <- rep.int(NA,times=length(dates))
  SlpCsv <- cbind.data.frame(dates,slp_start,slp_end,Interrups)
  colnames(SlpCsv) <- c("dates","SlpStart","SlpEnd","Interrups")
  if(!is.null(path))
  {
    if(type=="xlsx")
    {
      write.xlsx(SlpCsv,file = path, row.names = FALSE)
    }
    else
    {
      write.table(SlpCsv,file = path, row.names = FALSE, sep=separator, dec=dec)
    }
  }
  return(SlpCsv)
}

readSleepFile <- function(SED_ACT_ALL, Slp.Path,separated.csv=TRUE, idNr, separator=",", dec=".", is.xlsx=FALSE, sheetIndex=1,idlength=5)
{
  if(is.xlsx)
  {
    if(separated.csv)
    {
      lsSlpFiles <- list.files(path=Slp.Path, pattern="\\.xlsx$")
      IDs <- substr(basename(lsSlpFiles),1,idlength)
      rslpTab <- read.xlsx(Slp.Path[which(IDs==idNr)],sheetIndex,header=TRUE)
    }
    else
    {
      rslpTab <- read.xlsx(Slp.Path,sheetIndex,header=TRUE)
      rslpTab <- rslpTab[which(rslpTab[,1]==idNr),-c(1)]
    }
  }
  else
  {
    if(separated.csv)
    {
      lsSlpFiles <- list.files(path=Slp.Path, pattern="\\.csv$")
      IDs <- substr(basename(lsSlpFiles),1,idlength)
      rslpTab <- read.csv(Slp.Path[which(IDs==idNr)], header=TRUE, sep=separator, dec=dec,stringsAsFactors=FALSE)
    }
    else
    {
      rslpTab <- read.csv(Slp.Path, header=TRUE, sep=separator, dec=dec,stringsAsFactors=FALSE)
      rslpTab <- rslpTab[which(rslpTab[,1]==idNr),-c(1)]
    }
  }
  unq.ACTAll.dte <- unique(as.Date(SED_ACT_ALL[,1]))
  unq.ACTAll.dte <- unq.ACTAll.dte[-c(length(unq.ACTAll.dte))]
  unq.Slp.dte <- unique(as.Date(as.character(rslpTab[,1]),tz=getOption("tz"),format="%Y-%m-%d"))
  for(slpdte in unq.Slp.dte)
  {
    if(length(which(unq.ACTAll.dte == slpdte))<1)
    {
      rslpTab <- rslpTab[-which(as.Date(as.character(rslpTab[,1]),tz=getOption("tz"),format="%Y-%m-%d")==slpdte),]
    }
  }
  
  slpTab <- adjustSlpTab(rslpTab)
  return(slpTab)
}

adjustSlpTab <- function(slpTab)
{
  Date <- as.Date(as.character(slpTab[,1]),tz=getOption("tz"),format="%Y-%m-%d")
  Slpstart <- as.POSIXct(paste(Date,slpTab[,2]), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
  SlpEnd <- as.POSIXct(paste(Date,slpTab[,3]), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
  SlpEnd <- SlpEnd + 86400
  seconds <- as.numeric(difftime(SlpEnd,slpTab[,1],units = "secs"))
  minutes <- as.numeric(difftime(SlpEnd,slpTab[,1],units = "mins"))
  segments <- seconds/15
  interrups <- slpTab[4]
  begin <- format(Slpstart,"%H:%M:%S")
  finish <- format(SlpEnd,"%H:%M:%S")
  SleepTab <- cbind.data.frame(Slpstart,begin,finish,seconds,minutes,segments,interrups)
  return(SleepTab)
}

################ Read ACC File Info ######

read_activPAL <- function(inputpath) 
  # read activPAL data and set formats right + Timefraction +
{
  n_tab <- read.csv(inputpath, header=TRUE, sep=",")
  colnames(n_tab) <- c("time","StepCount","Activity_Score","Sedentary","Upright","Stepping","Sed_to_Up","Up_to_Sed","Channel1","Channel2","Channel3")
  
  #calc activPAL version
  is_v1 <-  all(is.na(n_tab[,10])) & all(is.na(n_tab[,11])) # if version == 1 then uniaxial and variable is TRUE
  
  ###calc date and time
  if(is_v1) # no sample data for v1
  {
    n_date <- as.character(n_tab[,1])
    n_date <- substring(n_date, 2)
    n_date <- strptime(n_date,format="%y-%m-%d, %H:%M:%S", tz=getOption("tz"))
  }
  else # tested
  {
    temp_val <- n_tab[,1]
    n_date <- as.POSIXct((temp_val + 693960 - 719529)*86400, origin = "1970-01-01", tz=getOption("tz"))
  }
  
  n_tab <- cbind(n_date,n_tab[,2:11])
  colnames(n_tab)[1] <- "Date"
  # Anmerkung: Umgang mit 00:00:00 unbekannt in dieser Funktion muss noch getestet werden
  
  ### generate Timefraction
  epochlength <- round(as.numeric(difftime(n_tab[2,1],n_tab[1,1],units = "secs")),digits = 0)
  epochfactor <- 60/epochlength
  # 24*60=1440 minutes in a day
  # 1440 minutes * 4 X (15seconds)segment = 5760(15second segments)=1 day
  t_frac <- 1440 * (epochfactor) 
  hour <- as.numeric(format(n_tab[,1], "%H"))
  minute <- as.numeric(format(n_tab[,1], "%M"))
  sec <- as.numeric(format(n_tab[,1], "%S"))
  TimeFraction <- ((hour*60*epochfactor)+(minute*epochfactor)+(sec/epochlength))/t_frac
  n_tab <- cbind(TimeFraction,n_tab)
  
  ### get standing time
  Standing <- n_tab$Upright - n_tab$Stepping
  n_tab <- cbind(n_tab[,1:7],Standing, n_tab[,8:12])
  return(n_tab)
}

getID <- function(input, id.length, id.start=1)
  # return ID from ActivPal-data-path
{
  ret.nam <- get_filename(input)
  ret.nam <- substring(ret.nam,first = id.start,last = (id.start+id.length-1))
  return(ret.nam)
}

get_filename <- function(inputpath)
  # return filename of ActivPal-data
{
  name_parts <- strsplit(inputpath, "[\\]")[[1]]
  name <-name_parts[length(name_parts)] 
  name <- strsplit(name, "[.]")[[1]]
  name <- name[1]
  name <- strsplit(name," ")[[1]]
  filename <- paste(name[1],name[2],sep = "_")
  
  return(filename)
}


######### Create Acc Sed Summary ########

create_Summary <- function(DATA, SED_adresses)
  # create summary of sedentary-epochs
{
  temp_vec <-DATA$Sedentary
  SedLength_segments <- numeric(length=nrow(SED_adresses))
  for(ii in 1:nrow(SED_adresses))
  {
    SedLength_segments[ii] <- sum(temp_vec[seq(from=SED_adresses[ii,1],to=SED_adresses[ii,2])])
  }
  Mean <- mean(SedLength_segments)
  Stdev <- sd(SedLength_segments)
  Max <- max(SedLength_segments)
  Min <- min(SedLength_segments)
  N <- length(SedLength_segments)
  
  #fehlendes cleaning nach NW siehe matlab row 956
  
  vec_summary <- c(Mean,Stdev,Max,Min,N)
  rm("SED_adresses")
  return(vec_summary)
}

FN_CPAHR_Summary <- function(input,output,Threshold_static=3600,is.csvout=T)
  # Create sedentary summary from ActivPal-dataset
{
  print(Sys.time())
  tab <- read_activPAL(inputpath = input)
  epochlength <- round(as.numeric(difftime(tab[2,2],tab[1,2],units = "secs")),digits = 0)
  print(Sys.time())
  Sed_addresses_tab <- Sed_detect_transition(tab)
  vec1 <- create_Summary(tab, Sed_addresses_tab)
  Sed_addresses_tab2 <- check_SegmentThreshold(Sed_addresses_tab, Threshold_static, epochlength=epochlength)
  vec2 <- create_Summary(tab, Sed_addresses_tab2)
  sumary <- rbind(vec1,vec2)
  colnames(sumary) <- c("Mean","Stdev","Max","Min","N")
  rownames(sumary) <- c("Sedentary activity","Sedentary activity <3600 seconds")
  print(Sys.time())
  if(is.csvout)
  {
    name <- get_filename(input)
    outputfile <- paste(output,name,"_Summary_Stats.csv",sep = "")
    write.csv(sumary, file=outputfile, row.names = TRUE)
    warnings()
  }
  else
  {
    print(sumary)
  }
}

################ Table Manipulations ##################

select_timeframe <- function(DATA, timeframe.start, timeframe.end)
  #return specific Timeframe from Dataset (Sedentary-Bout-data)
{
  datetime <- DATA$Date
  date<- as.character(as.Date(datetime,format="%y-%m-%d", tz=getOption("tz")))
  time.start <- paste(date,timeframe.start,sep=" ")
  time.end <- paste(date,timeframe.end,sep=" ")
  time.start <- as.POSIXlt(time.start, tz=getOption("tz"))
  time.end <- as.POSIXlt(time.end, tz=getOption("tz"))
  befor.start <- difftime(datetime,time.start,units = "secs")
  after.end <- difftime(time.end,datetime,units = "secs")
  befor <- which(as.numeric(befor.start) < 0)#(-1) may be better #activPAL starts with 23:59:59 so 06:59:59 is not included by 07:00:00 cutoff
  after <- which(as.numeric(after.end)<0)
  n_DATA <- DATA[-c(befor,after),]
  return(n_DATA)
}

delete_days <- function(Mastersheet, del.firstday=FALSE, del.lastday=FALSE)
  #Platzhalter
{
  
}

delete_NW <- function(SED_ACT_ALL, NW.csv.Path,separated.csv=TRUE, JoinedNWs=NULL, id=NULL, separator=",", dec=".", is.xlsx=FALSE, sheetIndex=1)
  # Delete sedentary-bouts which are within the Nonwear-supportfile
{
  NW.tab <- get_NWforID(NW.csv.Path,separated.csv=separated.csv, JoinedNWs=JoinedNWs, id=id, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
  dte.all <- as.character(as.Date(SED_ACT_ALL[,1], tz=getOption("tz"),format="%Y-%m-%d"))
  dte.NW <- as.character(as.Date(NW.tab[,1], tz=getOption("tz"),format="%Y-%m-%d"))
  unq.dates <- unique(dte.all)
  for(day in 1:length(unq.dates))
  {
    day.tab <- SED_ACT_ALL[which(dte.all == unq.dates[day]),]
    start.bout <- as.POSIXct(paste(unq.dates[day],day.tab[,2],sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
    fin.bout <- as.POSIXct(paste(unq.dates[day],day.tab[,3],sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
    #reason.vec <- NW.tab[which(dte.NW == unq.dates[day]),seq(from=4,to=ncol(NW.tab),by=3)]
    #reason.vec <- as.character(reason.vec)
    #del.vec <- which(reason.vec == "-")
    NW.seq <- seq(from=2,to=(ncol(NW.tab)-2),by=3)
    null.nws <-  which(is.na(NW.tab[which(dte.NW == unq.dates[day]),NW.seq]))
    if(length(null.nws)>0)
    {
      NW.seq <- NW.seq[-null.nws]
    }
    #if(length(del.vec >0))
    #{
    #  NW.seq <- NW.seq[-del.vec,]
    #}
    del.com <- NULL
    for(i in NW.seq)
    {
      del.NW1 <- which(start.bout > NW.tab[which(dte.NW == unq.dates[day]),i] 
                       & start.bout < NW.tab[which(dte.NW == unq.dates[day]),(i+1)])
      del.NW2 <- which(fin.bout > NW.tab[which(dte.NW == unq.dates[day]),i] 
                       & fin.bout < NW.tab[which(dte.NW == unq.dates[day]),(i+1)])
      del.NW3 <- which(start.bout < NW.tab[which(dte.NW == unq.dates[day]),i] 
                       & fin.bout > NW.tab[which(dte.NW == unq.dates[day]),(i+1)])
      if(is.null(del.com))
      {
        del.com <- c(del.NW1,del.NW2,del.NW3)
      }
      else
      {
        del.com <- c(del.com,del.NW1,del.NW2,del.NW3)
      }
    }
    del.com <- unique(del.com)
    if(day == 1)
    {
      if(length(del.com)>0)
      {
        Comb.Day <- day.tab[-del.com,]
      }
      else
      {
        Comb.Day <- day.tab
      }
    }
    else
    {
      if(length(del.com)>0)
      {
        Day.part <- day.tab[-del.com,]
      }
      else
      {
        Day.part <- day.tab
      }
      Comb.Day <- rbind(Comb.Day,Day.part)
    }
  }
  return(Comb.Day)
}

adjustDayBreak <- function() #Empty
{
  
}

######### Domains ##########################

Part_to_Domain <- function(path=NULL, SED_ACT_ALL,separated.csv=TRUE, JoinedDomains=NULL, id=NULL, separator=",", dec=".", is.xlsx=FALSE, sheetIndex=1)
  # categroize sedentary bouts to specific domains
{
  DomainCSV <- getDomainsforID(path=path, separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=id, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
  dats <- as.Date(SED_ACT_ALL[,1], "%y-%m-%d", tz=getOption("tz"))
  x <- vector("list",length=nrow(DomainCSV))
  for(i in 1:nrow(DomainCSV))
  {
    date <- which(dats == as.character(DomainCSV[i,1]))
    subtable <- SED_ACT_ALL[date,]
    times <- subtable[,1]
    for(k in 1:nrow(subtable))
    {
      d <- as.Date(subtable[k,1], "%y-%m-%d", tz=getOption("tz"))
      if(k == 1)
      {
        enti <- paste(d,subtable[k,3], sep=" ")
      }
      else
      {
        enti <- c(enti,paste(d,subtable[k,3], sep=" "))
      }
    }
    enti <- as.POSIXct(enti, tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
    nDomains <- seq(from=2, to=(ncol(DomainCSV)-2),by=3)
    ind <- 1
    ls <- NULL
    for(j in nDomains)
    {
      temp_tab <- subtable[which(times >= DomainCSV[i,j] & enti < DomainCSV[i,j+1]),]
      befstart_tab <- subtable[which(times < DomainCSV[i,j] & enti > DomainCSV[i,j] & enti < DomainCSV[i,j+1]),]
      afend_tab <- subtable[which(times < DomainCSV[i,j+1] & times >= DomainCSV[i,j] & enti >= DomainCSV[i,j+1]),]
      midint_tab <- subtable[which(times < DomainCSV[i,j] & enti > DomainCSV[i,j+1]),]
      
      if(nrow(befstart_tab)>0) #cut ends for start befor domain bouts
      {
        befstart_tab[,2] <- as.POSIXct(DomainCSV[i,j], tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
        temp_time <- vector(mode = "logical", length=nrow(befstart_tab))
        for(k in 1:nrow(befstart_tab))
        {
          d <- as.Date(befstart_tab[k,1], "%Y-%m-%d", tz=getOption("tz"))
          temp_time[k]<-paste(d, befstart_tab[k,3],sep=" ")
        }
        befstart_tab[,3] = as.POSIXct(temp_time, tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
        for(k in 1:nrow(befstart_tab))
        {
          befstart_tab[k,4] <- difftime(befstart_tab[k,3],befstart_tab[k,2],units="secs")
          befstart_tab[k,5] <- difftime(befstart_tab[k,3],befstart_tab[k,2],units="mins")
        }
        befstart_tab[,6] <- befstart_tab[,4]/15
        befstart_tab[,2] = strftime(befstart_tab[,2], tz=getOption("tz"),format = "%H:%M:%S")
        befstart_tab[,3] = strftime(befstart_tab[,3], tz=getOption("tz"),format = "%H:%M:%S")
      }
      
      if(nrow(afend_tab)>0) #cut ends for end after domain bouts
      {
        afend_tab[,3] <- as.POSIXct(DomainCSV[i,j+1], tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
        temp_time <- vector(mode = "logical", length=nrow(afend_tab))
        for(k in 1:nrow(afend_tab))
        {
          d <- as.Date(afend_tab[k,1], "%Y-%m-%d", tz=getOption("tz"))
          temp_time[k]<-paste(d, afend_tab[k,2],sep=" ")
        }
        afend_tab[,2] = as.POSIXct(temp_time, tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
        for(k in 1:nrow(afend_tab))
        {
          if(length(afend_tab[k,3]-afend_tab[k,2])==0)
          {
            afend_tab <- afend_tab[-k,]
          }
          else
          {
            afend_tab[k,4] <- difftime(afend_tab[k,3],afend_tab[k,2],units="secs")
            afend_tab[k,5] <- difftime(afend_tab[k,3],afend_tab[k,2],units="mins")
          }
        }
        if(nrow(afend_tab)>0)
        {
          afend_tab[,6] <- afend_tab[,4]/15
          afend_tab[,2] = strftime(afend_tab[,2], tz=getOption("tz"),format = "%H:%M:%S")
          afend_tab[,3] = strftime(afend_tab[,3], tz=getOption("tz"),format = "%H:%M:%S")
        }
      }
      
      if(nrow(midint_tab)>0) #cut ends for end extending over domain bouts
      {
        midint_tab[,2] <- as.POSIXct(DomainCSV[i,j], tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
        midint_tab[,3] <- as.POSIXct(DomainCSV[i,j+1], tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
        for(k in 1:nrow(midint_tab))
        {
          midint_tab[k,4] <- difftime(midint_tab[k,3],midint_tab[k,2],units="secs")
          midint_tab[k,5] <- difftime(midint_tab[k,3],midint_tab[k,2],units="mins")
        }
        if(nrow(midint_tab)>0)
        {
          midint_tab[,6] <- midint_tab[,4]/15
          midint_tab[,2] = strftime(midint_tab[,2], tz=getOption("tz"),format = "%H:%M:%S")
          midint_tab[,3] = strftime(midint_tab[,3], tz=getOption("tz"),format = "%H:%M:%S")
        }
      }
      temp_tab <- rbind(befstart_tab,temp_tab,midint_tab,afend_tab)
      if(is.null(ls))
      {
        if(nrow(temp_tab)>0)
        {
          ls <- cbind(temp_tab,rep.int(ind,times = nrow(temp_tab)))
          colnames(ls)[ncol(ls)] <- "Domain"
        }
      }
      else
      {
        if(nrow(temp_tab)>0)
        {
          temp_tab <- cbind(temp_tab,rep.int(ind,times = nrow(temp_tab)))
          colnames(temp_tab)[ncol(temp_tab)] <- "Domain"
          ls <- rbind(ls,temp_tab)
        }
      }
      ind <- ind + 1
    }
    x[[i]] <- ls
  }
  return(x)
}

Misc_Domain <- function(DomainFile=NULL, SED_ACT_ALL, separated.csv=TRUE, JoinedDomains=NULL, id=NULL, separator=",", dec=".", is.xlsx=FALSE, sheetIndex=1)
  # create categroie misc for sedentary bouts that could not be categorized to specific domains
{
  DomainCSV <- getDomainsforID(path=DomainFile, separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=id, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
  dats <- as.Date(SED_ACT_ALL[,1], "%y-%m-%d", tz=getOption("tz"))
  nDomains <- seq(from=2, to=(ncol(DomainCSV)-2),by=3)
  x <- vector("list",length=nrow(DomainCSV))
  misctab <- NULL
  miscDoms <- NULL
  for(i in 1:nrow(DomainCSV))
  {
    date <- which(dats == as.character(DomainCSV[i,1]))
    subtable <- SED_ACT_ALL[date,]
    tims <- as.POSIXlt(subtable[,1], tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
    ind  <- 1
    for(dom in nDomains)
    {
      for(k in 1:nrow(subtable))
      {
        d <- as.Date(subtable[k,1], "%y-%m-%d", tz=getOption("tz"))
        if(k == 1)
        {
          enti <- paste(d,subtable[k,3], sep=" ")
        }
        else
        {
          enti <- c(enti,paste(d,subtable[k,3], sep=" "))
        }
      }
      enti <- as.POSIXlt(enti, tz=getOption("tz"), format="%Y-%m-%d %H:%M:%S")
      if(ind==1)
      {
        sts <- as.POSIXlt(paste(d,"00:00:00",sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
        ed <- as.POSIXlt(as.character(DomainCSV[i,dom]), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
        miscDoms <- subtable[which(tims>=sts[ind] & enti < ed[ind]),]
        if(is.null(miscDoms))
        {
          befstart_tab <- subtable[which(tims < sts[ind] & enti > sts[ind] & enti < ed[ind]),]
          afend_tab <- subtable[which(tims < ed[ind] & tims >= sts[ind] & enti >= ed[ind]),]
          midint_tab <- subtable[which(tims < sts[ind] & enti > ed[ind]),]
          if(nrow(befstart_tab)>0)
          {
            cccc <- numeric(length = nrow(befstart_tab))
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            ddd <- numeric(length = nrow(befstart_tab))
            for(ij in 1:nrow(befstart_tab))
            {
              dd <- paste(d,befstart_tab[ij,3],sep = " ")
              cc <- as.POSIXct(sts[ind], tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              ddd[ij] <- as.POSIXct(dd,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            ddd <- as.POSIXlt(ddd, tz=getOption("tz"),origin="1970-01-01")
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            befstart_tab[,2] <- substr(as.character(cccc),start=12,stop=19)
            befstart_tab[,3] <- substr(as.character(ddd),start=12,stop=19)
            befstart_tab[,4] <- difftime(ddd,cccc,units = "secs")
            befstart_tab[,5] <- difftime(ddd,cccc,units = "mins")
            befstart_tab[,6] <- befstart_tab[,4]/15
          }
          if(nrow(afend_tab)>0)
          {
            cccc <- numeric(length = nrow(afend_tab))
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            for(ij in 1:nrow(afend_tab))
            {
              cc <- as.POSIXct(ed[ind], tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              
            }
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            afend_tab[,3] <- substr(as.character(cccc),start=12,stop=19)
            afend_tab[,4] <- difftime(cccc,afend_tab[,1],units = "secs")
            afend_tab[,5] <- difftime(cccc,afend_tab[,1],units = "mins")
            afend_tab[,6] <- afend_tab[,4]/15
          }
          if(nrow(midint_tab)>0)
          {
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            cccc <- numeric(length = nrow(befstart_tab))
            ddd <- numeric(length = nrow(befstart_tab))
            for(ij in 1:nrow(befstart_tab))
            {
              dd <- as.POSIXct(ed[ind], tz=getOption("tz"))
              cc <- as.POSIXct(sts[ind], tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              ddd[ij] <- as.POSIXct(dd,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            ddd <- as.POSIXlt(ddd, tz=getOption("tz"),origin="1970-01-01")
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            midint_tab[,2] <- substr(as.character(cccc),start=12,stop=19)
            midint_tab[,3] <- substr(as.character(ddd),start=12,stop=19)
            midint_tab[,4] <- difftime(ddd,cccc,units = "secs")
            midint_tab[,5] <- difftime(ddd,cccc,units = "mins")
            midint_tab[,6] <- midint_tab[,4]/15
          }
          miscDoms <- rbind(befstart_tab,afend_tab,midint_tab)
          miscDoms <- cbind(miscDoms,rep.int(ind,times = nrow(miscDoms)))
        }
        else
        {
          befstart_tab <- subtable[which(tims < sts[ind] & enti > sts[ind] & enti < ed[ind]),]
          afend_tab <- subtable[which(tims < ed[ind] & tims >= sts[ind] & enti >= ed[ind]),]
          midint_tab <- subtable[which(tims < sts[ind] & enti > ed[ind]),]
          if(nrow(befstart_tab)>0)
          {
            cccc <- numeric(length = nrow(befstart_tab))
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            ddd <- numeric(length = nrow(befstart_tab))
            for(ij in 1:nrow(befstart_tab))
            {
              dd <- paste(d,befstart_tab[ij,3],sep = " ")
              cc <- as.POSIXct(sts[ind], tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              ddd[ij] <- as.POSIXct(dd,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            ddd <- as.POSIXlt(ddd, tz=getOption("tz"),origin="1970-01-01")
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            befstart_tab[,2] <- substr(as.character(cccc),start=12,stop=19)
            befstart_tab[,3] <- substr(as.character(ddd),start=12,stop=19)
            befstart_tab[,4] <- difftime(ddd,cccc,units = "secs")
            befstart_tab[,5] <- difftime(ddd,cccc,units = "mins")
            befstart_tab[,6] <- befstart_tab[,4]/15
          }
          if(nrow(afend_tab)>0)
          {
            cccc <- numeric(length = nrow(afend_tab))
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            for(ij in 1:nrow(afend_tab))
            {
              cc <- as.POSIXct(ed[ind], tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            afend_tab[,3] <- substr(as.character(cccc),start=12,stop=19)
            afend_tab[,4] <- difftime(cccc,afend_tab[,1],units = "secs")
            afend_tab[,5] <- difftime(cccc,afend_tab[,1],units = "mins")
            afend_tab[,6] <- afend_tab[,4]/15
          }
          if(nrow(midint_tab)>0)
          {
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            cccc <- numeric(length = nrow(befstart_tab))
            ddd <- numeric(length = nrow(befstart_tab))
            for(ij in 1:nrow(befstart_tab))
            {
              dd <- as.POSIXct(ed[ind], tz=getOption("tz"))
              cc <- as.POSIXct(sts[ind], tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              ddd[ij] <- as.POSIXct(dd,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            ddd <- as.POSIXlt(ddd, tz=getOption("tz"),origin="1970-01-01")
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            midint_tab[,2] <- substr(as.character(cccc),start=12,stop=19)
            midint_tab[,3] <- substr(as.character(ddd),start=12,stop=19)
            midint_tab[,4] <- difftime(ddd,cccc,units = "secs")
            midint_tab[,5] <- difftime(ddd,cccc,units = "mins")
            midint_tab[,6] <- midint_tab[,4]/15
          }
          miscDoms <- rbind(miscDoms,befstart_tab,afend_tab,midint_tab)
        }
        miscDoms <- cbind(miscDoms,rep.int(ind,times = nrow(miscDoms)))
        colnames(miscDoms) <- c("Date","start","finish","seconds","minutes","segments","miscDomain")
      }
      else
      {
        sts <- c(sts,as.POSIXlt(as.character(DomainCSV[i,dom-1]), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S"))
        ed <- c(ed,as.POSIXlt(as.character(DomainCSV[i,dom]), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S"))
        temp_tab <- subtable[which(tims>=sts[ind] & enti < ed[ind]),]
        temp_tab <- cbind(temp_tab,rep.int(ind,times = nrow(temp_tab)))
        colnames(temp_tab) <- c("Date","start","finish","seconds","minutes","segments","miscDomain")
        if(is.null(temp_tab))
        {
          befstart_tab <- subtable[which(tims < sts[ind] & enti > sts[ind] & enti < ed[ind]),]
          afend_tab <- subtable[which(tims < ed[ind] & tims >= sts[ind] & enti >= ed[ind]),]
          midint_tab <- subtable[which(tims < sts[ind] & enti > ed[ind]),]
          if(nrow(befstart_tab)>0)
          {
            cccc <- numeric(length = nrow(befstart_tab))
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            ddd <- numeric(length = nrow(befstart_tab))
            for(ij in 1:nrow(befstart_tab))
            {
              dd <- paste(d,befstart_tab[ij,3],sep = " ")
              cc <- as.POSIXct(sts[ind], tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              ddd[ij] <- as.POSIXct(dd,,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            ddd <- as.POSIXlt(ddd, tz=getOption("tz"),origin="1970-01-01")
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            befstart_tab[,2] <- substr(as.character(cccc),start=12,stop=19)
            befstart_tab[,3] <- substr(as.character(ddd),start=12,stop=19)
            befstart_tab[,4] <- difftime(ddd,cccc,units = "secs")
            befstart_tab[,5] <- difftime(ddd,cccc,units = "mins")
            befstart_tab[,6] <- befstart_tab[,4]/15
          }
          if(nrow(afend_tab)>0)
          {
            cccc <- numeric(length = nrow(afend_tab))
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            for(ij in 1:nrow(afend_tab))
            {
              cc <- as.POSIXct(ed[ind], tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              
            }
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            afend_tab[,3] <- substr(as.character(cccc),start=12,stop=19)
            afend_tab[,4] <- difftime(cccc,afend_tab[,1],units = "secs")
            afend_tab[,5] <- difftime(cccc,afend_tab[,1],units = "mins")
            afend_tab[,6] <- afend_tab[,4]/15
          }
          if(nrow(midint_tab)>0)
          {
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            cccc <- numeric(length = nrow(befstart_tab))
            ddd <- numeric(length = nrow(befstart_tab))
            for(ij in 1:nrow(befstart_tab))
            {
              dd <- as.POSIXct(ed[ind], tz=getOption("tz"))
              cc <- as.POSIXct(sts[ind], tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              ddd[ij] <- as.POSIXct(dd,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            ddd <- as.POSIXlt(ddd, tz=getOption("tz"),origin="1970-01-01")
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            midint_tab[,2] <- substr(as.character(cccc),start=12,stop=19)
            midint_tab[,3] <- substr(as.character(ddd),start=12,stop=19)
            midint_tab[,4] <- difftime(ddd,cccc,units = "secs")
            midint_tab[,5] <- difftime(ddd,cccc,units = "mins")
            midint_tab[,6] <- midint_tab[,4]/15
          }
          temp_tab <- rbind(befstart_tab,afend_tab,midint_tab)
        }
        else
        {
          befstart_tab <- subtable[which(tims < sts[ind] & enti > sts[ind] & enti < ed[ind]),]
          afend_tab <- subtable[which(tims < ed[ind] & tims >= sts[ind] & enti >= ed[ind]),]
          midint_tab <- subtable[which(tims < sts[ind] & enti > ed[ind]),]
          if(nrow(befstart_tab)>0)
          {
            cccc <- numeric(length = nrow(befstart_tab))
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            ddd <- numeric(length = nrow(befstart_tab))
            for(ij in 1:nrow(befstart_tab))
            {
              dd <- paste(d,befstart_tab[ij,3],sep = " ")
              cc <- as.POSIXct(sts[ind], tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              ddd[ij] <- as.POSIXct(dd,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            ddd <- as.POSIXlt(ddd, tz=getOption("tz"),origin="1970-01-01")
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            befstart_tab[,2] <- substr(as.character(cccc),start=12,stop=19)
            befstart_tab[,3] <- substr(as.character(ddd),start=12,stop=19)
            befstart_tab[,4] <- difftime(ddd,cccc,units = "secs")
            befstart_tab[,5] <- difftime(ddd,cccc,units = "mins")
            befstart_tab[,6] <- befstart_tab[,4]/15
          }
          if(nrow(afend_tab)>0)
          {
            cccc <- numeric(length = nrow(afend_tab))
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            for(ij in 1:nrow(afend_tab))
            {
              cc <- as.POSIXct(ed[ind], tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            afend_tab[,3] <- substr(as.character(cccc),start=12,stop=19)
            afend_tab[,4] <- difftime(cccc,afend_tab[,1],units = "secs")
            afend_tab[,5] <- difftime(cccc,afend_tab[,1],units = "mins")
            afend_tab[,6] <- afend_tab[,4]/15
          }
          if(nrow(midint_tab)>0)
          {
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            cccc <- numeric(length = nrow(befstart_tab))
            ddd <- numeric(length = nrow(befstart_tab))
            for(ij in 1:nrow(befstart_tab))
            {
              dd <- as.POSIXct(ed[ind], tz=getOption("tz"))
              cc <- as.POSIXct(sts[ind], tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              ddd[ij] <- as.POSIXct(dd,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            ddd <- as.POSIXlt(ddd, tz=getOption("tz"),origin="1970-01-01")
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            midint_tab[,2] <- substr(as.character(cccc),start=12,stop=19)
            midint_tab[,3] <- substr(as.character(ddd),start=12,stop=19)
            midint_tab[,4] <- difftime(ddd,cccc,units = "secs")
            midint_tab[,5] <- difftime(ddd,cccc,units = "mins")
            midint_tab[,6] <- midint_tab[,4]/15
          }
          outerIntTab <- rbind(befstart_tab,afend_tab,midint_tab)
          outerIntTab <- cbind(outerIntTab,rep.int(ind,times = nrow(outerIntTab)))
          colnames(outerIntTab) <- c("Date","start","finish","seconds","minutes","segments","miscDomain")
          if(nrow(temp_tab)==0)
          {
            temp_tab <- outerIntTab
          }
          else
          {
            temp_tab <- rbind(temp_tab,outerIntTab)
          }
          colnames(temp_tab) <- c("Date","start","finish","seconds","minutes","segments","miscDomain")
        }
        miscDoms <- temp_tab
      }
      
      
      if(ind == length(nDomains))
      {
        sts <- as.POSIXlt(as.character(DomainCSV[i,dom+1]), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
        ed <- as.POSIXlt(paste(d,"23:59:59",sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
        sts <- as.POSIXlt(sts, tz=getOption("tz"))
        if(is.na(sts))
        {
          sts <- as.POSIXlt(paste(d,"00:00:00",sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
        }
        temp_tab <- subtable[which(tims>=sts & enti < ed),]
        temp_tab <- cbind(temp_tab,rep.int(ind,times = nrow(temp_tab)))
        colnames(temp_tab) <- c("Date","start","finish","seconds","minutes","segments","miscDomain")
        if(is.null(temp_tab))
        {
          befstart_tab <- subtable[which(tims < sts & enti > sts & enti < ed),]
          afend_tab <- subtable[which(tims < ed & tims >= sts & enti >= ed),]
          midint_tab <- subtable[which(tims < sts & enti > ed),]
          if(nrow(befstart_tab)>0)
          {
            cccc <- numeric(length = nrow(befstart_tab))
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            ddd <- numeric(length = nrow(befstart_tab))
            for(ij in 1:nrow(befstart_tab))
            {
              dd <- paste(d,befstart_tab[ij,3],sep = " ")
              cc <- as.POSIXct(sts, tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              ddd[ij] <- as.POSIXct(dd,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            ddd <- as.POSIXlt(ddd, tz=getOption("tz"),origin="1970-01-01")
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            befstart_tab[,2] <- substr(as.character(cccc),start=12,stop=19)
            befstart_tab[,3] <- substr(as.character(ddd),start=12,stop=19)
            befstart_tab[,4] <- difftime(ddd,cccc,units = "secs")
            befstart_tab[,5] <- difftime(ddd,cccc,units = "mins")
            befstart_tab[,6] <- befstart_tab[,4]/15
          }
          if(nrow(afend_tab)>0)
          {
            cccc <- numeric(length = nrow(afend_tab))
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            for(ij in 1:nrow(afend_tab))
            {
              cc <- as.POSIXct(ed, tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              
            }
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            afend_tab[,3] <- substr(as.character(cccc),start=12,stop=19)
            afend_tab[,4] <- difftime(cccc,afend_tab[,1],units = "secs")
            afend_tab[,5] <- difftime(cccc,afend_tab[,1],units = "mins")
            afend_tab[,6] <- afend_tab[,4]/15
          }
          if(nrow(midint_tab)>0)
          {
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            cccc <- numeric(length = nrow(befstart_tab))
            ddd <- numeric(length = nrow(befstart_tab))
            for(ij in 1:nrow(befstart_tab))
            {
              dd <- as.POSIXct(ed, tz=getOption("tz"))
              cc <- as.POSIXct(sts, tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              ddd[ij] <- as.POSIXct(dd,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            ddd <- as.POSIXlt(ddd, tz=getOption("tz"),origin="1970-01-01")
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            midint_tab[,2] <- substr(as.character(cccc),start=12,stop=19)
            midint_tab[,3] <- substr(as.character(ddd),start=12,stop=19)
            midint_tab[,4] <- difftime(ddd,cccc,units = "secs")
            midint_tab[,5] <- difftime(ddd,cccc,units = "mins")
            midint_tab[,6] <- midint_tab[,4]/15
          }
          temp_tab <- rbind(befstart_tab,afend_tab,midint_tab)
        }
        else
        {
          befstart_tab <- subtable[which(tims < sts[ind] & enti > sts[ind] & enti < ed[ind]),]
          afend_tab <- subtable[which(tims < ed[ind] & tims >= sts[ind] & enti >= ed[ind]),]
          midint_tab <- subtable[which(tims < sts[ind] & enti > ed[ind]),]
          if(nrow(befstart_tab)>0)
          {
            cccc <- numeric(length = nrow(befstart_tab))
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            ddd <- numeric(length = nrow(befstart_tab))
            for(ij in 1:nrow(befstart_tab))
            {
              dd <- paste(d,befstart_tab[ij,3],sep = " ")
              cc <- as.POSIXct(sts, tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              ddd[ij] <- as.POSIXct(dd,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            ddd <- as.POSIXlt(ddd, tz=getOption("tz"),origin="1970-01-01")
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            befstart_tab[,2] <- substr(as.character(cccc),start=12,stop=19)
            befstart_tab[,3] <- substr(as.character(ddd),start=12,stop=19)
            befstart_tab[,4] <- difftime(ddd,cccc,units = "secs")
            befstart_tab[,5] <- difftime(ddd,cccc,units = "mins")
            befstart_tab[,6] <- befstart_tab[,4]/15
          }
          if(nrow(afend_tab)>0)
          {
            cccc <- numeric(length = nrow(afend_tab))
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            for(ij in 1:nrow(afend_tab))
            {
              cc <- as.POSIXct(ed, tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            afend_tab[,3] <- substr(as.character(cccc),start=12,stop=19)
            afend_tab[,4] <- difftime(cccc,afend_tab[,1],units = "secs")
            afend_tab[,5] <- difftime(cccc,afend_tab[,1],units = "mins")
            afend_tab[,6] <- afend_tab[,4]/15
          }
          if(nrow(midint_tab)>0)
          {
            d <- as.Date(DomainCSV[i,dom], "%y-%m-%d", tz=getOption("tz"))
            cccc <- numeric(length = nrow(befstart_tab))
            ddd <- numeric(length = nrow(befstart_tab))
            for(ij in 1:nrow(befstart_tab))
            {
              dd <- as.POSIXct(ed, tz=getOption("tz"))
              cc <- as.POSIXct(sts, tz=getOption("tz"))
              cccc[ij] <- as.POSIXct(cc,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
              ddd[ij] <- as.POSIXct(dd,format="%Y-%m-%d %H:%M:%S", origin="1970-01-01", tz=getOption("tz"))
            }
            ddd <- as.POSIXlt(ddd, tz=getOption("tz"),origin="1970-01-01")
            cccc <- as.POSIXlt(cccc, tz=getOption("tz"),origin="1970-01-01")
            midint_tab[,2] <- substr(as.character(cccc),start=12,stop=19)
            midint_tab[,3] <- substr(as.character(ddd),start=12,stop=19)
            midint_tab[,4] <- difftime(ddd,cccc,units = "secs")
            midint_tab[,5] <- difftime(ddd,cccc,units = "mins")
            midint_tab[,6] <- midint_tab[,4]/15
          }
          outerIntTab <- rbind(befstart_tab,afend_tab,midint_tab)
          outerIntTab <- cbind(outerIntTab,rep.int(ind,times = nrow(outerIntTab)))
          colnames(outerIntTab) <- c("Date","start","finish","seconds","minutes","segments","miscDomain")
          if(nrow(temp_tab)==0)
          {
            temp_tab <- outerIntTab
          }
          else
          {
            temp_tab <- rbind(temp_tab,outerIntTab)
          }
          colnames(temp_tab) <- c("Date","start","finish","seconds","minutes","segments","miscDomain")
        }
        miscDoms <- rbind(miscDoms,temp_tab)
      }
      
      
      ind <- ind+1
    }
    if(i == 1)
    {
      retuTab <- miscDoms
    }
    else
    {
      retuTab <- rbind(retuTab,miscDoms)
    }
  }
  return(retuTab)
}

################ SED Analysis #############################

Find_The_Next_Sed_End <- function(Sed_addresses_start,Sed_addresses_end,Data)
  # Get Ends of the SED-bouts, because Start and End could be in the same epoch
{
  Sed_addresses_end_out <- numeric(length = length(Sed_addresses_start))
  for(ii in 1:length(Sed_addresses_start))
  {
    Sed_addresses_end_after = which(Sed_addresses_end == Sed_addresses_start[ii])
    if(length(Sed_addresses_end_after)==0) #isempty means that the end of sedentary starts after the current cell and not at the same address
    {
      Sed_addresses_end_after = Sed_addresses_end[min(which(Sed_addresses_end > Sed_addresses_start[ii]))] ## edited by Jan B.
    }
    else # The end of sedentary is at the same address, but we need to find out which one it belongs to.
    {
      Sed_addresses_start_addrs =  which(Sed_addresses_start > Sed_addresses_start[ii])
      Sed_addresses_end_addrs =  which(Sed_addresses_end > Sed_addresses_start[ii])
      if(length(Sed_addresses_start_addrs)==0 || length(Sed_addresses_end_addrs)==0)
      {
        Sed_addresses_end_after = Sed_addresses_end[min(which(Sed_addresses_end >= Sed_addresses_start[ii]))]
      }
      else
      {
        if(Sed_addresses_end[Sed_addresses_end_addrs[1]] < Sed_addresses_start[Sed_addresses_start_addrs[1]])
        {
          # if the address of the next ending is less than the next start then the previous start address must 
          #belong to the ending > not >=  
          Sed_addresses_end_after = Sed_addresses_end[Sed_addresses_end_addrs[1]]
        }
        else
        {
          # if the next start is less than the next ending then the
          # current ending and start are in the same address.
          Sed_addresses_end_after = Sed_addresses_end[Sed_addresses_end_after[1]]
        }
      }
    }
    Sed_addresses_end_out[ii] = Sed_addresses_end_after[1]
  }
  return(Sed_addresses_end_out)
}

Find_The_Next_Sed_End_V2 <- function(Sed_addresses_start,Sed_addresses_end,Data)
  # Get Ends of the SED-bouts, because Start and End could be in the same epoch, untested unused
{
  library(foreach)
  Sed_addresses_end_out <- foreach(ii = 1:length(Sed_addresses_start), .combine = 'c') %do%
  {
    Sed_addresses_end_after = which(Sed_addresses_end == Sed_addresses_start[ii])
    if(length(Sed_addresses_end_after)==0) #isempty means that the end of sedentary starts after the current cell and not at the same address
    {
      Sed_addresses_end_after = Sed_addresses_end[min(which(Sed_addresses_end > Sed_addresses_start[ii]))] ## edited by Jan B.
    }
    else # The end of sedentary is at the same address, but we need to find out which one it belongs to.
    {
      Sed_addresses_start_addrs =  which(Sed_addresses_start > Sed_addresses_start[ii])
      Sed_addresses_end_addrs =  which(Sed_addresses_end > Sed_addresses_start[ii])
      if(length(Sed_addresses_start_addrs)==0 || length(Sed_addresses_end_addrs)==0)
      {
        Sed_addresses_end_after = Sed_addresses_end[min(which(Sed_addresses_end >= Sed_addresses_start[ii]))]
      }
      else
      {
        if(Sed_addresses_end[Sed_addresses_end_addrs[1]] < Sed_addresses_start[Sed_addresses_start_addrs[1]])
        {
          # if the address of the next ending is less than the next start then the previous start address must 
          #belong to the ending > not >=  
          Sed_addresses_end_after = Sed_addresses_end[Sed_addresses_end_addrs[1]]
        }
        else
        {
          # if the next start is less than the next ending then the
          # current ending and start are in the same address.
          Sed_addresses_end_after = Sed_addresses_end[Sed_addresses_end_after[1]]
        }
      }
    }
    Sed_addresses_end_after[1]
  }
  return(Sed_addresses_end_out)
}

#detection SED by transitions
Sed_detect_transition <- function(ActivPaltab,PrintMode=0)
  # Detect Transitions between sitting/lying and upright
{
  #EndOfSed = which(ActivPaltab$Sed_to_Up == 1)
  #StartOfSed = which(ActivPaltab$Up_to_Sed == 1)
  EndOfSed = NULL
  StartOfSed = NULL
  
  EndOfSed_over = which(ActivPaltab$Sed_to_Up > 0)
  StartOfSed_over = which(ActivPaltab$Up_to_Sed > 0)
  
  # Since more than one sedentary period may occur in one second additional
  # numbers of Sedentary bouts were added in the columns 7 and 8
  if(PrintMode>2)
  {
    print(paste("01 Sed_detect_transition - time:",Sys.time(),sep="  "))
  }
  if(length(EndOfSed_over)>0 & length(EndOfSed_over)>0)
  {
    StartOfSed = 0;
    
    for(ij in 1:length(StartOfSed_over))
    {
      for(kk in 1:ActivPaltab[StartOfSed_over[ij],10])
      {
        StartOfSed = c(StartOfSed, StartOfSed_over[ij])
      }
    }
    StartOfSed <-  StartOfSed[2:length(StartOfSed)]
    EndOfSed = 0;
    for(ij in 1:length(EndOfSed_over))
    {
      for(kk in 1:ActivPaltab[EndOfSed_over[ij],10])
      {
        EndOfSed = c(EndOfSed, EndOfSed_over[ij])
      }
    }
    EndOfSed <-  EndOfSed[2:length(EndOfSed)]
    
    if(EndOfSed[1]<StartOfSed[1]) # The end of sedentary was  person was sedentary and went to upright, thus the start of the file is all sedentary
    {
      StartOfSed = c(1 , StartOfSed)
    }
    if(StartOfSed[length(StartOfSed)]>=EndOfSed[length(EndOfSed)]) # The last thing the person did was go from sedentary to upright, thus the end of the file is all dynamic
    {
      EndOfSed = c(EndOfSed, nrow(ActivPaltab))
    }
  }
  
  else
  {
    #  Take care of the situation where there is no change
    if(length(EndOfSed_over)==0 & length(EndOfSed_over)==0)
    {
      if(mean(ActivPaltab$Sedentary)==15)
      {
        StartOfSed = 1;
        EndOfSed = nrow(ActivPaltab);
      }
      else
      {
        EndOfSed = 1;
        StartOfSed = nrow(ActivPaltab);
      }
    }
    if(length(EndOfSed_over)==0 & length(EndOfSed_over)>0)
    {
      EndOfSed = nrow(ActivPaltab)
      if(mean(ActivPaltab$Sedentary)!=15)
      {
        stop('Incorrect selection of the EndOfSed variable')
      }
    }
    if(length(EndOfSed_over)>0 & length(EndOfSed_over)==0)
    {
      StartOfSed = 1;
      if(mean(ActivPaltab$Sedentary)!=15)
      {
        stop('Incorrect selection of the StartOfSed variable')
      }
    }
  }
  if(PrintMode>2)
  {
    print(paste("02 Sed_detect_transition - time:",Sys.time(),sep="  "))
  }
  Sed_addresses_start = StartOfSed;
  Sed_addresses_end = EndOfSed;
  
  # The situation does occur where you have a bout of activity starting
  # during a recording period with dynamic activity.
  # this needs to be taken care of.
  
  Sed_addresses_ending <- Find_The_Next_Sed_End(Sed_addresses_start,Sed_addresses_end,ActivPaltab)
  rm("Sed_addresses_end")
  Sed_addresses_end = Sed_addresses_ending
  
  ## Error checkig
  if(length(Sed_addresses_start) != length(Sed_addresses_end))
    stop('The length of the Sed_addresses_start and the Sed_addresses_end are not equal.')
  else
  {
    SedSegLs = (Sed_addresses_end - Sed_addresses_start)+1
    SedSegLsWithFull15s = (SedSegLs - 2)*15
    for(jj in 1:length(Sed_addresses_start))
    {
      temp_vec <-ActivPaltab$Sedentary
      SedSum = sum(temp_vec[seq(from=Sed_addresses_start[jj],to=Sed_addresses_end[jj])])
      
      if(SedSegLsWithFull15s[jj]>SedSum)
      {
        stop(paste("There is less than expected sedentary time between two addresses. Recheck the static/dynamic detection algorithm. Address: ",jj,sep=""))
      }
    }
  }
  if(PrintMode>2)
  {
    print(paste("03 Sed_detect_transition - time:",Sys.time(),sep="  "))
  }
  Sed_addresses_tab <- cbind(Sed_addresses_start,Sed_addresses_end)
  return(Sed_addresses_tab)
}

create_SED_ACT_ALL <- function(DATA, SED_adresses)
  # create a table with all sedentary-bouts and their duration
{
  Date <- DATA[SED_adresses[,1],2]
  Enddate <- DATA[SED_adresses[,2],2]
  segments <- SED_adresses[,2] - SED_adresses[,1]
  
  temp_vec <-DATA$Sedentary
  SedLength_segments <- numeric(length=nrow(SED_adresses))
  for(ii in 1:nrow(SED_adresses))
  {
    SedLength_segments[ii] <- sum(temp_vec[seq(from=SED_adresses[ii,1],to=SED_adresses[ii,2])])
  }
  seconds <- SedLength_segments
  minutes <- SedLength_segments/60
  
  start <- strftime(Date, format="%H:%M:%S", tz=getOption("tz"))
  finish <- strftime(Enddate, format="%H:%M:%S", tz=getOption("tz"))
  SAA <- cbind.data.frame(Date,start,finish,seconds,minutes,segments)
  return(SAA)
}

check_maxSed_Threshold <- function(DATA, Threshold)
  # Checks all SED-bouts if length < Threshold and deltes otherwise, unused and obsolete because of NonWear-files
{
  x <- which(DATA$seconds < Threshold)
  n_DATA <- DATA[x,]
  return(n_DATA)
}


FN_CPAHR_SED_ACT_ALL <- function(input,output,Threshold_static=Inf,is.csvout=T, ID.nr=NULL, separator=",", dec=".",save_as_xlsx=FALSE,PrintMode=0)
  #create SED-Bout-Table and when is.csvout=True write SED_ACT_ALL.csv
{
  Sys.time()
  tab <- read_activPAL(inputpath = input)
  Sys.time()
  Sed_addresses_tab <- Sed_detect_transition(tab,PrintMode=PrintMode)
  Sys.time()
  SED_ACT_ALL<-create_SED_ACT_ALL(tab, Sed_addresses_tab)
  SED_ACT_ALL <- check_maxSed_Threshold(SED_ACT_ALL,Threshold_static)
  
  if(is.csvout)
  {
    if(is.null(ID.nr))
    {
      name <- get_filename(input)
    }
    else
    {
      name <- ID.nr
    }
    
    if(save_as_xlsx)
    {
      outputfile <- paste(output,name,"_SED_ACT_ALL.xlsx",sep = "")
      write.xlsx(SED_ACT_ALL, file=outputfile, sheetName=paste(name,"_SED_ACT_ALL",sep = ""),  col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
    }
    else
    {
      outputfile <- paste(output,name,"_SED_ACT_ALL.csv",sep = "")
      write.table(SED_ACT_ALL, file=outputfile, row.names = FALSE, sep=separator, dec=dec)
    }
    warnings()
  }
  return(SED_ACT_ALL)
}

check_SegmentThreshold <- function(SED_adresses, Threshold_static, epochlength=15)
  # Checks all SED-bouts if number of segments < Threshold and deltes otherwise, unused and obsolete because of NonWear-files
{
  Threshold <- Threshold_static/epochlength #get segment-threshold from seconds-threshold by seconds/segmentlength
  segments <- SED_adresses[,2] - SED_adresses[,1]#get number of segments per bout
  keep_segments <- which(segments < Threshold)
  n_tab <- SED_adresses[keep_segments,]
  return(n_tab)
}

######### Bout Analysis ################

Sed_Bout_Length <- function(input, partedDomainsLS=NULL, miscTab=NULL, DailyDomains =5, DayCut="00:00:00") 
  # Create Table with length categorized infromationen of sedentary-bouts
  # Tages?bergang ?berarbeiten
{ # Like the Excel Makros bur for all days at once
  dats <- as.Date(input[,1], "%Y-%m-%d",tz=getOption("tz"))
  x <- numeric(length = length(dats))
  x[1] <- 1
  for(i in 2 : length(dats))
  {
    x[i] <- dats[i]-dats[i-1]
  }
  daycuts <- which(x != 0)
  daycuts <- c(daycuts,nrow(input))
  days <- as.character(dats[daycuts])
  
  # ### Hier alternative daycuts setzen jenach der Uhrzeit welche als Daycut dient
  # unq.days <- unique(dats)
  # print(input[1:5,])
  # temp.input <- input
  # for(i in 1:nrow(temp.input))## Auftrennen wenn Bout in 2 Tagen liegt
  # {
  #   Begin <- as.POSIXlt(temp.input[i,1],format="%Y-%m-%d %H:%M:%S", tz="UTC")
  #   End <- as.POSIXlt(paste(as.character(as.Date(temp.input[i,1], "%Y-%m-%d")),temp.input[i,3],sep=" "),format="%Y-%m-%d %H:%M:%S", tz="UTC")
  #   Cut <- as.POSIXlt(as.character(paste(as.character(as.Date(temp.input[i,1], "%Y-%m-%d")),DayCut,sep=" ")),format="%Y-%m-%d %H:%M:%S", tz="UTC")
  #   if(difftime(End,Begin,units = "secs") < 0)
  #   {
  #     End <- End + 86400
  #     print(End)
  #   }
  #   #print(Cut)
  #   
  #   if(difftime(Cut,Begin,units = "secs")>0 && difftime(End,Cut,units = "secs")>0)
  #   {
  #     temp.input <- rbind(temp.input[1:i,],temp.input[i,],temp.input[(i+1):nrow(temp.input),])
  #     temp.input[i,2] <- strptime(temp.input[i,2],format="%H:%M:%S",tz="UTC")
  #     temp.input[i,3] <- strptime(Cut,format="%H:%M:%S",tz="UTC")
  #     temp.input[i,4] <- difftime(Cut,Begin,unit="secs")
  #     temp.input[i,5] <- difftime(Cut,Begin,unit="mins")
  #     temp.input[i,5] <- NA
  # 
  #     temp.input[(i+1),2] <- strptime(Cut,format="%H:%M:%S",tz="UTC")
  #     print(strptime(Cut,format="%H:%M:%S"))
  #     temp.input[(i+1),4] <- difftime(End,Cut,unit="secs")
  #     temp.input[(i+1),5] <- difftime(End,Cut,unit="mins")
  #     temp.input[(i+1),5] <- NA
  #     #print(temp.input[i:(i+1),3])
  #   }
  # 
  # }
  # for(i in 1:length(unq.days))
  # {
  #   DateTimes <- as.POSIXlt(temp.input[,1],format="%Y-%m-%d %H:%M:%S", tz="UTC")
  #   Cut <- as.POSIXlt(as.character(paste(as.character(unq.days[i]),DayCut,sep=" ")),format="%Y-%m-%d %H:%M:%S", tz="UTC")
  #   Day.start <- which.min(as.numeric(difftime(Cut,DateTimes,units = "secs"))>0)
  #   #print(Day.start)
  #   #print(temp.input[Day.start,])
  # }
  
  ###
  
  daycuts[length(daycuts)] <- daycuts[length(daycuts)]+1
  lst_Bouts <- FN_Bout_Freqs(input, daycuts,category=days,cat.name="Day")
  if(!is.null(partedDomainsLS))
  {
    dom_ls <- vector("list", length = 2)
    temp_dom1 <- NULL
    temp_dom2 <- NULL
    date_vec <- NULL
    for(i in 1:length(partedDomainsLS))
    {
      if(is.null(partedDomainsLS[[i]]))
      {
        day <- ((i-1) %/% DailyDomains)+1
        date <- as.Date(lst_Bouts[[1]][day,1],tz=getOption("tz"))
        SEDB1 <- rep.int(0,times = 12)
        SEDB2 <- rep.int(0,times = 12)
      }
      else
      {
        temp_tab <- as.data.frame(partedDomainsLS[[i]],tz=getOption("tz"))
        Doms <- temp_tab$Domain
        x <- numeric(length = length(Doms))
        if(length(Doms)<2)
        {
          Doms <- c(Doms,Doms)
        }
        for(d in 2:length(Doms))
        {
          x[d] <- Doms[d]-Doms[d-1]
        }
        domains <- which(x > 0)
        domains <- c(1,domains, length(Doms))
        dms <- Doms[domains]
        domains[length(domains)] <- domains[length(domains)]+1
        SEDB_ls <- FN_Bout_Freqs(temp_tab, domains,dms,cat.name="Domain")
        dates <- as.Date(temp_tab[,1], "%y-%m-%d",tz=getOption("tz"))
        dt <- dates[domains]
        date <- dt[-length(dt)]
        #SEDB_ls[[1]] <- cbind.data.frame(date,as.data.frame(SEDB_ls[[1]]))
        #SEDB_ls[[2]] <- cbind.data.frame(date,as.data.frame(SEDB_ls[[2]]))
        SEDB1 <- as.data.frame(SEDB_ls[[1]])
        SEDB2 <- as.data.frame(SEDB_ls[[2]])
        
      }
      if(i == 1)
      {
        temp_dom1 <- SEDB1
        temp_dom2 <- SEDB2
        date_vec <- date
      }
      else
      {
        if(is.null(colnames(temp_dom1)) && !is.vector(temp_dom1))
        {
          colnames(temp_dom1) <- colnames(SEDB1)
          colnames(temp_dom2) <- colnames(SEDB2)
        }
        temp_dom1 <- rbind(temp_dom1,SEDB1)
        temp_dom2 <- rbind(temp_dom2,SEDB2)
        date_vec <- c(date_vec,date)
      }
    }
    unqDomsday <- unique(date_vec)
    unqDaysGes <- unique(as.Date(lst_Bouts[[1]][,1],tz=getOption("tz")))
    for(i in 1:length(unqDaysGes))
    {
      if(length(which(unqDomsday==unqDaysGes[i]))<1)
      {
        if(difftime(unqDaysGes[i],unqDomsday[1],units = "days")>0)
        {
          date_vec <- c(date_vec,unqDaysGes[i])
          temp_dom1 <- rbind.data.frame(temp_dom1,rep.int(0,times = 12))
          temp_dom2 <- rbind.data.frame(temp_dom2,rep.int(0,times = 12))
        }
        else
        {
          date_vec <- c(unqDaysGes[i],date_vec)
          temp_dom1 <- rbind.data.frame(rep.int(0,times = 12),temp_dom1)
          temp_dom2 <- rbind.data.frame(rep.int(0,times = 12),temp_dom2)
        }
      }
    }
    dom_ls[[1]] <- cbind.data.frame(date_vec,temp_dom1)
    dom_ls[[2]] <- cbind.data.frame(date_vec,temp_dom2)
    Rest <- vector("list",length = 2)
    LSdom <- vector("list", length = 2)
    for(day in 1:(length(days)-1))
    {
      Freq <- as.data.frame(dom_ls[[1]])
      Dom_date <- Freq[,1]
      selection <- which(Dom_date==days[day])
      Freq <- Freq[selection,]
      sumsDomains1 <- c(sum(Freq[,3]),sum(Freq[,4]),sum(Freq[,5]),sum(Freq[,6]),sum(Freq[,7]),
                        sum(Freq[,8]),sum(Freq[,9]),sum(Freq[,10]),sum(Freq[,11]),sum(Freq[,12]),sum(Freq[,13]))
      Freq <- as.data.frame(dom_ls[[2]])
      Freq <- Freq[selection,]
      sumsDomains2 <- c(sum(Freq[,3]),sum(Freq[,4]),sum(Freq[,5]),sum(Freq[,6]),sum(Freq[,7]),
                        sum(Freq[,8]),sum(Freq[,9]),sum(Freq[,10]),sum(Freq[,11]),sum(Freq[,12]),sum(Freq[,13]))
      if(day == 1)
      {
        LSdom[[1]] <- c(sumsDomains1)
        LSdom[[2]] <- c(sumsDomains2)
      }
      else
      {
        LSdom[[1]] <- rbind(LSdom[[1]],c(sumsDomains1))
        LSdom[[2]] <- rbind(LSdom[[2]],c(sumsDomains2))
      }
      ls.bout1 <- as.data.frame(lst_Bouts[[1]])[day,]
      ls.bout1 <- unlist(ls.bout1,use.names = T)
      lsl1 <- as.numeric(as.character(ls.bout1[2:length(ls.bout1)]))
      ls.bout2 <- as.data.frame(lst_Bouts[[2]])[day,]
      ls.bout2 <- unlist(ls.bout2,use.names = T)
      lsl2 <- as.numeric(as.character(ls.bout2[2:length(ls.bout2)]))
      if(day==1)
      {
        Rest[[1]] <- c(lsl1[1]-sumsDomains1[1],lsl1[2]-sumsDomains1[2],lsl1[3]-sumsDomains1[3],lsl1[4]-sumsDomains1[4]
                       ,lsl1[5]-sumsDomains1[5],lsl1[6]-sumsDomains1[6],lsl1[7]-sumsDomains1[7],lsl1[8]-sumsDomains1[8]
                       ,lsl1[9]-sumsDomains1[9],lsl1[10]-sumsDomains1[10],lsl1[11]-sumsDomains1[11])
        Rest[[2]] <- c(lsl2[1]-sumsDomains2[1],lsl2[2]-sumsDomains2[2],lsl2[3]-sumsDomains2[3],lsl2[4]-sumsDomains2[4]
                       ,lsl2[5]-sumsDomains2[5],lsl2[6]-sumsDomains2[6],lsl2[7]-sumsDomains2[7],lsl2[8]-sumsDomains2[8]
                       ,lsl2[9]-sumsDomains2[9],lsl2[10]-sumsDomains2[10],lsl2[11]-sumsDomains2[11])
      }
      else
      {
        if(day==2)
        {
          temp_rest1 <- c(lsl1[1]-sumsDomains1[1],lsl1[2]-sumsDomains1[2],lsl1[3]-sumsDomains1[3],lsl1[4]-sumsDomains1[4]
                          ,lsl1[5]-sumsDomains1[5],lsl1[6]-sumsDomains1[6],lsl1[7]-sumsDomains1[7],lsl1[8]-sumsDomains1[8]
                          ,lsl1[9]-sumsDomains1[9],lsl1[10]-sumsDomains1[10],lsl1[11]-sumsDomains1[11])
          temp_rest2 <- c(lsl2[1]-sumsDomains2[1],lsl2[2]-sumsDomains2[2],lsl2[3]-sumsDomains2[3],lsl2[4]-sumsDomains2[4]
                          ,lsl2[5]-sumsDomains2[5],lsl2[6]-sumsDomains2[6],lsl2[7]-sumsDomains2[7],lsl2[8]-sumsDomains2[8]
                          ,lsl2[9]-sumsDomains2[9],lsl2[10]-sumsDomains2[10],lsl2[11]-sumsDomains2[11])
          Rest[[1]] <- rbind(as.vector(Rest[[1]]),temp_rest1)
          Rest[[2]] <- rbind(as.vector(Rest[[2]]),temp_rest2)
        }
        else
        {
          temp_rest1 <- c(lsl1[1]-sumsDomains1[1],lsl1[2]-sumsDomains1[2],lsl1[3]-sumsDomains1[3],lsl1[4]-sumsDomains1[4]
                          ,lsl1[5]-sumsDomains1[5],lsl1[6]-sumsDomains1[6],lsl1[7]-sumsDomains1[7],lsl1[8]-sumsDomains1[8]
                          ,lsl1[9]-sumsDomains1[9],lsl1[10]-sumsDomains1[10],lsl1[11]-sumsDomains1[11])
          temp_rest2 <- c(lsl2[1]-sumsDomains2[1],lsl2[2]-sumsDomains2[2],lsl2[3]-sumsDomains2[3],lsl2[4]-sumsDomains2[4]
                          ,lsl2[5]-sumsDomains2[5],lsl2[6]-sumsDomains2[6],lsl2[7]-sumsDomains2[7],lsl2[8]-sumsDomains2[8]
                          ,lsl2[9]-sumsDomains2[9],lsl2[10]-sumsDomains2[10],lsl2[11]-sumsDomains2[11])
          Rest[[1]] <- rbind(as.data.frame(Rest[[1]]),temp_rest1)
          Rest[[2]] <- rbind(as.data.frame(Rest[[2]]),temp_rest2)
        }
      }
    }
    if(nrow(lst_Bouts[[1]]) == 1)
    {
      LSdom[[1]] <- c(lst_Bouts[[1]][,1],LSdom[[1]])
      LSdom[[2]] <- c(lst_Bouts[[1]][,1],LSdom[[2]])
      names(LSdom[[1]]) <- c("Day","SBo <5","SB 5-10","SB 10-20","SB <20",
                                "SB >20", "SB 20-30", "SB 30-40", "SB >40","SB 40-60",
                                "SB >60", "SB >90")
      names(LSdom[[2]]) <- c("Day","T SB <5","T SB 5-10","T SB 10-20","T SB <20",
                                "T SB >20", "T SB 20-30", "T SB 30-40", "T SB >40","T SB 40-60",
                                "T SB >60", "T SB >90")
    }
    else
    {
      LSdom[[1]] <- cbind(as.data.frame(lst_Bouts[[1]])[,1],as.data.frame(LSdom[[1]]))
      LSdom[[2]] <- cbind(as.data.frame(lst_Bouts[[1]])[,1],as.data.frame(LSdom[[2]]))
      colnames(LSdom[[1]]) <- c("Day","SBo <5","SB 5-10","SB 10-20","SB <20",
                                "SB >20", "SB 20-30", "SB 30-40", "SB >40","SB 40-60",
                                "SB >60", "SB >90")
      colnames(LSdom[[2]]) <- c("Day","T SB <5","T SB 5-10","T SB 10-20","T SB <20",
                                "T SB >20", "T SB 20-30", "T SB 30-40", "T SB >40","T SB 40-60",
                                "T SB >60", "T SB >90")
    }

    if(nrow(lst_Bouts[[2]]) == 1)
    {
      Rest[[1]] <- c(lst_Bouts[[2]][,1],Rest[[1]])
      Rest[[2]] <- c(lst_Bouts[[2]][,1],Rest[[2]])
      names(Rest[[1]]) <- c("Day","SBo <5","SB 5-10","SB 10-20","SB <20",
                               "SB >20", "SB 20-30", "SB 30-40", "SB >40","SB 40-60",
                               "SB >60", "SB >90")
      names(Rest[[2]]) <- c("Day","T SB <5","T SB 5-10","T SB 10-20","T SB <20",
                               "T SB >20", "T SB 20-30", "T SB 30-40", "T SB >40","T SB 40-60",
                               "T SB >60", "T SB >90")
    }
    else
    {
      Rest[[1]] <- cbind(as.data.frame(lst_Bouts[[2]])[,1],as.data.frame(Rest[[1]]))
      Rest[[2]] <- cbind(as.data.frame(lst_Bouts[[2]])[,1],as.data.frame(Rest[[2]]))
      colnames(Rest[[1]]) <- c("Day","SBo <5","SB 5-10","SB 10-20","SB <20",
                               "SB >20", "SB 20-30", "SB 30-40", "SB >40","SB 40-60",
                               "SB >60", "SB >90")
      colnames(Rest[[2]]) <- c("Day","T SB <5","T SB 5-10","T SB 10-20","T SB <20",
                               "T SB >20", "T SB 20-30", "T SB 30-40", "T SB >40","T SB 40-60",
                               "T SB >60", "T SB >90")
    }
  }
  if(!is.null(miscTab))
  {
    all.unqdat <- unique(as.Date(dom_ls[[1]][,1], "%y-%m-%d",tz=getOption("tz")))
    days <- as.Date(miscTab[,1],tz=getOption("tz"))
    unq_day <- unique(days)
    for(Nrday in 1:length(unq_day))
    {
      wh_day <- which(days==unq_day[Nrday])
      if(Nrday == 1)
      {
        dayInt <- wh_day[1]
      }
      else
      {
        dayInt <- c(dayInt,wh_day[1])
      }
    }
    dayInt <- c(dayInt , nrow(miscTab)+1)
    MiscBouts <- FN_Bout_Freqs(miscTab, dayInt,as.character(unq_day),cat.name="Day")
    for(i in 1:length(all.unqdat))
    {
      if(is.vector(MiscBouts[[1]]))
      {
        is.date <- which(MiscBouts[[1]][1] == all.unqdat[i])
        MiscB.times <- (length(MiscBouts[[1]])-1)
      }
      else
      {
        is.date <- which(MiscBouts[[1]][,1] == all.unqdat[i])
        MiscB.times <- (ncol(MiscBouts[[1]])-1)
      }
      
      if(length(is.date)>0)
      {
        day.row1 <- MiscBouts[[1]][is.date,]
        day.row2 <- MiscBouts[[2]][is.date,]
      }
      else
      {
        day.row1 <- c(as.character(all.unqdat[i]), rep.int(0,times = MiscB.times))
        day.row2 <- c(as.character(all.unqdat[i]), rep.int(0,times = MiscB.times))
      }
      if(i == 1)
      {
        misc.tab1 <- day.row1
        misc.tab2 <- day.row2
      }
      else
      {
        misc.tab1 <- rbind(misc.tab1,day.row1)
        misc.tab2 <- rbind(misc.tab2,day.row2)
      }
    }
    MiscBouts[[1]] <- misc.tab1
    MiscBouts[[2]] <- misc.tab2
    rownames(MiscBouts[[1]]) <- NULL
    rownames(MiscBouts[[2]]) <- NULL
  }
  
  if(!is.null(partedDomainsLS))
  {
    rownames(Rest[[1]]) <- NULL
    rownames(Rest[[2]]) <- NULL
    colnames(dom_ls[[1]])[1] <- "Day"
    colnames(dom_ls[[2]])[1] <- "Day"
    if(!is.null(miscTab))
    {
      SuLs <- list(as.data.frame(lst_Bouts[[1]]),as.data.frame(dom_ls[[1]]),as.data.frame(LSdom[[1]]),
                   as.data.frame(Rest[[1]]),as.data.frame(MiscBouts[[1]]))
      TiLs <- list(as.data.frame(lst_Bouts[[2]]),as.data.frame(dom_ls[[2]]),as.data.frame(LSdom[[2]]),
                   as.data.frame(Rest[[2]]),as.data.frame(MiscBouts[[2]]))
      outls <- list(SuLs,TiLs)
    }
    else
    {
      SuLs <- list(as.data.frame(lst_Bouts[[1]]),as.data.frame(dom_ls[[1]]),as.data.frame(LSdom[[1]]),
                   as.data.frame(Rest[[1]]))
      TiLs <- list(as.data.frame(lst_Bouts[[2]]),as.data.frame(dom_ls[[2]]),as.data.frame(LSdom[[2]]),
                   as.data.frame(Rest[[2]]))
      outls <- list(SuLs,TiLs)
    }
  }
  else
  {
    if(!is.null(miscTab))
    {
      SuLs <- list(as.data.frame(lst_Bouts[[1]]),as.data.frame(MiscBouts[[1]]))
      TiLs <- list(as.data.frame(lst_Bouts[[2]]),as.data.frame(MiscBouts[[2]]))
      outls <- list(SuLs,TiLs)
    }
    else
    {
      SuLs <- list(as.data.frame(lst_Bouts[[1]]))
      TiLs <- list(as.data.frame(lst_Bouts[[2]]))
      outls <- list(SuLs,TiLs)
    }
  }
  return(outls)
}

FN_Bout_Freqs <- function(input, Boutintervalls,category,cat.name="Category")
  # calculates number and duration of sedentary for sedentary-bout categories 
{                                               # R?ckgabe einer Liste mit 2 Tabellen 1 f?r H?ufigkeit 1 f?r Summe
  if(is.na(Boutintervalls[1]) || is.na(Boutintervalls[2]))
  {
    NumberBouts <- rep.int(0,times = 12)
    TimeBouts <- rep.int(0,times = 12)
  }
  else
  {
    if(length(Boutintervalls) < 2 || Boutintervalls[1] >  Boutintervalls[2])
    {
      NumberBouts <- rep.int(0,times = 12)
      TimeBouts <- rep.int(0,times = 12)
    }
    else
    {
      NumberBouts <- matrix(nrow=(length(Boutintervalls)-1) , ncol=12)
      TimeBouts <- matrix(nrow=(length(Boutintervalls)-1) , ncol=12)
      for(k in 2:length(Boutintervalls))#
      {
        temp_min<-input[Boutintervalls[k-1]:(Boutintervalls[k]-1),5]
        minLT5 <- which(temp_min < 5)
        min5be10 <- which(temp_min >= 5 & temp_min < 10)
        min10be20 <- which(temp_min >= 10 & temp_min < 20)
        minLT20 <- which(temp_min < 20)
        minGT20 <- which(temp_min > 20)
        min20be30 <- which(temp_min >= 20 & temp_min < 30)
        min30be40 <- which(temp_min >= 30 & temp_min < 40)
        minGT40 <- which(temp_min > 40)
        min40be60 <- which(temp_min >= 40 & temp_min < 60)
        minGT60 <- which(temp_min > 60)
        minGT90 <- which(temp_min > 90)
        
        NumberBouts[k-1,] <- c(category[k-1],length(minLT5),length(min5be10),length(min10be20),length(minLT20),
                               length(minGT20),length(min20be30),length(min30be40),length(minGT40),
                               length(min40be60),length(minGT60),length(minGT90))
        TimeBouts[k-1,] <- c(category[k-1],sum(temp_min[minLT5]),sum(temp_min[min5be10]),sum(temp_min[min10be20]),sum(temp_min[minLT20]),
                             sum(temp_min[minGT20]),sum(temp_min[min20be30]),sum(temp_min[min30be40]),sum(temp_min[minGT40]),
                             sum(temp_min[min40be60]),sum(temp_min[minGT60]),sum(temp_min[minGT90]))
      }
    }
  }
  
  if(ncol(as.data.frame(NumberBouts)) < 12)
  {
    NumberBouts <- rbind(NumberBouts,NumberBouts)
    colnames(NumberBouts) <- c(cat.name,"SBo <5","SB 5-10","SB 10-20","SB <20",
                               "SB >20", "SB 20-30", "SB 30-40", "SB >40","SB 40-60",
                               "SB >60", "SB >90")
    NumberBouts <- NumberBouts[1,]
    
    TimeBouts <- rbind(TimeBouts,TimeBouts)
    colnames(TimeBouts) <- c(cat.name,"SBo <5","SB 5-10","SB 10-20","SB <20",
                             "SB >20", "SB 20-30", "SB 30-40", "SB >40","SB 40-60",
                             "SB >60", "SB >90")
    TimeBouts <- TimeBouts[1,]
  }
  else
  {
    colnames(NumberBouts) <- c(cat.name,"SBo <5","SB 5-10","SB 10-20","SB <20",
                               "SB >20", "SB 20-30", "SB 30-40", "SB >40","SB 40-60",
                               "SB >60", "SB >90")
    colnames(TimeBouts) <- c(cat.name,"T SB <5","T SB 5-10","T SB 10-20","T SB <20",
                             "T SB >20", "T SB 20-30", "T SB 30-40", "T SB >40","T SB 40-60",
                             "T SB >60", "T SB >90")
  }
  return(list(NumberBouts,TimeBouts))
}


######### Activity Analysis ###################

hour_functional <- function(activPal,domainPath=NULL,Sleep_tab,NW_path=NULL, separated.csv=TRUE, JoinedDomains=NULL, JoinedNWs=NULL, id=NULL, separator=",", dec=".",
                            time.unit="mins", mvpa_steps_cutoff=25, is.xlsx=FALSE, sheetIndex=1, calcBySteps=TRUE, mvpa_count_thresh=2296, non.stop=TRUE)
  # creates Table with informations about the activity-levls for the day, domains and misc
{
  tab <- activPal
  if(!is.null(NW_path))
  {
    NW.tab <- get_NWforID(NW_path,separated.csv=separated.csv, JoinedNWs=JoinedNWs, id=id, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
    NW.seq.general <- seq(from=2,to=(ncol(NW.tab)-2),by=3)
    dte.NW <- as.character(as.Date(NW.tab[,1],tz=getOption("tz"),format="%Y-%m-%d"))
  }
  else
  {
    NW.tab <- NULL
    NW.seq.general <- NULL
  }
  dates <- as.Date(tab[,2],tz=getOption("tz"))
  unq.dates <- unique(dates)
  new.tab <- NULL
  
  domains <- getDomainsforID(domainPath, separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=id, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
  if(!is.null(domains))
  {
    domstarts <- seq(from=2, to=(ncol(domains)-2),by=3)
    unq.domdates <- unique(as.Date(domains[,1],tz=getOption("tz")))
    
    del.doms <- NULL
    for(i in 1:length(unq.domdates))# deleting Domains when Date is not in ActivPal Data
    {
      if(length(which(unq.dates==unq.domdates[i]))<1)
      {
        del.doms <- c(del.doms,which(as.Date(domains[,1],tz=getOption("tz"))==unq.domdates[i]))
      }
    }
    if(!is.null(del.doms))
    {
      domains <- domains[-del.doms,]
    }
    # Needs Work !!!!!!!!!!!!!
  }
  for(i in 1:length(unq.dates))
  {
    day.tab <- tab[which(dates == unq.dates[i]),]
    if(non.stop)
    {
      if(i > nrow(Sleep_tab))
      {
        rise <- NA
        bed <- NA
      }
      else
      {
        rise <- as.POSIXlt(paste(unq.dates[i] ,Sleep_tab[i,2],sep = " "),tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
        bed <- as.POSIXlt(paste(unq.dates[i] ,Sleep_tab[i,3],sep = " "),tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
      }
      midday <- as.POSIXlt(paste(unq.dates[i] ,"12:00:00",sep = " "),tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
      
      if(!is.na(bed))
      {
        if(i< length(unq.dates) && bed < midday)
        {
          bed <- bed +(24*60*60)
        }
      }
      if(is.na(rise))
      {
        sleep.del <- which(as.POSIXlt(day.tab[,2],tz=getOption("tz")) >= bed)
      }
      else
      {
        if(is.na(bed))
        {
          sleep.del <- which(as.POSIXlt(day.tab[,2],tz=getOption("tz")) <= rise)
        }
        else
        {
          sleep1 <- which(as.POSIXlt(day.tab[,2],tz=getOption("tz")) <= rise )
          sleep2 <- which(as.POSIXlt(day.tab[,2],tz=getOption("tz")) >= bed)
          sleep.del <- c(sleep1, sleep2)
        }
      }
      if(length(sleep.del)>0)
      {
        day.tab <- day.tab[-sleep.del,]
      }
    }
    
    if(i==1)
    {
      new.tab <- day.tab
    }
    else
    {
      new.tab <- rbind.data.frame(new.tab,day.tab)
    }
  }
  tab <- new.tab
  dates <- as.Date(tab[,2],tz=getOption("tz"))
  if(!is.null(NW.tab))
  {
    new.tab <- NULL
    for(day in 1:length(unq.dates))
    {
      day.tab <- tab[which(dates == unq.dates[day]),]
      if(length(which(dte.NW == unq.dates[day]))>0)
      {
        null.nws <-  which(is.na(NW.tab[which(dte.NW == unq.dates[day]),NW.seq.general]))
        if(length(null.nws)>0)
        {
          NW.seq <- NW.seq.general[-null.nws]
        }
        else
        {
          NW.seq <- NW.seq.general
        }
        for(i in NW.seq)
        {
          if(!is.na(NW.tab[day,(i+2)]))
          {
            if(NW.tab[day,(i+2)] != "-" )
            {
              vek1 <- which(as.POSIXlt(day.tab[,2],tz=getOption("tz")) <= NW.tab[day,i])
              vek2 <- which(as.POSIXlt(day.tab[,2],tz=getOption("tz")) >= NW.tab[day,(i+1)])
              keep.vek <- c(vek1,vek2)
            }
            
          }
          day.tab <- day.tab[keep.vek,]
        }
      }
      if(day==1)
      {
        new.tab <- day.tab
      }
      else
      {
        new.tab <- rbind.data.frame(new.tab,day.tab)
      }
    }
    tab <- new.tab
  }
  dates <- as.Date(tab[,2],tz=getOption("tz"))
  outtab <- NULL
  fullday <- NULL
  if(!is.null(domains))
  {
    for(i in 1:nrow(domains))
    {
      dayTab <- tab[which(dates==as.Date(domains[i,1],tz=getOption("tz"))),]
      dte <- as.character(as.Date(domains[i,1],tz=getOption("tz"),format="%Y-%m-%d"))
      if(calcBySteps)
      {
        #print(colnames(dayTab))
        tt <- ActivityBySteps(dayTab = dayTab, dte=dte, mvpa_steps_cutoff=mvpa_steps_cutoff,Domain=NULL, time.unit=time.unit)
      }
      else
      {
        tt <-ActivityByChannel(dayTab = dayTab, dte=dte, mvpa_count_thresh=mvpa_count_thresh,Domain=NULL, time.unit=time.unit)
      }
      if(is.null(fullday) || nrow(fullday)==0)
      {
        fullday <- tt
      }
      else
      {
        fullday <- rbind(fullday, tt)
      }
      workH_tab <- NULL
      for(j in 1:length(domstarts))
      {
        temp_tab <- dayTab[which(dayTab[,2]>=domains[i,domstarts[j]] & dayTab[,2]<domains[i,(domstarts[j]+1)]),]
        if(is.null(workH_tab) || nrow(workH_tab)==0)
        {
          dte <- as.character(as.Date(domains[i,1],tz=getOption("tz"),format="%Y-%m-%d"))
          if(calcBySteps)
          {
            workH_tab <- ActivityBySteps(dayTab = temp_tab, dte=dte, mvpa_steps_cutoff=mvpa_steps_cutoff,Domain=j, time.unit=time.unit)
          }
          else
          {
            workH_tab <- ActivityByChannel(dayTab = temp_tab, dte=dte, mvpa_count_thresh=mvpa_count_thresh,Domain=j, time.unit=time.unit)
          }
        }
        else
        {
          dte <- as.character(as.Date(domains[i,1],tz=getOption("tz"),format="%Y-%m-%d"))
          if(calcBySteps)
          {
            temp_t <- ActivityBySteps(dayTab = temp_tab, dte=dte, mvpa_steps_cutoff=mvpa_steps_cutoff,Domain=j, time.unit=time.unit)
            
          }
          else
          {
            temp_t <- ActivityByChannel(dayTab = temp_tab, dte=dte, mvpa_count_thresh=mvpa_count_thresh,Domain=j, time.unit=time.unit)
          }
          workH_tab <- rbind(workH_tab,temp_t)
        }
      }
      if(is.null(outtab) || nrow(outtab)==0)
      {
        outtab <- workH_tab
      }
      else
      {
        outtab <- rbind(outtab, workH_tab)
      }
    }
    
    for(i in 1:nrow(fullday))
    {
      part.DomainFunc <- outtab[which(outtab[,1]==fullday[i,1]),]
      if(length(which(outtab[,1]==fullday[i,1]))==1)
      {
        part.DomainFunc <- rbind(part.DomainFunc,c(part.DomainFunc[1:2],rep.int(0,times = 8)))
      }
      Sed_Tim <- as.numeric(fullday[i,2]) - sum(as.numeric(part.DomainFunc[,3]))
      Sta_Tim <- as.numeric(fullday[i,3]) - sum(as.numeric(part.DomainFunc[,4]))
      Stepping_Tim <- as.numeric(fullday[i,4]) - sum(as.numeric(part.DomainFunc[,5]))
      Light_Tim <- as.numeric(fullday[i,5]) - sum(as.numeric(part.DomainFunc[,6]))
      MVPA_Tim <- as.numeric(fullday[i,6]) - sum(as.numeric(part.DomainFunc[,7]))
      Steps <- as.numeric(fullday[i,8]) - sum(as.numeric(part.DomainFunc[,9]))
      work_H <- as.numeric(fullday[i,9]) - sum(as.numeric(part.DomainFunc[,10]))
      if(Sed_Tim < 1e-10) Sed_Tim<-0
      if(Sta_Tim < 1e-10) Sta_Tim<-0
      if(Stepping_Tim < 1e-10) Stepping_Tim<-0
      if(Light_Tim < 1e-10) Light_Tim<-0
      if(MVPA_Tim< 1e-10) MVPA_Tim<-0
      if(Steps < 0.5) Steps<-0
      if(work_H < 1e-10) work_H<-0
      MVPA_Tim_Min <- MVPA_Tim*60
      if(i==1)
      {
        miscTab <- c(fullday[i,1],Sed_Tim,Sta_Tim,Stepping_Tim,Light_Tim,MVPA_Tim,MVPA_Tim_Min,Steps,work_H)
      }
      else
      {
        tempTab <- cbind(fullday[i,1],Sed_Tim,Sta_Tim,Stepping_Tim,Light_Tim,MVPA_Tim,MVPA_Tim_Min,Steps,work_H)
        miscTab <- rbind(miscTab,tempTab)
      }
    }
    row.names(fullday) <- NULL
    row.names(outtab) <- NULL
    row.names(miscTab) <- NULL
    
    if(is.null(nrow(miscTab)))
    {
      names(miscTab) <- colnames(fullday)
    }
    else
    {
      colnames(miscTab) <- colnames(fullday)
    }
    
    
    outlist <- list(fullday,outtab,miscTab)
  }
  else
  {
    #edited
    unq.dates <- unique(dates)
    for(i in 1:length(unq.dates))
    {
      
      dayTab <- tab[which(dates==unq.dates[i]),]
      dte <- as.character(unq.dates[i])
      if(calcBySteps)
      {
        tt <- ActivityBySteps(dayTab = dayTab, dte=dte, mvpa_steps_cutoff=mvpa_steps_cutoff,Domain=NULL, time.unit=time.unit)
      }
      else
      {
        tt <- ActivityByChannel(dayTab = dayTab, dte=dte, mvpa_count_thresh=mvpa_count_thresh,Domain=NULL, time.unit=time.unit)
      }
      if(is.null(fullday) || nrow(fullday)==0)
      {
        fullday <- tt
      }
      else
      {
        fullday <- rbind(fullday, tt)
      }
    }
    row.names(fullday) <- NULL
    outlist <- fullday
  }
  return(outlist)
}

ActivityBySteps <- function(dayTab,dte, mvpa_steps_cutoff=25, Domain=NULL, time.unit="hours")
  # categorisation of activity labels in epochs by the step count of the ActivPal-data
{
  ttemp <- dayTab[,-c(1)]
  GT25Steps <- numeric(length = nrow(ttemp))
  MVPA_Secs <- numeric(length = nrow(ttemp))
  LIPA <- numeric(length = nrow(ttemp))
  if(nrow(ttemp)>0)
  {
    for(k in 1:length(GT25Steps))
    {
      if(ttemp[k,2]>mvpa_steps_cutoff)
      {
        GT25Steps[k] <- 1
        MVPA_Secs[k] <- ttemp[k,5] ## Seconds upright in this epoch
      }
      else
      {
        GT25Steps[k] <- 0
        MVPA_Secs[k] <- 0
      }
      LIPA[k] <- 15-(ttemp[k,4]+ttemp[k,7]+MVPA_Secs[k])
      if(LIPA[k] < 0)
      {
        ## hier ensteht der Unterschied zwischen Hrs.Awake und work_H
        # wird 0 gesetzt da eine negative LIPA Zeit unlogisch ist, jedoch durch ?berg?nge
        # kann es zu -1 werden
        LIPA[k] <- 0
      }
    }
    ttemp<-cbind.data.frame(ttemp,LIPA,GT25Steps,MVPA_Secs)
  }
  if(time.unit=="mins" || time.unit=="minute")
  {
    Sed_Tim <- ((sum(ttemp[,4])/60))
    Sta_Tim <- ((sum(ttemp[,7])/60))
    Light_Tim <- ((sum(ttemp$LIPA)/60))
    MVPA_Tim <- ((sum(ttemp$MVPA_Secs)/60))
    Steps <- sum(ttemp[,2])
    MVPA_Tim_Min <- (sum(ttemp$MVPA_Secs)/60)
    work_H <- Sed_Tim+Sta_Tim+Light_Tim+MVPA_Tim
    Stepping_Tim <- work_H-(Sed_Tim+Sta_Tim)
  }
  else
  {
    if(time.unit=="hours")
    {
      Sed_Tim <- ((sum(ttemp[,4])/60)/60)
      Sta_Tim <- ((sum(ttemp[,7])/60)/60)
      Light_Tim <- ((sum(ttemp$LIPA)/60)/60)
      MVPA_Tim <- ((sum(ttemp$MVPA_Secs)/60)/60)
      Steps <- sum(ttemp[,2])
      MVPA_Tim_Min <- (sum(ttemp$MVPA_Secs)/60)
      work_H <- Sed_Tim+Sta_Tim+Light_Tim+MVPA_Tim
      Stepping_Tim <- work_H-(Sed_Tim+Sta_Tim)
    }
    else
    {
      #print("Activity-Analysis in secs")
      Sed_Tim <- (sum(ttemp[,4]))
      Sta_Tim <- (sum(ttemp[,7]))
      Light_Tim <- (sum(ttemp$LIPA))
      MVPA_Tim <- (sum(ttemp$MVPA_Secs))
      Steps <- sum(ttemp[,2])
      MVPA_Tim_Min <- (sum(ttemp$MVPA_Secs))
      work_H <- Sed_Tim+Sta_Tim+Light_Tim+MVPA_Tim
      Stepping_Tim <- work_H-(Sed_Tim+Sta_Tim)
    }
  }
  tt <- NULL
  if(is.null(Domain))
  {
    if(length(dte) < 1)
    {
      dte <- NA
    }
    tt <- cbind.data.frame(dte,Sed_Tim,Sta_Tim,Stepping_Tim,Light_Tim,MVPA_Tim,MVPA_Tim_Min,Steps,work_H)
  }
  else
  {
    if(length(dte) < 1)
    {
      dte <- NA
    }
    tt <- cbind.data.frame(dte,Domain,Sed_Tim,Sta_Tim,Stepping_Tim,Light_Tim,MVPA_Tim,MVPA_Tim_Min,Steps,work_H)
  }
  return(tt)
}

ActivityByChannel <- function(dayTab,dte, mvpa_count_thresh=2300, Domain=NULL, time.unit="hours")
  # categorisation of activity labels in epochs by the vector magnitude of the 3 channels count of the ActivPal-data
  # mvpa_count_thresh threshold for 15 sec epoch
{
  #[1 ]"TimeFraction"   "Date"           "StepCount"      "Activity_Score" "Sedentary"      "Upright"       
  #[7 ]"Stepping"       "Standing"       "Sed_to_Up"      "Up_to_Sed"      "Channel1"       "Channel2"      
  #[13]"Channel3"
  mvpathresh.sec <- mvpa_count_thresh/15
  ttemp <- dayTab[,-c(1)]
  
  vm_channels <- sqrt(((ttemp$Channel1)**2)+((ttemp$Channel2)**2)+((ttemp$Channel3)**2))
  ttemp <- cbind.data.frame(ttemp,vm_channels)
  
  GT25Steps <- numeric(length = nrow(ttemp))
  MVPA_Secs <- numeric(length = nrow(ttemp))
  LIPA <- numeric(length = nrow(ttemp))
  
  if(nrow(ttemp)>0)
  {
    for(k in 1:length(GT25Steps))
    {
      if(vm_channels[k]>=(ttemp[k,6]*(mvpathresh.sec))) # ????????????????
      {
        GT25Steps[k] <- 1
        MVPA_Secs[k] <- ttemp[k,6] ## Seconds upright in this epoch
      }
      else
      {
        GT25Steps[k] <- 0
        MVPA_Secs[k] <- 0
      }
      LIPA[k] <- 15-(ttemp[k,4]+ttemp[k,7]+MVPA_Secs[k])
      if(LIPA[k] < 0)
      {
        ## hier ensteht der Unterschied zwischen Hrs.Awake und work_H
        # wird 0 gesetzt da eine negative LIPA Zeit unlogisch ist, jedoch durch ?berg?nge
        # kann es zu -1 werden
        LIPA[k] <- 0
      }
    }
    ttemp<-cbind(ttemp,LIPA,GT25Steps,MVPA_Secs)
  }
  if(time.unit=="mins" || time.unit=="minute")
  {
    Sed_Tim <- ((sum(ttemp[,4])/60))
    Sta_Tim <- ((sum(ttemp[,7])/60))
    Light_Tim <- ((sum(ttemp$LIPA)/60))
    MVPA_Tim <- ((sum(ttemp$MVPA_Secs)/60))
    Steps <- sum(ttemp[,2])
    MVPA_Tim_Min <- (sum(ttemp$MVPA_Secs)/60)
    work_H <- Sed_Tim+Sta_Tim+Light_Tim+MVPA_Tim
    Stepping_Tim <- work_H-(Sed_Tim+Sta_Tim)
  }
  else
  {
    if(time.unit=="hours")
    {
      Sed_Tim <- ((sum(ttemp[,4])/60)/60)
      Sta_Tim <- ((sum(ttemp[,7])/60)/60)
      Light_Tim <- ((sum(ttemp$LIPA)/60)/60)
      MVPA_Tim <- ((sum(ttemp$MVPA_Secs)/60)/60)
      Steps <- sum(ttemp[,2])
      MVPA_Tim_Min <- (sum(ttemp$MVPA_Secs)/60)
      work_H <- Sed_Tim+Sta_Tim+Light_Tim+MVPA_Tim
      Stepping_Tim <- work_H-(Sed_Tim+Sta_Tim)
    }
    else
    {
      #print("Activity-Analysis in secs")
      Sed_Tim <- (sum(ttemp[,4]))
      Sta_Tim <- (sum(ttemp[,7]))
      Light_Tim <- (sum(ttemp$LIPA))
      MVPA_Tim <- (sum(ttemp$MVPA_Secs))
      Steps <- sum(ttemp[,2])
      MVPA_Tim_Min <- (sum(ttemp$MVPA_Secs))
      work_H <- Sed_Tim+Sta_Tim+Light_Tim+MVPA_Tim
      Stepping_Tim <- work_H-(Sed_Tim+Sta_Tim)
    }
  }
  tt <- NULL
  if(is.null(Domain))
  {
    tt <- cbind.data.frame(dte,Sed_Tim,Sta_Tim,Stepping_Tim,Light_Tim,MVPA_Tim,MVPA_Tim_Min,Steps,work_H)
  }
  else
  {
    tt <- cbind.data.frame(dte,Domain,Sed_Tim,Sta_Tim,Stepping_Tim,Light_Tim,MVPA_Tim,MVPA_Tim_Min,Steps,work_H)
  }
  return(tt)
}

clac_MVPA_byChannels<-function(activPal, Cutoff)
  #Platzhalter
{
  
  return(activPal)
}


################ Sleep Analysis ###################

delete_sleeptime <- function(SED_ACT_ALL,night="20:00:00",morning="12:00:00",break.lim=15,min.sleeplength=45, sleepMode="setTimes",
                             sleep.start="22:00:00", sleep.end="08:00:00", is.StartAfterMidNight=FALSE,sleep.file=NULL,separated.csv=TRUE,
                             idNr,separator=",", dec=".", is.xlsx=FALSE, sheetIndex=1,idlength=5)
  # Deletes observed sleeping-time from sedentary-bout-table
{
  #without.sleep <- detect_sleepingtime(SED_ACT_ALL,earliest = night, latest = morning,max.break=break.lim,min.sleeplength=min.sleeplength)
  if(sleepMode == "default")
  {
    temp.sleepTab <- getSleepTimes(SED_ACT_ALL=SED_ACT_ALL,earliest=earliest,latest=latest,max.break=max.break,min.sleeplength=min.sleeplength)
  }
  if(sleepMode == "setTimes")
  {
    temp.sleepTab <- setSleep(SED_ACT_ALL=SED_ACT_ALL,sleep.start=sleep.start, sleep.end=sleep.end, is.StartAfterMidNight=is.StartAfterMidNight)
  }
  if(sleepMode == "file")
  {
    SlpIdInfo <- readSleepFile(SED_ACT_ALL=SED_ACT_ALL, Slp.Path=sleep.file,separated.csv=separated.csv, idNr=idNr, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex,idlength=idlength)
    temp.sleepTab <- adjustSlpTab(SlpIdInfo)
  }
  if(sleepMode != "default" & sleepMode != "setTimes" & sleepMode != "file")
  {
    temp.sleepTab <- setSleep(SED_ACT_ALL=SED_ACT_ALL,sleep.start="00:00:00", sleep.end="00:00:00", is.StartAfterMidNight=TRUE)
  }
  without.sleep <- deleteSlp(SED_ACT_ALL=SED_ACT_ALL,temp.sleepTab)
  return(without.sleep)
}

deleteSlp <- function(SED_ACT_ALL,SlpTab)
{
  ACTALL_Date <- as.Date(SED_ACT_ALL[,1])
  SED_ACT_ALL[,2] <- as.character(SED_ACT_ALL[,2])
  SED_ACT_ALL[,3] <- as.character(SED_ACT_ALL[,3])
  del.vec <- NULL
  for(i in 1:nrow(SlpTab))
  {
    posSlp_start <- which.min(difftime(SlpTab[i,1],SED_ACT_ALL[,1])>=0)
    EndSlp <- (as.POSIXct(paste(as.Date(SlpTab[i,1]),SlpTab[i,3]), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")+86400)
    SedActEnd <- as.POSIXct(paste(as.Date(SED_ACT_ALL[,1]),SED_ACT_ALL[,3]), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
    
    
    for(j in 1:length(SedActEnd))
    {
      if(difftime(SedActEnd[j],SED_ACT_ALL[j,1])<0)
      {
        SedActEnd[j]+86400
      }
    }
    posSlp_end <- which.max(difftime(SedActEnd,EndSlp)>=0)
    if(posSlp_end == 1)
    {
      posSlp_end <- length(SedActEnd)
    }
    
    if(posSlp_start[1] <= posSlp_end[length(posSlp_end)])
    {
      del.vec <- c(del.vec,seq(posSlp_start[1],posSlp_end[length(posSlp_end)],1))
    }
    
    if(posSlp_start > 1)
    {
      if(difftime(SedActEnd[(posSlp_start-1)],SlpTab[i,1])>0)
      {
        SED_ACT_ALL[(posSlp_start-1),3] <- as.character(SlpTab[i,2])
        SED_ACT_ALL[(posSlp_start-1),4] <- as.numeric(difftime(SlpTab[i,1],SED_ACT_ALL[(posSlp_start-1),1],units = "secs"))
        SED_ACT_ALL[(posSlp_start-1),5] <- as.numeric(difftime(SlpTab[i,1],SED_ACT_ALL[(posSlp_start-1),1],units = "mins"))
        SED_ACT_ALL[(posSlp_start-1),6] <- SED_ACT_ALL[(posSlp_start-1),4]/15
      }
    }
    
    if(posSlp_end < nrow(SED_ACT_ALL))
    {
      if(difftime(EndSlp,SED_ACT_ALL[(posSlp_end+1),1])>0)  
      {
        SED_ACT_ALL[(posSlp_start-1),1] <- EndSlp
        SED_ACT_ALL[(posSlp_start-1),2] <- SlpTab[i,3]
        temp.time <- (as.POSIXct(paste(as.Date(SlpTab[i,1]),SlpTab[i,3]), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")+86400)
        SED_ACT_ALL[(posSlp_start-1),4] <- as.numeric(difftime(SedActEnd[(posSlp_end+1)],EndSlp,units = "secs"))
        SED_ACT_ALL[(posSlp_start-1),5] <- as.numeric(difftime(SedActEnd[(posSlp_end+1)],EndSlp,units = "mins"))
        SED_ACT_ALL[(posSlp_start-1),6] <- SED_ACT_ALL[(posSlp_start-1),4]/15
      }
    }
  }
  if(!is.null(del.vec))
  {
    SAA_Slpdel <- SED_ACT_ALL[-del.vec,]
  }
  else
  {
    SAA_Slpdel <- SED_ACT_ALL
  }
  return(SAA_Slpdel)
}


create.sleepTab <-function(SED_ACT_ALL,earliest="20:00:00",latest="12:00:00",max.break=15,min.sleeplength=45, sleepMode="setTimes",
                           sleep.start="22:00:00", sleep.end="08:00:00", is.StartAfterMidNight=FALSE, sleep.file=NULL,separated.csv=TRUE,
                           idNr,separator=",", dec=".", is.xlsx=FALSE, sheetIndex=1,idlength=5)
  # Creates table with informatione of the sleeping time
{
  if(sleepMode == "default")
  {
    temp.sleepTab <- getSleepTimes(SED_ACT_ALL=SED_ACT_ALL,earliest=earliest,latest=latest,max.break=max.break,min.sleeplength=min.sleeplength)
  }
  if(sleepMode == "setTimes")
  {
    temp.sleepTab <- setSleep(SED_ACT_ALL=SED_ACT_ALL,sleep.start=sleep.start, sleep.end=sleep.end, is.StartAfterMidNight=is.StartAfterMidNight)
  }
  if(sleepMode == "file")
  {
    SlpIdInfo <- readSleepFile(SED_ACT_ALL=SED_ACT_ALL, Slp.Path=sleep.file,separated.csv=separated.csv, idNr=idNr, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex,idlength=idlength)
    temp.sleepTab <- adjustSlpTab(SlpIdInfo)
  }
  if(sleepMode != "default" & sleepMode != "setTimes" & sleepMode != "file")
  {
    temp.sleepTab <- setSleep(SED_ACT_ALL=SED_ACT_ALL,sleep.start="00:00:00", sleep.end="00:00:00", is.StartAfterMidNight=TRUE)
  }
  
  SleepDates <- c(as.Date(temp.sleepTab[,1], tz=getOption("tz")))
  SleepDates <- c(SleepDates, (SleepDates[length(SleepDates)]+1))
  
  for(i in which(is.na(SleepDates)))
  {
    if(i > 1)
    {
      SleepDates[i] <- (as.Date(temp.sleepTab[(i-1),1], tz=getOption("tz")))+ceiling((temp.sleepTab[(i-1),5]/1440))
    }
  }

  for(Day in 1:(nrow(temp.sleepTab)+1))
  {
    if(Day == 1)
    {
      Rise.Time <- "-"
      Bed.Time <- as.character(temp.sleepTab[Day,2])
      Sleep.Mins <- NA 
      Sleep.Hrs <- NA
      Non.BedHours <- NA
      interrupt <- NA
      SleepVec <- c(Day,Rise.Time,Bed.Time,Sleep.Mins,Sleep.Hrs,Non.BedHours,interrupt)
    }
    else
    {
      if(Day == (nrow(temp.sleepTab)+1))
      {
        Rise.Time <- as.character(temp.sleepTab[(Day-1),3])
        Bed.Time <- "-"
        Sleep.Mins <- temp.sleepTab[(Day-1),5]
        Sleep.Hrs <- Sleep.Mins/60
        interrupt <- temp.sleepTab[(Day-1),7]
        Non.BedHours <- NA
      }
      else
      {
        Rise.Time <- as.character(temp.sleepTab[(Day-1),3])
        Bed.Time <- as.character(temp.sleepTab[Day,2])
        Sleep.Mins <- temp.sleepTab[(Day-1),5]
        Sleep.Hrs <- Sleep.Mins/60
        interrupt <- temp.sleepTab[(Day-1),7]
        dte <- as.Date(temp.sleepTab[Day,1], tz=getOption("tz"))
        temp1 <- paste(dte,as.character(Rise.Time),sep = " ")
        temp2 <- paste(dte,as.character(Bed.Time),sep = " ")
        ristime <- as.POSIXct(temp1, tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
        bedtime <- as.POSIXct(temp2, tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
        Non.BedHours <- difftime(bedtime,ristime,units="hours")
        #print(ristime)
        if(!is.na(ristime))
        {
          if(ristime > as.POSIXct(paste(dte,earliest,sep = " "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S"))
          {
            Rise.Time <- as.character(NA)
            Non.BedHours <- difftime(bedtime,as.POSIXct(paste(dte,"00:00:00",sep = " "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S"),units="hours")
            Sleep.Mins <- NA
            Sleep.Hrs <- NA
            interrupt <- NA
          }
        }
        if(is.na(Non.BedHours))
        {
          Non.BedHours <- NA
        }
        else
        {
          if(Non.BedHours < 0)
          {
            Non.BedHours <- 24 + Non.BedHours
          }
        }
        
      }
      temp.Vec <- c(Day,Rise.Time,Bed.Time,Sleep.Mins,Sleep.Hrs,Non.BedHours,interrupt)
      SleepVec <- rbind(SleepVec,temp.Vec)
    }
  }
  rownames(SleepVec) <- NULL
  colnames(SleepVec) <- c("Day","Rise.Time","Bed.Time","Sleep.Minutes","Sleep.Hrs","Hrs.Awake","Sleep.interrupt")
  
  SleepVec <- cbind.data.frame(SleepDates,SleepVec)
  colnames(SleepVec)[1] <- c("SlpDate")

  return(SleepVec)
}

getSleepTimes <- function(SED_ACT_ALL,earliest="20:00:00",latest="12:00:00",max.break=15,min.sleeplength=45)
  # returns sleeping-time in the sedentary-bout-table
  # Algorithm searches for longest sedentary bout between night and morning and observes the previous and following sed-bouts 
{
  sleep.vec <- SED_ACT_ALL[1:2,]
  colnames(SED_ACT_ALL)[ncol(SED_ACT_ALL)] <- c("sortFlag")
  dte <- as.character(as.Date(SED_ACT_ALL[,1], tz=getOption("tz"),format="%Y-%m-%d"))
  start <- as.POSIXct(paste(dte,SED_ACT_ALL[,2],sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
  b.end <- as.POSIXct(paste(dte,SED_ACT_ALL[,3],sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
  unq.dates <- unique(dte)
  interrups <- numeric(length = (length(unq.dates)-1))
  for(i in 1:length(dte))
  {
    if(start[i] > b.end[i])
    {
      b.end[i] <- as.POSIXct(paste(dte[i+1],SED_ACT_ALL[i,3],sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
    }
  }
  for(i in 1:(length(unq.dates)-1))
  {
    first_date <- which(dte == unq.dates[i])
    lat <- as.POSIXct(paste(unq.dates[i+1],latest,sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
    earl <- as.POSIXct(paste(unq.dates[i],earliest,sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
    intmed <- which(b.end < lat & start >= earl)
    leftborder <- which(start < earl & b.end > earl & b.end < lat)
    rightborder <- which(start >= earl & start < lat & b.end > lat)
    overlap <- which(start < earl & b.end > lat)
    posSleepT <- SED_ACT_ALL[c(overlap,leftborder,intmed,rightborder),]
    max_pos <- c(which(posSleepT[,4] == max(posSleepT[,4])))
    sl.sta <- start[c(overlap,leftborder,intmed,rightborder)]
    sl.end <- b.end[c(overlap,leftborder,intmed,rightborder)]
    is.lb <- TRUE
    is.rb <- TRUE
    temp_pos <- max_pos
    temp_pos <- temp_pos[order(temp_pos)]
    if(length(temp_pos)>0)
    {
      while(temp_pos[1]>1)
      {
        if(posSleepT[temp_pos,1] > earl && temp_pos[1] > 1)
        {
          difleft <- difftime(sl.sta[temp_pos[1]],sl.end[(temp_pos[1]-1)],units = "mins")
          if(difleft <= max.break)
          {
            if(posSleepT[(temp_pos[1]-1),5] >= min.sleeplength)
            {
              temp_pos <- c((temp_pos[1]-1),temp_pos)
            }
            else
            {
              break
            }
          }
          else
          {
            break
          }
        }
        else
        {
          break
        }
      }
      
      while(temp_pos[length(temp_pos)] < nrow(posSleepT))
      {
        if(sl.end[temp_pos[length(temp_pos)]] < lat && temp_pos[length(temp_pos)] < nrow(posSleepT))
        {
          difleft <- difftime(sl.sta[(temp_pos[length(temp_pos)]+1)],sl.end[temp_pos[length(temp_pos)]],units = "mins")
          if(difleft <= max.break)
          {
            if(posSleepT[(temp_pos[length(temp_pos)]+1),5] >= min.sleeplength)
            {
              temp_pos <- c(temp_pos,(temp_pos[length(temp_pos)]+1))
            }
            else
            {
              break
            }
          }
          else
          {
            break
          }
        }
        else
        {
          break
        }
      }
      begin <- posSleepT[temp_pos[1],1:2]
      fin <- posSleepT[temp_pos[length(temp_pos)],3]
      dur.sec <- difftime(sl.end[(temp_pos[length(temp_pos)])],sl.sta[temp_pos[1]],units = "secs")
      dur.min <- difftime(sl.end[(temp_pos[length(temp_pos)])],sl.sta[temp_pos[1]],units = "mins")
      dur.seg <- dur.sec/15
      interrups[i] <- (length(temp_pos)-1)
      temp_sleeps <- cbind.data.frame(begin,fin,dur.sec,dur.min,dur.seg)
      colnames(temp_sleeps) <- colnames(sleep.vec)
    }
    else
    {
      temp_sleeps <- cbind(NA,NA,NA,NA,NA,NA)
      names(temp_sleeps) <- colnames(sleep.vec)
      colnames(temp_sleeps) <- colnames(sleep.vec)
    }
    sleep.vec <- rbind(sleep.vec,temp_sleeps)
  }
  sleep.vec <- sleep.vec[-c(1,2),]
  sleep.vec <- cbind.data.frame(sleep.vec,interrups)
  return(sleep.vec)
}

adjust_sleeptab <- function(sleeptab,day.begin=NULL,SED_ACT_ALL=NULL,early="20:00:00",Activpal_data=NULL)
  # Adjusts format of sleeping tab for output
{
  unq.dates <- sleeptab[,1]
  sleeptab <- sleeptab[,-c(1)]
  if(!is.null(Activpal_data))
  {
    date.first <- as.Date(Activpal_data[1,2], tz=getOption("tz"))
    date.last <- as.Date(Activpal_data[nrow(Activpal_data),2], tz=getOption("tz"))
    bedtime.first <- as.POSIXlt(paste(date.first,sleeptab[1,3],sep=" "),format="%Y-%m-%d %H:%M:%S", tz=getOption("tz"))
    sleeptab[1,6] <- as.character(as.numeric(difftime(bedtime.first,Activpal_data[1,2],units = "hours")))
    sleeptab[1,2]<- strsplit(as.character(Activpal_data[1,2])," ")[[1]][2]
    
    rise.last <- as.POSIXlt(paste(date.last,sleeptab[nrow(sleeptab),2],sep=" "),format="%Y-%m-%d %H:%M:%S", tz=getOption("tz"))
    sleeptab[nrow(sleeptab),6] <- as.character(as.numeric(difftime(Activpal_data[nrow(Activpal_data),2],rise.last,units = "hours")))
    sleeptab[nrow(sleeptab),3]<- strsplit(as.character(Activpal_data[nrow(Activpal_data),2])," ")[[1]][2]
  }
  if(day.begin == "00:00:00")
  {
    day.begin <- NULL
  }
  if(!is.null(day.begin))
  {
    # if(is.null(SED_ACT_ALL))
    # {
    #   print("Warning: SED_ACT_ALL is null")
    #   for(i in as.numeric(sleeptab[,1]))
    #   {
    #     if(i == as.numeric(sleeptab[1,1]))
    #     {
    #       unq.dates <- Sys.Date()-i
    #     }
    #     else
    #     {
    #       unq.dates <- c((Sys.Date()-i),unq.dates)
    #     }
    #   }
    #   unq.dates <- as.Date(unq.dates,format="%Y-%m-%d", tz=getOption("tz"))
    # }
    # else
    # {
    #   unq.dates <- unique(as.Date(SED_ACT_ALL[,1], tz=getOption("tz")))
    # }
    begintime.vec <- as.POSIXlt(paste(unq.dates,day.begin,sep=" "),format="%Y-%m-%d %H:%M:%S", tz=getOption("tz"))
    begintime.vec <- c(begintime.vec,(begintime.vec[length(begintime.vec)]+86400))
    hours.awake.vec <- as.numeric(sleeptab[,6])
    hours.sleep.vec <- numeric(length = nrow(sleeptab))
    if(!nrow(sleeptab)==length(unq.dates))
    {
      print(paste("Problem der Anzahl and Tagen - Sleeptab:",nrow(sleeptab)," - SED_ACT_ALL:",length(unq.dates),sep=""))
      return(sleeptab)
    }
    
    for(i in 1:nrow(sleeptab))
    {
      risetime <- as.POSIXlt(paste(unq.dates[i],sleeptab[i,2],sep=" "),format="%Y-%m-%d %H:%M:%S", tz=getOption("tz"))
      bedtime  <- as.POSIXlt(paste(unq.dates[i],sleeptab[i,3],sep=" "),format="%Y-%m-%d %H:%M:%S", tz=getOption("tz"))
      if(is.na(risetime) || is.na(bedtime))
      {
        hours.sleep.vec[i] <- 0
        hours.awake.vec[i] <- 24
      }
      else
      {
        if(bedtime < risetime || bedtime < as.POSIXlt(paste(unq.dates[i],early,sep=" "),format="%Y-%m-%d %H:%M:%S", tz=getOption("tz")))
        {
          bedtime <- bedtime + 86400
        }
        diff.riseDayCut <- as.numeric(difftime(risetime,begintime.vec[i],units = "hours"))
        diff.bedDayCut <- as.numeric(difftime(begintime.vec[i+1],bedtime,units = "hours"))
        hours.sleep.vec[i] <- max(0,diff.riseDayCut)+max(0,diff.bedDayCut)
        if(diff.riseDayCut < 0)
        {
          if(i > 1)
          {
            hours.awake.vec[i-1] <- hours.awake.vec[i-1] - diff.riseDayCut
          }
        }
        if(diff.bedDayCut < 0)
        {
          if(i < nrow(sleeptab))
          {
            hours.awake.vec[i+1] <- hours.awake.vec[i+1] - diff.bedDayCut
          }
        }
      }
    }
    min.sleep.vec <- hours.sleep.vec*60
    for(i in 1:nrow(sleeptab))
    {
      sleeptab[i,4] <- as.character(min.sleep.vec[i])
      sleeptab[i,5] <- as.character(hours.sleep.vec[i])
      sleeptab[i,6] <- as.character(hours.awake.vec[i])
      if(i==1)
      {
        sleeptab[i,4] <- as.character(NA)
        sleeptab[i,5] <- as.character(NA)
      }
    }
  }
  
  return(sleeptab)
}

deleteWrongSlpDates <- function(Sleep_tab=Sleep_tab, Bout.list=Bout.list)
  # controll and adjustment of displayed date and the information of the sleeping table
{
  unqSED.dte <- unique(as.Date(as.character(Bout.list[[1]][[1]][,1]), tz=getOption("tz")))
  for(i in 1:nrow(Sleep_tab))#Troubleshooting when Sleeptime starts after midnight
  {
    HourPart.Bed <- as.numeric(substr(Sleep_tab[i,4],0,2))
    #print(HourPart.Bed)
    if(i < nrow(Sleep_tab))
    {
      if(!is.na(HourPart.Bed))
      {
        if(HourPart.Bed < 12 && as.numeric(Sleep_tab[i+1,6]) < 24)
        {
          Sleep_tab[i,1] <- Sleep_tab[i,1]-1
        }
      }
    }
  }
 
  delSlpR<- NULL
  if(length(unqSED.dte) < nrow(Sleep_tab))
  {
    Slp.dte <- as.Date(Sleep_tab[,1])
    for(i in 1:length(Slp.dte))
    {
      if(length(which(unqSED.dte == Slp.dte[i])) < 1)
      {
        delSlpR <- c(delSlpR,i)
      }
    }
  }
  
  if(!is.null(delSlpR))
  {
    for(i in 1:length(delSlpR))
    {
      if(i != nrow(Sleep_tab))
      {
        Sleep_tab[(delSlpR[i]+1),5] <- NA
        Sleep_tab[(delSlpR[i]+1),6] <- NA
      }
    }
    Sleep_tab <- Sleep_tab[-delSlpR,]
  }
  
  if(is.na(Sleep_tab[nrow(Sleep_tab),2]))
  {
    Sleep_tab <- Sleep_tab[-nrow(Sleep_tab),]
  }
  
  return(Sleep_tab)
}

NullslpTab <- function(SED_ACT_ALL)
  # Create empty Sleep-information table when sleeping time is not calculated
{
  unq.dates <- unique(as.Date(SED_ACT_ALL[,1]))
  #print(unq.dates)
  nullSlp <- cbind.data.frame(seq(1,length(unq.dates),1),rep.int(NA,times = length(unq.dates)),rep.int(NA,times = length(unq.dates)),rep.int(NA,times = length(unq.dates))
                              ,rep.int(NA,times = length(unq.dates)),rep.int(24,times = length(unq.dates)),rep.int(NA,times = length(unq.dates)))
  colnames(nullSlp) <- c("Day","Rise.Time","Bed.Time","Sleep.Minutes","Sleep.Hrs","Hrs.Awake","Sleep.interrupt")
  #print("NULLslp")
  return(nullSlp)
}

setSleep <- function(SED_ACT_ALL, sleep.start="22:00:00", sleep.end="08:00:00", is.StartAfterMidNight=FALSE)
{
  dte <- as.character(as.Date(SED_ACT_ALL[,1], tz=getOption("tz"),format="%Y-%m-%d"))
  unq.dte <- unique(dte)
  Date <- as.POSIXct(paste(unq.dte,sleep.start), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
  if(is.StartAfterMidNight)
  {
    Date <- Date+86400
  }
  start <- format(Date, "%H:%M:%S")
  temp.fin <- (as.POSIXct(paste(unq.dte,sleep.end), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")+86400)
  finish <- format(temp.fin, "%H:%M:%S")
  seconds <- as.numeric(difftime(temp.fin,Date,units = "secs"))
  minutes <- as.numeric(difftime(temp.fin,Date,units = "mins"))
  segments <- seconds/15
  interrups <- rep.int(NA, times = length(Date))

  SleepTab <- cbind.data.frame(Date,start,finish,seconds,minutes,segments,interrups)
  SleepTab <- SleepTab[-nrow(SleepTab),] #Last day has no sleep start
  return(SleepTab)
}

just_critSleepBouts <- function(input,output,Threshold_static,is.csvout=T,morning,night)
  # Return possible sleep-Bouts, a testing function for morning night parameter and to get an overview in the timeintervall 
{
  Sys.time()
  tab <- read_activPAL(inputpath = input)
  Sys.time()
  Sed_addresses_tab <- Sed_detect_transition(tab)
  Sys.time()
  SED_ACT_ALL<-create_SED_ACT_ALL(tab, Sed_addresses_tab)
  SED_ACT_ALL <- check_maxSed_Threshold(SED_ACT_ALL,Threshold_static)
  Sys.time()
  date <- as.Date(SED_ACT_ALL[,1], tz=getOption("tz"))
  unq_date <- unique(date)
  for(i in 1:length(unq_date))
  {
    temptab <- SED_ACT_ALL[which(date == unq_date[i]),]
    mor <- paste(unq_date[i],morning,sep="")
    nig <- paste(unq_date[i],night,sep="")
    mor <- as.POSIXlt(mor, tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
    nig <- as.POSIXlt(nig, tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
    subtab1 <- temptab[which(temptab[,1] <= mor),]
    subtab2 <- temptab[which(temptab[,1] >= nig),]
    if(i == 1)
    {
      subtab <- rbind(subtab1,subtab2)
    }
    else
    {
      subtab <- rbind(subtab,subtab1,subtab2)
    }
  }
  
  if(is.csvout)
  {
    name <- get_filename(input)
    outputfile <- paste(output,name,"_SleepBouts.csv",sep = "")
    write.csv(subtab, file=outputfile, row.names = FALSE)
    warnings()
  }
  return(subtab)
}

################ Nw Detection ###################

NwNaiv <- function() #EMPTY
{
  
}

NwChoi <- function() #EMPTY
{
  
}

NwTroiano <- function() #EMPTY
{
  
}

NwRF <- function() #EMPTY
{
  
}

CreateEvents <- function(AccTab, CatVar,id) #EMPTY
  
{
  Catcol <- which(colnames(AccTab)==CatVar)
  CatChage <- c(AccTab[Catcol],0) - c(0,AccTab[,Catcol])
  CatPosStart <- which(CatChage==1)
  CatPosEnd <- which(CatChage==(-1))
  CatPosEnd <- CatPosEnd-1
  startTime <- AccTab[CatPosStart,1]
  EndTime   <- AccTab[CatPosEnd,1]
  if(length(startTime)==length(EndTime))
  {
    EventTab <- cbind.data.frame(rep.int(id,times = length(startTime)),startTime,EndTime)
    return(EventTab)
  }
  else
  {
    stop("Events for Categoire could not be created ")
  }
}

EventTabSupTab <- function(EventTab,EventName="") #EMPTY
{
  SupportTab <- NULL
  ids <- unique(EventTab[,i])
  for(ID in ids)
  {
    EvStart <- NULL
    EvEnd <- NULL
    IdEventTab <- EventTab[which(EventTab[,i]==ID),]
    for(i in 1:nrow(IdEventTab))
    {
      Eventdays <- floor(as.numeric(difftime(IdEventTab[i,3],IdEventTab[i,2],units="days")))
      if(!as.Date(IdEventTab[i,2],tz=getOption("tz")) == as.Date(IdEventTab[i,3],tz=getOption("tz")))
      {
        tempStart <- as.POSIXct(paste(as.character(as.Date(IdEventTab[i,3],tz=getOption("tz"))),
                                      "00:00:00",sep=" "),
                                tz=getOption("tz"),format="%y-%m-%d %H:%M:%S")
        tempEnd   <- as.POSIXct(paste(as.character(as.Date(IdEventTab[i,2],tz=getOption("tz"))),
                                      "23:59:59",sep=" "),
                                tz=getOption("tz"),format="%y-%m-%d %H:%M:%S")
        EvStart <- c(EvStart,IdEventTab[i,2],tempStart)
        EvEnd <- c(EvEnd,tempEnd,IdEventTab[i,3])
        
        if(Eventdays > 0)
        {
          tempT <- as.POSIXct(paste(as.character(as.Date(IdEventTab[i,3],tz=getOption("tz"))),
                                    format(IdEventTab[i,2],"%H:%M:%S"),sep=" "),
                              tz=getOption("tz"),format="%y-%m-%d %H:%M:%S")
          
          # When more days are included into the Event then the Daydiff suggests 
          # e.g. 01.01.1990 15:00:00 - 03.01.1990 03:00:00, daydiff=2,5 but 3 days effected
          if(as.numeric(difftime(tempT,IdEventTab[i,3],units="")) > 0)
          {
            Eventdays <- Eventdays+1
          }
          Eventdays <- Eventdays-1
          if(Eventdays > 0)
          {
            for(j in 1:Eventdays)
            {
              tempDay <- as.Date((IdEventTab[i,2]+(j*86400)),tz=getOption("tz"))
              tempStart <- as.POSIXct(paste(as.character(tempDay),"00:00:00",sep=" ")
                                      ,tz=getOption("tz"),format="%y-%m-%d %H:%M:%S")
              tempEnd   <- as.POSIXct(paste(as.character(tempDay),"23:59:59",sep=" ")
                                      ,tz=getOption("tz"),format="%y-%m-%d %H:%M:%S")
              EvStart <- c(EvStart,tempStart)
              EvEnd <- c(EvEnd,tempEnd)
            }
          }
        }
      }
    }
    unq.Dte <- unique(as.Date(EvStart,tz=getOption("tz")))
    maxEventsPerDay <- 0
    for(dte in unq.Dte)
    {
      NumEv <- length(which(as.Date(EvStart,tz=getOption("tz"))==dte))
      if(NumEv > maxEventsPerDay)
      {
        maxEventsPerDay <- NumEv
      }
    } 
    
    EventSubTab <- as.character(matrix(data="",nrow=length(unq.Dte),ncol=maxEventsPerDay))
    
    for(dteNr in 1:length(unq.Dte))
    {
      Events <- which(as.Date(EvStart,tz=getOption("tz"))==dte)
      for(EventNr in 1:length(Events))
      {
        EventSubTab[dteNr,((3*EventNr)-2)] <- as.character(EvStart[Events[EventNr]])
        EventSubTab[dteNr,((3*EventNr)-1)] <- as.character(EvEnd[Events[EventNr]])
        EventSubTab[dteNr,((3*EventNr))] <- "auto"
      }
    }
    
    if(is.null(SupportTab))
    {
      SupportTab <- cbind.data.frame(rep.int(ID,times=length(unq.Dte)),unq.Dte,EventSubTab)
    }
    else
    {
      SupportTab <- bind_rows(SupportTab,cbind.data.frame(rep.int(ID,times=length(unq.Dte)),unq.Dte,EventSubTab))
    }
  }
  
  for(entry in seq(3,ncol(SupportTab),3))
  {
    nr <- entry/3
    SupportTab[,entry] <- as.POSIXct(SupportTab[,entry],tz=getOption("tz"),format="%y-%m-%d %H:%M:%S")
    SupportTab[,(entry+1)] <- as.POSIXct(SupportTab[,(entry+1)],tz=getOption("tz"),format="%y-%m-%d %H:%M:%S")
    colnames(SupportTab)[entry:(entry+2)] <- paste(c("event_start","event_end","event_name"),nr,sep="")
  }
  return(SupportTab)
}

AddEventlessdays <- function(EventTab,AccData)
{
  
}

CalcNw <- function(AccPath,Method="naiv")
{
  
}

################ Create Output ###################

createDateDay <- function(functional_ls,NW_path,separated.csv=TRUE, JoinedNWs=NULL, idNr=NULL, separator=",", dec=".",is.xlsx=FALSE, sheetIndex=1)
  # Create controll table for nonweartime from activity-table
{
  DDTab <- NULL
  if(length(dim(functional_ls)) == 2)
  {
    functional.vec <- functional_ls[,1]
  }
  else
  {
    functional.dts <- functional_ls[[1]]
    functional.vec <- functional.dts[,1]
  }
  
  if(!is.null(NW_path))
  {
    #NW.tab <- get_NWforID(NW_path,separated.csv=separated.csv, JoinedNWs=JoinedNWs, id=id, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
    NW.dd <- as.data.frame(JoinedNWs[,c(1,2)])
    NW.dd <- NW.dd[(NW.dd[,1]==idNr),]
    
    inNW <- numeric(length = length(functional.vec))
    for(k in 1:length(functional.vec))
    {
      if(length(which(as.character(NW.dd[,2])==as.character(functional.vec[k])))>0)
      {
        inNW[k] <-  which(as.character(NW.dd[,2])==as.character(functional.vec[k]))
      }
    }
    DayNr <- 1:length(functional.vec)
    DDTab <- cbind.data.frame(functional.vec,DayNr,inNW)
  }
  else
  {
    NW.tab         <- NULL
    NW.seq.general <- NULL
    DayNr <- 1:length(functional.vec)
    inNW <- rep.int(0,times = length(functional.vec))
    DDTab <- cbind.data.frame(functional.vec,DayNr,inNW)
  }
  return(DDTab)
}

create_Mastersheet <- function(Bout.list,functional.list,sleep.tab,NW.tab,Domain.path=NULL,outpath=NULL,id.Nr, separated.csv=TRUE, JoinedDomains=NULL, separator=",", dec=".",WriteMastersheet=FALSE, is.xlsx=FALSE, sheetIndex=1,
                               time.unit = "hour", save_as_xlsx=FALSE,DateDayTab=NULL, non.stop=TRUE)
  # Merging created analysis table to one big anlysis-table for output
{
  TimeUnitMod <- 1
  if(time.unit == "minute" || time.unit == "mins")
  {
    TimeUnitMod <- 60
  }
  else
  {
    if(time.unit == "seconds")
    {
      TimeUnitMod <- 60*60
    }
  }
  Domain.list <- getDomainsforID(Domain.path, separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=id.Nr, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
  if(is.null(Domain.list))
  {
    H24_functional <- functional.list
  }
  else
  {
    H24_functional <- functional.list[[1]]
  }
  Bout.H24.Sum <- Bout.list[[1]][[1]]
  days <- H24_functional[,1]
  H24.BoutSum <- Bout.H24.Sum[,2:ncol(Bout.H24.Sum)]
  Bout.Sum <- Bout.list[[2]][[1]][,2:ncol(Bout.H24.Sum)]
  
  H24.dels <- which(H24_functional[,2] >= 1440 | H24_functional[,2] <= 0)
  if(length(H24.dels) > 0)
  {
    NW.del <- NULL
    for(dels in H24.dels)
    {
      NW.del <- c(NW.del,DateDayTab[which(DateDayTab[,2] == dels),3])
    }

    if(nrow(H24_functional) == nrow(sleep.tab))
    {
      sleep.tab <- sleep.tab[-H24.dels,]
    }
    if(!is.null(DateDayTab))
    {
      DateDayTab <- DateDayTab[-H24.dels,]
      if(is.null(nrow(NW.tab)))
      {
        if(NW.del == NW.tab[1])
        {
          NW.tab <- NULL
        }
      }
      else
      {
        NW.tab <- NW.tab[-NW.del,]
      }
    }
    H24_functional <- H24_functional[-H24.dels,]
  }
  
  
  if(nrow(H24.BoutSum)<nrow(H24_functional))
  {
    if((nrow(H24_functional)-nrow(H24.BoutSum))==1)
    {
      print("Warning: different number of days")
      #print(H24_functional[nrow(H24_functional),])
      H24.BoutSum <- rbind(H24.BoutSum,rep.int(NA,ncol(H24.BoutSum)))
      Bout.Sum <- rbind(Bout.Sum ,rep.int(NA,ncol(Bout.Sum)))
    }
    else
    {
      print("ERROR: different number of days")
    }
  }
  Abs_SED_H <- as.numeric(unlist(H24_functional[,2]))+as.numeric(sleep.tab[,5])
  Abs_SED_H <- as.character(Abs_SED_H)
  Tot.Sed.T <- numeric(length = nrow(Bout.Sum))
  SED.wak.H <- numeric(length = nrow(Bout.Sum))
  temp.BoSum <- Bout.Sum[,c(1,2,3,6,7,9,10)]
  for(nrw in 1:nrow(Bout.Sum))
  {
    Tot.Sed.T[nrw] <- sum(as.numeric(as.character(unlist(temp.BoSum[nrw,]))))
    SED.wak.H[nrw] <- Tot.Sed.T[nrw]/60
  }
  Bout.Sum <- cbind(Bout.Sum,Tot.Sed.T,SED.wak.H)
  PercSleep <- numeric(length = nrow(Bout.Sum))
  Stand.LIPA <- numeric(length = nrow(Bout.Sum))
  P.SEDTot.H <- numeric(length = nrow(Bout.Sum))
  P.StandTot.H <- numeric(length = nrow(Bout.Sum))
  P.LIPATot.H <- numeric(length = nrow(Bout.Sum))
  P.StandLIPATot <- numeric(length = nrow(Bout.Sum))
  P.MVPATot.H <- numeric(length = nrow(Bout.Sum))
  P.SEDWkHrs <- numeric(length = nrow(Bout.Sum))
  P.StandWkHrs <- numeric(length = nrow(Bout.Sum))
  P.LIPAWkHrs <- numeric(length = nrow(Bout.Sum))
  P.StandLIPAWKHrs <- numeric(length = nrow(Bout.Sum))
  P.MVPAWkHrs <- numeric(length = nrow(Bout.Sum))
  
  Rnw <- nrow(NW.tab)
  if(is.null(nrow(NW.tab)))
  {
    if(length(NW.tab) == 3)
    {
      Rnw <- 1
    }
  }
  
  if(is.null(NW.tab))
  {
    if(nrow(DateDayTab) > 1)
    {
      DNr <- DateDayTab[which(DateDayTab[,3]==0),2]
      nullnw <- cbind.data.frame(DNr,rep.int(0,times = length(DNr)),rep.int(24,times = length(DNr)))
      colnames(nullnw)    <- colnames(NW.tab)
      NW.tab <- rbind.data.frame(nullnw,NW.tab)
      NW.tab <- NW.tab[order(NW.tab[,1],na.last = TRUE),]
    }
    else
    {
      DNr <- DateDayTab[which(DateDayTab[3]==0),2]
      NW.tab <- c(DNr,0,24)
      names(NW.tab)    <- c("Day","NWHrs","TotalHrs-NW")
    }
  }
  else
  {
    if(Rnw < nrow(Bout.Sum))
    {
      
      if(Rnw  == 1)
      {
        if(nrow(DateDayTab) <= 1)
        {
          if(DateDayTab[3] == NW.tab[1])
          {
            NW.tab[1] <- DateDayTab[2]
          }
          else
          {
            print("Possiple wrong allocation of Nonwear Time to the Day of measurement")
          }
        }
        else
        {
          if(is.null(NW.tab))
          {
            
          }
          NW.tab[1] <- DateDayTab[which(DateDayTab[,3] == NW.tab[1]),2]
        }
        
      }
      else
      {
        for(checkNw in NW.tab[,1])
        {
          if(length(which(DateDayTab[,3]==checkNw))<1)
          {
            NW.tab <- NW.tab[-checkNw,]
          }
        }
        for(krow in 1:nrow(DateDayTab))
        {
          if(nrow(DateDayTab)>0)
          {
            if(DateDayTab[krow,3] > 0)
            {
              NW.tab[which(NW.tab[,1]==DateDayTab[krow,3]),1] <- DateDayTab[krow,2]
            }
          }
        }
      }
      
      DNr <- DateDayTab[which(DateDayTab[,3]==0),2]
      nullnw <- cbind.data.frame(DNr,rep.int(0,times = length(DNr)),rep.int(24,times = length(DNr)))
      colnames(nullnw)    <- colnames(NW.tab)
      NW.tab <- rbind.data.frame(nullnw,NW.tab)
      NW.tab <- NW.tab[order(NW.tab[,1],na.last = TRUE),]
    }
  }
  
  if(nrow(Bout.Sum) > nrow(sleep.tab))
  {
    if(nrow(Bout.Sum) == (nrow(sleep.tab)+1))
    {
      sleep.tab <- rbind.data.frame(sleep.tab,rep.int(NA,times = ncol(sleep.tab)))
      if(nrow(Bout.Sum) ==(nrow(H24_functional)+1))
      {
        H24_functional <- rbind.data.frame(H24_functional,rep.int(NA,times = ncol(H24_functional)))
      }
      if(nrow(Bout.Sum) ==(nrow(NW.tab)+1))
      {
        NW.tab <- rbind.data.frame(NW.tab,rep.int(NA,times = ncol(NW.tab)))
      }
    }
    else
    {
      print(2174)
    }
  }
  
  if(!non.stop)
  {
    if(nrow(sleep.tab) > nrow(H24_functional))
    {
      sleep.tab <- sleep.tab[1:nrow(H24_functional),]
    }
  }
  
  for(nrw in 1:nrow(Bout.Sum))
  {
    #PercSleep[nrw] <- (1-(as.numeric(as.character(unlist(sleep.tab[nrw,6])))/(24-(as.numeric(as.character(unlist(NW.tab[nrw,2])))))))*100
    PercSleep[nrw] <- (1-(as.numeric(as.character(unlist(sleep.tab[nrw,6])))/24))*100
    Stand.LIPA[nrw] <- as.numeric(as.character(unlist(H24_functional[nrw,3])))+as.numeric(as.character(unlist(H24_functional[nrw,5])))
    P.SEDTot.H[nrw] <- (as.numeric(as.character(unlist(H24_functional[nrw,2])))/(TimeUnitMod*as.numeric(as.character(unlist(NW.tab[nrw,3])))))*100
    P.StandTot.H[nrw] <- (as.numeric(as.character(unlist(H24_functional[nrw,3])))/(TimeUnitMod*as.numeric(as.character(unlist(NW.tab[nrw,3])))))*100
    P.LIPATot.H[nrw] <- (as.numeric(as.character(unlist(H24_functional[nrw,5])))/(TimeUnitMod*as.numeric(as.character(unlist(NW.tab[nrw,3])))))*100
    P.StandLIPATot[nrw] <-(as.numeric(as.character(unlist(Stand.LIPA[nrw])))/(TimeUnitMod*as.numeric(as.character(unlist(NW.tab[nrw,3])))))*100
    P.MVPATot.H[nrw] <-(as.numeric(as.character(unlist(H24_functional[nrw,6])))/(TimeUnitMod*as.numeric(as.character(unlist(NW.tab[nrw,3])))))*100
    P.SEDWkHrs[nrw] <- (as.numeric(as.character(unlist(H24_functional[nrw,2])))/(TimeUnitMod*as.numeric(as.character(unlist(sleep.tab[nrw,6])))))*100
    P.StandWkHrs[nrw] <- (as.numeric(as.character(unlist(H24_functional[nrw,3])))/(TimeUnitMod*as.numeric(as.character(unlist(sleep.tab[nrw,6])))))*100
    P.LIPAWkHrs[nrw] <- (as.numeric(as.character(unlist(H24_functional[nrw,5])))/(TimeUnitMod*as.numeric(as.character(unlist(sleep.tab[nrw,6])))))*100
    P.StandLIPAWKHrs[nrw] <- (as.numeric(as.character(unlist(Stand.LIPA[nrw])))/(TimeUnitMod*as.numeric(as.character(unlist(sleep.tab[nrw,6])))))*100
    P.MVPAWkHrs[nrw] <- (as.numeric(as.character(unlist(H24_functional[nrw,6])))/(TimeUnitMod*as.numeric(as.character(unlist(sleep.tab[nrw,6])))))*100
    
  }
  
  Perc.tab <- cbind(PercSleep,Stand.LIPA,P.SEDTot.H,P.StandTot.H,P.LIPATot.H,P.StandLIPATot,P.MVPATot.H,
                    P.SEDWkHrs,P.StandWkHrs,P.LIPAWkHrs,P.StandLIPAWKHrs,P.MVPAWkHrs)
  colnames(Perc.tab) <- c("P.Sleep","Sta.Light","P.SED.H","P.Sta.H","P.Light.H","P.Sta.Light","P.MVPA.H","P.SED.Awake",
                          "P.Sta.Awake","P.Light.Awake","P.Sta.Light.Aw","P.MVPA.Awake")

  if(is.null(Abs_SED_H))
  {
    Abs_SED_H <- rep.int(NA, times = nrow(sleep.tab))
  }
  if(length(Abs_SED_H) < 1)
  {
    Abs_SED_H <- rep.int(NA, times = nrow(sleep.tab))
  }
  
  if(nrow(H24_functional)>1)
  {
    vec <- cbind(H24_functional[,1],weekdays(as.Date(H24_functional[,1], tz=getOption("tz"))),sleep.tab,NW.tab[,2:3],H24_functional[,2:ncol(H24_functional)],Abs_SED_H,
                 H24.BoutSum,Bout.Sum,Perc.tab)
    colnames(vec)[1:2] <- c("Date","Weekday")
  }
  else
  {
    vec <- cbind(H24_functional[,1],weekdays(as.Date(H24_functional[,1], tz=getOption("tz"))),sleep.tab,NW.tab[,2],sleep.tab,NW.tab[,3],H24_functional[,2:ncol(H24_functional)],Abs_SED_H,
                 H24.BoutSum,Bout.Sum,Perc.tab)
    colnames(vec)[1:2] <- c("Date","Weekday")
  }
  
  ### edited colnames
  
  edited.colNames <- c("Date","Weekday","Day","Rise_Time","Bed_Time","Sleep_Minutes","Sleep_Hrs","Hrs_Awake","Sleep_interrupt", "Nonwear_time", "TotalHrs-NW","SED_Total"
                       ,"Standing_Total","Stepping_Total","LPA_Total","MVPA_Total","MVPA_Tim","Steps_Total","Weartime_total","Abs_SED_H","Num_SED_lt5","Num_SED_5-10"
                       ,"Num_SED_10-20","Num_SED_lt20","Num_SED_gt20","Num_SED_20-30","Num_SED_30-40","Num_SED_gt40","Num_SED_40-60","Num_SED_gt60","Num_SED_gt90"
                       ,"SED_lt5","SED_5-10","SED_10-20","SED_lt20","SED_gt20","SED_20-30","SED_30-40","SED_gt40","SED_40-60","SED_gt60","SED_gt90","Tot_Sed_T","SED_wak_H"
                       ,"Pro_Sleep","Sta_Light","Pro_SED","Pro_Standing","Pro_LPA","P_Sta_Light","Pro_MVPA","Pro_SED_Awake","Pro_Standing_Awake","Pro_LPA_Awake","Pro_Standing_Light_Awake"
                       ,"Pro_MVPA_Awake")
  dom_genName <- c("DomainName","SED","Standing","Stepping","LPA","MVPA","MVPA_Tim","Steps","Weartime","Num_SED_lt5","Num_SED_5-10","Num_SED_10-20","Num_SED_lt20","Num_SED_gt20","Num_SED_20-30"
                   ,"Num_SED_30-40","Num_SED_gt40","Num_SED_40-60","Num_SED_gt60","Num_SED_gt90","SED_lt5","SED_5-10","SED_10-20","SED_lt20","SED_gt20","SED_20-30","SED_30-40","SED_gt40","SED_40-60"
                   ,"SED_gt60","SED_gt90")
  ###
  colnames(vec) <- edited.colNames
  if(!is.null(Domain.list))
  {
    doms <- as.numeric(unique(functional.list[[2]][,2]))
    for(day in 1:length(days))
    {
      domainNames <- Domain.list[day,seq(from=4,to=(ncol(Domain.list)),by=3)]
      domainNames <- as.character(unlist(domainNames))
      Bout.day.doms <- Bout.list[[1]][[2]][which(as.character(Bout.list[[1]][[2]][,1]) == days[day]), ]
      Bout.day.dom2 <- Bout.list[[2]][[2]][which(as.character(Bout.list[[2]][[2]][,1]) == days[day]), ]
      func.day.doms <- functional.list[[2]][which(as.character(functional.list[[2]][,1]) == days[day]),]
      #if(day == 1)
      #{
      #  print(Bout.day.doms)
      #  print(func.day.doms)
      #}
      for(i in doms)
      {
        if(length(doms)==1)
        {
          Bout.day.doms.vec <- Bout.day.doms[which(Bout.day.doms[,2]==i),]
          Bout.day.dom2.vec <- Bout.day.dom2[which(Bout.day.dom2[,2]==i),]
          func.day.doms.vec <- func.day.doms 
        }
        else
        {
          Bout.day.doms.vec <- Bout.day.doms[which(Bout.day.doms[,2]==i),]
          Bout.day.dom2.vec <- Bout.day.dom2[which(Bout.day.dom2[,2]==i),]
          func.day.doms.vec <- func.day.doms[which(func.day.doms[,2]==i),] 
        }
        
        if(nrow(Bout.day.doms.vec)==0)
        {
          dom.prt <- c(0,0,0,0,0,0,0,0,0,0,0)
        }
        else
        {
          dom.prt <- Bout.day.doms.vec[,3:ncol(Bout.day.doms.vec)]
        }
        if(nrow(Bout.day.dom2.vec)==0)
        {
          dom.prt2 <- c(0,0,0,0,0,0,0,0,0,0,0)
        }
        else
        {
          dom.prt2 <- Bout.day.dom2.vec[,3:ncol(Bout.day.dom2.vec)]
        }
        func.day.doms.vec <- func.day.doms.vec[3:length(func.day.doms.vec)]
        if(i == 1)
        {
          dom.vec <- as.vector(c(NA,func.day.doms.vec,dom.prt,dom.prt2),mode = "numeric")
          cname <- paste("D",i,"_",dom_genName,sep="")
        }
        else
        {
          dom.vec <- as.vector(c(dom.vec,NA,func.day.doms.vec,dom.prt,dom.prt2),mode = "numeric")
          cname <- c(cname,paste("D",i,"_",dom_genName,sep=""))
        }
      }
      if(day==1)
      {
        day.tab <- rbind.data.frame(dom.vec,dom.vec)
      }
      else
      {
        day.tab <- rbind.data.frame(day.tab,dom.vec)
      }
    }
    day.tab <- day.tab[2:nrow(day.tab),]
    colnames(day.tab) <- cname
    if(length(H24.dels) > 0)
    {
      day.tab <- day.tab[-H24.dels,]
    }
    vec <- cbind.data.frame(vec,day.tab)
    H24_functional <- functional.list[[3]]
    Bout.H24.Sum <- Bout.list[[1]][[5]]
    
    if(is.null(nrow(H24_functional)))
    {
      days <- H24_functional[1]
      Rh24Func <- 1
    }
    else
    {
      days <- H24_functional[,1]
      Rh24Func <- nrow(H24_functional)
    }
    if(ncol(Bout.H24.Sum) == 1)
    {
      Bout.H24.Sum <- t(Bout.H24.Sum)
    }
    H24.BoutSum <- Bout.H24.Sum[,2:ncol(Bout.H24.Sum)]

    if(ncol(Bout.list[[2]][[5]])==1)
    {
      Bout.Sum <- t(Bout.list[[2]][[5]][2:ncol(Bout.H24.Sum),])
    }
    else
    {
      Bout.Sum <- Bout.list[[2]][[5]][,2:ncol(Bout.H24.Sum)]
    }
    Tot.Sed.T <- numeric(length = nrow(Bout.Sum))
    SED.wak.H <- numeric(length = nrow(Bout.Sum))
    temp.BoSum <- Bout.Sum[,c(1,2,3,6,7,9,10)]
    for(nrw in 1:nrow(Bout.Sum))
    {
      if(nrow(Bout.Sum)==1)
      {
        Tot.Sed.T[nrw] <- sum(as.numeric(as.character(temp.BoSum)))
      }
      else
      {
        Tot.Sed.T[nrw] <- sum(as.numeric(as.character(unlist(temp.BoSum[nrw,]))))
      }
      
      SED.wak.H[nrw] <- Tot.Sed.T[nrw]/60
    }
    Bout.Sum <- cbind(Bout.Sum,Tot.Sed.T,SED.wak.H)
    if(length(H24.dels) > 0)
    {
      H24_functional <- H24_functional[-H24.dels,]
    }
    
    if(is.null(nrow(H24_functional)))
    {
      days <- H24_functional[1]
      Rh24Func <- 1
    }
    else
    {
      days <- H24_functional[,1]
      Rh24Func <- nrow(H24_functional)
    }
    
    if((nrow(Bout.Sum)+1) == Rh24Func)
    {
      Bout.Sum <- rbind.data.frame(Bout.Sum,rep.int(0,times = ncol(Bout.Sum)))
      if(is.null(ncol(H24.BoutSum)))
      {
        H24.BoutSum <- rbind.data.frame(H24.BoutSum,rep.int(0,times = length(H24.BoutSum)))
      }
      else
      {
        H24.BoutSum <- rbind.data.frame(H24.BoutSum,rep.int(0,times = ncol(H24.BoutSum)))
      }
      
    }
    
    if(Rh24Func < nrow(Bout.Sum))
    {
      del.Bout <- which(is.na(H24.BoutSum[,1]))
      if(length(del.Bout) <= 0)
      {
        Sum.vec <-NULL
        for(rBsum in 1:nrow(Bout.Sum))
        {
          rSum <-NULL
          for(cBsum in 1:ncol(Bout.Sum))
          {
            rSum <- c(rSum,as.numeric(as.character(Bout.Sum[rBsum,cBsum])))
          }
          Sum.vec <- c(Sum.vec,sum(rSum,na.rm = TRUE))
        }
        del.Bout <- which(Sum.vec <= 0)
      }
      H24.BoutSum <- H24.BoutSum[-del.Bout,]
      Bout.Sum <- Bout.Sum[-del.Bout,]
      
      
    }
    
    if(Rh24Func == 1)
    {
      Misc.tab <-c(H24_functional[2:length(H24_functional)],H24.BoutSum,Bout.Sum[1:(length(Bout.Sum)-2)])
      names(Misc.tab) <- paste("Misc_",dom_genName[2:length(dom_genName)],sep="")
    }
    else
    {
      if(is.null(nrow(H24.BoutSum)))
      {
        Misc.tab <-c(H24_functional[2:length(H24_functional)],H24.BoutSum,Bout.Sum[,1:(length(Bout.Sum)-2)])
        names(Misc.tab) <- paste("Misc_",dom_genName[2:length(dom_genName)],sep="")
      }
      else
      {
        Misc.tab <-cbind(H24_functional[,2:ncol(H24_functional)],H24.BoutSum,Bout.Sum[,1:(ncol(Bout.Sum)-2)])
        colnames(Misc.tab) <- paste("Misc_",dom_genName[2:length(dom_genName)],sep="")
      }
      
    }
    if(nrow(vec) > 1)
    {
      vec <- cbind(vec,Misc.tab)
    }
    else
    {
      temp1 <- rbind.data.frame(vec,vec)
      temp2 <- rbind.data.frame(Misc.tab,Misc.tab)
      colnames(temp2) <- paste("Misc_",dom_genName[2:length(dom_genName)],sep="")
      vec <- cbind.data.frame(temp1,temp2)
      vec <- vec[1,]
      temp1 <- NULL
      temp2 <- NULL
    }
    
  }
  vec <- cbind(rep.int(as.character(id.Nr),times = nrow(vec)),vec)
  
  if(!is.null(Domain.list))
  {
    unq.doms <- unique(as.Date(Domain.list[,1], tz=getOption("tz")))
    unq.dates <- unique(as.Date(vec[,2], tz=getOption("tz")))
    del.doms <- NULL
    for(i in 1:length(unq.doms))
    {
      if(length(which(unq.dates==unq.doms[i]))<1)
      {
        del.doms <- c(del.doms,which(as.Date(Domain.list[,1], tz=getOption("tz"))==unq.doms[i]))
      }
    }
    if(!is.null(del.doms))
    {
      Domain.list <- Domain.list[-del.doms,]
    }
    doms.Cnames <- Domain.list[,seq(from=4,to=ncol(Domain.list),by=3)]
    if(ncol(Domain.list)==4)
    {
      doms.Cnames <- as.vector(doms.Cnames,mode = "character")
      doms.Cnames[which(doms.Cnames==" ")] <- rep.int("unknown", times = length(which(doms.Cnames==" ")))
      for(cNr in 1:length(doms.Cnames))
      {
        vec[cNr,58] <- doms.Cnames[cNr]
      }
    }
    else
    {
      last.DomNameNr <- 58+(31*length(doms.Cnames))
      cNR.names <- seq(from=58,to=last.DomNameNr, by=31)
      for(cNr in 1:length(doms.Cnames))
      {
        vec[,cNR.names[cNr]] <- doms.Cnames[cNr]
      }
    }
  }
  
  ### delete not-needed coloums
  vec <- vec[,-which((colnames(vec)=="TotalHrs-NW" | grepl("MVPA_Tim",colnames(vec)) | grepl("Abs_SED_H",colnames(vec))
                      | colnames(vec)=="Tot_Sed_T" | colnames(vec)=="SED_wak_H" | colnames(vec)=="Sta_Light" | colnames(vec)=="P_Sta_Light"))]
  ###
  for(i in 1:ncol(vec))
  {
    vec[,i] <- gsub('.',dec,vec[,i],fixed = TRUE)
  }
  
  #vec <- cbind.data.frame(as.numeric(vec[,1]),vec[,c(2,3)],as.numeric(vec[,4:ncol(vec)]))
  if(WriteMastersheet)
  {
    if(save_as_xlsx)
    {
      file.absPath <- paste(outpath,as.character(id.Nr),"_Mastersheets.xlsx",sep="")
      write.xlsx(vec, file.absPath, sheetName=paste(id.Nr,"Mastersheet",sep="_"),  col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
    }
    else
    {
      file.absPath <- paste(outpath,as.character(id.Nr),"_Mastersheets.csv",sep="")
      write.table(vec,file=file.absPath,row.names = F, sep=separator, dec=dec)
    }
    
    
  }
  return(vec)
}

################ Run Functions ##################



complete_run <- function(input,output,ACT.ALL.out,domainCsv=NULL,NW_path=NULL,early="20:00:00", late="12:00:00",
                         max.break=15,min.sleeplength=45,id.length=8,id.start=1,separated.csv=TRUE, 
                         JoinedDomains=NULL,JoinedNWs=NULL, separator=",", dec=".", fill_sleepTab=FALSE,
                         day.begin="00:00:00",time.unit="hours",mvpa_steps_cutoff=25, WriteMastersheet=FALSE,
                         is.xlsx=FALSE, sheetIndex=1, calcBySteps=TRUE, mvpa_count_thresh=2296, save_as_xlsx=FALSE, non.stop=TRUE,id.asnumeric=FALSE,
                         sleepMode="setTimes", sleep.start="22:00:00", sleep.end="08:00:00", is.StartAfterMidNight=FALSE, sleep.file=NULL,
                         PrintMode=2)
  # runs all analysis function for 1 id and ActivPal-dataset and returns a Mastersheet file
{
  #print(Sys.time())
  #print(input)
  if(id.asnumeric)
  {
    id.NR <- as.numeric(getID(input,id.length, id.start = id.start))
  }
  else
  {
    id.NR <- as.character(getID(input,id.length, id.start = id.start))
  }
  
  x <- NULL
  y <- NULL
  if(!is.null(NW_path) || !is.null(JoinedNWs))
  {
    x <- get_NWforID(path=NW_path,separated.csv=separated.csv, JoinedNWs=JoinedNWs, id=id.NR, separator=separator, dec=dec,is.xlsx=is.xlsx, sheetIndex=sheetIndex)
  }
  if(!is.null(domainCsv) || !is.null(JoinedDomains))
  {
    y <- getDomainsforID(path=domainCsv, separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=id.NR, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
  }
  
  if(is.null(x))
  {
    NW_path   <- NULL
    JoinedNWs <- NULL
  }
  else
  {
    if(nrow(x)<1)
    {
      NW_path   <- NULL
      JoinedNWs <- NULL
    }
  }
  
  if(is.null(y))
  {
    domainCsv     <- NULL
    JoinedDomains <- NULL
  }
  else
  {
    if(nrow(y)<1)
    {
      domainCsv     <- NULL
      JoinedDomains <- NULL
    }
  }
  if(PrintMode>0)
  {
    print(paste("prcosessing ID:",id.NR,sep=" "))
  }
  SED_ACT_ALL <- FN_CPAHR_SED_ACT_ALL(input,output=output,Threshold_static=Inf,is.csvout=ACT.ALL.out
                                      ,ID.nr=id.NR, separator=separator, dec=dec, save_as_xlsx=save_as_xlsx,PrintMode=PrintMode)
  if(PrintMode>1)
  {
    print(paste("FN_CPAHR_SED_ACT_ALL ... finished",Sys.time(),sep = " ... "))
  }
  if(length(unique(as.Date(SED_ACT_ALL[,1], tz=getOption("tz")))) < 2)
  {
    non.stop <- FALSE
  }
  if(!non.stop)
  {
    Sleep_tab <- NullslpTab(SED_ACT_ALL)
    fill_sleepTab <- FALSE
  }
  else
  {
    Sleep_tab <- create.sleepTab(SED_ACT_ALL,earliest=early,latest=late,max.break=max.break,min.sleeplength=min.sleeplength,sleepMode=sleepMode,
                                 sleep.start=sleep.start, sleep.end=sleep.end, is.StartAfterMidNight=is.StartAfterMidNight, sleep.file=sleep.file,separated.csv=separated.csv,
                                 idNr=id.NR,separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex,idlength=length)
    SED_ACT_ALL <- delete_sleeptime(SED_ACT_ALL,night = early, morning = late, break.lim=max.break,min.sleeplength=min.sleeplength,sleepMode=sleepMode,
                                    sleep.start=sleep.start, sleep.end=sleep.end, is.StartAfterMidNight=is.StartAfterMidNight, sleep.file=sleep.file,separated.csv=separated.csv,
                                    idNr=id.NR,separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex,idlength=length)
    #print(paste("delete_sleeptime ... finished",Sys.time(),sep = " ... "))
  }
  if(PrintMode>1)
  {
    print(paste("create.sleepTab ... finished",Sys.time(),sep = " ... "))
  }
  
  if(is.null(NW_path) && is.null(JoinedNWs))
  {
    if(PrintMode>1)
    {
      print("no Nws imported")
    }
    pNW.tab <- NUll.NWtab(SED_ACT_ALL)
  }
  else
  {
    pNW.tab <- getNWtable(SED_ACT_ALL,NW_path, separated.csv=separated.csv, JoinedNWs=JoinedNWs, id=id.NR, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
    SED_ACT_ALL <- delete_NW(SED_ACT_ALL,NW_path, separated.csv=separated.csv, JoinedNWs=JoinedNWs, id=id.NR, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
  }
  if(PrintMode>1)
  {
    print(paste("NW.table ... created",Sys.time(),sep = " ... "))
  }
  
  if(is.null(domainCsv))
  {
    if(is.null(JoinedDomains))
    {
      if(PrintMode>1)
      {
        print(paste("Number of Domains per Day: 0"))
      }
      Domain_list <- NULL
      misc <- NULL
    }
    else
    {
      temp.Doms <- getDomainsforID(domainCsv,separated.csv=FALSE, JoinedDomains=JoinedDomains, id=id.NR, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
      DailyDomains <- ncol(temp.Doms)
      DailyDomains <- (DailyDomains-1)/3
      if(PrintMode>2)
      {
        print(paste("Number of Domains per Day: ",DailyDomains,sep = ""))
      }
      if(is.null(temp.Doms))
      {
        domainCsv <- NULL
        JoinedDomains <- NULL
        Domain_list <- NULL
        misc <- NULL
      }
      else
      {
        Domain_list <- Part_to_Domain(domainCsv,SED_ACT_ALL,separated.csv=FALSE, JoinedDomains=JoinedDomains, id=id.NR, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
        if(PrintMode>2)
        {
          print(paste("Part_to_Domain ... finished",Sys.time(),sep = " ... "))
        }
        misc <- Misc_Domain(domainCsv, SED_ACT_ALL,separated.csv=FALSE, JoinedDomains=JoinedDomains, id=id.NR, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
        if(PrintMode>2)
        {
          print(paste("Misc_Domain ... finished",Sys.time(),sep = " ... "))
        }
      }
      temp.Doms <- NULL
    }
  }
  else
  {
    temp.Doms <- getDomainsforID(domainCsv,separated.csv=FALSE, JoinedDomains=JoinedDomains, id=id.NR, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
    DailyDomains <- ncol(temp.Doms)
    DailyDomains <- (DailyDomains-1)/3
    if(PrintMode>2)
    {
      print(paste("Number of Domains per Day: ",DailyDomains,sep = ""))
    }
    if(is.null(temp.Doms))
    {
      domainCsv <- NULL
      JoinedDomains <- NULL
      Domain_list <- NULL
      misc <- NULL
    }
    else
    {
      Domain_list <- Part_to_Domain(domainCsv,SED_ACT_ALL,separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=id.NR, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
      if(PrintMode>2)
      {
        print(paste("Part_to_Domain ... finished",Sys.time(),sep = " ... "))
      }
      misc <- Misc_Domain(domainCsv, SED_ACT_ALL,separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=id.NR, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
      if(PrintMode>2)
      {
        print(paste("Misc_Domain ... finished",Sys.time(),sep = " ... "))
      }
    }
    temp.Doms <- NULL

  }
  
  Bout_ls<-Sed_Bout_Length(SED_ACT_ALL,Domain_list,misc,DailyDomains=DailyDomains,DayCut=day.begin)
  if(PrintMode>1)
  {
    print(paste("Sed_Bout_Length ... finished",Sys.time(),sep = " ... "))
  }
  Sleep_tab <- deleteWrongSlpDates(Sleep_tab=Sleep_tab,Bout.list=Bout_ls)
  
  activPal<-read_activPAL(inputpath = input)
  unq.dat1 <- unique(as.Date(activPal[,2], tz=getOption("tz")))
  unq.dat2 <- unique(as.Date(SED_ACT_ALL[,1], tz=getOption("tz")))
  unq.dat2 <- unique(as.Date(SED_ACT_ALL[,1], tz=getOption("tz")))
  del.Days.wo.SED <- NULL
  if(PrintMode>3)
  {
    print(unq.dat1)
  }
  for(i in 1:length(unq.dat1)) #delete all day where no sedentary is measured, whole day notworn is 24H standing
  {
    if(length(which(unq.dat2 == unq.dat1[i]))<1)
    {
      if(is.null(del.Days.wo.SED))
      {
        del.Days.wo.SED <- which(as.Date(activPal[,2], tz=getOption("tz")) == unq.dat1[i])
      }
      else
      {
        del.Days.wo.SED <- c(del.Days.wo.SED,which(as.Date(activPal[,2], tz=getOption("tz")) == unq.dat1[i]))
      }
    }
  }
  if(!is.null(del.Days.wo.SED))
  {
    activPal <- activPal[-del.Days.wo.SED,]
  }
  if(nrow(activPal)< 2)
  {
    print(id.NR)
  }
  if(fill_sleepTab)
  {
    Sleep_tab <- adjust_sleeptab(sleeptab = Sleep_tab,day.begin=day.begin,SED_ACT_ALL = SED_ACT_ALL,early="15:00:00",Activpal_data = activPal)
  }
  if(PrintMode>2)
  {
    print("reworked sleep and nonwear")
  }
  functional_ls<-hour_functional(activPal,domainCsv,Sleep_tab,NW_path=NW_path, separated.csv=separated.csv, JoinedDomains=JoinedDomains, JoinedNWs=JoinedNWs, id=id.NR, separator=separator, dec=dec,time.unit=time.unit,mvpa_steps_cutoff=mvpa_steps_cutoff
                                 , is.xlsx=is.xlsx, sheetIndex=sheetIndex, calcBySteps=calcBySteps, mvpa_count_thresh=mvpa_count_thresh, non.stop=non.stop)
  if(PrintMode>1)
  {
    print(paste("hour_functional ... finished",Sys.time(),sep = " ... "))
  }
  DateDayTab <- createDateDay(functional_ls,NW_path=NW_path,JoinedNWs=JoinedNWs, idNr=id.NR, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
  ka <- create_Mastersheet(Bout_ls,functional_ls,sleep.tab=Sleep_tab,NW.tab=pNW.tab,Domain.path=domainCsv,outpath=output,
                           id.Nr=id.NR,separated.csv=separated.csv, JoinedDomains=JoinedDomains, separator=separator, dec=dec,WriteMastersheet=WriteMastersheet
                           , is.xlsx=is.xlsx, sheetIndex=sheetIndex, time.unit = time.unit, save_as_xlsx=save_as_xlsx, DateDayTab=DateDayTab, non.stop=non.stop)
  if(PrintMode>1)
  {
    print(paste("create_Mastersheet ... finished",Sys.time(),sep = " ... "))
  }
  return(ka)
}

MasterSheet.lapply <- function(accfile,output,ACT.ALL.out,domainCsv=NULL,NW_path,early="20:00:00", late="12:00:00",max.break=15,min.sleeplength=45,id.length,id.start=1,separated.csv=TRUE, JoinedDomains=NULL
                               ,JoinedNWs=NULL, separator=",", dec=".",fill_sleepTab=FALSE, day.begin="00:00:00",time.unit="hours",mvpa_steps_cutoff=25,WriteMastersheet=FALSE, is.xlsx=FALSE, sheetIndex=1,
                               calcBySteps=TRUE, mvpa_count_thresh=2296, save_as_xlsx=FALSE, non.stop=TRUE,id.asnumeric=FALSE,sleepMode="setTimes", sleep.start="22:00:00", sleep.end="08:00:00", 
                               is.StartAfterMidNight=FALSE, sleep.file=NULL,PrintMode=2) 
  # Loads supportfiles and starts complete_run function
{
  masterSheet <- NULL
  acc.file <- get_filename(accfile)
  if(!is.null(domainCsv))
  {
    if(separated.csv)
    {
      Dom.files <- list.files(path=domainCsv, pattern="\\.csv$")
    }
  }
  
  if(is.null(NW_path))
  {
    acc.ID <- substring(acc.file,first = 1,last = id.length)
    if(is.null(domainCsv))
    {
      ID.DomFile <- NULL
    }
    else
    {
      if(separated.csv)
      {
        Dom.names <- substring(Dom.files,first = 1,last = id.length)
        Dom.files <- paste(domainCsv,Dom.files,sep="")
        ID.DomFile <- Dom.files[which(Dom.names==acc.ID)]
      }
      else
      {
        ID.DomFile <- domainCsv
      }
    }
    if(length(ID.DomFile)<=1)
    {
      masterSheet <- complete_run(accfile,output,ACT.ALL.out=ACT.ALL.out,ID.DomFile,NW_path=NULL,early="20:00:00", late="12:00:00",
                                  max.break=15,min.sleeplength=45,id.length=id.length, id.start = id.start,separated.csv=separated.csv,
                                  JoinedDomains=JoinedDomains,JoinedNWs=JoinedNWs, separator=separator, dec=dec, fill_sleepTab=fill_sleepTab,
                                  day.begin=day.begin,time.unit=time.unit,mvpa_steps_cutoff=mvpa_steps_cutoff, WriteMastersheet=WriteMastersheet, is.xlsx=is.xlsx, sheetIndex=sheetIndex,
                                  calcBySteps=calcBySteps, mvpa_count_thresh=mvpa_count_thresh, save_as_xlsx=save_as_xlsx, non.stop=non.stop,id.asnumeric=id.asnumeric,
                                  sleepMode=sleepMode, sleep.start=sleep.start, sleep.end=sleep.end, is.StartAfterMidNight=is.StartAfterMidNight, sleep.file=sleep.file,PrintMode=PrintMode)
    }
    else
    {
      print(paste("unequal 1 DomainFiles found for the ID",acc.ID ,sep=": "))
    }
  }
  else
  {
    acc.ID <- substring(acc.file,first = 1,last = id.length)
    
    if(is.null(domainCsv))
    {
      ID.DomFile <- NULL
    }
    else
    {
      if(separated.csv)
      {
        Dom.names <- substring(Dom.files,first = 1,last = id.length)
        Dom.files <- paste(domainCsv,Dom.files,sep="")
        ID.DomFile <- Dom.files[which(Dom.names==acc.ID)]
      }
      else
      {
        ID.DomFile <- domainCsv
      }
    }
    
    if(separated.csv)
    {
      NW.files  <- list.files(path=NW_path, pattern="\\.csv$")
      NW.names <- substring(NW.files,first = 1,last = id.length)
      NW.files <- paste(NW_path,NW.files,sep="")
      ID.NWFile <- NW.files[which(NW.names==acc.ID)]
    }
    else
    {
      ID.NWFile <- NW_path
    }
    
    if(length(ID.NWFile)==1)
    {
      if(length(ID.DomFile)<=1)
      {
        masterSheet <- complete_run(accfile,output,ACT.ALL.out,ID.DomFile,NW_path=ID.NWFile,early="20:00:00", late="12:00:00",
                                    max.break=15,min.sleeplength=45,id.length=id.length,id.start = id.start
                                    ,separated.csv=separated.csv, JoinedDomains=JoinedDomains,JoinedNWs=JoinedNWs, separator=separator, dec=dec
                                    ,fill_sleepTab=fill_sleepTab,day.begin=day.begin,time.unit=time.unit,mvpa_steps_cutoff=mvpa_steps_cutoff, WriteMastersheet=WriteMastersheet
                                    , is.xlsx=is.xlsx, sheetIndex=sheetIndex, calcBySteps=calcBySteps, mvpa_count_thresh=mvpa_count_thresh, save_as_xlsx=save_as_xlsx, non.stop=non.stop
                                    ,id.asnumeric=id.asnumeric,sleepMode=sleepMode, sleep.start=sleep.start, sleep.end=sleep.end, 
                                    is.StartAfterMidNight=is.StartAfterMidNight, sleep.file=sleep.file,PrintMode=PrintMode)
      }
      else
      {
        print(paste("more than 1 Domain-File found for the ID",acc.ID ,sep=": "))
      }
    }
    else
    {
      print(paste("more than 1 NonWear-File found for the ID",acc.ID ,sep=": "))
    }
  }
  return(masterSheet)
}

batch_run <- function(input,output,ACT.ALL.out=FALSE,domainCsv=NULL,NW_path=NULL,early="20:00:00", late="12:00:00",max.break=15,
                      min.sleeplength=45,id.length,id.start=1,separated.csv=TRUE, JoinedDomains=NULL,JoinedNWs=NULL, 
                      separator=",", dec=".", run.from=1, run.to=NULL,fill_sleepTab=FALSE, day.begin="00:00:00",time.unit="hours", mvpa_steps_cutoff=25, WriteMastersheet_perID=FALSE
                      , is.xlsx=FALSE, sheetIndex=1, calcBySteps=TRUE, mvpa_count_thresh=2296,save_as_xlsx=FALSE, non.stop=TRUE,id.asnumeric=FALSE,
                      sleepMode="setTimes", sleep.start="22:00:00", sleep.end="08:00:00",is.StartAfterMidNight=FALSE, sleep.file=NULL,PrintMode=2)
  # Runs MasterSheet.lapply for all Ids and merges the Mastersheetfiles together
{
  if(is.null(domainCsv))
  {
    if(!separated.csv)
    {
      if(!is.null(NW_path))
      {
        if(is.null(JoinedNWs))
        {
          if(is.xlsx)
          {
            JoinedNWs <- xlsxToTableFormat(NW_path, sheetIndex=sheetIndex)
          }
          else
          {
            JoinedNWs <- readNWs.joined(NW_path,separator=separator, dec=dec)
          }
        }
      }
    }
  }
  else
  {
    if(!separated.csv)
    {
      if(is.null(JoinedDomains))
      {
        if(is.xlsx)
        {
          JoinedDomains <- xlsxToTableFormat(domainCsv, sheetIndex=sheetIndex)
        }
        else
        {
          JoinedDomains <- readDomains.joined(domainCsv,separator=separator, dec=dec)
        }
      }
      if(!is.null(NW_path))
      {
        if(is.null(JoinedNWs))
        {
          if(is.xlsx)
          {
            JoinedNWs <- xlsxToTableFormat(NW_path, sheetIndex=sheetIndex)
          }
          else
          {
            JoinedNWs <- readNWs.joined(NW_path,separator=separator, dec=dec)
          }
        }
      }
    }
  }
  Master.Sheet <- NULL
  acc.list <- list.files(path=input, pattern="\\.csv$")
  acc.list <- paste(input,acc.list,sep="")
  
  if(is.null(run.to))
  {
    run.to <- length(acc.list)
  }
  acc.list <- acc.list[run.from:run.to]
  
  if(PrintMode>1)
  {
    print("Start MasterSheet.lapply")
  }
  for(i in 1:length(acc.list))
  {
    if(i == 1)
    {
      if(PrintMode>0)
      {
        print("---------------------------")
        print(paste("started processing",length(acc.list),"files","at",Sys.time(),sep=" "))
        print("---------------------------")
      }
      Master.Sheet <- MasterSheet.lapply(acc.list[i],output=output,ACT.ALL.out=ACT.ALL.out,domainCsv=domainCsv,
                                         NW_path=NW_path,early="20:00:00", late="12:00:00",max.break=15,min.sleeplength=45,
                                         id.length=id.length,id.start=id.start,separated.csv=separated.csv, JoinedDomains=JoinedDomains,
                                         JoinedNWs=JoinedNWs, separator=separator, dec=dec,fill_sleepTab=fill_sleepTab,day.begin=day.begin
                                         ,time.unit=time.unit, mvpa_steps_cutoff=mvpa_steps_cutoff, WriteMastersheet=WriteMastersheet_perID
                                         , is.xlsx=is.xlsx, sheetIndex=sheetIndex, calcBySteps=calcBySteps, mvpa_count_thresh=mvpa_count_thresh,
                                         save_as_xlsx=save_as_xlsx, non.stop=non.stop,id.asnumeric=id.asnumeric, sleepMode=sleepMode, 
                                         sleep.start=sleep.start, sleep.end=sleep.end, is.StartAfterMidNight=is.StartAfterMidNight, sleep.file=sleep.file,PrintMode=PrintMode)
    }
    else
    {
      Master.Sheet <- bind_rows(Master.Sheet,MasterSheet.lapply(acc.list[i],output=output,ACT.ALL.out=ACT.ALL.out,domainCsv=domainCsv,
                                                                NW_path=NW_path,early="20:00:00", late="12:00:00",max.break=15,
                                                                min.sleeplength=45,id.length=id.length,id.start=id.start,separated.csv=separated.csv, JoinedDomains=JoinedDomains,
                                                                JoinedNWs=JoinedNWs, separator=separator, dec=dec,fill_sleepTab=fill_sleepTab,day.begin=day.begin
                                                                ,time.unit=time.unit,mvpa_steps_cutoff=mvpa_steps_cutoff, WriteMastersheet=WriteMastersheet_perID
                                                                , is.xlsx=is.xlsx, sheetIndex=sheetIndex, calcBySteps=calcBySteps, mvpa_count_thresh=mvpa_count_thresh,
                                                                save_as_xlsx=save_as_xlsx, non.stop=non.stop,id.asnumeric=id.asnumeric,sleepMode=sleepMode, sleep.start=sleep.start, 
                                                                sleep.end=sleep.end, is.StartAfterMidNight=is.StartAfterMidNight, sleep.file=sleep.file,PrintMode=PrintMode))
    }
    if(PrintMode>0)
    {
      print("---------------------------")
      print(paste("File",i,"of",length(acc.list),"processed","at",Sys.time(),sep=" "))
      print("---------------------------")
    }
  }
  colnames(Master.Sheet)[1] <- c("ID")
  
  temp.MasterSheet <- Master.Sheet[,1:6]
  for(i in 7:ncol(Master.Sheet))
  {
    if(length(grep("DomainName",colnames(Master.Sheet)[i]))>0)
    {
      temp.MasterSheet <- cbind.data.frame(temp.MasterSheet,as.character(Master.Sheet[,i]))
    }
    else
    {
      temp.MasterSheet <- cbind.data.frame(temp.MasterSheet,as.numeric(as.character(Master.Sheet[,i])))
    }
    
  }
  colnames(temp.MasterSheet) <- colnames(Master.Sheet)
  Master.Sheet <- temp.MasterSheet
  
  if(!is.null(output))
  {
    if(save_as_xlsx)
    {
      filename <- paste(output,"comp_Mastersheet.xlsx",sep="")
      write.xlsx(Master.Sheet, file = filename, sheetName="Mastersheet",  col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
    }
    else
    {
      filename <- paste(output,"comp_Mastersheet.csv",sep="")
      write.table(Master.Sheet,file = filename, row.names = F, sep=separator, dec=dec)
    }
    
  }
  return(Master.Sheet)
}

generate_supportfiles <- function(Acc.input, Domain.out=NULL, NW.out=NULL, id.length,id.start=1, separated=TRUE, separator=",", dec=".", save_as_xlsx=FALSE,
                                  Slp.out=NULL)
  # function to create nonwear-time and domain-supportfiles for all ActivPal-datasets 
{
  acc.list <- list.files(path=Acc.input, pattern="\\.csv$")
  if(separated)
  {
    for(i in 1:length(acc.list))
    {
      acc.file <- get_filename(acc.list[i])
      acc.ID <- substring(acc.file,first = id.start,last = (id.start+id.length))
      acc.absPath <- paste(Acc.input,acc.list[i],sep="")

      if(!is.null(Domain.out))
      {
        Domain.path <- paste(Domain.out,acc.ID,"_Domains.csv",sep = "")
        create_DomainCSV(path=Domain.path,accdata=acc.absPath, separator=separator, dec=dec)
      }
      if(!is.null(NW.out))
      {
        NW.path <- paste(NW.out,acc.ID,"_NonWearTimes.csv",sep = "")
        createNWcsv(path=NW.path,acc.absPath, separator=separator, dec=dec)
      }
      if(!is.null(Slp.out))
      {
        if(save_as_xlsx)
        {
          Slp.path <- paste(Slp.out,acc.ID,"_Sleep.xlsx",sep = "")
          createSleepCsv(path=Slp.path,accdata=acc.absPath, separator=separator, dec=dec, type="xlsx")
        }
        else
        {
          Slp.path <- paste(Slp.out,acc.ID,"_Sleep.csv",sep = "")
          createSleepCsv(path=Slp.path,accdata=acc.absPath, separator=separator, dec=dec, type="csv")
        }
      }
    }
  }
  else
  {
    DomFile <- NULL
    NWFile <- NULL
    SlpFile <- NULL
    #Domain.path <- paste(Domain.out,"Domains.csv",sep = "")
    #NW.path <- paste(NW.out,"NonWearTimes.csv",sep = "")
    Domain.path <- Domain.out
    NW.path <- NW.out
    for(i in 1:length(acc.list))
    {
      acc.file <- get_filename(acc.list[i])
      acc.ID <- substring(acc.file,first = id.start,last = (id.start+id.length-1))
      acc.absPath <- paste(Acc.input,acc.list[i],sep="")
      if(!is.null(Domain.path))
      {
        Doms <- create_DomainCSV(path=NULL,accdata=acc.absPath, separator=separator, dec=dec)
        Doms <- cbind(rep.int(acc.ID,times = nrow(Doms)),Doms)
        colnames(Doms)[1] <- "ID"
      }
      if(!is.null(NW.path))
      {
        NWs <- createNWcsv(path=NULL,acc.absPath, separator=separator, dec=dec)
        NWs <- cbind(rep.int(acc.ID,times = nrow(NWs)),NWs)
        colnames(NWs)[1] <- "ID"
      }
      if(!is.null(Slp.out))
      {
        Slps <- createSleepCsv(path=NULL,accdata=acc.absPath, separator=separator, dec=dec, type="csv")
        Slps <- cbind(rep.int(acc.ID,times = nrow(Slps)),Slps)
        colnames(Slps)[1] <- "ID"
      }
      if(i==1)
      {
        if(!is.null(Domain.path))
        {
          DomFile <- Doms
        }
        if(!is.null(NW.path))
        {
          NWFile <- NWs
        }
        if(!is.null(Slp.out))
        {
          SlpFile <- Slps
        }
      }
      else
      {
        if(!is.null(Domain.path))
        {
          DomFile <- rbind(DomFile,Doms)
        }
        if(!is.null(NW.path))
        {
          NWFile <- rbind(NWFile,NWs)
        }
        if(!is.null(Slp.out))
        {
          SlpFile <- rbind(SlpFile,Slps)
        }
      }
    }
    if(save_as_xlsx)
    {
      if(!is.null(Domain.path))
      {
        write.xlsx(DomFile,file=Domain.path,row.names = FALSE)
      }
      if(!is.null(NW.path))
      {
        write.xlsx(NWFile,file = NW.path,row.names = FALSE)
      }
      if(!is.null(Slp.out))
      {
        write.xlsx(SlpFile,file = Slp.out,row.names = FALSE)
      }
    }
    else
    {
      if(!is.null(Domain.path))
      {
        write.table(DomFile,file = Domain.path,row.names = FALSE, sep=separator, dec=dec)
      }
      if(!is.null(NW.path))
      {
        write.table(NWFile,file = NW.path,row.names = FALSE, sep=separator, dec=dec)
      }
      if(!is.null(Slp.out))
      {
        write.table(SlpFile,file = Slp.out,row.names = FALSE, sep=separator, dec=dec)
      }
    }
    
  }
}

Individual_Avg_data <- function(mastersheet, outfile=NULL, separator=",", dec=".", ignore.firstday=FALSE, ignore.lastday=FALSE, save_as_xlsx=FALSE)
  # calculate Averages for all analyse-variables from the mastersheet per id
{
  id_av_tab <- NULL
  uni.IDs <- unique(mastersheet$ID)
  for(id in uni.IDs)
  {
    vec <- as.numeric(id)
    ID.sheet <- mastersheet[which(mastersheet$ID == id),]
    if(ignore.firstday)
    {
      ID.sheet <- ID.sheet[-1,]
    }
    if(ignore.lastday)
    {
      ID.sheet <- ID.sheet[-nrow(ID.sheet),]
    }
    for(i in 7:ncol(ID.sheet))
    {
      ID.sheet[,i] <- gsub(dec,'.',ID.sheet[,i],fixed = TRUE)
      if(is.double(suppressWarnings(try(as.numeric(ID.sheet[,i]),silent = TRUE))))
      {
        suppressWarnings({vec <- c(vec, mean(as.numeric(ID.sheet[,i]),na.rm=T))})
      }
      else
      {
        vec <- c(vec, NA)
      }
    }
    if(is.null(id_av_tab))
    {
      id_av_tab <- vec
    }
    else
    {
      id_av_tab <- rbind(id_av_tab,vec)
    }
  }
  rownames(id_av_tab) <- NULL
  if(length(uni.IDs)==1)
  {
    id_av_tab <- rbind(id_av_tab, rep.int(0,times=(length(colnames(mastersheet)[7:ncol(mastersheet)])+1)))
  }
  colnames(id_av_tab) <- c("ID",paste("avg",colnames(mastersheet)[7:ncol(mastersheet)],sep="_"))
  
  if(!is.null(outfile))
  {
    if(save_as_xlsx)
    {
      write.xlsx(id_av_tab, file=outfile, sheetName="AVG_Mastersheet",  col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
    }
    else
    {
      write.table(id_av_tab,outfile,row.names = F,sep=separator, dec=dec)
    }
    
  }
  return(id_av_tab)
}

batch_start <- function(Acc.input,id.length,output,Domain.out=NULL,NW.out=NULL,separated.csv=FALSE,JoinedDomains=NULL,JoinedNWs=NULL,
                        separator=";",dec=",",fill_sleepTab=TRUE,day.begin="00:00:00",ACT.ALL.out=FALSE,early="20:00:00",late="12:00:00",
                        max.break=15,min.sleeplength=45,time.unit="mins", run.from=1, run.to=NULL, AVG_Filepath=NULL, ignore.firstday=TRUE, ignore.lastday=TRUE,
                        generateSupport=FALSE, calcIndivualAvg=TRUE, mvpa_steps_cutoff=25, WriteMastersheet_perID=FALSE, is.xlsx=FALSE, sheetIndex=TRUE, calcBySteps=TRUE, mvpa_count_thresh=2296,
                        save_as_xlsx=FALSE, non.stop=TRUE,id.asnumeric=FALSE,Slp.Path=NULL, sleepMode="setTimes", sleep.start="22:00:00", sleep.end="08:00:00",is.StartAfterMidNight=FALSE, PrintMode=2)
  # controll function to start generate_supportfiles or batch_run with the possible option of running Individual_Avg_data
{
  x <- NULL
  y <- NULL
  if(generateSupport)
  {
    generate_supportfiles(Acc.input, Domain.out, NW.out, id.length, separated=separated.csv, separator=separator, dec=dec, save_as_xlsx=save_as_xlsx,Slp.out=Slp.Path)
  }
  else
  {
    ## generate mastersheets from ActivPal-csv-data
    suppressWarnings( 
      x <- batch_run(input=Acc.input,output=output,ACT.ALL.out=ACT.ALL.out,domainCsv=Domain.out,NW_path=NW.out,
                   early=early, late=late ,max.break=max.break,min.sleeplength=min.sleeplength,id.length=id.length
                   ,separated.csv=separated.csv, JoinedDomains=JoinedDomains,JoinedNWs=JoinedNWs, separator=separator, dec=dec
                   , run.from=run.from, run.to=run.to, fill_sleepTab=fill_sleepTab,day.begin=day.begin, time.unit=time.unit, mvpa_steps_cutoff=mvpa_steps_cutoff, 
                   WriteMastersheet_perID=WriteMastersheet_perID, is.xlsx=is.xlsx, sheetIndex=sheetIndex, calcBySteps=calcBySteps, mvpa_count_thresh=mvpa_count_thresh,save_as_xlsx=save_as_xlsx
                   , non.stop=non.stop,id.asnumeric=id.asnumeric,sleepMode=sleepMode, sleep.start=sleep.start, sleep.end=sleep.end, is.StartAfterMidNight=is.StartAfterMidNight, sleep.file=Slp.Path,
                   PrintMode=PrintMode)
     )
    if(calcIndivualAvg)
    {
      if(is.null(AVG_Filepath))
      {
        if(save_as_xlsx)
        {
          AVG_Filepath <- paste(output,"Individual_AV.xlsx",sep="")
        }
        else
        {
          AVG_Filepath <- paste(output,"Individual_AV.csv",sep="")
        }
      }
      if(PrintMode>0)
      {
        print("---------------------------")
        print(paste("generate individual averages started at",Sys.time(),sep = " ... "))
      }
      ## generate Files for averages for ids from batchrun mastersheets
      y <- Individual_Avg_data(x,AVG_Filepath, separator=separator, dec=dec,ignore.firstday=ignore.firstday, ignore.lastday=ignore.lastday,save_as_xlsx=save_as_xlsx)
      if(PrintMode>0)
      {
        print(paste("generate individual averages finished at",Sys.time(),sep = " ... "))
        print("---------------------------")
      }
    }
  }
  return(list(MasterSheet=x,ID_Averages=y))
}