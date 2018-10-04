    #####
    # Program : MVPA_Bout_detection_alpha.R
    #
    # MVPA Bout detection for activPal_Functions
    #
    # Input:  15s epoch.csv  file from activPAL
    #
    #####
    
    
    #setwd("G:\\PASpecialUnit\\Heidelberg_ActivPal\\")
    #source("H:\\Lokale-Testdurchl?ufe\\ActivPal_Software_Test\\activPal_Functions_alpha_02.R")
    
    
    ############ Create MVPA-Bout  ################
    
    FN_CPAHR_MVPA_ACT_ALL <- function(input,output,is.csvout=T, ID.nr=NULL, separator=",", dec=".",save_as_xlsx=FALSE, MVPA_cutoff=23000, bystep=FALSE)
      # Create MVPA-Bout table
    {
      Sys.time()
      tab <- read_activPAL(inputpath = input)
      Sys.time()
      MVPA_addresses_tab <- MVPA_detect_transition(tab, MVPA_cutoff=MVPA_cutoff, bystep=bystep)
      if(is.null(MVPA_addresses_tab))
      {
        return(NULL)
      }
      Sys.time()
      MVPA_ACT_ALL<-create_MVPA_ACT_ALL(tab, MVPA_addresses_tab)
      
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
          outputfile <- paste(output,name,"_MVPA_ACT_ALL.xlsx",sep = "")
          write.xlsx(MVPA_ACT_ALL, file=outputfile, sheetName=paste(name,"_MVPA_ACT_ALL",sep = ""),  col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
        }
        else
        {
          outputfile <- paste(output,name,"_MVPA_ACT_ALL.csv",sep = "")
          write.table(MVPA_ACT_ALL, file=outputfile, row.names = FALSE, sep=separator, dec=dec)
        }
        warnings()
      }
      return(MVPA_ACT_ALL)
      
    }
    
    MVPA_detect_transition <- function(ActivPaltab, MVPA_cutoff, bystep=FALSE)
      # Detect Transition von from and to MVPA
    {
      # Create low_to_MVPA Variable
      low_to_MVPA <- numeric(length = nrow(ActivPaltab))
      MVPA_to_low <- numeric(length = nrow(ActivPaltab))
      if(bystep)
      {
        if(ActivPaltab$StepCount[1] > MVPA_cutoff)
        {
          low_to_MVPA[1] <- 1
          if(ActivPaltab$StepCount[2] <= MVPA_cutoff)
          {
            MVPA_to_low[1] <- 1
          }
        }
        for(i in 2: (length(low_to_MVPA)-1))
        {
          if(ActivPaltab$StepCount[i] > MVPA_cutoff)
          {
            if(ActivPaltab$StepCount[i-1] <= MVPA_cutoff)
            {
              low_to_MVPA[i] <- 1
            }
            if(ActivPaltab$StepCount[i+1] <= MVPA_cutoff)
            {
              MVPA_to_low[i] <- 1
            }
          }
        }
        if(ActivPaltab$StepCount[nrow(ActivPaltab)] > MVPA_cutoff)
        {
          MVPA_to_low[nrow(ActivPaltab)] <- 1
          if(ActivPaltab$StepCount[nrow(ActivPaltab)-1] <= MVPA_cutoff)
          {
            low_to_MVPA[nrow(ActivPaltab)] <- 1
          }
        }
      }
      else
      {
        vm_channels <- sqrt(((ActivPaltab$Channel1)**2)+((ActivPaltab$Channel2)**2)+((ActivPaltab$Channel3)**2))
        MVPA_cutoff.Psec <- MVPA_cutoff/15
        
        if(vm_channels[1] > (MVPA_cutoff.Psec*ActivPaltab$Upright[1]))
        {
          low_to_MVPA[1] <- 1
          if(vm_channels[2] <= (MVPA_cutoff.Psec*ActivPaltab$Upright[2]))
          {
            MVPA_to_low[1] <- 1
          }
        }
        for(i in 2: (length(low_to_MVPA)-1))
        {
          if(vm_channels[i] > (MVPA_cutoff.Psec*ActivPaltab$Upright[i]))
          {
            if(vm_channels[i-1] <= (MVPA_cutoff.Psec*ActivPaltab$Upright[i-1]))
            {
              low_to_MVPA[i] <- 1
            }
            if(vm_channels[i+1] <= (MVPA_cutoff.Psec*ActivPaltab$Upright[i+1]))
            {
              MVPA_to_low[i] <- 1
            }
          }
        }
        if(vm_channels[nrow(ActivPaltab)] > (MVPA_cutoff.Psec*ActivPaltab$Upright[nrow(ActivPaltab)]))
        {
          MVPA_to_low[nrow(ActivPaltab)] <- 1
          if(vm_channels[nrow(ActivPaltab)-1] <= (MVPA_cutoff.Psec*ActivPaltab$Upright[nrow(ActivPaltab)-1]))
          {
            low_to_MVPA[nrow(ActivPaltab)] <- 1
          }
        }
      }
      ActivPaltab <- cbind.data.frame(ActivPaltab,low_to_MVPA,MVPA_to_low)
    
      EndOfMVPA = NULL
      StartOfMVPA = NULL
      
      EndOfMVPA_over = which(ActivPaltab$MVPA_to_low > 0)
      StartOfMVPA_over = which(ActivPaltab$low_to_MVPA > 0)
      
      # Since more than one Upright period may occur in one second additional
      # numbers of Upright bouts were added in the columns 7 and 8
      print(paste("01 MVPA_detect_transition - time:",Sys.time(),sep="  "))
      if(length(EndOfMVPA_over)>0 & length(EndOfMVPA_over)>0)
      {
        StartOfMVPA = 0;
        
        for(ij in 1:length(StartOfMVPA_over))
        {
          for(kk in 1:ActivPaltab[StartOfMVPA_over[ij],14])
          {
            StartOfMVPA = c(StartOfMVPA, StartOfMVPA_over[ij])
          }
        }
        StartOfMVPA <-  StartOfMVPA[2:length(StartOfMVPA)]
        EndOfMVPA = 0;
        for(ij in 1:length(EndOfMVPA_over))
        {
          for(kk in 1:ActivPaltab[EndOfMVPA_over[ij],14])
          {
            EndOfMVPA = c(EndOfMVPA, EndOfMVPA_over[ij])
          }
        }
        EndOfMVPA <-  EndOfMVPA[2:length(EndOfMVPA)]
        
        if(EndOfMVPA[1]<StartOfMVPA[1]) # The end of Upright was  person was Upright and went to upright, thus the start of the file is all Upright
        {
          StartOfMVPA = c(1 , StartOfMVPA)
        }
        if(StartOfMVPA[length(StartOfMVPA)]>=EndOfMVPA[length(EndOfMVPA)]) # The last thing the person did was go from Upright to upright, thus the end of the file is all dynamic
        {
          EndOfMVPA = c(EndOfMVPA, nrow(ActivPaltab))
        }
      }
      else
      {
        #  Take care of the situation where there is no change
        if(length(EndOfMVPA_over)==0 & length(EndOfMVPA_over)==0)
        {
          if(mean(ActivPaltab$Sedentary)==0)
          {
            StartOfMVPA = 1;
            EndOfMVPA = nrow(ActivPaltab);
          }
          else
          {
            return(NULL)
            #EndOfMVPA = 1;
            #StartOfMVPA = nrow(ActivPaltab);
          }
        }
        if(length(EndOfMVPA_over)==0 & length(EndOfMVPA_over)>0)
        {
          EndOfMVPA = nrow(ActivPaltab)
          if(mean(ActivPaltab$Sedentary)!=0)
          {
            stop('Incorrect selection of the EndOfMVPA variable')
          }
        }
        if(length(EndOfMVPA_over)>0 & length(EndOfMVPA_over)==0)
        {
          StartOfMVPA = 1;
          if(mean(ActivPaltab$Sedentary)!=0)
          {
            stop('Incorrect selection of the StartOfMVPA variable')
          }
        }
      }
      print(paste("02 MVPA_detect_transition - time:",Sys.time(),sep="  "))
      MVPA_addresses_start = StartOfMVPA;
      MVPA_addresses_end = EndOfMVPA;

      # The situation does occur where you have a bout of activity starting
      # during a recording period with dynamic activity.
      # this needs to be taken care of.
  
      MVPA_addresses_ending <- Find_The_Next_MVPA_End(MVPA_addresses_start,MVPA_addresses_end,ActivPaltab)
      rm("MVPA_addresses_end")
      MVPA_addresses_end = MVPA_addresses_ending
      
      ## Error checkig
      if(length(MVPA_addresses_start) != length(MVPA_addresses_end))
        stop('The length of the MVPA_addresses_start and the MVPA_addresses_end are not equal.')
      else
      {
        MVPA_addresses_end[1:20]
        MVPASegLs = (MVPA_addresses_end - MVPA_addresses_start)+1
        MVPASegLsWithFull15s = (MVPASegLs - 2)*15
        for(jj in 1:length(MVPA_addresses_start))
        {
          temp_vec <-ActivPaltab$Upright
          MVPASum = sum(temp_vec[seq(from=MVPA_addresses_start[jj],to=MVPA_addresses_end[jj])])
          
          #if(MVPASegLsWithFull15s[jj]>MVPASum)
          #{
          #  stop(paste("There is less than expected Upright time between two addresses. Recheck the static/dynamic detection algorithm. Address: ",jj,sep=""))
          #}
        }
      }
      print(paste("03 MVPA_detect_transition - time:",Sys.time(),sep="  "))
      MVPA_addresses_tab <- cbind(MVPA_addresses_start,MVPA_addresses_end)
      return(MVPA_addresses_tab)
    }
    
    create_MVPA_ACT_ALL <- function(DATA, MVPA_adresses)
      # create a table with all mvpa-bouts and their duration
    {
      Date <- DATA[MVPA_adresses[,1],2]
      Enddate <- DATA[MVPA_adresses[,2],2]
      segments <- (MVPA_adresses[,2] - MVPA_adresses[,1])+1
      
      temp_vec <-DATA$Upright
      SedLength_segments <- numeric(length=nrow(MVPA_adresses))
      for(ii in 1:nrow(MVPA_adresses))
      {
        SedLength_segments[ii] <- sum(temp_vec[seq(from=MVPA_adresses[ii,1],to=MVPA_adresses[ii,2])])
      }
      seconds <- SedLength_segments
      minutes <- SedLength_segments/60
      
      start <- strftime(Date, format="%H:%M:%S", tz=getOption("tz"))
      finish <- strftime(Enddate, format="%H:%M:%S", tz=getOption("tz"))
      SAA <- cbind.data.frame(Date,start,finish,seconds,minutes,segments)
      return(SAA)
    }
    
    Find_The_Next_MVPA_End <- function(Sed_addresses_start,Sed_addresses_end,Data)
      # Get Ends of the MVPA-bouts, because Start and End could be in the same epoch
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
    
    MVPABout_NW_delete <- function(MVPA_ACT_ALL, NW.csv.Path,separated.csv=TRUE, JoinedNWs=NULL, id=NULL, separator=",", dec=".", is.xlsx=FALSE, sheetIndex=1)
      # Delete Nonwear Times in MVPA-bout-table
    {
      NW.tab <- get_NWforID(NW.csv.Path,separated.csv=separated.csv, JoinedNWs=JoinedNWs, id=id, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
      dte.all <- as.character(as.Date(MVPA_ACT_ALL[,1], tz=getOption("tz"),format="%Y-%m-%d"))
      dte.NW <- as.character(as.Date(NW.tab[,1], tz=getOption("tz"),format="%Y-%m-%d"))
      unq.dates <- unique(dte.all)
      for(day in 1:length(unq.dates))
      {
        day.tab <- MVPA_ACT_ALL[which(dte.all == unq.dates[day]),]
        start.bout <- as.POSIXct(paste(unq.dates[day],day.tab[,2],sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
        fin.bout <- as.POSIXct(paste(unq.dates[day],day.tab[,3],sep=" "), tz=getOption("tz"),format="%Y-%m-%d %H:%M:%S")
        NW.seq <- seq(from=2,to=(ncol(NW.tab)-2),by=3)
        null.nws <-  which(is.na(NW.tab[which(dte.NW == unq.dates[day]),NW.seq]))
        if(length(null.nws)>0)
        {
          NW.seq <- NW.seq[-null.nws]
        }
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
    
    split_MVPA_domains <- function(path=NULL, MVPA_ACT_ALL,separated.csv=TRUE, JoinedDomains=NULL, id=NULL, separator=",", dec=".", is.xlsx=FALSE, sheetIndex=1)
      # Categorize Domain for MVPA-Bouts
    {
      DomainCSV <- getDomainsforID(path=path, separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=id, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
      dats <- as.Date(MVPA_ACT_ALL[,1], "%y-%m-%d", tz=getOption("tz"))
      x <- vector("list",length=nrow(DomainCSV))
      for(i in 1:nrow(DomainCSV))
      {
        date <- which(dats == as.character(DomainCSV[i,1]))
        subtable <- MVPA_ACT_ALL[date,]
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
    
    create_MVPA_misc_domain <- function(DomainFile=NULL, MVPA_ACT_ALL, separated.csv=TRUE, JoinedDomains=NULL, id=NULL, separator=",", dec=".", is.xlsx=FALSE, sheetIndex=1)
      # Categorize Misc for MVPA-Bouts that are not categorized into any domain
    {
      DomainCSV <- getDomainsforID(path=DomainFile, separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=id, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
      dats <- as.Date(MVPA_ACT_ALL[,1], "%y-%m-%d", tz=getOption("tz"))
      nDomains <- seq(from=2, to=(ncol(DomainCSV)-2),by=3)
      x <- vector("list",length=nrow(DomainCSV))
      misctab <- NULL
      miscDoms <- NULL
      for(i in 1:nrow(DomainCSV))
      {
        date <- which(dats == as.character(DomainCSV[i,1]))
        subtable <- MVPA_ACT_ALL[date,]
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
    
    MVPA_Bout_Length <- function(input, partedDomainsLS=NULL, miscTab=NULL, DailyDomains =5, DayCut="00:00:00")
      # Generate Table with Information of Frequencie and Duration of MVPA-bouts
    {
      # Like the Excel Makros bur for all days at once
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
    
    createMvpaBoutSheet <- function(MVPABouts, time.unit="minute",Domain.path=NULL,outpath=NULL,id.Nr, separated.csv=TRUE, JoinedDomains=NULL, separator=",", dec=".",WriteMastersheet=FALSE, is.xlsx=FALSE, sheetIndex=1, save_as_xlsx=FALSE,Ndoms=0)
    #anpassen
    {
      TimeUnitMod <- 1
      if(time.unit == "hour" || time.unit == "hr" || time.unit == "h")
      {
        TimeUnitMod <- 1/60
      }
      else
      {
        if(time.unit == "seconds" || time.unit == "sec" || time.unit == "s")
        {
          TimeUnitMod <- 60
        }
      }
      Domain.list <- getDomainsforID(Domain.path, separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=id.Nr, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
      Bout.H24.Sum <- MVPABouts[[1]][[1]]
      H24.BoutSum <- Bout.H24.Sum[,2:ncol(Bout.H24.Sum)]
      Bout.Sum <- MVPABouts[[2]][[1]][,2:ncol(Bout.H24.Sum)]
      Tot.Sed.T <- numeric(length = nrow(Bout.Sum))
      temp.BoSum <- Bout.Sum[,c(1,2,3,6,7,9,10)]
      for(nrw in 1:nrow(Bout.Sum))
      {
        Tot.Sed.T[nrw] <- sum(as.numeric(as.character(unlist(temp.BoSum[nrw,]))))
      }
      DayDate <- as.POSIXct(as.character(MVPABouts[[1]][[1]][,1]),format="%Y-%m-%d",tz=getOption("tz"))
      Bout.Sum <- cbind.data.frame(Bout.Sum,Tot.Sed.T)
      ### edited colnames
      
      edited.colNames <- c("Date","Weekday","Day","Num_MVPA_lt5","Num_MVPA_5-10"
                           ,"Num_MVPA_10-20","Num_MVPA_lt20","Num_MVPA_gt20","Num_MVPA_20-30","Num_MVPA_30-40","Num_MVPA_gt40","Num_MVPA_40-60","Num_MVPA_gt60","Num_MVPA_gt90"
                           ,"MVPA_lt5","MVPA_5-10","MVPA_10-20","MVPA_lt20","MVPA_gt20","MVPA_20-30","MVPA_30-40","MVPA_gt40","MVPA_40-60","MVPA_gt60","MVPA_gt90","Total_MVPA")
      dom_genName <- c("DomainName","Num_MVPA_lt5","Num_MVPA_5-10","Num_MVPA_10-20","Num_MVPA_lt20","Num_MVPA_gt20","Num_MVPA_20-30"
                       ,"Num_MVPA_30-40","Num_MVPA_gt40","Num_MVPA_40-60","Num_MVPA_gt60","Num_MVPA_gt90","MVPA_lt5","MVPA_5-10","MVPA_10-20","MVPA_lt20","MVPA_gt20","MVPA_20-30","MVPA_30-40","MVPA_gt40","MVPA_40-60"
                       ,"MVPA_gt60","MVPA_gt90","Total_MVPA")
      
      ###
      vec <- cbind(DayDate,weekdays(as.Date(DayDate, tz=getOption("tz"))),c(1:length(DayDate)),H24.BoutSum,Bout.Sum)
      colnames(vec) <- edited.colNames
      
      if(!is.null(Domain.list) || !Ndoms == 0)
      {
        doms <- seq(1,Ndoms,1)
        for(day in 1:length(DayDate))
        {
          domainNames <- Domain.list[day,seq(from=4,to=(ncol(Domain.list)),by=3)]
          domainNames <- as.character(unlist(domainNames))
          Bout.day.doms <- MVPABouts[[1]][[2]][which(as.character(MVPABouts[[1]][[2]][,1]) == as.character(DayDate[day])), ]
          Bout.day.dom2 <- MVPABouts[[2]][[2]][which(as.character(MVPABouts[[2]][[2]][,1]) == as.character(DayDate[day])), ]
          for(i in doms)
          {
            if(length(doms)==1)
            {
              Bout.day.doms.vec <- Bout.day.doms[which(Bout.day.doms[,2]==i),]
              Bout.day.dom2.vec <- Bout.day.dom2[which(Bout.day.dom2[,2]==i),]
            }
            else
            {
              Bout.day.doms.vec <- Bout.day.doms[which(Bout.day.doms[,2]==i),]
              Bout.day.dom2.vec <- Bout.day.dom2[which(Bout.day.dom2[,2]==i),]
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
            Tot.Sed.T <- sum(dom.prt2[c(1,2,3,6,7,9,10)])
            if(i == 1)
            {
              dom.vec <- as.vector(c(NA,dom.prt,dom.prt2,Tot.Sed.T),mode = "numeric")
              cname <- paste("D",i,"_",dom_genName,sep="")
            }
            else
            {
              dom.vec <- as.vector(c(dom.vec,NA,dom.prt,dom.prt2,Tot.Sed.T),mode = "numeric")
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
        vec <- cbind.data.frame(vec,day.tab)
        Bout.H24.Sum <- MVPABouts[[1]][[5]]
        if(ncol(Bout.H24.Sum) == 1)
        {
          Bout.H24.Sum <- t(Bout.H24.Sum)
        }
        H24.BoutSum <- Bout.H24.Sum[,2:ncol(Bout.H24.Sum)]
        
        if(ncol(MVPABouts[[2]][[5]])==1)
        {
          Bout.Sum <- t(MVPABouts[[2]][[5]][2:ncol(Bout.H24.Sum),])
        }
        else
        {
          Bout.Sum <- MVPABouts[[2]][[5]][,2:ncol(Bout.H24.Sum)]
        }
        Tot.Sed.T <- numeric(length = nrow(Bout.Sum))
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
        }
        Bout.Sum <- cbind(Bout.Sum,Tot.Sed.T)
      if(is.null(nrow(H24.BoutSum)))
      {
        Misc.tab <-c(H24.BoutSum,Bout.Sum[,1:(length(Bout.Sum))])
        names(Misc.tab) <- paste("Misc_",dom_genName[2:length(dom_genName)],sep="")
      }
      else
      {
        Misc.tab <-cbind(H24.BoutSum,Bout.Sum[,1:(ncol(Bout.Sum))])
        colnames(Misc.tab) <- paste("Misc_",dom_genName[2:length(dom_genName)],sep="")
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
      colnames(vec)[1] <- "ID"
      
      if(!is.null(Domain.list))
      {
        unq.doms <- unique(as.Date(Domain.list[,1], tz=getOption("tz")))
        unq.dates <- unique(as.Date(as.character(vec[,2]), tz=getOption("tz")))
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
            vec[cNr,28] <- doms.Cnames[cNr]
          }
        }
        else
        {
          last.DomNameNr <- 28+(24*length(doms.Cnames))
          cNR.names <- seq(from=28,to=last.DomNameNr, by=24)
          for(cNr in 1:length(doms.Cnames))
          {
            vec[,cNR.names[cNr]] <- doms.Cnames[cNr]
          }
        }
      }
      
      
      for(i in 1:ncol(vec))
      {
        vec[,i] <- gsub('.',dec,vec[,i],fixed = TRUE)
      }
      
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
    
    
    ################### ANPASSEN ############################
    
    
    MVPA_Batch <- function(input,output,ACT.ALL.out=FALSE,domainCsv=NULL,NW_path=NULL,early="20:00:00", late="12:00:00",max.break=15,
                          min.sleeplength=45,id.length,id.start=1,separated.csv=TRUE, JoinedDomains=NULL,JoinedNWs=NULL, 
                          separator=",", dec=".", run.from=1, run.to=NULL,fill_sleepTab=FALSE, day.begin="00:00:00",time.unit="hours", mvpa_steps_cutoff=25, WriteMastersheet_perID=FALSE
                          , is.xlsx=FALSE, sheetIndex=1, calcBySteps=TRUE, mvpa_count_thresh=2296,save_as_xlsx=FALSE, non.stop=TRUE,id.asnumeric=FALSE)
      # Runs MasterSheet.lapply for all Ids and merges the Mastersheetfiles together
    {
      if(is.null(domainCsv))
      {
        if(!separated.csv)
        {
          JoinedDomains <- JoinedDomains
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
      
      print("Start MasterSheet.lapply")
      for(i in 1:length(acc.list))
      {
        print(paste("start dataset:",i,"at",Sys.time(),sep=" "))
        if(i == 1)
        {
          Master.Sheet <- MVPASheet.lapply(acc.list[i],output=output,ACT.ALL.out=ACT.ALL.out,domainCsv=domainCsv,
                                             NW_path=NW_path,early="20:00:00", late="12:00:00",max.break=15,min.sleeplength=45,
                                             id.length=id.length,id.start=id.start,separated.csv=separated.csv, JoinedDomains=JoinedDomains,
                                             JoinedNWs=JoinedNWs, separator=separator, dec=dec,fill_sleepTab=fill_sleepTab,day.begin=day.begin
                                             ,time.unit=time.unit, mvpa_steps_cutoff=mvpa_steps_cutoff, WriteMastersheet=WriteMastersheet_perID
                                             , is.xlsx=is.xlsx, sheetIndex=sheetIndex, calcBySteps=calcBySteps, mvpa_count_thresh=mvpa_count_thresh,
                                             save_as_xlsx=save_as_xlsx, non.stop=non.stop,id.asnumeric=id.asnumeric)
        }
        else
        {
          Master.Sheet <- bind_rows(Master.Sheet, MVPASheet.lapply(acc.list[i],output=output,ACT.ALL.out=ACT.ALL.out,domainCsv=domainCsv,
                                                                    NW_path=NW_path,early="20:00:00", late="12:00:00",max.break=15,
                                                                    min.sleeplength=45,id.length=id.length,id.start=id.start,separated.csv=separated.csv, JoinedDomains=JoinedDomains,
                                                                    JoinedNWs=JoinedNWs, separator=separator, dec=dec,fill_sleepTab=fill_sleepTab,day.begin=day.begin
                                                                    ,time.unit=time.unit,mvpa_steps_cutoff=mvpa_steps_cutoff, WriteMastersheet=WriteMastersheet_perID
                                                                    , is.xlsx=is.xlsx, sheetIndex=sheetIndex, calcBySteps=calcBySteps, mvpa_count_thresh=mvpa_count_thresh,
                                                                    save_as_xlsx=save_as_xlsx, non.stop=non.stop,id.asnumeric=id.asnumeric))
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
          filename <- paste(output,"comp_MvpaSheet.xlsx",sep="")
          write.xlsx(Master.Sheet, file = filename, sheetName="Mastersheet",  col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
        }
        else
        {
          filename <- paste(output,"comp_MvpaSheet.csv",sep="")
          write.table(Master.Sheet,file = filename, row.names = F, sep=separator, dec=dec)
        }
        
      }
      return(Master.Sheet)
    }
    
    MVPASheet.lapply <- function(accfile,output,ACT.ALL.out,domainCsv=NULL,NW_path,early="20:00:00", late="12:00:00",max.break=15,min.sleeplength=45,id.length,id.start=1,separated.csv=TRUE, JoinedDomains=NULL
                                   ,JoinedNWs=NULL, separator=",", dec=".",fill_sleepTab=FALSE, day.begin="00:00:00",time.unit="hours",mvpa_steps_cutoff=25,WriteMastersheet=FALSE, is.xlsx=FALSE, sheetIndex=1,
                                   calcBySteps=TRUE, mvpa_count_thresh=2296, save_as_xlsx=FALSE, non.stop=TRUE,id.asnumeric=FALSE) 
      # Loads supportfiles and starts complete_run function
    {
      if(calcBySteps)
      {
        MVPA_cutoff <- mvpa_steps_cutoff
      }
      else
      {
        MVPA_cutoff <- mvpa_count_thresh
      }
      MVPASheet <- NULL
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
          
          MVPASheet <- MVPASheet_run(accfile,output,ACT.ALL.out=ACT.ALL.out,id.length=id.length,id.start = id.start,id.asnumeric=id.asnumeric,
                                     domainCsv=ID.DomFile,NW_path=NW_path,separated.csv=separated.csv,JoinedDomains=JoinedDomains,JoinedNWs=JoinedNWs,
                                     separator=separator, dec=dec,time.unit=time.unit,MVPA_cutoff=MVPA_cutoff,calcBySteps=calcBySteps, WriteSheet=WriteMastersheet,
                                     is.xlsx=is.xlsx,sheetIndex=sheetIndex,save_as_xlsx=save_as_xlsx)
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
            MVPASheet <- MVPASheet_run(accfile,output,ACT.ALL.out=ACT.ALL.out,id.length=id.length,id.start = id.start,id.asnumeric=id.asnumeric,
                                       domainCsv=ID.DomFile,NW_path=NW_path,separated.csv=separated.csv,JoinedDomains=JoinedDomains,JoinedNWs=JoinedNWs,
                                       separator=separator, dec=dec,time.unit=time.unit,MVPA_cutoff=MVPA_cutoff,calcBySteps=calcBySteps, WriteSheet=WriteMastersheet,
                                       is.xlsx=is.xlsx,sheetIndex=sheetIndex,save_as_xlsx=save_as_xlsx)
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
      return(MVPASheet)
    }
    
    
    MVPASheet_run <- function(accfile,output,ACT.ALL.out=FALSE,id.length=8,id.start = 1,id.asnumeric=FALSE,
                              domainCsv=NULL,NW_path=NULL,separated.csv=TRUE,JoinedDomains=NULL,JoinedNWs=NULL,
                              separator=",", dec=".",time.unit="minute",MVPA_cutoff=25,calcBySteps=TRUE, WriteSheet=FALSE,
                              is.xlsx=FALSE,sheetIndex=1,save_as_xlsx=FALSE)
    {
      if(id.asnumeric)
      {
        ID.nr <- as.numeric(getID(accfile,id.length, id.start = id.start))
      }
      else
      {
        ID.nr <- as.character(getID(accfile,id.length, id.start = id.start))
      }
      
      print(ID.nr)
      
      mvpaTab <- FN_CPAHR_MVPA_ACT_ALL(input=accfile,output,is.csvout=ACT.ALL.out, ID.nr=ID.nr, separator=separator, dec=dec,save_as_xlsx=save_as_xlsx, MVPA_cutoff=MVPA_cutoff, bystep=calcBySteps)
      
      if(is.null(mvpaTab))
      {
        return(NULL)
      }
      
      x <- NULL
      y <- NULL
      
      if(!is.null(NW_path))
      {
        x <- get_NWforID(path=NW_path,separated.csv=separated.csv, JoinedNWs=JoinedNWs, id=ID.nr, separator=separator, dec=dec,is.xlsx=is.xlsx, sheetIndex=sheetIndex)
      }
      if(!is.null(domainCsv))
      {
        y <- getDomainsforID(path=domainCsv, separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=ID.nr, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
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
      
      x <- NULL
      y <- NULL
      
      if(!is.null(NW_path))
      {
        mvpaTab <- MVPABout_NW_delete(mvpaTab, NW.csv.Path=NW_path ,separated.csv=separated.csv, JoinedNWs=JoinedNWs, id=ID.nr, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
      }
      
      if(is.null(domainCsv))
      {
        if(is.null(JoinedDomains))
        {
          print(paste("Number of Domains per Day: 0"))
          MVPAdomains <- NULL
          miscDom <- NULL
        }
        else
        {
          temp.Doms <- getDomainsforID(domainCsv,separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=ID.nr, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
          DailyDomains <- ncol(temp.Doms)
          DailyDomains <- (DailyDomains-1)/3
          print(paste("Number of Domains per Day: ",DailyDomains,sep = ""))
          if(is.null(temp.Doms))
          {
            domainCsv <- NULL
            JoinedDomains <- NULL
            MVPAdomains <- NULL
            miscDom <- NULL
          }
          else
          {
            MVPAdomains <- split_MVPA_domains(path=domainCsv, MVPA_ACT_ALL=mvpaTab,separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=ID.nr, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
            
            miscDom <- create_MVPA_misc_domain(DomainFile=domainCsv, MVPA_ACT_ALL=mvpaTab, separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=ID.nr, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
          }
          temp.Doms <- NULL
        }
      }
      else
      {
        temp.Doms <- getDomainsforID(domainCsv,separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=ID.nr, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
        DailyDomains <- ncol(temp.Doms)
        DailyDomains <- (DailyDomains-1)/3
        print(paste("Number of Domains per Day: ",DailyDomains,sep = ""))
        if(is.null(temp.Doms))
        {
          domainCsv <- NULL
          JoinedDomains <- NULL
          MVPAdomains <- NULL
          miscDom <- NULL
        }
        else
        {
          MVPAdomains <- split_MVPA_domains(path=domainCsv, MVPA_ACT_ALL=mvpaTab,separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=ID.nr, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
          
          miscDom <- create_MVPA_misc_domain(DomainFile=domainCsv, MVPA_ACT_ALL=mvpaTab, separated.csv=separated.csv, JoinedDomains=JoinedDomains, id=ID.nr, separator=separator, dec=dec, is.xlsx=is.xlsx, sheetIndex=sheetIndex)
        }
        temp.Doms <- NULL
        
      }

      if(is.null(JoinedDomains) & is.null(domainCsv))
      {
        DailyDomains <- 0
      }
      
      MVPABouts <- MVPA_Bout_Length(input=mvpaTab, partedDomainsLS=MVPAdomains, miscTab=miscDom, DailyDomains =DailyDomains, DayCut="00:00:00")
      
      masterMVPA <- createMvpaBoutSheet(MVPABouts, time.unit=time.unit,Domain.path=domainCsv,outpath=output,id.Nr=ID.nr, separated.csv=separated.csv, JoinedDomains=JoinedDomains, separator=separator, dec=dec,
                                        WriteMastersheet=WriteSheet, is.xlsx=is.xlsx, sheetIndex=sheetIndex, save_as_xlsx=save_as_xlsx,Ndoms=DailyDomains)
      
      return(masterMVPA)
    }
    
    MVPA_start <- function(Acc.input,id.length,output,Domain.out=NULL,NW.out=NULL,separated.csv=FALSE,JoinedDomains=NULL,JoinedNWs=NULL,
                            separator=";",dec=",",fill_sleepTab=TRUE,day.begin="00:00:00",ACT.ALL.out=FALSE,early="20:00:00",late="12:00:00",
                            max.break=15,min.sleeplength=45,time.unit="mins", run.from=1, run.to=NULL, AVG_Filepath=NULL, ignore.firstday=TRUE, ignore.lastday=TRUE,
                            generateSupport=FALSE, calcIndivualAvg=TRUE, mvpa_steps_cutoff=25, WriteMastersheet_perID=FALSE, is.xlsx=FALSE, sheetIndex=TRUE, calcBySteps=TRUE, mvpa_count_thresh=2296,
                            save_as_xlsx=FALSE, non.stop=TRUE,id.asnumeric=FALSE)
      # controll function to start generate_supportfiles or batch_run with the possible option of running Individual_Avg_data
    {
      x <- NULL
      y <- NULL
      if(generateSupport)
      {
        generate_supportfiles(Acc.input, Domain.out, NW.out, id.length, separated=separated.csv, separator=separator, dec=dec, save_as_xlsx=save_as_xlsx)
      }
      else
      {
        ## generate mastersheets from ActivPal-csv-data
        suppressWarnings( 
          x <- MVPA_Batch(input=Acc.input,output=output,ACT.ALL.out=ACT.ALL.out,domainCsv=Domain.out,NW_path=NW.out,
                         early=early, late=late ,max.break=max.break,min.sleeplength=min.sleeplength,id.length=id.length
                         ,separated.csv=separated.csv, JoinedDomains=JoinedDomains,JoinedNWs=JoinedNWs, separator=separator, dec=dec
                         , run.from=run.from, run.to=run.to, fill_sleepTab=fill_sleepTab,day.begin=day.begin, time.unit=time.unit, mvpa_steps_cutoff=mvpa_steps_cutoff, 
                         WriteMastersheet_perID=WriteMastersheet_perID, is.xlsx=is.xlsx, sheetIndex=sheetIndex, calcBySteps=calcBySteps, mvpa_count_thresh=mvpa_count_thresh,save_as_xlsx=save_as_xlsx
                         , non.stop=non.stop,id.asnumeric=id.asnumeric)
        )
        if(calcIndivualAvg)
        {
          print("not implemented yet")
          # if(is.null(AVG_Filepath))
          # {
          #   if(save_as_xlsx)
          #   {
          #     AVG_Filepath <- paste(output,"Individual_AV.xlsx",sep="")
          #   }
          #   else
          #   {
          #     AVG_Filepath <- paste(output,"Individual_AV.csv",sep="")
          #   }
          # }
          # print(paste("generate individual averages ... start",Sys.time(),sep = " ... "))
          # ## generate Files for averages for ids from batchrun mastersheets
          # y <- Individual_Avg_data(x,AVG_Filepath, separator=separator, dec=dec,ignore.firstday=ignore.firstday, ignore.lastday=ignore.lastday,save_as_xlsx=save_as_xlsx)
          # print(paste("generate individual averages ... finished",Sys.time(),sep = " ... "))
        }
      }
      return(list(MVPASheet=x,ID_MVPA_Averages=y))
    }