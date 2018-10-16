    #####
    # Program : activPAL_main_alpha.R
    #
    # Reproduction of the matlab-program "CPAHR_activPAL_software_version_0.0.8"
    #
    # Input: 15s epoch.csv  file from activPAL
    # Used Scripts: activPal_Functions_alpha.R
    #
    # Author: Jan Behrens           DATE: 01.10.2018
    # Version: a_3.7
    #####
    rm(list = ls())
    
    ### Program parameters
    # general Startparameter
    Scriptpath <- "G:\\PASpecialUnit\\ActivPAL-R-package\\AcvtivPal_analyses_program\\Programms\\" # Path to the folder with the ActivPAL_main.R and ActivPAL_V01.R files
    Acc.input <- "G:\\PASpecialUnit\\ActivPAL-R-package\\AcvtivPal_analyses_program\\Example_input\\ActivPal_csv_files\\"
    output <- "H:\\Lokale-Testdurchläufe\\ActivPal_Software_Test\\out\\"
    id.length <- 5
    id.asnumeric <- FALSE
    non.stop <- TRUE

    
    
    ###general Parameters for the Domain and Nonweartime-Files
    #Domain.out <- "G:\\PASpecialUnit\\ActivPAL-R-package\\AcvtivPal_analyses_program\\Example_input\\Domain_files\\Domains.xlsx" #NULL is a possibile value
    #NW.out <- NULL #"G:\\PASpecialUnit\\ActivPAL-R-package\\AcvtivPal_analyses_program\\Example_input\\Nonwear_files\\NonWearTimesV2.xlsx"
    
    Domain.out <- "H:\\Lokale-Testdurchläufe\\ActivPal_Software_Test\\ActivPal_Test\\Domains.xlsx"
    NW.out <- "H:\\Lokale-Testdurchläufe\\ActivPal_Software_Test\\ActivPal_Test\\NWs.csv"
    
    JoinedDomains <- NULL
    JoinedNWs <- NULL#readRDS("H:\\Lokale-Testdurchläufe\\ActivPal_Software_Test\\ActivPal_Test\\Nws.rds")
    separated.csv <- FALSE
    
    
    ### for Domain- and Nonweartime-files in csv-format
    separator <- ","
    dec <- "."
    
    ### for Domain- and Nonweartime-files in xlsx-format
    is.xlsx <- FALSE
    sheetIndex <- 1
    save_as_xlsx <- TRUE
    
    ### Analysis parameter
    day.begin <- "00:00:00"
    time.unit <- "mins"
    calcBySteps <- TRUE
    mvpa_steps_cutoff <- 25
    mvpa_count_thresh <- 2296
    
    ### additional optional outputs 
    ACT.ALL.out <- TRUE ###Bouts output for each individual ID
    WriteMastersheet_perID = TRUE  ###Mastersheet output for each individual ID
    
    ### values for automated sleepdetection
    early <- "21:00:00" #earliest time for bout to be accounted as sleeptime
    late <- "07:00:00" #latest time for bout to be accounted as sleeptime
    max.break <- 15 #maximum break length between bout for them to be merged as sleeptime
    min.sleeplength <- 45 #minimum boutlength for the bout to be added to the sleeptime
    fill_sleepTab <- TRUE
    
    ### Selection of functionalities to run
    generateSupport <- FALSE
    calcIndivualAvg <- FALSE
    
    ### Testparameter f?r alle Standard 
    run.from <- 1 # Standard=1
    run.to <-  NULL # Standard=NULL
    
    #Parameter Average data function
    AVG_Filepath <- paste(output,"Individual_AVG.xlsx",sep="")
    ignore.firstday <- FALSE
    ignore.lastday <- FALSE
    
    #### new Parameter Version 3.7
    sleepMode <- "file" #Options "default":= old slp detection; "setTimes":= for all Dates static Sleep Times; "file":= Use File to detect Slp
    Slp.Path <- "H:\\Lokale-Testdurchläufe\\ActivPal_Software_Test\\ActivPal_Test\\Sleep.xlsx"
    sleep.start <- "22:00:00" # only if "setTimes"
    sleep.end <- "08:00:00" # only if "setTimes"
    is.StartAfterMidNight <- FALSE # only if "setTimes"
    PrintMode <- 1 #0 = no console output; 1=print processed file id to console; 2=print processing steps to console; 3=print all infos to console ; 4 = print all
    RunSedBouts <- TRUE # TRUE/FALSE if Mastersheet for SED Bouts should be calculated
    RunMVPA_Bouts <- TRUE # TRUE/FALSE if Mastersheet for SED Bouts should be calculated
    SedBoutMasterTab <- NULL # is not NULL if existing RTab as SED Bout Mastersheet should be used; only effectiv if RunSedBouts=FALSE
    NwMethod <- "naiv" #Method how Nws are created for support files, default=static Example Values, naiv=naiv Method see choi et. all., 
    # choi= Choi Method see choi et. all.,troiano = troiano Method see troiano et all
    null.thresh <- 400
    window.size <- 90
    unitercp.min <- 30
    spike.min <- 2
    maxcount.thresh <- 1000
    ValuesbyDefault <- TRUE
    
    #### Packages and Scripts used; dont edit!
    {
      setwd(Scriptpath)
      source(paste(Scriptpath,"activPal_Functions_alpha_03.R",sep=""))
      #source(paste(Scriptpath,"MVPA_Bout_detection_alpha.R",sep=""))
    }

    y <- batch_start(Acc.input=Acc.input,output=output,ACT.ALL.out=ACT.ALL.out,Domain.out=Domain.out,NW.out=NW.out,
               early=early, late=late ,max.break=max.break,min.sleeplength=min.sleeplength,id.length=id.length
               ,separated.csv=separated.csv, JoinedDomains=JoinedDomains,JoinedNWs=JoinedNWs, separator=separator, dec=dec
               ,run.from=run.from, run.to=run.to, fill_sleepTab=fill_sleepTab,day.begin=day.begin, time.unit=time.unit,
               ignore.firstday=ignore.firstday, ignore.lastday=ignore.lastday,AVG_Filepath=AVG_Filepath,
               generateSupport=generateSupport, calcIndivualAvg=calcIndivualAvg,mvpa_steps_cutoff=mvpa_steps_cutoff,
               WriteMastersheet_perID=WriteMastersheet_perID, is.xlsx=is.xlsx, sheetIndex=sheetIndex, calcBySteps=calcBySteps, mvpa_count_thresh=mvpa_count_thresh,
               save_as_xlsx=save_as_xlsx, non.stop=non.stop,id.asnumeric=id.asnumeric,Slp.Path=Slp.Path,
               sleepMode=sleepMode, sleep.start=sleep.start, sleep.end=sleep.end,is.StartAfterMidNight=is.StartAfterMidNight,PrintMode=PrintMode,
               RunSedBouts=RunSedBouts,RunMVPA_Bouts=RunMVPA_Bouts, SedBoutMasterTab=SedBoutMasterTab,
               NwMethod=NwMethod,null.thresh=null.thresh,window.size=window.size,unitercp.min=unitercp.min,spike.min=spike.min, maxcount.thresh=maxcount.thresh, ValuesbyDefault=ValuesbyDefault)
    

