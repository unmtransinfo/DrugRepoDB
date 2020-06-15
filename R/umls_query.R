##########################################################################
# umls_query.R - Retreive UMLS information
# 2016: Adam Brown; last Update: 11/30/16
##########################################################################
# 2020: Repo forked, updated for DrugCentral-2020 and AACT snapshot.
# DrugBank maybe not available, nor needed.
##########################################################################
library('httr')
library('xml2')
library("stringr")
library("data.table")
library("yaml")

## UMLS
AUTH_URI <- "https://utslogin.nlm.nih.gov/cas/v1/tickets"

# UMLS license is required for the use of this script.
# Register at:  https://utslogin.nlm.nih.gov
# Either: (1) API_USERNAME and API_PASSWORD, or: (2) API_KEY
# Store in $HOME/.umls.yaml e.g.
# API_KEY: "=======REPLACE_WITH_API_KEY======"

params <- read_yaml(paste0(Sys.getenv("HOME"), "/.umls.yaml"))

# Get Ticket Granting Ticket, Valid for 8 Hours
getTGT <- function(params, auth_uri) {
  if (!is.null(params[["API_USERNAME"]]) & !is.null(params[["API_PASSWORD"]])) {
    tgt_response <- POST(auth_uri, body = list(username = params[["API_USERNAME"]], password = params[["API_PASSWORD"]]), encode = "form")
  } else if (!is.null(params[["API_KEY"]])) {
    tgt_response <- POST(auth_uri, body = list(apikey = params[["API_KEY"]]), encode = "form")
  } else {
    message("ERROR: UMLS credentials not found.")
    return(NULL)
  }
  warn_for_status(tgt_response)
  tgt_uri       <- headers(tgt_response)$location
  tgt_timestamp <- Sys.time()
  return(list(tgt_uri, tgt_timestamp) )
}

tgt <- getTGT(params, AUTH_URI)


# Get Service Ticket, Valid for 5 Minutes
getST <- function(tgt, verbose) {
    timeout(15)
    auth_uri <- paste(tgt[[1]])
    st_response <- POST(auth_uri, body = list(service = "http://umlsks.nlm.nih.gov"), encode = "form")
    warn_for_status(st_response)
    st <- content(st_response, type="text/plain", encoding="UTF-8")
    if (verbose) {
        print(paste("getst: ", http_status(st_response)$message, sep = ""))
        print(paste("     st= ", st, sep = ""))
        print(paste("    tgt= ", str_split(tgt, "/")[[1]][7], sep = ""))
    }
    # no sleeping time neccessary of run at LHC
    # random sleep of 5-10ms for each ST
    # sleep_time <- round(runif(1, 0.005, 0.010), digits = 3) 
    # Sys.sleep(sleep_time)
    return(st)
}

paste("st=[", getST(tgt, TRUE), "]", sep="") #Debug

# Get Semantic Type
getSemTyp <- function(cui) {
    res <- GET(paste0('https://uts-ws.nlm.nih.gov/rest/content/current/CUI/', cui, '?ticket=', getST(tgt, verbose=F)))
    con <- content(res, encoding="UTF-8")
    out <- con$result$semanticTypes[[1]]$name
    if (is.null(out)) return(NA)
    else return(out)
}

getName <- function(cui) {
    res <- GET(paste0('https://uts-ws.nlm.nih.gov/rest/content/current/CUI/', cui, '?ticket=', getST(tgt, verbose=F)))
    con <- content(res, encoding="UTF-8")
    out <- con$result$name
    if (is.null(out)) return(NA)
    else return(out)
}

# Search
getCUI <- function(string, search_typ, version, verbose) {
 
    if (! search_typ %in% c("exact", "words", "leftTruncation", "rightTruncation", "approximate", "normalizedString")) {
        warning("Invalid search type")
        break
    }
    if (verbose) {
        print(paste("START getCUI for [", string, "][",search_typ,"]", sep = ""))
    }
    
    timeout(5)
    search_uri <- paste("https://uts-ws.nlm.nih.gov/rest/search", version, sep = "/")
    
    concepts       <- list()
    page           <- 0
    attempt        <- 0
    max_attempt    <- 6        # maximum number of tries if a http error occurs [6 is good]
    snooze_time    <- 10       # time (seconds) of the pause when error occurs [10 is good]
    Res            <- NULL
    sleep_time     <- 0        # no sleeping time neccessary of run at LHC
    
    repeat {
        page <- (page+1)
        concepts_cnt <- length(concepts)
        response <- GET(url=search_uri, query=list(string = string, ticket = getST(tgt, verbose), searchType=search_typ, pageNumber=page))
        
        # no sleeping time neccessary of run at LHC
        # sleep_time <- round(runif(1, 0.015, 0.025), digits = 3) # random sleep of 10-25ms
        # Sys.sleep(sleep_time)
        
        if (verbose) { print(paste("getCUI: random sleep (sec)= ", sleep_time, sep = "")) }
        
        if (http_error(response)) {
            page <- 0
            
            if (attempt<max_attempt) {
                attempt <- (attempt + 1)
                sleep_time <- snooze_time*attempt*runif(1, 0.5, 1)
                message_for_status(response, paste("mapping term [", string, "]", "[Attempt #", attempt, "]", sep = ""))
                print(paste("[Now taking a ", sleep_time, " sec break]", sep = ""))
                Sys.sleep(sleep_time)
                next
            }
            stop("Too many failures, unable tor perform mapping.")
        }
        else
        {
            Res <- content(response, encoding="UTF-8")
            for (i in 1:length(Res$result$results)) {
                concepts[[concepts_cnt+i]] <- c(Res$result$results[[i]]$ui,
                                                Res$result$results[[i]]$name,
                                                Res$result$results[[i]]$rootSource,
                                                Res$result$results[[i]]$uri)
            }
        }
        if (Res$result$results[[1]]$ui == "NONE") {
            break
        }
    }
    
    if (length(concepts) > 1) {
        if (verbose) { print(paste("END getCUI for [", string, "][",(length(concepts)-1)," CUI found]", sep = "")) }
        concepts <- concepts[1:(length(concepts)-1)]
    }
    else
    {
        if (verbose) { print(paste("END getCUI for [", string, "][0 CUI found]", sep = "")) }
        concepts[[1]] <- c("NO_CONCEPT_MAPPED_TO","","","")
    }
    return(concepts) 
}

message("Done: (umls_query.R)")