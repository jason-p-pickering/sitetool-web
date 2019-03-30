require(datapackr)
require(scales)
options(shiny.maxRequestSize = 100 * 1024 ^ 2)
options("baseurl" = "http://127.0.0.1:8080/")
#options("baseurl" = "https://www.datim.org/")

DHISLogin <- function(baseurl, username, password) {
  httr::set_config(httr::config(http_version = 0))
  url <- URLencode(URL = paste0(getOption("baseurl"), "api/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url,
                 httr::authenticate(username, password),
                 httr::timeout(60))
  if (r$status != 200L) {
    return(FALSE)
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))
    options("organisationUnit" = me$organisationUnits$id)
    return(TRUE)
  }
}

validateDataElementOrgunits<-function(d) {
  
  datasets <- c("nIHNMxuPUOR", "sBv1dj90IX6")
  vr_data <- d$datim$site_data
  names(vr_data) <- c("dataElement",
                      "period",
                      "orgUnit",
                      "categoryOptionCombo",
                      "attributeOptionCombo",
                      "value")
  
  de_check <-
    datimvalidation::checkDataElementOrgunitValidity(
      data = vr_data,
      datasets = ds,
      organisationUnit = d$info$datapack_uid
    ) 
  
  if (inherits(de_check, "data.frame")) {
    messages<-append(paste(
      NROW(de_check),
      "invalid data element/orgunit associations found!"
    ), messages)
    
    d$datim$dataelement_disagg_check<-de_check
    d$info$warningMsg<-append(messages,d$info$warningMsg)
    d$info$had_error<-TRUE
  } 
  
  d
}

validateDataElementDisaggs<-function(d){
  #Validation rule checking
  vr_data <- d$datim$site_data
  names(vr_data) <- c("dataElement",
                      "period",
                      "orgUnit",
                      "categoryOptionCombo",
                      "attributeOptionCombo",
                      "value")
  datasets <- c("nIHNMxuPUOR", "sBv1dj90IX6")
  
  des_disagg_check<-datimvalidation::checkDataElementDisaggValidity(data=vr_data,
                                                                    datasets=datasets)
  
  
  if (inherits(des_disagg_check, "data.frame")) {
    
    d$datim$des_disagg_check<-des_disagg_check
    msg <- "ERROR!: Invalid data element / disagg combinations found!"
    d$info$warningMsg<-append(msg,d$info$warningMsg)
    d$info$had_error<-TRUE
  }
  
  d
}

validateSiteData <- function(d) {
  #Validation rule checking
  vr_data <- d$datim$site_data
  names(vr_data) <- c("dataElement",
                      "period",
                      "orgUnit",
                      "categoryOptionCombo",
                      "attributeOptionCombo",
                      "value")
  vr_data$attributeOptionCombo <-
    datimvalidation::remapMechs(vr_data$attributeOptionCombo,
                                getOption("organisationUnit"),
                                "code",
                                "id")
  datasets_uid <- c("nIHNMxuPUOR", "sBv1dj90IX6")
  if ( Sys.info()["sysname"] == "Linux") {
    ncores <- parallel::detectCores() - 1
    doMC::registerDoMC( cores = ncores )
    is_parallel <- TRUE
  } else {
    is_parallel <- FALSE
  } 
  vr_violations <- datimvalidation::validateData(vr_data,
                                                 datasets = datasets_uid,
                                                 parallel = is_parallel)
   rules_to_keep <- c(
     "L76D9NGEPRS",
     "rVVZmdG1KTb",
     "zEOFo6X436M",
     "oVtpQHVVeCV",
     "r0CC6MQW5zc",
     "vrS3kAtlJ4F",
     "WB338HNucS7",
     "tiagZGzSh6G",
     "vkFHYHgfqCf",
     "coODsuNsoXu",
     "qOnTyseQXv8",
     "Ry93Kc34Zwg",
     "g0XwMGLB5XP",
     "eb02xBNx7bD",
     "SNzoIyNuanF"
   )
  
  vr_violations<-vr_violations[ vr_violations$id %in% rules_to_keep, ]
  
  diff <- gsub(" <= ", "/", vr_violations$formula)
  
  vr_violations$diff <-
    sapply( diff, function(x) {
      round( ( eval( parse( text = x) ) - 1) * 100, 2 )
    })
  
  d$datim$vr_rules_check <- vr_violations %>% dplyr::filter(diff >= 5) %>%
    dplyr::select(name,ou_name,mech_code,formula,diff) 
  
  d
  
}

adornMechanisms <- function(d) {
  
  cached_mechs <- "/srv/shiny-server/apps/sitetool/mechs.rds"
  
  if ( file.access(cached_mechs,4) == 0 ) {
    
    mechs <-readRDS(cached_mechs)
    
  } else {
    
    mechs <- paste0(getOption("baseurl"), "api/sqlViews/fgUtV6e9YIX/data.csv") %>%
      httr::GET() %>%
      httr::content(., "text") %>%
      readr::read_csv(col_names = TRUE) %>%
      dplyr::select(mechanismCode = "code", partner, agency, ou)
  }
  
  if  ( !inherits(mechs,"data.frame") ) {stop("COULD NOT RETREIVE MECHANISM LIST")}
  
  d$datim$site_data_pretty <- d$datim$site_data %>%
    dplyr::left_join( mechs, by = c(  "attributeOptionCombo" = "mechanismCode" ))
  
  d
  
  
}

adornDataElements<-function(d){
  
  modality_map <- paste0(getOption("baseurl"),"api/dataElementGroupSets/Jm6OwL9IqEa?fields=dataElementGroups[name,dataElements[id]]") %>%
    URLencode(.) %>%
    httr::GET(.) %>%
    httr::content(.,"text") %>%
    jsonlite::fromJSON(.,flatten = TRUE) %>%
    purrr::pluck(.,"dataElementGroups") %>% 
    dplyr::mutate_if(is.list, purrr::simplify_all) %>% 
    tidyr::unnest() %>%
    dplyr::distinct() %>%
    dplyr::select(dataElement = dataElements,
                  hts_modality = name ) %>%
    dplyr::mutate(hts_modality = stringr::str_remove(hts_modality,"FY19R/FY20T"))
  
  d$datim$site_data_pretty %<>%  dplyr::left_join( modality_map, by = "dataElement")
  
  d
  
  
}

adornSites<-function(d) {
  
  site_list <- getSiteList(d$info$datapack_uid) %>%
    dplyr::select(site_name=name,
                  site_type,
                  psnu,
                  orgUnit=id)
  
  d$datim$site_data_pretty %<>% 
    dplyr::left_join(site_list, by="orgUnit") %>%
    dplyr::select(-orgUnit)
  
  d
  
}

adornMetdata<-function(d) {
  
  d$datim$site_data_pretty %<>%
    dplyr::mutate(
      dataElement = datimvalidation::remapDEs(dataElement,"id","shortName"),
      categoryOptionCombo = datimvalidation::remapCategoryOptionCombos(categoryOptionCombo,"id","shortName")
    ) 
  
  d
  
}


modalitySummaryChart <- function(d) {
  
  age_order<-c(
    "<1", 
    "1-4",
    "5-9" ,
    "10-14",
    "15-19",
    "20-24",
    "25-29",
    "30-34",
    "35-39",
    "40-44",
    "45-49",
    "50+")
  
  foo <- d$datim$site_data_pretty %>% 
    dplyr::filter(!is.na(hts_modality)) %>% 
    dplyr::mutate(hts_sex = ifelse(stringr::str_detect(categoryOptionCombo,"Male"),"Male","Female")) %>% 
    tidyr::separate(categoryOptionCombo,into=c("Age","Status","Sex"),sep=",") %>%
    dplyr::mutate_if(is.character, stringr::str_trim) %>%
    dplyr::group_by(hts_modality,hts_sex,Age) %>%
    dplyr::summarise(value=sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Age=factor(as.character(Age),levels=age_order))
  
  ggplot(foo, aes(x=Age,y=value, fill=hts_sex)) + 
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::comma) + 
    facet_wrap(~hts_modality) + 
    theme() +
    scale_fill_manual(values = c("#548dc0", "#59BFB3")) +
    labs(y = "", x = "",
         title = "COP19/FY20 Testing Targets") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom",
      legend.title = element_blank(),
      text = element_text(color = "#595959", size = 14),
      plot.title = element_text(face = "bold"))
  
}