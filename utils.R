require(datapackr)
require(scales)
options(shiny.maxRequestSize = 100 * 1024 ^ 2)
options("baseurl" = "http://127.0.0.1:8080/")

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
  # rules_to_keep <- c(
  #   "L76D9NGEPRS",
  #   "rVVZmdG1KTb",
  #   "zEOFo6X436M",
  #   "oVtpQHVVeCV",
  #   "r0CC6MQW5zc",
  #   "vrS3kAtlJ4F",
  #   "WB338HNucS7",
  #   "tiagZGzSh6G",
  #   "vkFHYHgfqCf",
  #   "coODsuNsoXu",
  #   "qOnTyseQXv8",
  #   "Ry93Kc34Zwg",
  #   "g0XwMGLB5XP",
  #   "eb02xBNx7bD",
  #   "SNzoIyNuanF"
  # )
  #
  #vr_violations<-vr_violations[ vr_violations$id %in% rules_to_keep, ]
  
  diff <- gsub(" <= ", "/", vr_violations$formula)
  
  vr_violations$diff <-
    sapply( diff, function(x) {
      round( ( eval( parse( text = x) ) - 1) * 100, 2 )
    })
  
  d$datim$vr_rules_check <- vr_violations %>% dplyr::filter(diff >= 5) %>%
    dplyr::select(name,ou_name,mech_code,formula,diff) 

 d
  
}


adornSiteData<-function(d) {
  
  
  cached_mechs <- "/srv/shiny-server/apps/sitetool/mechs.rds"
  
  if ( file.access(cached_mechs,4) == -1 ) {
    
    mechs <-readRDS(cached_mechs)
    
  } else {
    
    mechs <- paste0(getOption("baseurl"), "api/sqlViews/fgUtV6e9YIX/data.csv") %>%
      httr::GET() %>%
      httr::content(., "text") %>%
      readr::read_csv(col_names = TRUE) %>%
      dplyr::select(mechanismCode = "code", partner, agency, ou)
  }
  

  
  site_list <- getSiteList(d$info$datapack_uid) %>%
    dplyr::select(site_name=name,
                  site_type,
                  psnu,
                  orgUnit=id)
  
  d$datim$site_data_pretty <- d$datim$site_data %>%
    dplyr::mutate(
      dataElement = datimvalidation::remapDEs(dataElement,"id","shortName"),
      categoryOptionCombo = datimvalidation::remapCategoryOptionCombos(categoryOptionCombo,"id","shortName")
      ) %>%
    dplyr::left_join(site_list, by="orgUnit") %>%
    dplyr::left_join( mechs, by = c(  "attributeOptionCombo" = "mechanismCode" )) %>% 
    dplyr::select(-orgUnit)
  
  return(d)
}
  

modalitySummaryChart <- function(df) {

   df %>% 
    dplyr::filter(!is.na(modality)) %>%
    dplyr::group_by(modality, resultstatus) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(modality, desc(resultstatus)) %>% 
    dplyr::mutate(resultstatus = factor(resultstatus, c("Negative", "Positive"))) %>%
    ggplot(aes(
      y = value,
      x = reorder(modality, value, sum),
      fill = resultstatus
    )) +
    geom_col() +
    scale_y_continuous(labels = scales::comma) +
    coord_flip() +
    scale_fill_manual(values = c("#548dc0", "#59BFB3")) +
    labs(y = "", x = "",
         title = "COP19/FY20 Testing Targets",
         subtitle = "modalities ordered by total tests") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(color = "#595959", size = 14),
          plot.title = element_text(face = "bold"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "#595959"),
          panel.grid.minor.y = element_blank())

}