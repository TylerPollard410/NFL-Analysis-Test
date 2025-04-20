library(rsconnect)

# writeManifest(appDir = "app", 
#               appPrimaryDoc = "ui.R")

depFiles <- listDeploymentFiles(appDir = "~/Desktop/NFLAnalysisTest/app")
depFiles
depFiles <- stringr::str_subset(depFiles, "modelFits", negate = TRUE)
depFiles <- depFiles[!depFiles %in% c("check_package_deps.R",
                                      "deploy.R")]
depFiles

deployApp(appDir = "~/Desktop/NFLAnalysisTest/app", 
          appName = "NFL_Analysis", 
          server = "shinyapps.io",
          account = "tylerpollard410", 
          appFiles  = depFiles,
          forceUpdate = TRUE)

