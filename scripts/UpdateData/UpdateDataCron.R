library(cronR)

cron_ls()

path <- "/Users/tylerpollard/Desktop/NFLAnalysisTest/scripts/UpdateData/UpdateDataCron.R"

cmd <- cron_rscript(path)

cron_add(command = cmd, 
         frequency = "daily", 
         at = "20:30", 
         id = "test1",
         description = "My process 1", 
         tags = c("lab", "xyz")
         )

cron_ls()

cron_rm("test1")
