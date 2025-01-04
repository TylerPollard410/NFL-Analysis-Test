library(cronR)

cron_ls()

path <- "~/Desktop/NFL Analysis Test/scripts/Update Data/UpdateDataCron.R"

cmd <- cron_rscript(path)

cron_add(command = cmd, 
         frequency = "daily", 
         at = "20:13", 
         id = "test1",
         description = "My process 1", 
         tags = c("lab", "xyz")
         )

cron_ls()

cron_rm("test1")
