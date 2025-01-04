library(cronR)

cron_ls()

path1 <- "/Users/tylerpollard/Desktop/NFLAnalysisTest/scripts/UpdateData/UpdateDataCronScript.R"
path2 <- "/Users/tylerpollard/Desktop/NFLAnalysisTest/scripts/UpdateData/UpdateDataCronGit.sh"

cmd1 <- cron_rscript(path1)
cmd2 <- cron_rscript(path2)

cron_add(command = cmd1, 
         frequency = "daily", 
         at = "21:00", 
         id = "test1",
         description = "My process 1"
)
cron_add(command = cmd2, 
         frequency = "daily", 
         at = "21:02", 
         id = "test2",
         description = "My process 2"
)

cron_ls()

cron_rm("job_eca3e1bd13736aff8749d48be8b42aa4")
