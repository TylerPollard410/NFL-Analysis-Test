library(cronR)

cron_ls()

path1 <- "/Users/tylerpollard/Desktop/NFLAnalysisTest/scripts/UpdateData/UpdateDataCronScript.R"
path2 <- "/Users/tylerpollard/Desktop/NFLAnalysisTest/scripts/UpdateData/UpdateDataCronGit.sh"

cmd1 <- cron_rscript(path1)
cmd2 <- cron_rscript(path2)

cron_add(command = cmd1, 
         frequency = "daily", 
         at = "21:13", 
         id = "test1",
         description = "My process 1"
)
cron_add(command = cmd2, 
         frequency = "daily", 
         at = "21:02", 
         id = "test2",
         description = "My process 2"
)

## cronR job
## id:   updateNFLdata
## tags: 
## desc: Automated script to update nfl data for app
55 21 * * * '/Users/tylerpollard/Desktop/NFLAnalysisTest/scripts/UpdateData/UpdateDataCronGit.sh'  >> '/Users/tylerpollard/Desktop/NFLAnalysisTest/scripts/UpdateData/UpdateDataCronGit.log' 2>&1

cron_ls()

cron_rm("job_abc0f4a0148a92b5520c5760f78d983e")
