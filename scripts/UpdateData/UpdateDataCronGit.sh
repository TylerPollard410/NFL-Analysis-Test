#!/bin/bash

/Library/Frameworks/R.framework/Resources/bin/Rscript '/Users/tylerpollard/Desktop/NFLAnalysisTest/scripts/UpdateData/UpdateDataCronScript.R'

# Navigate to the local Git repository
cd /Users/tylerpollard/Desktop/NFLAnalysisTest

# Add changes to the staging area
git add -A

# Commit the changes with a timestamp message
git commit -m "Auto-update: $(date +'%Y-%m-%d %H:%M:%S')"

# Push the changes to the remote repository
git push
