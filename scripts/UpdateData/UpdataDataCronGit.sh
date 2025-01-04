#!/bin/bash

# Navigate to the local Git repository
cd /path/to/your/local/repo

# Add changes to the staging area
git add .

# Commit the changes with a timestamp message
git commit -m "Auto-update: $(date +'%Y-%m-%d %H:%M:%S')"

# Push the changes to the remote repository
git push origin main
