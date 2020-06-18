date_path=$(date +%Y-%m-%d)
Rscript pipeline.R
git add *
git commit -m "Update ${date_path}" 
git push