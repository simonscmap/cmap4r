#!/bin/bash

Rscript -e "library(cmap4r); initialize_cmap(cmap_key='your_key_goes_here'); bookdown::render_book('.')"

echo "==================================" >> bookdown-monthly-diff.txt
echo "Difference in bookdown for this month:" >> bookdown-monthly-diff.txt
echo $(date) >> bookdown-monthly-diff.txt
git diff . >> bookdown-monthly-diff.txt
echo "==================================" >> bookdown-monthly-diff.txt

# sendmail robohyun66@gmail.com < bookdown-monthly-diff.txt
mail -s 'Monthly cmap4r bookdown diff' robohyun66@gmail.com < bookdown-monthly-diff.txt
