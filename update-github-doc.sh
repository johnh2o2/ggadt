github_doc_dir=~/Desktop/Draine_temp/GGADT_JohnsMac/gh-pages/ggadt
cp doc/htmldoc/* $github_doc_dir
cd $github_doc_dir
git add --all
git commit -m "Automatic update!"
git push -u origin gh-pages
