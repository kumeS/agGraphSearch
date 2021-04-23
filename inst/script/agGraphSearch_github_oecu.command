#!/bin/bash

MY_DIRNAME=$(dirname $0)
cd $MY_DIRNAME

export https_proxy="http://wwwproxy.osakac.ac.jp:8080"
export ftp_proxy="http://wwwproxy.osakac.ac.jp:8080"

cd ./agGraphSearch

du -a | grep .DS_Store | xargs rm -rf

git add -A

git commit -m "first version"

git push origin main

git config pull.rebase false

git pull

open "https://github.com/kumeS/agGraphSearch"

exit

