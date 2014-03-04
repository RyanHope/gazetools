oi#!/bin/bash

echo -e "Publishing staticdocs...\n"
export

ls -lah

cp -r inst/web ${HOME}
cd $HOME

ls -lah

git config --global user.email "travis@travis-ci.org"
git config --global user.name "travis-ci"
git clone --quiet --branch=gh-pages https://${GH_TOKEN}@github.com/ryanhope/gazetools gh-pages > /dev/null

# Commit and Push the Changes
cd gh-pages
git rm -rf *
cp -Rf $HOME/web/* .
git add -A
git commit -m "Lastest staticdoc on successful travis build $TRAVIS_BUILD_NUMBER auto-pushed to gh-pages"
git push -fq origin gh-pages# > /dev/null

echo -e "Published staticdocs to gh-pages.\n"
