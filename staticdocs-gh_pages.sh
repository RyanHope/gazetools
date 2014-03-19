#!/bin/bash

export
ls
if [ "$TRAVIS_REPO_SLUG" == "${GH_REF}" ] && [ "$TRAVIS_PULL_REQUEST" == "false" ] && [ "$TRAVIS_BRANCH" == "master" ]; then

  echo -e "Publishing staticdocs...\n"
  
  cp -r inst/web ${HOME}
  cd $HOME
  
  git config --global user.email "travis@travis-ci.org"
  git config --global user.name "travis-ci"
  git clone --quiet --branch=gh-pages https://${GH_TOKEN}@github.com/${GH_REF} gh-pages > /dev/null
  
  # Commit and Push the Changes
  cd gh-pages
  git rm -rf *
  cp -Rf $HOME/web/* .
  touch .nojekyll
  git add -A
  git commit -m "Lastest staticdoc on successful travis build $TRAVIS_BUILD_NUMBER auto-pushed to gh-pages"
  git push -fq origin gh-pages
  
  echo -e "Published staticdocs to gh-pages.\n"

fi