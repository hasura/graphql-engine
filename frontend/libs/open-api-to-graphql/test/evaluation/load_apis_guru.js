// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

'use strict';

const git = require('isomorphic-git');
const http = require('isomorphic-git/http/node');
const fs = require('fs');
const rimraf = require('rimraf');

const REPO_URL = 'https://github.com/APIs-guru/openapi-directory.git';
const FOLDER_PATH = 'tmp';

/**
 * Download all OAS from APIs.guru.
 *
 * @return {Promise} Resolves on array of OAS
 */
const downloadOas = () => {
  return git.clone({
    fs: fs,
    http,
    dir: FOLDER_PATH,
    url: REPO_URL,
    singleBranch: true,
    depth: 1,
  });
};

/**
 * Helpers
 */
const emptyTmp = () => {
  rimraf.sync(FOLDER_PATH);
};

// go go go:
emptyTmp();
downloadOas()
  .then(() => {
    console.log(`Loaded files from ${REPO_URL} to ${FOLDER_PATH}`);
  })
  .catch(console.error);
