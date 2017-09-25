#!/usr/bin/env bash

./build.sh

rm -fR app
mkdir app
cp -R dist/* app

serverless deploy

serverless syncToS3

serverless domainInfo
