#!/usr/bin/env bash
rg --files | entr -s 'stack build && stack exec apple-health-parser-exe health_trunc.xml'
