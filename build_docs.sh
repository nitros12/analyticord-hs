#!/bin/bash
rm -rf docs/*
stack haddock
cp -r $(stack path --local-doc-root)/analyticord-hs-*/. docs/.
python3 fix_docs.py
