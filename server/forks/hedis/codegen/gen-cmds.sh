#!/usr/bin/env bash

echo "generating Commands.hs"
runhaskell codegen/GenCmds.hs\
    < codegen/commands.json\
    > src/Database/Redis/Commands.hs 
