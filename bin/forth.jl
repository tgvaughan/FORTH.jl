#!/bin/bash

if [ -z "$1" ]; then 
    exec julia -e "import forth; forth.run()"
else
    exec julia -e "import forth; forth.run(\"$1\")"
fi
