#!/bin/sh

exec julia -e "import forth; forth.run(\"$1\")"
