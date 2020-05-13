#!/bin/bash
for script in scripts/*.R ; do
	echo "Now running $script"
	Rscript script
done
rm Rplots.pdf
