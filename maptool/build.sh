#!/bin/bash

set -x

convert ../Tanoa.png -resize '25%' x1.png
convert ../Tanoa.png -resize '50%' x2.png
cp ../Tanoa.png x3.png

function tile_image {
	local num=$1;
	mkdir ${num};
	convert x${num}.png -crop 1000x1000 \
		-background none \
		-gravity northwest -extent 1000x1000 \
		-set filename:tile "x%[fx:floor(page.x/1000+0.1)]_y%[fx:floor(page.y/1000+0.1)]" \
		-quality '85%' \
		+repage  +adjoin \
		"./${num}/%[filename:tile].jpg";
}

tile_image 1
tile_image 2
tile_image 3
