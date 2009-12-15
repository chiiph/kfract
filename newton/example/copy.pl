#!/usr/bin/perl

#Copies files named fractal####.jpg to fractal****.jpg where **** is
#250 higher than the previous.

use File::Copy; 
for (glob "fractal*.jpg") { 
    /^fractal(\d{4})\.jpg$/ or next; 
    if ($1 >= 250) {
	die;
    } 
    $n = sprintf "fractal%04d.jpg",  $1 + 250; 
    copy $_, $n or die "$_: $!" 
}
