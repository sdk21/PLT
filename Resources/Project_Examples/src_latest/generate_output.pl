#! /usr/bin/perl 

use strict;
use warnings;
use File::Basename;

my $mcb = $ARGV[0];

my @paths = fileparse($mcb,qr/\.[^.]*/);
my $path = $paths[1];
my $filename = $paths[0];

print "$mcb\n";
system("./matcab -a < $mcb > $path$filename.ast 2>&1");
system("./matcab -c < $mcb > $path$filename.java 2>&1");
system("./matcab -e < $mcb > $path$filename.output 2>&1");





