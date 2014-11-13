#! /usr/bin/perl 

use strict;
use warnings;
use Text::Diff;
use File::Basename;

my $mcb = $ARGV[0];

my @paths = fileparse($mcb,qr/\.[^.]*/);
my $path = $paths[1];
my $filename = $paths[0];

system("./matcab -e < $mcb > $path$filename.run 2>&1");
my $diff = diff "$path$filename.run" ,"$path$filename.output";
if ($diff eq ""){
	print $mcb." Pass\n";
}else{
	print $diff."\n";
}
system("rm  $path$filename.run");
#./matcab -e < test/var/var9.mcb > a 2>&1


