#!/usr/bin/perl -w

use lib qw(. ./lib ./blib/lib ./blib/arch);
use word2vec::Model;

use PDL;
use Getopt::Long;
use File::Basename qw(basename);
use open qw(:std :utf8);
use strict;

##==============================================================================
## Command-line
my $prog = basename($0);
my ($help);
my $outfile = '-';
my $fmt = '%.8g';

my %model = (minn=>0,maxn=>0, logOOV=>'trace');
my %log   = (level=>'DEBUG', rootLevel=>'FATAL');
GetOptions(
	   'h|help' => \$help,
	   'minn=i' => \$model{minn},
	   'maxn=i' => \$model{maxn},
	   #'i|ids!' => \$dictids,
	   'F|format=s' => \$fmt,

	   ##-- logging
	   'log-level|level|ll=s' => sub { $log{level} = uc($_[1]); },
	   'log-option|logopt|lo=s' => \%log,
	  );
if ($help || @ARGV < 1) {
  print STDERR <<EOF;

Usage: $0 [OPTIONS] MODEL [WORDLISTFILE=-]

Options:
  -h,  -help                # this help message
  -F,  -format FORMAT       # printf format for vector components (default=$fmt)
       -minn MINN           # minimum n-gram length for OOV words (default=$model{minn})
       -maxn MAXN           # minimum n-gram length for OOV words (default=$model{maxn})
  -ll, -log-level LEVEL     # set log-level LEVEL (default=$log{level})
  -lo, -log-option OPT=VAL  # set generic logging option

EOF
#    -i, -[no]ids              # do/don't include dictionary IDs (default=$dictids)
  exit ($help ? 0 : 1);
}

##==============================================================================
## MAIN

##-- setup logger
DiaColloDB::Logger->ensureLog(%log);

##-- open model
my $ftbase   = shift(@ARGV);
my $model = word2vec::Model->new(%model,base=>$ftbase)
  or die("$0: failed to open model '$ftbase': $!");

##-- open output file
open(my $outfh, ">$outfile")
  or die("$0: open failed for output file '$outfile': $!");

##-- process targets
my $infile = @ARGV ? shift(@ARGV) : '-';
$model->info("processing word-list file '$infile'");
open(my $infh,"<$infile")
  or die("$0: failed to open input file '$infile': $!");
my ($w,$wi,$wv);
my $nunkw = 0;
my $nw    = 0;
while (defined($_=<$infh>)) {
  chomp;
  next if (/^\s*$/);
  $w  = $_;
  $w  =~ s/\s/_/g;
  $wv = $model->any2v($w);
  ++$nunkw if (!any($wv));
  ++$nw;

  print $outfh join(' ',$w,(map {sprintf($fmt,$_)} $wv->list)), "\n";
}
close($infh)
  or die("$0: failed to close input file '$infile': $!");
close($outfh)
  or die("$0: failed to close output file '$outfile': $!");

##-- summarize
$model->info("processed $nw word(s), $nunkw unknowns (".sprintf("%.1f %%",$nw ? (100*$nunkw/$nw) : 'nan').")");

