#!/usr/bin/perl -w

use lib qw(. ./lib ./blib/lib ./blib/arch);
use word2vec::Model;

use PDL;
use PDL::VectorValued;
use Getopt::Long qw(:config no_ignore_case);
use File::Basename qw(basename);
use open qw(:std :utf8);
use strict;

##==============================================================================
## Command-line
my $prog = basename($0);
my ($help);
my $k = 10;
my %model = (minn=>0,maxn=>0,nganchor=>0,ngweight=>0);
my %log  = (level=>'TRACE', rootLevel=>'FATAL');

GetOptions(
	   'h|help' => \$help,
	   'k|knn=i' => \$k,

	   'minn=i' => \$model{minn},
	   'maxn=i' => \$model{maxn},
	   'a|anchor|nganchor!' => \$model{nganchor},
	   'w|weight|ngweight!' => \$model{ngweight},

	   ##-- logging
	   'log-level|level|ll=s' => sub { $log{level} = uc($_[1]); },
	   'log-option|logopt|lo=s' => \%log,
	  );
if ($help || @ARGV < 2) {
  print STDERR <<EOF;

Usage: $prog \[OPTIONS] MODEL WORD...

Options:
  -h,  -help                # this help message
  -k,  -knn K               # number of nearest neighbors to extract (default=$k)
       -minn MINN           # minimum n-gram length for OOV words (default=$model{minn})
       -maxn MAXN           # minimum n-gram length for OOV words (default=$model{maxn})
       -[no]anchor          # do/don't use only anchored (BOW,EOW) n-gram regexes (default=$model{nganchor})
       -[no]weight          # do/don't weight regex matches by frequency if available (default=$model{ngweight})
  -ll, -log-level LEVEL     # set log-level LEVEL (default=$log{level})
  -lo, -log-option OPT=VAL  # set generic logging option

EOF
  exit ($help ? 0 : 1);
}


##==============================================================================
## MAIN

##-- setup logger
DiaColloDB::Logger->ensureLog(%log);

##-- open model
my $modelbase = shift(@ARGV);
my $model = word2vec::Model->new(%model,base=>$modelbase)
  or die("$prog: failed to open model '$modelbase': $!");

##-- get query vector
my $qv = $model->expr2v(@ARGV);
#print STDERR "qv = $qv\n";

##-- get distances
my ($nwi,$sim) = $model->knn($qv,$k);
my ($ki,$vi,$v);
for ($ki=0; $ki < $k; ++$ki) {
  $vi = $nwi->at($ki);
  $v  = $model->{wenum}->i2s($vi) // '(undef)';
  printf(" %".length($k)."d:\t%.4f\t%s\n", $ki+1,$sim->at($ki),$v);
}
