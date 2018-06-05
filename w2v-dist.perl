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
my $stats = 1;
my %model = (minn=>0,maxn=>0,nganchor=>0,ngweight=>1);
my %log  = (level=>'INFO', rootLevel=>'FATAL');

GetOptions(
	   'h|help' => \$help,
	   'stats|s!' => \$stats,

	   'minn=i' => \$model{minn},
	   'maxn=i' => \$model{maxn},
	   'a|anchor|nganchor!' => \$model{nganchor},
	   'w|weight|ngweight!' => \$model{ngweight},

	   ##-- logging
	   'log-level|level|ll=s' => sub { $log{level} = uc($_[1]); },
	   'log-option|logopt|lo=s' => \%log,
	  );
if ($help || @ARGV < 3) {
  print STDERR <<EOF;

Usage: $prog \[OPTIONS] MODEL WORD WORD...

Options:
  -h,  -help                # this help message
  -s,  -[no]stats           # do/don't report statistics (default=do)
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

##-- get query vectors
my $qmat = zeroes($model->{rwmat}->type, $model->{nr}, scalar(@ARGV));
foreach (0..$#ARGV) {
  utf8::decode($ARGV[$_]) if (!utf8::is_utf8($ARGV[$_]));
  $qmat->slice(",($_)") .= $model->expr2v($ARGV[$_]);
}

##-- get distances
my $dmat = $qmat->vv_vcos($qmat);

##-- dump distances
my ($i,$j, $dij);
for ($i=0; $i <= $#ARGV; ++$i) {
  for ($j=$i+1; $j <= $#ARGV; ++$j) {
    $dij = sprintf("%.4f",$dmat->at($i,$j));
    print join("\t", $dij, $ARGV[$i], $ARGV[$j]), "\n";
  }
}

##-- get stats
if ($stats) {
  my $dflat = $dmat->where($dmat->yvals > $dmat->xvals);
  my ($avg,$prms,$median,$min,$max,$adev,$rms) = $dflat->statsover;
  print STDERR
    (
     "# $0 summary:\n",
     "# - number of arguments: ", scalar(@ARGV), "\n",
     "# - number of off-diagonal comparisons: ", $dflat->nelem, "\n",
     "# - mean: $avg\n",
     "# - median: $median\n",
     "# - minimum: $min\n",
     "# - maximum: $max\n",
     "# - standard deviation: $rms\n",
     "# - coefficient of variance: ", $rms/$avg, "\n",
    );
}
