#!/usr/bin/perl -w

use lib qw(. ./lib ./blib/lib ./blib/arch);
use word2vec::Model;
use word2vec::Kcluster;

use Getopt::Long qw(:config no_ignore_case);
use File::Basename qw(basename);
use open qw(:std :utf8);
use strict;

##==============================================================================
## Command-line
my $prog = basename($0);
my ($help);
my %copt = (
	    nc => 0, ##-- default: 10
	    ncw => 0,
	    npass => 5,
	    dist => 'u', ##-- qw(e:Euclid b:Manhatten c:Pearson a:abs(Pearson) u:Cosine x:abs(Cosine) s:Spearman k:Kendall)
	    ctr => 'm', ##-- qw(a:mean m:median)
	   );

my $kmodelfile = '';
my $outfile = '-';
my $verbose = 1;
my $outmode = 'text';
our $compile = 0;
our $tmpbase = undef;
END {
  if (defined($tmpbase)) {
    unlink($_) foreach (grep {-e $_} glob("$tmpbase.*"));
  }
}

my %model    = (minn=>0,maxn=>0,nganchor=>0,ngweight=>1, logOOV=>'off');
our %log  = (level=>'DEBUG', rootLevel=>'FATAL');
GetOptions(
	   'h|help' => \$help,
	   'v|verbose!' => \$verbose,
	   'minn=i' => \$model{minn},
	   'maxn=i' => \$model{maxn},
	   'c|compile!' => \$compile,
	   'o|output=s' => \$outfile,
	   'k|known=s' => \$kmodelfile,
	   'T|text!' => sub { $outmode = 'text' },
	   'J|json!' => sub { $outmode = 'json' },

	   'cn|n|nc|nclusters=i' => $copt{nc},
	   'cs|ncw|nwc|csize=i' => \$copt{ncw},
	   'ct|t|nt|ntry|try|cp|np|npass|pass|p=i' => \$copt{npass},
	   'cd|distance=s' => \$copt{dist},
	   'cc|centroid=s' => \$copt{ctr},

	   ##-- logging
	   'log-level|level|ll=s' => sub { $log{level} = uc($_[1]); },
	   'log-option|logopt|lo=s' => \%log,
	  );
if ($help || @ARGV < 1) {
  print STDERR <<EOF;

Usage: $prog \[OPTIONS] MODEL_OR_VECFILE

Options:
  -h,  -help                # this help message
  -v,  -[no]verbose         # do/don't include verbose output columns (distances; default=do)
  -c,  -[no]compile         # do/don't compile text-model FTMODEL (default=don't)
  -o,  -output OUTFILE      # set output file (default=STDOUT)
  -T,  -text                # select text output mode
  -J,  -json                # select json output mode
       -minn MINN           # minimum n-gram length for OOV words (default=$model{minn})
       -maxn MAXN           # minimum n-gram length for OOV words (default=$model{maxn})
       -[no]anchor          # do/don't use only anchored (BOW,EOW) n-gram regexes (default=$model{nganchor})
       -[no]weight          # do/don't weight regex matches by frequency if available (default=$model{ngweight})
  -k,  -kmodel KMODEL       # mark "known/unknown" field according to MODEL (default=none)
  -cs, -csize CSIZE         # target average cluster size; if specified sets NCLUS=(NWORDS/CSIZE)
  -cn, -nclusters NCLUS     # number of output clusters (overrides CSIZE; fallback default=10)
  -cd, -distance DIST       # distance flag (e:Euclid b:Manhattan c:Pearson a:abs(Pearson) u:Cosine x:abs(Cosine) s:Spearman k:Kendall; default=$copt{dist})
  -cc, -center CENTER       # centroid method (a:average m:median; default=$copt{ctr})
  -cp, -npass NPASS         # number of kcluster passes (default=$copt{npass})
  -ll, -log-level LEVEL     # set log-level LEVEL (default=$log{level})
  -lo, -log-option OPT=VAL  # set generic logging option

EOF
  exit ($help ? 0 : 1);
}

##==============================================================================
## subs

## "$min / $max / $med / $avg / $sd" = pdlsummary($pdl)
sub pdlsummary {
  my $p = shift;
  my ($avg,$prms,$med,$min,$max,$adev,$rms) = $p->stats;
  return join(" / ", map {sprintf("%8.3g", $_)} ($min,$max,$med,$avg,$rms));
}

##==============================================================================
## MAIN

##-- setup logger
DiaColloDB::Logger->ensureLog(%log);

##-- open or compile
my ($model);
my $modelfile = shift(@ARGV);
if ($compile) {
  $tmpbase = "/tmp/wvkc$$";
  word2vec::Model->info("compiling temporary model '$tmpbase.*'");
  $model = word2vec::Model->compile($modelfile, %model,type=>'double',start=>0,nodims=>1,base=>$tmpbase)
    or die("$prog: failed to compile text-model '$modelfile' to '$tmpbase.*': $!");
} else {
  $model = word2vec::Model->new(%model,base=>$modelfile)
    or die("$prog: failed to open binary model $modelfile.*: $!");
}

##-- do clustering
my $kc = $model->kcluster(%copt);

##-- optionally set "unknowns"
if ($kmodelfile) {
  my $kmodel = word2vec::Model->new(base=>$kmodelfile)
    or die("$prog: failed to open known model $kmodelfile.*: $!");
  $kc->setknown($kmodel)
    or die("$prog: failed to mark unknown word(s) using $kmodelfile.*");
}


##-- show summary
$kc->info("Summary:");
$kc->info(" + $_") foreach ($kc->summary());

##-- dump
if ($outmode eq 'json') {
  $kc->saveJsonFile($outfile)
    or die("$prog: failed to save clustering result in JSON-mode to '$outfile': $!");
} else {
  $kc->saveTextFile($outfile)
    or die("$prog: failed to save clustering result in text-mode to '$outfile': $!");
}

##-- cleanup
undef $kc;
$model->unlink() if ($compile);
