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
my $outbase = ''; ##-- output basename (default: input filename)

my %model  = (minn=>0,maxn=>0,nganchor=>0,ngweight=>1,logOOV=>'trace');
my %omodel = (%model, start=>0, type=>'float');
my %log   = (level=>'DEBUG', rootLevel=>'FATAL');
GetOptions(
	   'h|help' => \$help,

	   ##-- input model
	   'minn=i' => \$model{minn},
	   'maxn=i' => \$model{maxn},
	   'a|anchor|nganchor!' => \$model{nganchor},
	   'w|weight|ngweight!' => \$model{ngweight},

	   ##-- output model
	   'o|output=s' => \$outbase,
	   's|start=i' => \$omodel{start},
	   't|type=s' => \$omodel{type},

	   ##-- logging
	   'log-level|level|ll=s' => sub { $log{level} = uc($_[1]); },
	   'log-option|logopt|lo=s' => \%log,
	  );
if ($help || @ARGV < 1) {
  print STDERR <<EOF;

Usage: $0 [OPTIONS] MODEL [WORDLISTFILE=-]

 General Options:
  -h,  -help                # this help message

 Input Model Options:
       -minn MINN           # minimum n-gram length for OOV words (default=$model{minn})
       -maxn MAXN           # minimum n-gram length for OOV words (default=$model{maxn})
       -[no]anchor          # do/don't use only anchored (BOW,EOW) n-gram regexes (default=$model{nganchor})
       -[no]weight          # do/don't weight regex matches by frequency if available (default=$model{ngweight})

 Output Model Options:
  -o,  -output OUTBASE      # output basename (default=WORDLISTFILE)
  -s,  -start OFFSET        # offset of first word index (default=$omodel{start})
  -t,  -type                # PDL datatype (default=$omodel{type})

 Logging Options:
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
my $ibase  = shift(@ARGV);
my $imodel = word2vec::Model->new(%model,base=>$ibase)
  or die("$0: failed to open input model '$ibase': $!");

##-- read inputs
push(@ARGV,'-') if (!@ARGV);
my $infile = $ARGV[0];
my @words = qw();
while (defined($_=<>)) {
  chomp;
  next if (/^\s*$/);
  #s/\s/_/g;
  push(@words,$_);
}

##-- project model
$outbase = $infile if (($outbase//'') eq '');
my $omodel = $imodel->project(\@words,%omodel,base=>$outbase)
  or $imodel->logconfess("failed to project output model: $!");

##-- summarize
$omodel->info("projected submodel for ", scalar(@words), " word(s) to '$outbase.*'");


