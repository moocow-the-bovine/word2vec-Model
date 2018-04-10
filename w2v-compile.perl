#!/usr/bin/perl -w

use lib qw(. ./lib ./blib/lib ./blib/arch);
use word2vec::Model;

use File::Basename qw(basename);
use Getopt::Long qw(:config no_ignore_case);
use open qw(:std :utf8);
use strict;

##==============================================================================
## Command-line
my $prog = basename($0);
my ($help);
my $outbase = '';
my %model = (type=>'float', nodims=>0, start=>1, freqfile=>undef);
my %log  = (level=>'TRACE', rootLevel=>'FATAL');


GetOptions(
	   'h|help' => \$help,
	   'o|output=s' => \$outbase,
	   't|type' => \$model{type},
	   'd|dims!' => sub { $model{nodims}=!$_[1] },
	   's|start=i' => \$model{start},
	   'f|freqs=s' => \$model{freqfile},

	   ##-- logging
	   'log-level|level|ll=s' => sub { $log{level} = uc($_[1]); },
	   'log-option|logopt|lo=s' => \%log,
	  );
if ($help || !@ARGV) {
  print STDERR <<EOF;

Usage: $prog \[OPTIONS] VECFILE

Options:
  -h,  -help                # this help message
  -d,  -[no]dims            # do/don't load dimensions from 1st line of VECFILE (default=do)
  -s,  -start OFFSET        # offset of first word index (default=1, requires -dims)
  -f,  -freqs FREQFILE      # frequency file (WORD FREQ; optional)
  -t,  -type                # PDL datatype (default=$model{type})
  -o,  -output BASE         # output basename (default=VECFILE)
  -ll, -log-level LEVEL     # set log-level LEVEL (default=$log{level})
  -lo, -log-option OPT=VAL  # set generic logging option

EOF
  exit ($help ? 0 : 1);
}

##==============================================================================
## MAIN

##-- setup logger
DiaColloDB::Logger->ensureLog(%log);

##-- defaults
my $vecfile = $ARGV[0];
$outbase = "$vecfile" if (($outbase//'') eq '');

##-- object
my $model = word2vec::Model->new()
  or die("$0: failed to create model object");
$model->compile($vecfile, %model, base=>$outbase)
  or die("$0: failed to compile vector-file '$vecfile': $!");
