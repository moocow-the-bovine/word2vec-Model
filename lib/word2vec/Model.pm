##-*- Mode: CPerl -*-

package word2vec::Model;
use PDL;
use PDL::IO::FastRaw;
use PDL::VectorValued;
use PDL::CCS; ##-- for nnz()
use PDL::Cluster qw();
use DiaColloDB::Logger;
use DiaColloDB::EnumFile::MMap;
use open qw(:std :utf8);
use strict;

our @ISA = qw(DiaColloDB::Persistent);
our $VERSION = '0.0.1';

BEGIN {
  no warnings 'once';
  $PDL::BIGPDL = 1;

  push(@{$DiaColloDB::Logger::defaultLogOpts{logwhich}}, __PACKAGE__)
    if (!grep {$_ eq __PACKAGE__} @{$DiaColloDB::Logger::defaultLogOpts{logwhich}});
}

##==============================================================================
## constructors

## $model = CLASS_OR_OBJECT->new(%args)
##  + %args, %$model:
##    (
##     base  => $base,    ##-- file basename
##     start => $wi0,     ##-- first word-ID (default=1)
##     type  => $type,    ##-- type for rwmat (default='float')
##     ##
##     ##-- logging
##     logOOV => $level,  ##-- log-level for OOV words (default='warn')
##     ##
##     ##-- guts
##     wenum => $wenum,   ##-- word (lemma) enum
##     nr    => $nr,      ##-- number of "topics" / "latent dimensions" of model / model "rank"
##     nw    => $nw,      ##-- number of enum-ified words
##     rwmat => $rwmat,   ##-- topic-word matrix ($nr,$nw): [$ri,$wi] => $rval_at_w
##    )
sub new {
  my $that = shift;
  my $model = bless({
		  base  => undef,
		  start => 1,
		  logOOV => 'warn',
		  wenum => DiaColloDB::EnumFile::MMap->new(flags=>'r'),
		  nr  => undef,
		  nw  => undef,
		  rwmat => undef,
		  @_,
		 }, ref($that)||$that);
  return $model->open() if (defined($model->{base}));
  return $model;
}

## $model = $model->clear()
##  + clears model
sub clear {
  my $model = shift;
  $model->close();
  delete @$model{qw(wenum rwmat nw nr)};
  return $model;
}

sub DESTROY {
  $_[0]->clear();
}

##======================================================================
## DiaColloDB::Persistent API

## @files = $obj->diskFiles()
sub diskFiles {
  my $model = shift;
  return ($model->{base} ? (map {glob("$model->{base}.$_*")} qw(pdl enum)) : qw());
}


##======================================================================
## I/O

##--------------------------------------------------------------
## I/O: compile

## $model = $model->compile($fasttext_text_vectors_file_or_fh, %opts)
##  + %opts: clobber %$model, also:
##     nodims => $bool,      ##-- don't try to load dimensions from 1st line
##  + populates @$model{qw(wenum rwmat nr nw)}
sub compile {
  my ($model,$file,%opts) = @_;

  ##-- local options
  my $nodims = $opts{nodims} // 0;
  delete($opts{nodims});

  ##-- initialization
  $model = $model->new() if (!ref($model));
  @$model{keys %opts} = values %opts;
  $model->clear();

  ##-- basic variables
  my $start  = ($model->{start} //= ($nodims ? 0 : 1));  ##-- start enumerating words at id=1
  my $type   = $model->{type} || 'float';

  ##-- sanity check(s)
  my $base = $model->{base};
  $model->logconfess("compile(): no output {base} specified") if (!$base);

  if ($start > 0 && $nodims) {
    $model->logwarn("compile(): cannot use nontrivial {start} offset $start if {nodims} is active; setting start=0");
    $start = $model->{start} = 0;
  }

  ##-- open input file
  $model->info("compile($file): type=$type ; start=$start ; nodims=$nodims\n");
  my $fh  = ref($file) ? $file : IO::File->new("<$file");
  $model->logconfess("compile(): open failed for $file: $!") if (!$fh);

  ##-- load dimensions (if requested)
  my ($nw,$nr) = (0,$opts{nr});
  if (!$nodims) {
    my $vdims = <$fh>;
    chomp($vdims);
    ($nw,$nr) = split(' ',$vdims,2); ##-- $nw=$n_words, $nk=$n_topics
    $nw += $start;
  }

  ##-- get basic pdl datatype info
  my $pdlbase = "$base.pdl";
  my $ptype   = $PDL::Types::typenames{$type}
    or $model->logconfess("compile(): unknown PDL type '$type'; use one of (",
		       join(' ', map {$_->{ioname}} sort {$a->{numval}<=>$b->{numval}} values %PDL::Types::typehash), ")"
		      );
  my $packas = $PDL::Types::pack[$ptype]
    or $model->logconfess("compile(): no pack-type for PDL type '$type'!");

  ##-- open raw pdl outfile
  $model->info("compile(): preparing pdl-file $pdlbase");
  CORE::open(my $pdlfh, ">$pdlbase")
    or $model->logconfess("compile(): open failed for raw PDL file $pdlbase: $!");
  binmode($pdlfh,':raw');

  ##-- pdl file: write zeroes until start offset
  if (defined($nr) && $start > 0) {
    $model->info("compile(): prepending initial zero-vector(s) (start=$start, nr=$nr)");
    my $buf = pack($packas,0) x $nr;
    print $pdlfh ($buf x $start) if ($start > 0);
  }

  ##-- churn vector data, saving IDs
  my $i2s = [];                              ##-- $w=$i2s->[$wi]
  $i2s->[$start-1] = undef if ($start > 0);  ##-- honor start offset
  $model->info("compile(): processing text-format vector data from $file");
  my ($w,@v);
  while (defined($_=<$fh>)) {
    chomp;
    next if ($nodims && /^[0-9]+\s+[0-9]+$/); ##-- ignore dims even if they're there
    ($w,@v) = split(' ',$_);
    $model->logconfess("compile(): bad data in input file '$file'") if (defined($nr) && @v != $nr);
    $nr //= scalar(@v);
    print $pdlfh pack($packas,@v);
    push(@$i2s,$w);
  }
  ##-- finalize pdl-file
  CORE::close($pdlfh)
      or $model->logconfess("compile(): close failed for raw PDL-file '$pdlbase': $!");

  ##-- finalize dimensions
  $model->{nr} = ($nr //= 0);
  $model->{nw} = $nw = scalar(@$i2s);

  ##-- write pdl header
  $model->info("compile(): writing pdl-header $pdlbase.hdr");
  CORE::open(my $hdrfh,">$pdlbase.hdr")
  or $model->logconfess("compile() open failed for '$pdlbase.hdr': $!");
  binmode($hdrfh,":raw");
  print $hdrfh
    ("$ptype\n",   ##-- type-id
     "2\n",        ##-- ndims
     "$nr $nw\n",  ##-- dims
    );
  CORE::close($hdrfh)
    or $model->logconfess("$0: failed to close header file '$pdlbase.hdr': $!");

  ##-- create enum
  $model->info("compile(): creating enum $base.enum.*");
  my $ebase = "$base.enum";
  my $wenum = $model->{wenum} = DiaColloDB::EnumFile::MMap->new(flags=>'rw',base=>$ebase);
  $wenum->fromArray($i2s)
    or $model->logconfess("compile(): failed to initialize enum from array");
  $wenum->save()
    or $model->logconfess("compile(): failed to save enum to '$ebase.*': $!");

  ##-- now open the model
  #$model->info("compile(): opening newly compiled model");
  return $model->open();
}

##--------------------------------------------------------------
## I/O: open/close

##------------------------------------------------------
## $bool = $model->opened()
sub opened {
  my $model = shift;
  return defined($model->{rwmat}) && defined($model->{wenum}) && $model->{wenum}->opened;
}

##------------------------------------------------------
## $model = $model->open()
## $model = $model->open($base)
sub open {
  my $model = shift;
  $model->close();
  $model->{base} = shift if (defined($_[0]));
  $model->logconfess("open(): no {base} attribute specified") if (!defined($model->{base}));
  my $base = $model->{base};
  $model->debug("open($base)");

  ##-- open enum
  $model->{wenum}->open("$base.enum",'r')
    or $model->logconfess("open(): failed to open enum-file(s) $base.enum.*: $!");

  ##-- open PDL
  $model->{rwmat} = mapfraw("$base.pdl",{ReadOnly=>1});
  defined($model->{rwmat})
    or $model->logconfess("open(): failed to mmap PDL file '$base.pdl': $!");

  ##-- get dimensions
  @$model{qw(nr nw)} = $model->{rwmat}->dims();

  return $model;
}

##------------------------------------------------------
## $model = $model->close()
sub close {
  my $model = shift;
  return undef if (!$model->opened);
  $model->debug("close()");
  delete @$model{qw(rwmat nr nw)};
  return $model->{wenum}->close() if ($model->{wenum} && $model->{wenum}->opened);
  return 1;
}

##======================================================================
## API: word vectors

##--------------------------------------------------------------#
## $nullv = $model->nullv()
##  + returns null vector
sub nullv {
  my $model = shift;
  return zeroes($model->{rwmat}->type, $model->{rwmat}->dim(0));
}

##--------------------------------------------------------------
## $wv      = $model->wi2v($wi)
## ($wv,$n) = $model->wi2v($wi)
##  + gets raw word-vector for known word with index $wi
##  + returns null-vector if $wi is out-of-bounds
##  + in list-context, second component $n is 1 iff $w is known, otherwise 0
sub wi2v {
  my ($model,$wi) = @_;
  $model->trace("wi2v($wi)");
  if (defined($wi) && $wi >= 0 && $wi < $model->{wenum}{size}) {
    my $wv = $model->{rwmat}->slice(",($wi)");
    return wantarray ? ($wv,1) : $wv;
  }
  $model->vlog($model->{logOOV}, "wi2v(): returning null vector for unknown word index #$wi");
  return wantarray ? ($model->nullv(),0) : $model->nullv;
}

##--------------------------------------------------------------
## $wv      = $model->w2v($w)
## ($wv,$n) = $model->w2v($w)
##  + gets raw word-vector for word $w
##  + returns null-vector if $w is unknown
##  + in list-context, second component $n is 1 iff $w is known, otherwise 0
sub w2v {
  my ($model,$w) = @_;
  utf8::decode($w) if (!utf8::is_utf8($w));
  $model->trace("w2v($w)");
  if (defined(my $wi = $model->{wenum}->s2i($w))) {
    return $model->wi2v($wi);
  }
  $model->vlog($model->{logOOV}, "w2v(): returning null vector for unknown word '$w'");
  return wantarray ? ($model->nullv,0) : $model->nullv;
}

##--------------------------------------------------------------
## $wv      = $model->re2v($re)
## ($wv,$n) = $model->re2v($re)
##  + gets average word-vector for all known words matching regex $re
##  + returns null-vector if $re doesn't match anything
##  + in list-context, second component $n is number of matches for $re
sub re2v {
  my ($model,$re) = @_;
  utf8::decode($re) if (!ref($re) && !utf8::is_utf8($re));
  $model->trace("re2v($re)");

  my $wv  = $model->nullv;
  my $wis = $model->{wenum}->re2i($re);
  if (@$wis) {
    $wv += $model->{rwmat}->slice(",($_)") foreach (@$wis);
    $wv /= scalar(@$wis);
  } else {
    $model->vlog($model->{logOOV}, "re2v(): returning null vector for unknown regex $re");
  }
  return wantarray ? ($wv,scalar(@$wis)) : $wv;
}

##--------------------------------------------------------------
## $wv      = $model->ngrams2v($w,%opts)
## ($wv,$n) = $model->ngrams2v($w,%opts)
##  + gets average word-vector for all n-grams between lengths $model->{minn} and $model->{maxn}
##  + not really helpful (resulting vectors are too generic / only garbage neighbors)
##  + in list-context, second component $n is number of matches for generated $re
sub ngrams2v {
  my ($model,$w) = @_;
  utf8::decode($w) if (!utf8::is_utf8($w));
  $model->trace("ngrams2v($w)");

  my ($minn,$maxn) = map {($_//0)} @$model{qw(minn maxn)};
  if ($minn > 0 && $maxn >= $minn) {
    my @ngrams = qw();
    my ($i,$j,$len);
    my $ww = '^'.$w.'$';
    for ($i=0; $i < length($ww); ++$i) {
      for ($j=$i+1; $j <= length($ww); ++$j) {
	$len = $j-$i;
	if ($len >= $minn && $len <= $maxn) {
	  push(@ngrams, quotemeta(substr($ww,$i,$len)));
	}
      }
    }
    my $re_str = join('|', map {s/^\\\^/^/; s/\\\$$/\$/; "(?:$_)"} grep {$_ ne "\\^" && $_ ne "\\\$" } @ngrams);
    $model->vlog($model->{logOOV}, "ngrams2v(): unknown word '$w' - creating n-gram regex (minn=$minn; maxn=$maxn): regex=/$re_str/");
    return $model->re2v($re_str);
  }
  else {
    $model->vlog($model->{logOOV}, "ngrams2v(): unknown word '$w', but n-gram regexes are disabled - returning null vector");
    return wantarray ? ($model->nullv,0) : $model->nullv;
  }
}

##--------------------------------------------------------------
## $wv      = $model->any2v($w_or_regex)
## ($wv,$n) = $model->any2v($w_or_regex)
sub any2v {
  my ($model,$w) = @_;
  utf8::decode($w) if (!utf8::is_utf8($w));
  $model->trace("any2v($w)");

  if ($w =~ m{^\/.*\/[gimsadlux]*}) {
    ##-- word-vector: regex
    return $model->re2v($w);
  }
  elsif (defined(my $wi = $model->{wenum}->s2i($w))) {
    ##-- word-vector: literal (known)
    return $model->wi2v($wi);
  }
  elsif ($model->{minn} && $model->{maxn} && $model->{maxn} >= $model->{minn}) {
    ##-- word-vector: n-grams (not really helpful)
    return $model->ngrams2v($w);
  }
  else {
    ##-- word-vector: null
    $model->vlog($model->{logOOV}, "any2v(): returning null vector for unknown word '$w'");
    return wantarray ? ($model->nullv,0) : $model->nullv;
  }
}

##--------------------------------------------------------------
## $wv = $model->expr2v(@exprs)
sub expr2v {
  my ($model,@exprs) = @_;
  foreach (@exprs) {
    utf8::decode($_) if (!utf8::is_utf8($_));
  }
  my $ev = $model->nullv;
  $model->trace("expr2v(", join(' ',@exprs), ")");
  foreach my $w (@exprs) {
    my $coef = ($w =~ s/^\s*[\-\!]\s*// ? -1.0 : 1.0);
    $w      =~ s/^\s*\+\s*//;
    $ev     += ($coef * $model->any2v($w));
  }
  return $ev;
}


##==============================================================================
## API: k-nearest-neighbors

##--------------------------------------------------------------
## $wids = $model->knn($qv,$k)
## ($wids,$sims) = $model->knn($qv,$k)
##  + get k-nearest-neighbor IDs for vector $qv
sub knn {
  my ($model,$qv,$k) = @_;
  $k ||= 10;
  $qv  = $qv->convert($model->{rwmat}->type) if ($qv->type > $model->{rwmat}->type);
  my $sim  = $model->{rwmat}->vv_vcos($qv);
  my $maxi = zeroes(indx,$k);
  $sim->maximum_n_ind($maxi);

  return wantarray ? ($maxi,$sim->index($maxi)->sever) : $maxi;
}


##==============================================================================
## API: k-means / k-medoids clustering

##--------------------------------------------------------------
##  %cluster = $model->kcluster(%opts)
## \%cluster = $model->kcluster(%opts)
##  + options %opts:
##    (
##     dist  => $distanceFlag, ##-- qw(e:Euclid b:Manhatten c:Pearson a:abs(Pearson) u:Cosine x:abs(Cosine) s:Spearman k:Kendall); default='u'
##     ctr   => $ctrFlag,      ##-- qw(a:mean m:median); default='m'
##     npass => $npass,        ##-- number of passes (default=5)
##     ncw   => $ncw,          ##-- target average cluster size; if specified sets $nc=($nw/$csize)
##     nc    => $nc,           ##-- number of target clusters (overrides $ncw; fallback default=10)
##    )
##  + returned cluster data %cluster:
##    (
##     %opts,                  ##-- options are passed out
##     unknown    => $uwids,   ##-- pdl($nu)     : ($ui) => $wi s.t. $wi is unknown (null-vector)
##     clusterids => $cids,    ##-- pdl($nw)     : ($wi) => $ci s.t. $ci==cluster($wi)
##     csizes     => $csizes,  ##-- cluster sizes
##     error      => $error,   ##-- total distance of all words to their respective assigned clusters
##     nfound     => $nfound,  ##-- number of passes on which best solution was found
##     rcmat      => $rcmat,   ##-- centroid matrix       : pdl($nr,$nc) : ($ri,$ci) => $rval_at_cluster_c
##     wcdist     => $wcdist,  ##-- word-cluster distances: pdl($nw)     : ($wi)     => distance($wi,$cids->at($wi))
##    )
sub kcluster {
  my ($model,%opts) = @_;

  ##-- model properties
  my $rwmat    = $model->{rwmat};
  my ($nr,$nw) = $rwmat->dims;

  ##-- options
  my $dist = ($opts{dist} ||= 'u');
  my $ctr  = ($opts{ctr}  ||= 'm');
  my $npass = ($opts{npass} ||= 5);
  my $nc = ($opts{nc}
	    || ($opts{ncw} ? int($nw/$opts{ncw}) : undef)
	    || 10);
  $opts{nc} ||= $nc;

  ##-- track unknown words
  $model->info("kcluster(): checking for unknown words");
  my $kmask = ones(byte,$nw);
  $kmask->where($rwmat->nnz==0) .= 0;
  my ($knowni,$unki) = $kmask->which_both;
  if (!all($kmask)) {
    $model->info("kcluster(): ignoring ", $unki->nelem, " unknown word(s)");
    $rwmat = $rwmat->dice_axis(1,$knowni);
  } else {
    $model->info("kcluster(): no unknown words detected (and there was much rejoicing)");
  }

  ##-- cluster
  $model->info("kcluster() configuration: NCLUS=$nc, NPASS=$npass, DIST=$dist, CTR=$ctr");
  my $mask   = ones(long,   $rwmat->dims);
  my $weight = ones(double, $nr);
  my $clusterids = zeroes(long, $nw);           ##-- all words
  my $cids       = $clusterids->index($knowni); ##-- known words only
  $clusterids->index($unki) .= -1;              ##-- $ci==-1: "unknown" cluster
  my ($error,$nfound);
  PDL::Cluster::kcluster($nc, $rwmat,$mask,$weight, $npass,
			 $cids,
			 ($error=null),
			 ($nfound=null),
			 $dist, $ctr,
			);

  ##-- get cluster centroids and word-cluster distances
  my $cdata = zeroes(double,$nr,$nc);
  my $cmask = zeroes(long,$nr,$nc);
  my $wcdist = zeroes(double,$nw);
  my $kcdist = $wcdist->index($knowni);
  PDL::Cluster::getclustercentroids($rwmat,$mask, $cids, $cdata,$cmask, $ctr);
  for (my $ci=0; $ci < $nc; ++$ci) {
    my $kis    = which($cids==$ci);
    my $crow   = $cdata->slice(",$ci");
    my $cwmat  = $crow->glue(1,$rwmat->dice_axis(1,$kis));
    my $cwmask = ones(long,$cwmat->dims);
    PDL::Cluster::rowdistances($cwmat,$cwmask,$weight, zeroes(long,$kis->nelem), ($kis->xvals+1), $kcdist->index($kis), $dist);
  }

  ##-- get cluster sizes
  my $csizes = zeroes(long,$nc);
  PDL::Cluster::clustersizes($cids, $csizes);

  ##-- return
  my %rc = (
	    %opts,
	    unknown => $unki,
	    clusterids => $clusterids,
	    error => $error,
	    nfound => $nfound,
	    rcmat => $cdata,
	    wcdist => $wcdist,
	    csizes => $csizes,
	   );
  return wantarray ? %rc : \%rc;
}

##--------------------------------------------------------------
## "$min / $max / $med / $avg / $sd" = pdl_summary($pdl)
## "$min / $max / $med / $avg / $sd" = pdl_summary($pdl,$fmt)
sub pdl_summary {
  my ($p,$fmt) = @_;
  $fmt ||= '%8.3g';
  my ($avg,$prms,$med,$min,$max,$adev,$rms) = $p->stats;
  return join(" / ", map {sprintf($fmt, $_)} ($min,$max,$med,$avg,$rms));
}

##--------------------------------------------------------------
## @summary = $model->cluster_summary(\%cluster)
##  + summarizes clustering result
sub cluster_summary {
  my ($model,$clu) = @_;

  ##-- common vars
  my $nw    = $model->{nw};
  my $nunkw = $clu->{unknown}->nelem;

  return (
	  "number of target words: $nw",
	  "number of unknown targets (ignored): $nunkw (".sprintf("%.1f %%", 100*$nunkw/$nw).")",
	  "number of passes: $clu->{npass}",
	  "number of times solution found: $clu->{nfound}",
	  "number of clusters: $clu->{nc}",
	  "cluster distance function: $clu->{dist}",
	  "cluster centroid method: $clu->{ctr}",
	  "cluster sizes (min/max/med/avg/sd): ".pdl_summary($clu->{csizes}),
	  "cluster dists (min/max/med/avg/sd): ".pdl_summary($clu->{wcdist}),
	  "total dist = ".sprintf("%.3g", $clu->{error}->sclr)."\n",
	 );
}
