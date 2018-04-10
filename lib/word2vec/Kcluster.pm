##-*- Mode: CPerl -*-

package word2vec::Kcluster;
use word2vec::Model;
use PDL;
use PDL::VectorValued;
use PDL::CCS; ##-- for nnz()
use PDL::Cluster qw();
use JSON;
use open qw(:std :utf8);
use strict;

our @ISA = qw(DiaColloDB::Persistent);
our $VERSION = '0.0.2';

BEGIN {
  push(@{$DiaColloDB::Logger::defaultLogOpts{logwhich}}, __PACKAGE__)
    if (!grep {$_ eq __PACKAGE__} @{$DiaColloDB::Logger::defaultLogOpts{logwhich}});
}

##==============================================================================
## constructors etc

## $model = CLASS_OR_OBJECT->new(%args)
##  + %args, %$model:
##    (
##     ##-- user options
##     model => $model,        ##-- a word2vec::Model object to be clustered (REQUIRED, or call cluster() yourself)
##     dist  => $distanceFlag, ##-- qw(e:Euclid b:Manhatten c:Pearson a:abs(Pearson) u:Cosine x:abs(Cosine) s:Spearman k:Kendall); default='u'
##     ctr   => $ctrFlag,      ##-- qw(a:mean m:median); default='m'
##     npass => $npass,        ##-- number of passes (default=5)
##     ncw   => $ncw,          ##-- target average cluster size; if specified sets $nc=($nw/$csize)
##     nc    => $nc,           ##-- number of target clusters (overrides $ncw; fallback default=10)
##     ##
##     ##-- guts (after cluster())
##     kmask      => $kmkask,  ##-- pdl($nw)     : ($wi) => $bool_w_is_known
##     unknown    => $uwids,   ##-- pdl($nuw)    : ($uwi) => $wi s.t. $wi is unknown (null-vector)
##     clusterids => $cids,    ##-- pdl($nw)     : ($wi) => $ci s.t. $ci==cluster($wi)
##     csizes     => $csizes,  ##-- cluster sizes
##     error      => $error,   ##-- total distance of all words to their respective assigned clusters
##     nfound     => $nfound,  ##-- number of passes on which best solution was found
##     rcmat      => $rcmat,   ##-- centroid matrix       : pdl($nr,$nc) : ($ri,$ci) => $rval_at_cluster_c
##     wcdist     => $wcdist,  ##-- word-cluster distances: pdl($nw)     : ($wi)     => distance($wi,$cids->at($wi))
##    )
sub new {
  my $that = shift;
  my $kc = bless({
		  model => undef,
		  dist => 'u',
		  ctr  => 'm',
		  npass => 5,
		  nc  => undef,
		  ncw => undef,
		  @_,
		 }, ref($that)||$that);
  return $kc->{model} ? $kc->cluster() : $kc;
}

## $kc = $kc->clear()
##  + clears clustering result
sub clear {
  my $kc = shift;
  delete @$kc{qw(kmask known unknown clusterids csizes error nfound rcmat wcdist)};
  return $kc;
}

##==============================================================================
## API: k-means / k-medoids clustering

##--------------------------------------------------------------
## $kc = $kc->cluster(%opts)
##  + options %opts : override %$kc
sub cluster {
  my ($kc,%opts) = @_;

  ##-- override kc
  @$kc{keys %opts} = values %opts;
  $kc->logconfess("cluster() called but no {model} defined") if (!$kc->{model});

  ##-- model properties
  my $model    = $kc->{model};
  my $rwmat    = $model->{rwmat};
  my ($nr,$nw) = $rwmat->dims;

  ##-- options
  my $dist = ($kc->{dist} ||= 'u');
  my $ctr  = ($kc->{ctr}  ||= 'm');
  my $npass = ($kc->{npass} ||= 5);
  my $nc = ($kc->{nc}
	    || ($kc->{ncw} ? int($nw/$kc->{ncw}) : undef)
	    || 10);
  $kc->{nc} ||= $nc;

  ##-- track unknown words
  $kc->info("checking for unknown words");
  my $kmask = $kc->{kmask} = ones(byte,$nw);
  $kmask->where($rwmat->nnz==0) .= 0;
  my ($knowni,$unki) = @$kc{qw(known unknown)} = $kmask->which_both;
  if (!all($kmask)) {
    $kc->info("ignoring ", $unki->nelem, " unknown word(s)");
    $rwmat = $rwmat->dice_axis(1,$knowni);
  } else {
    $kc->info("no unknown words detected (and there was much rejoicing)");
  }

  ##-- cluster
  $kc->info("configuration: NCLUS=$nc, NPASS=$npass, DIST=$dist, CTR=$ctr");
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
  $kc->{clusterids} = $clusterids;
  $kc->{error} = $error;
  $kc->{nfound} = $nfound;
  $kc->{rcmat} = $cdata;
  $kc->{wcdist} = $wcdist;
  $kc->{csizes} = $csizes;
  return $kc;
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## $sclr = pdl_sclr($pdl_or_scalar)
sub pdl_sclr {
  return UNIVERSAL::isa($_[0],'PDL') ? $_[0]->sclr : $_[0];
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## \%stats = pdl_stats($pdl)
## \%stats = pdl_stats($pdl,$fmt)
## \%stats = pdl_stats($pdl,$fmt)
sub pdl_stats {
  my $p = shift;
  my %stats;
  @stats{qw(avg prms med min max adev sd)} = map {pdl_sclr($_)} $p->stats;
  if (@_) {
    my $fmt = shift;
    $_ = sprintf($fmt,$_) foreach (values %stats);
  }
  delete @stats{qw(prms adev)}; ##-- ignore these
  return \%stats;
}

## \%stats = pdl_stats($pdl, [$fmt])
##  + like pdl_stats() but numifies
sub json_stats {
  my $stats = pdl_stats(@_);
  $_ += 0 foreach (values %$stats);
  return $stats;
}

##--------------------------------------------------------------
## \%cinfo = $kc->cinfo($ci)
##   + get information for cluster $ci
##   + returns HASH-ref \%cinfo =
##     (
##      id => $ci,         ##-- cluster-id
##      ctr => $ctr,       ##-- cluster centroid: pdl($nr) : ($ri) => $rval_at_$c
##      wis => $wis,       ##-- word-ids assigned to this cluster: pdl($ncw) : ($cwi) => $wi
##      wdist => $wdists,  ##-- word distances:                  : pdl($ncw) : ($cwi) => dist($wi,$ctr)
##     )
##   + returned $wis and $wdists are sorted in ascending order by distance from cluster center
sub cinfo {
  my ($kc,$ci) = @_;
  my $wis   = which($kc->{clusterids}==$ci);
  my $wdist = $kc->{wcdist}->index($wis);
  my $sorti = $wdist->qsorti;

  return {
	  id  => $ci,
	  ctr => ($ci < 0 ? zeroes(double,$kc->{model}{nr}) : $kc->{rcmat}->slice(",($ci)")),
	  wis => $wis->index($sorti),
	  wdist => $wdist->index($sorti),
	 };
}

##--------------------------------------------------------------
## $kc = $kc->setknown($enum_or_model)
##  + (re-)mark @$kc{qw(kmask known unknown)} for words known to $enum
sub setknown {
  my ($kc,$src) = @_;
  $src = $src->{wenum} if (UNIVERSAL::isa($src,'word2vec::Model'));

  ##-- update known mask
  $kc->info("setknown(): marking unknown word(s) using '$src->{base}.*'");
  my $kmask = $kc->{kmask};
  $kmask  = ones(byte,$kc->{model}{nw}) if (!defined($kmask));
  $kmask .= 1;

  my ($ws);
  for (my $wi=0; $wi < $kc->{model}{nw}; ++$wi) {
    $ws = $kc->{model}{wenum}->i2s($wi);
    $kmask->set($wi,0) if (($ws//'') eq '' || !defined($src->s2i($ws)));
  }
  @$kc{qw(known unknown)} = $kmask->which_both;

  return $kc;
}

##======================================================================
## I/O

##--------------------------------------------------------------
## I/O: Summary

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## "$min / $max / $med / $avg / $sd" = pdl_summary($pdl)
## "$min / $max / $med / $avg / $sd" = pdl_summary($pdl,$fmt)
sub pdl_summary {
  my ($p,$fmt) = @_;
  $fmt ||= '%8.3g';
  my $stats = pdl_stats($p);
  return join(" / ", map {sprintf($fmt, $stats->{$_})} qw(min max med avg sd));
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## @summary = $kc->summary()
##  + summarizes clustering result
sub summary {
  my $kc = shift;
  return ("no clustering result defined")
    if (grep {!defined($kc->{$_})} qw(model clusterids csizes error));

  ##-- common vars
  my $nw    = $kc->{model}{nw};
  my $nunkw = which($kc->{clusterids}==-1)->nelem;

  return (
	  "model basename: $kc->{model}{base}",
	  "number of target words: $nw",
	  "number of unknown targets (ignored): $nunkw (".sprintf("%.1f %%", 100*$nunkw/$nw).")",
	  "number of passes: $kc->{npass}",
	  "number of times solution found: $kc->{nfound}",
	  "number of clusters: $kc->{nc}",
	  "cluster distance function: $kc->{dist}",
	  "cluster centroid method: $kc->{ctr}",
	  "cluster sizes (min/max/med/avg/sd): ".pdl_summary($kc->{csizes}),
	  "cluster dists (min/max/med/avg/sd): ".pdl_summary($kc->{wcdist}),
	  "total dist = ".sprintf("%.3g", pdl_sclr($kc->{error}))."\n",
	 );
}


##--------------------------------------------------------------
## I/O: Text


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## $bool = $obj->saveTextFile($filename_or_handle, %opts)
##  + INHERITED from DiaColloDB::Persistent

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## $bool = $obj->saveTextFh($fh, %opts)
##  + save text representation to a filehandle (dummy)
sub saveTextFh {
  my ($kc,$fh) = @_;
  binmode($fh,":utf8");

  ##-- sanity check(s)
  $kc->logconfess("saveTextFh(): no clustering result to save!")
    if (grep {!defined($_)} @$kc{qw(model clusterids csizes error wcdist)});

  ##-- save summary information
  print $fh (map {"# ".ref($kc).": $_\n"} $kc->summary());

  ##-- save "unknown" cluster
  $kc->saveTextCluster($fh, -1, label=>"UNKNOWN", ctag=>"u") if (any($kc->{clusterids}==-1));

  ##-- save nontrivial clusters
  for (my $ci=0; $ci < $kc->{nc}; ++$ci) {
    $kc->saveTextCluster($fh, $ci, label=>"CLUSTER_$ci");
  }

  return $kc;
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sub saveTextCluster {
  my ($kc,$fh,$ci,%opts) = @_;
  my $label = $opts{label} || "CLUSTER_$ci";
  my $ctag  = $opts{ctag}  || "c$ci";

  my $cinfo = $kc->cinfo($ci);
  my $stats = pdl_stats($cinfo->{wdist},"%.3g");
  print $fh
    ("# $label (".join('; ', ("size=".$cinfo->{wis}->nelem), map {"d_$_=$stats->{$_}"} qw(min max med avg sd)).")\n",
     (map {
       join("\t",
	    $kc->{kmask}->at($_),
	    $ctag,
	    sprintf("%.4f",$kc->{wcdist}->at($_)),
	    $kc->{model}{wenum}->i2s($_)
	   )."\n"
	 } $cinfo->{wis}->list),
     "\n"
    );
}


##--------------------------------------------------------------
## I/O: JSON

## $thingy = $obj->TO_JSON()
##   + JSON module wrapper; default just returns anonymous HASH-ref
sub TO_JSON {
  my ($kc,%opts) = shift;

  ##-- sanity check(s)
  $kc->logconfess("TO_JSON(): no clustering result to save!")
    if (grep {!defined($_)} @$kc{qw(model clusterids csizes error wcdist)});

  my $cls = ref($kc);
  $cls =~ s/^word2vec:://;
  my $hdr = {
	     'class'=>$cls,
	     'version'=>$VERSION,
	     'libversion'=>"PDL::Cluster v$PDL::Cluster::VERSION",
	     (map {($_=>$kc->{$_})} qw(dist ctr)),
	     (map {($_=>pdl_sclr($kc->{$_})+0)} qw(npass nc)),
	     'stats' => {
			 'error' => sprintf("%.4g",pdl_sclr($kc->{error}))+0,
			 'nfound' => pdl_sclr($kc->{nfound}),
			 'csizes' => json_stats($kc->{csizes},"%.4g"),
			 'cdists' => json_stats($kc->{wcdist},"%.4g"),
			},
	    };
  my @clus = qw();
  push(@clus, $kc->saveJsonCluster(-1,label=>"UNKNOWN",ctag=>'u')) if (any($kc->{clusterids}==-1));
  for (my $ci = 0; $ci < $kc->{nc}; ++$ci) {
    push(@clus, $kc->saveJsonCluster($ci));
  }

  return { head=>$hdr, clusters=>\@clus };
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sub saveJsonCluster {
  my ($kc,$ci,%opts) = @_;
  my $label = $opts{label} || "CLUSTER_$ci";
  my $ctag  = $opts{ctag}  || "c$ci";

  my $cinfo  = $kc->cinfo($ci);
  my @citems = map { {dist=>$kc->{wcdist}->at($_), known=>$kc->{kmask}->at($_), label=>$kc->{model}{wenum}->i2s($_)} } $cinfo->{wis}->list;
  my $jc = {
	    label  =>$label,
	    tag    =>$ctag,
	    size   =>$cinfo->{wis}->nelem,
	    dstats =>json_stats($cinfo->{wdist},"%.4f"),
	    items  =>\@citems,
	   };
  return $jc;
}
