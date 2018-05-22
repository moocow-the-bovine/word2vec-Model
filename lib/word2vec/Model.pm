##-*- Mode: CPerl -*-

package word2vec::Model;
use PDL;
use PDL::IO::FastRaw;
use PDL::VectorValued;
use DiaColloDB::Logger;
use DiaColloDB::EnumFile::MMap;
use open qw(:std :utf8);
use strict;

our @ISA = qw(DiaColloDB::Persistent);
our $VERSION = '0.0.3';

BEGIN {
  no warnings 'once';
  $PDL::BIGPDL = 1;

  my $logpkg = __PACKAGE__;
  $logpkg =~ s/::.*$//;

  push(@{$DiaColloDB::Logger::defaultLogOpts{logwhich}}, $logpkg)
    if (!grep {$_ eq $logpkg} @{$DiaColloDB::Logger::defaultLogOpts{logwhich}});
}

##==============================================================================
## constructors

## $model = CLASS_OR_OBJECT->new(%args)
##  + %args, %$model:
##    (
##     base  => $base,    ##-- file basename
##     start => $wi0,     ##-- first word-ID (default=1)
##     type  => $type,    ##-- type for rwmat (default='float')
##     minn  => $minn,    ##-- minimum n-gram length for OOV vector estimation (default=0:disable)
##     maxn  => $maxn,    ##-- maximum n-gram length for OOV vector estimation (default=0:disable)
##     nganchor => $bool, ##-- if true, only use anchored (BOW,EOW) n-grams (default=0)
##     ngweight => $bool, ##-- if true, regex vectors are weighted by {wfreq} if available (default=1)
##     xweight => $bool,  ##-- if true, query vectors are weighted by {wfreq} if available (default=0)
##     ##
##     ##-- logging
##     logOOV => $level,  ##-- log-level for OOV words (default='warn')
##     ##
##     ##-- guts
##     wenum => $wenum,   ##-- word (lemma) enum
##     nr    => $nr,      ##-- number of "topics" / "latent dimensions" of model / model "rank"
##     nw    => $nw,      ##-- number of enum-ified words
##     rwmat => $rwmat,   ##-- "$base.pdl"     : topic-word matrix ($nr,$nw): [$ri,$wi] => $rval_at_w
##     wfreq => $wfreq,   ##-- "$base.wfreq.pdl": word frequencies, optional: ($nw): [$wi] => $f_w
##    )
sub new {
  my $that = shift;
  my $model = bless({
		     base  => undef,
		     start => 1,
		     minn => 0,
		     maxn => 0,
		     nganchor => 0,
		     ngweight => 1,
		     xweight => 0,
		     logOOV => 'warn',
		     wenum => DiaColloDB::EnumFile::MMap->new(flags=>'r'),
		     nr  => undef,
		     nw  => undef,
		     rwmat => undef,
		     wfreq => undef,
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
  delete @$model{qw(wenum rwmat wfreq nw nr)};
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
  return ($model->{base} ? (map {glob("$model->{base}.$_*")} qw(pdl enum freq)) : qw());
}


##======================================================================
## I/O

##--------------------------------------------------------------
## I/O: compile

## $model = $model->compile($fasttext_text_vectors_file_or_fh, %opts)
##  + %opts: clobber %$model, also:
##     nodims => $bool,       ##-- don't try to load dimensions from 1st line
##     freqfile => $freqfile, ##-- compile {wfreq} from $freqfile (WORD FREQ), optional
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
    or $model->logconfess("compile(): failed to close header file '$pdlbase.hdr': $!");

  ##-- create enum
  $model->info("compile(): creating enum $base.enum.*");
  my $ebase = "$base.enum";
  my $wenum = $model->{wenum} = DiaColloDB::EnumFile::MMap->new(flags=>'rw',base=>$ebase);
  $wenum->fromArray($i2s)
    or $model->logconfess("compile(): failed to initialize enum from array");
  $wenum->save()
    or $model->logconfess("compile(): failed to save enum to '$ebase.*': $!");

  ##-- compile frequency file
  if ( (my $freqfile = $opts{freqfile}) ) {
    $model->info("compile(): compiling word frequencies from '$freqfile'");
    open(my $finfh, "<$freqfile")
      or $model->logconfess("compile(): open failed for word-frequency input file '$freqfile': $!");
    my $wfreq = zeroes(PDL::Type->new($ptype),$nw);
    my ($wi,$wf,$rest);
    while (defined($_=<$finfh>)) {
      chomp;
      next if (/^\s*$/ || /^\S+$/); ##-- ignore dimensions
      ($w,$wf,$rest) = split(' ',$_,3);
      if (!defined($wi = $wenum->s2i($w))) {
	$model->logwarn("compile(): unknown word '$w' in word-frequency file '$freqfile' - ignoring");
	next;
      }
      $wfreq->set($wi => $wf);
    }
    CORE::close($finfh);
    $wfreq->writefraw("$base.wfreq.pdl")
      or $model->logconfess("compile(): failed to write '$base.wfreq.pdl': $!");
  }

  ##-- now open the model
  #$model->info("compile(): opening newly compiled model");
  return $model->open();
}

##--------------------------------------------------------------
## I/O: project

## $pmodel = $model->project(\@words, %opts)
##  + returns $model restricted to @words
##  + %opts are passed to ref($model)->new() for $pmodel
##  + default %opts:
##     start => 0
sub project {
  my ($imodel,$words,%opts) = @_;

  ##-- initialization
  my $base  = $opts{base};
  $imodel->logconfess("project(): no output {base} specified") if (!$base);

  my $model = ref($imodel)->new(%opts, base=>undef);
  $model->clear();

  ##-- basic variables
  my $start  = ($model->{start} //= 0);
  my $type   = $model->{type} || 'float';

  ##-- get dimensions
  my $nw = $model->{nw} = scalar(@$words);
  my $nr = $model->{nr} = $imodel->{nr};

  ##-- get basic pdl datatype info
  my $pdlbase = "$base.pdl";
  my $ptype   = $PDL::Types::typenames{$type}
    or $model->logconfess("project(): unknown PDL type '$type'; use one of (",
			  join(' ', map {$_->{ioname}} sort {$a->{numval}<=>$b->{numval}} values %PDL::Types::typehash), ")"
			 );
  my $packas = $PDL::Types::pack[$ptype]
    or $model->logconfess("project(): no pack-type for PDL type '$type'!");

  ##-- open raw pdl outfile
  $model->info("project(): preparing pdl-file $pdlbase");
  CORE::open(my $pdlfh, ">$pdlbase")
    or $model->logconfess("project(): open failed for raw PDL file $pdlbase: $!");
  binmode($pdlfh,':raw');

  ##-- pdl file: write zeroes until start offset
  if (defined($nr) && $start > 0) {
    $model->info("project(): prepending initial zero-vector(s) (start=$start, nr=$nr)");
    my $buf = pack($packas,0) x $nr;
    print $pdlfh ($buf x $start) if ($start > 0);
  }

  ##-- generate pdl-vectors
  my $i2s = ($start==0 ? $words : []);      ##-- $w=$i2s->[$wi]
  $i2s->[$start-1] = undef if ($start > 0); ##-- honor start offset
  my ($w,$wv);
  foreach $w (@$words) {
    push(@$i2s,$w) if ($start != 0);
    $wv = $imodel->any2v($w);
    print $pdlfh pack($packas,$wv->list); ##-- possible type conversion via perl pack
  }
  ##-- finalize pdl-vectors
  CORE::close($pdlfh)
      or $model->logconfess("project(): close failed for raw PDL-file '$pdlbase': $!");

  ##-- finalize dimensions
  $model->{nw} = $nw = scalar(@$i2s);

  ##-- write pdl header
  $model->info("project(): writing pdl-header $pdlbase.hdr");
  CORE::open(my $hdrfh,">$pdlbase.hdr")
  or $model->logconfess("project() open failed for '$pdlbase.hdr': $!");
  binmode($hdrfh,":raw");
  print $hdrfh
    ("$ptype\n",   ##-- type-id
     "2\n",        ##-- ndims
     "$nr $nw\n",  ##-- dims
    );
  CORE::close($hdrfh)
    or $model->logconfess("project(): failed to close header file '$pdlbase.hdr': $!");

  ##-- create enum
  $model->info("project(): creating enum $base.enum.*");
  my $ebase = "$base.enum";
  my $wenum = $model->{wenum} = DiaColloDB::EnumFile::MMap->new(flags=>'rw',base=>$ebase);
  $wenum->fromArray($i2s)
    or $model->logconfess("project(): failed to initialize enum from array");
  $wenum->save()
    or $model->logconfess("project(): failed to save enum to '$ebase.*': $!");

  ##-- TODO: project frequencies (NOT YET IMPLEMENTED)

  ##-- now open the model
  #$model->info("project(): opening newly compiled model");
  return $model->open($base);
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

  ##-- open matrix PDL
  $model->{rwmat} = mapfraw("$base.pdl",{ReadOnly=>1});
  defined($model->{rwmat})
    or $model->logconfess("open(): failed to mmap PDL file '$base.pdl': $!");

  ##-- (optional): open frequency pdl
  if (-e "$base.wfreq.pdl") {
    $model->{wfreq} = mapfraw("$base.wfreq.pdl",{ReadOnly=>1});
    defined($model->{wfreq})
      or $model->logconfess("open(): failed to mmap frequency PDL file '$base.wfreq.pdl': $!");
  }

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
  delete @$model{qw(rwmat nr nw wfreq)};
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
##  + gets raw word-vector for known word with index $wi
##  + returns null-vector if $wi is out-of-bounds
##  + in list-context, second component $n is 1 iff $w is known, otherwise 0
sub wi2v {
  my ($model,$wi) = @_;
  $model->trace("wi2v($wi)");
  if (defined($wi) && $wi >= 0 && $wi < $model->{wenum}{size}) {
    my $wv = $model->{rwmat}->slice(",($wi)");
    return (defined($model->{wfreq}) && $model->{xweight} ? ($wv*$model->{wfreq}->index($wi)) : $wv);
  }
  $model->vlog($model->{logOOV}, "wi2v(): returning null vector for unknown word index #$wi");
  return $model->nullv;
}

##--------------------------------------------------------------
## $wv = $model->w2v($w)
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
  return $model->nullv;
}

##--------------------------------------------------------------
## $wv = $model->re2v($re)
##  + gets average word-vector for all known words matching regex $re
##  + returns null-vector if $re doesn't match anything
##  + match contributions are weighted by $model->{wfreq} if available and $model->{ngweight} is true
sub re2v {
  my ($model,$re) = @_;
  utf8::decode($re) if (!ref($re) && !utf8::is_utf8($re));
  $model->trace("re2v($re)");

  my $wis = $model->{wenum}->re2i($re);
  if ($wis && @$wis) {
    my $wip = pdl(indx,$wis);
    if (defined($model->{wfreq}) && $model->{ngweight}) {
      ##-- weight by word frequencies
      my $wmat = $model->{rwmat}->dice_axis(1,$wip)->xchg(0,1);
      my $wf   = $model->{wfreq}->index($wip);
      my $wv   = ($wmat * $wf)->sumover;
      $wv     /= $wf->sumover;
      return $wv;
    } else {
      ##-- uniform weighting
      my $wv = $model->{rwmat}->dice_axis(1,$wip)->xchg(0,1)->sumover / $wip->nelem;
      return $wv;
    }
  }
  $model->vlog($model->{logOOV}, "re2v(): returning null vector for unknown regex $re");
  return $model->nullv;
}

##--------------------------------------------------------------
## $wv = $model->ngrams2v($w,%opts)
##  + gets average word-vector for all n-grams between lengths $model->{minn} and $model->{maxn}
##  + implicitly calls re2v() on generated n-gram regex
##  + not really helpful (resulting vectors are too generic / only garbage neighbors)
##    - frequency-weighting helps sometimes (nn("Ochsenhaus")="Haus"), but is an ugly post-hoc workaround (nn("Ochsenöl") = "Sachsen")
sub ngrams2v {
  my ($model,$w) = @_;
  utf8::decode($w) if (!utf8::is_utf8($w));
  $model->trace("ngrams2v($w)");

  my ($minn,$maxn,$nganchor) = map {($_//0)} @$model{qw(minn maxn nganchor)};
  if ($minn > 0 && $maxn >= $minn) {
    my @ngrams = qw();
    my ($i,$j,$len);
    my $ww = '^'.$w.'$';
    for ($i=0; $i < length($ww); ++$i) {
      for ($j=$i+1; $j <= length($ww); ++$j) {
	$len = $j-$i;
	if ($len >= $minn && $len <= $maxn && (!$nganchor || $i==0 || $j==length($ww))) {
	  push(@ngrams, quotemeta(substr($ww,$i,$len)));
	}
      }
    }
    my $re_str = join('|', map {s/^\\\^/^/; s/\\\$$/\$/; "(?:$_)"} grep {$_ ne "\\^" && $_ ne "\\\$" } @ngrams);
    $model->vlog($model->{logOOV}, "ngrams2v(): unknown word '$w' - creating n-gram regex (minn=$minn; maxn=$maxn; nganchor=$nganchor; ngweight=$model->{ngweight}): regex=/$re_str/");
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
  if (defined($model->{wfreq}) && $model->{xweight}) {
    $sim  *= $model->{wfreq};
    $sim  /= $sim->abs->maximum;
  }
  my $maxi = zeroes(indx,$k);
  $sim->maximum_n_ind($maxi);

  return wantarray ? ($maxi,$sim->index($maxi)->sever) : $maxi;
}


##==============================================================================
## API: k-means / k-medoids clustering

##--------------------------------------------------------------
## $kc = $model->kcluster(%opts)
## $kc = $model->kcluster(%opts)
##  + options %opts as for word2vec::Kcluster->new()
sub kcluster {
  my ($model,%opts) = @_;
  require word2vec::Kcluster;
  return word2vec::Kcluster->new(%opts,model=>$model);
}
