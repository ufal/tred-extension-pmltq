package PMLTQ::TrEd;
# pajas@ufal.mff.cuni.cz          24 úno 2009

use 5.008;
use strict;
use warnings;
use Carp;

BEGIN { import TredMacro  }

sub matching_nodes {
  my ($self,$filename,$tree_number,$tree)=@_;
  return unless $self->{current_result};
  my $fn = $filename.'##'.($tree_number+1);
  my @nodes = ($tree,$tree->descendants);
  my @positions;
  for my $n (0..$#{$self->{current_result}}) {
    my $f = $self->get_nth_result_filename($n);
    if ($f=~/^\Q$fn\E\.(\d+)$/) {
      push @positions, $1
    }
  }
  return @nodes[@positions];
}

sub show_next_result {
  my ($self)=@_;
  return $self->show_result('next');
}

sub show_prev_result {
  my ($self)=@_;
  return $self->show_result('prev');
}

sub show_current_result {
  my ($self)=@_;
  return $self->show_result('current');
}


sub show_result {
  my ($self,$dir)=@_;
  my @save = ($this,$root,$grp);
  return unless $self->have_results();
  my @wins=$self->get_result_windows();
  my $seen=$self->_find_shown_result_indexes(\@wins);
  $self->update_label('Loading results ...');
  eval {
    { # attempt to locate the current node in one of the windows
      my $idx = $self->node_index_in_last_query($this);
      if (defined $idx) {
	# ok, this window shows the query
	unless ($seen->{$idx}) {
	  # this node is not shown in any window
	    my $m = $self->{current_result}[$idx];
	    if ($m and $m=~/^(([^#]+)(?:\#\#\d+|\#[^0-9#][^#]*))/g) {
	      my $win = $seen->{$1}||$seen->{$2};
	      if ($win) {
		SetMinorModeData('PMLTQ_Results','index',$idx,$win);
		$seen->{$idx}=$win;
	      } else {
		$win=$wins[0];
		SetMinorModeData('PMLTQ_Results','index',$idx,$win);
		$seen->{$idx}=$win;
		$seen->{$1}=$win;
		$seen->{$2}=$win;
	      }
	    }
	  }
      }
    }
    $self->prepare_results($dir,\@wins);
    for my $win (@wins) {
      $grp=$win;
      my $idx = GetMinorModeData('PMLTQ_Results','index');
      local $win->{noRedraw}=1;
      my $result_fn;
      if (!defined($idx) or $idx>$#{$self->{current_result}}) {
	$idx = $self->_assign_first_result_index_not_shown($seen,$win);
	SetMinorModeData('PMLTQ_Results','index',$idx);
      }
      if (defined $idx) {
	my $result_fn = $self->get_nth_result_filename($idx);
	if (defined($result_fn) and length($result_fn)) {
	  Open($result_fn,{-keep_related=>1});
	} else {
	  CloseFileInWindow($win);
	}
      } else {
	# CloseFileInWindow($win);
      }
      $win->{noRedraw}=0;
      unless ($win==$save[2]) {
	Redraw($win);
      } else {
	$save[0]=CurrentNodeInOtherWindow($win);
      }
    }
  };
  my $err=$@;
  $self->update_label;
  ($this,$root,$grp)=@save;
  die $err if $err;
  return;
}

sub get_result_windows {
  my ($self)=@_;
  my @wins = grep { IsMinorModeEnabled('PMLTQ_Results',$_) } TrEdWindows();
  unless (@wins) {
    my $win = SplitWindowVertically();
    EnableMinorMode('PMLTQ_Results',$win);
    die $@ if $@;
    @wins=($win);
  }
  return @wins;
}

sub _find_shown_result_indexes {
  my ($self,$wins)=@_;
  my $cur_res = $self->{current_result};
  my %seen;
  return unless $cur_res;
  for my $win (@$wins) {
    my $idx = GetMinorModeData('PMLTQ_Results','index',$win);
    if (defined($idx) and $idx<@$cur_res) {
      my $m = $cur_res->[$idx];
      $seen{$idx}=$win;
      if ($m=~/^(([^#]+)(?:\#\#\d+|\#[^0-9#][^#]*))/) {
	$seen{$1}=$win;
	$seen{$2}=$win;
      }
    }
  }
  return \%seen;
}

sub _assign_first_result_index_not_shown {
  my ($self,$seen,$win)=@_;
  $win||=$grp;
  $seen||=$self->_find_shown_result_indexes([ grep { IsMinorModeEnabled('PMLTQ_Results',$_) } TrEdWindows() ]);
  my $cur_res = $self->{current_result};
  return unless ref $cur_res and @$cur_res;
  # first try a specific file
  for my $i (0..$#{$cur_res}) {
    next if $seen->{$i};
    my $m = $cur_res->[$i];
    if (defined $m and $m=~/^(([^#]+)(?:\#\#\d+|\#[^0-9#][^#]*))/g) {
      if (!$seen->{$2}) {
	$seen->{$i}=$win;
	$seen->{$2}=$win;
	$seen->{$1}=$win;
	return $i;
      }
    }
  }
  # first then a specific tree
  for my $i (0..$#{$cur_res}) {
    next if $seen->{$i};
    my $m = $cur_res->[$i];
    if (defined $m and $m=~/^(([^#]+)(?:\#\#\d+|\#[^0-9#][^#]*))/g) {
      return $i if !$seen->{$2};
      if (!$seen->{$1}) {
	$seen->{$i}=$win;
	$seen->{$1}=$win;
	return $i;
      }
    }
  }
  # then a specific query node
  for my $i (0..$#{$cur_res}) {
    if (!$seen->{$i}) {
      $seen->{$i}=$win;
      return $i;
    }
  }
  return;
}


1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

PMLTQ::TrEd - common super-class for TrEd interfaces of PMLTQ search backends

=head1 SYNOPSIS

   use PMLTQ::TrEd;

=head1 DESCRIPTION

This class contains methods that display results of a PMLTQ search object in TrEd.
It assumes the following methods and keys to be defined on the derived class:

  $self->{current_result}  # an array-ref of results

  $self->have_results()       # return true if there are some results to display
  $self->update_label($text)  # update toolbar label with a given text
  $self->node_index_in_last_query($node) # translate result node to an index in the
                                         # $self->{current_result} array
  $self->prepare_results($dir) # prepare {current_result} array ($dir = next/prev/current)
  $self->get_nth_result_filename($n); # return filename/URL + position suffixes for the n-th result
                                      # ($self->{current_result}[$n])


It defines the following methods from the PMLTQ Search interface:

  $self->show_next_result;
  $self->show_prev_result;
  $self->show_current_result;
  $self->matching_nodes;

=head2 EXPORT

None by default.

=head1 SEE ALSO

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Petr Pajas

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=head1 BUGS

None reported... yet.

=cut

