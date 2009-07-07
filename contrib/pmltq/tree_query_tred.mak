# -*- cperl -*-
############################################################
{

package Tree_Query::TrEdSearch;
use Benchmark;
use Carp;
use strict;
use warnings;
BEGIN { import TredMacro  }

use Tree_Query::TrEd ();
use Tree_Query::TypeMapper ();

use base qw(Tree_Query::TrEd Tree_Query::TypeMapper);

use vars qw($DEBUG);
BEGIN {
  *DEBUG = \$Tree_Query::BtredEvaluator::DEBUG;
}

$Tree_Query::TrEdSearchPreserve::object_id=0; # different NS so that TrEd's reload-macros doesn't clear it

sub new {
  my ($class,$opts)=@_;
  $opts||={};
  my $self = $class->SUPER::new($opts);
  $self->{object_id} =  $Tree_Query::TrEdSearchPreserve::object_id++;
  $self->{$_} = undef for qw(evaluator query results); # create keys but leave undefined
  my $ident = $self->identify;
  Tree_Query::CreateSearchToolbar($ident);
  (undef, $self->{label}) = Tree_Query::CreateSearchToolbar($ident);
  $self->{particular_trees} = $opts->{particular_trees};
  $self->{on_destroy} = MacroCallback(sub {
					DestroyUserToolbar($ident);
					ChangingFile(0);
				      });
  return $self;
}

sub DESTROY {
  my ($self)=@_;
  warn "DESTROING $self\n";
  RunCallback($self->{on_destroy}) if $self->{on_destroy};
  unregister_open_file_hook($self->{callback});
}

sub identify {
  my ($self)=@_;
  return 'TrEdSearch-'.$self->{object_id}.' '
    .($self->{filelist} ? 'Filelist: '.$self->{filelist} :
      $self->{file}     ? 'File: '     .$self->{file}    : '');
}

sub configure {
  # nothing to configure here, yet
  # in the future we could set file/filelist/one tree only,
  # non-tred filelist etc.
  return;
}

sub reconfigure {
  return;
}

sub search_first {
  my ($self, $opts)=@_;
  $opts||={};
  local $SIG{__DIE__} = sub {
    confess(@_)
  } if defined($DEBUG) and $DEBUG>3;
  my $query = $opts->{query} || $root;
  $self->{query}=$query;
  my $evaluator = $self->{evaluator} = Tree_Query::BtredEvaluator->new($query,
						      {
							type_mapper => $self,
							current_filelist => $self->{filelist} ? 1 : 0,
							particular_trees => $self->{particular_trees} ? 1 : 0,
							no_filters => $opts->{no_filters},
							count => $opts->{count},
						      });
  $self->{current_result} = undef;
  $self->{past_results}=[];
  $self->{next_results}=[];
  $self->{have_all_results}=undef;
  $self->{currentFilePos} = 0;
  $self->{currentFilelistPos} = 0;

  if ($self->{evaluator}{filters} and (!$opts->{no_filters} or $opts->{count})) {
    my $canvas = ToplevelFrame()->Canvas();
    my $search_win= TrEd::Window->new(TrEd::TreeView->new($canvas),framegroup=>$grp->{framegroup}); # main::newTreeView(TrEd())
    $search_win->{macroContext}='TredMacro';
    $search_win->{stylesheet} = STYLESHEET_FROM_FILE();
    $search_win->{noRedraw}=1;
    my @save = ($grp,$root,$this);
    $grp=$search_win;
    my $results;
    eval {
      if ($self->{filelist}) {
	SetCurrentFileList($self->{filelist},$search_win);
	GotoFileNo(0);
	GotoTree(1);
      } else {
	Open($self->{file},{-keep_related=>1});
	GotoTree(1);
      }

      local $main::no_secondary=1; # load secondary files lazily
      $evaluator->init_filters($evaluator->buffer_all_filter);
      while ($evaluator->find_next_match) {
	$evaluator->run_filters
      }
      CloseFile();

      $results = $evaluator->flush_filters;
    };
    ($grp,$root,$this)=@save;
    $canvas->destroy;
    undef $search_win;
    die $@ if $@;

    my $query_id = (ref($query) && $query->{id}) || '';
    my $how_many = scalar(@$results).' row'.(@$results != 1 ? 's' : '' );
    return $results unless
      (@$results < 200) or
	QuestionQuery('Results',
		      $how_many,
		      'Display','Cancel') eq 'Display';
    Tree_Query::ShowResultTable('Results ('.$how_many.')',
				$results,
				$query_id,
			       );
  } else {
    return $self->show_next_result;
  }
}

sub current_query {
  my ($self)=@_;
  return $self->{query};
}

sub have_results {
  my ($self) = @_;
  return $self->{evaluator} ? 1 : 0;
}

sub prepare_results {
  my ($self,$dir,$wins)=@_;
  if ($dir eq 'next') {
    return unless $self->{evaluator};
    if ($self->{current_result}
	and
	!($self->{have_all_results}
	    and !($self->{next_results} and @{$self->{next_results}}))) {
      push @{$self->{past_results}},
	$self->{current_result};
    }
    if ($self->{next_results} and @{$self->{next_results}}) {
      $self->{current_result} = pop @{$self->{next_results}};
    } elsif ($self->{have_all_results}) {
      QuestionQuery('TrEdSearch','No more matches','OK');
    } else {
      my $search_win;
      # try to find the window to continue the search in
      if ($self->{filelist}) {
	# find a window displaying the searched filelist
	$search_win = first {
	  my $fl = TredMacro::GetCurrentFileList($_);
	  $fl && $fl->name eq $self->{filelist}
	}  @$wins;
      } elsif ($self->{file}) {
	# find a window displaying the searched file
	if (UNIVERSAL::isa($self->{file},'FSFile')) {
	  $search_win = first { $_->{FSFile} == $self->{file} } @$wins;
	} else {
	  $search_win = first {
	    my $fsfile = $_->{FSFile};
	    $fsfile && ($fsfile->filename eq $self->{file})
	  } @$wins;
	}
      }
      # no window? ok, use the first one
      $search_win ||= $wins->[0];
      die "No search window to use!" if ! $search_win;

      my @save = ($grp,$root,$this);
      $grp=$search_win;

      local $search_win->{noRedraw}=1;
      if ($self->{filelist}) {
	SetCurrentFileListInWindow($self->{filelist},$search_win,{no_open=>1});
	GotoFileNo($self->{current_result} ? $self->{currentFilelistPos} : 0);
	GotoTree($self->{current_result} ? $self->{currentFilePos}+1 : 1);
	print STDERR "Current filename: ", ThisAddress(),"\n" if defined($DEBUG) && $DEBUG > 1;
      } else {
	Open($self->{file},{-keep_related=>1});
	GotoTree($self->{current_result} ? $self->{currentFilePos}+1 : 1);
      }
      my $result;
      local $main::no_secondary=1; # load secondary files lazily
      eval {
	$result = $self->{evaluator}->find_next_match();
	if ($result) {
	  my $result_files = $self->{evaluator}->get_result_files;
	  $self->{current_result} = [
	    map {
	      UNIVERSAL::isa($result->[$_],'FSNode') ? ThisAddress($result->[$_],$result_files->[$_]) : undef
	    } 0..$#$result
	   ];
	} else {
	  $self->{have_all_results}=1;
	  pop @{$self->{past_results}};
	}
      };
      $self->{currentFilePos} = CurrentTreeNumber($search_win);
      $self->{currentFilelistPos} = CurrentFileNo($search_win);
      # $Redraw='all';
      ($grp,$root,$this)=@save;
      die $@ if $@;
      unless ($result) {
	QuestionQuery('TrEdSearch','No more matches','OK');
      }
    }
  } elsif ($dir eq 'prev') {
    return unless $self->{evaluator};
    if ($self->{past_results} and @{$self->{past_results}}) {
      if ($self->{current_result}) {
	push @{$self->{next_results}},
	  $self->{current_result};
      }
      $self->{current_result} = pop @{$self->{past_results}};
    }
  }
}

sub get_result_windows {
  my ($self)=@_;
  my @wins = grep { IsMinorModeEnabled('Tree_Query_Results',$_) } TrEdWindows();
  if (!@wins) {
    if ($self->{file}) {
      @wins = map { $_->[0] } grep { $_->[1]->filename eq $self->{file} }
	grep ref($_->[1]), map [$_,CurrentFile($_)],
	grep { $_ != $grp }
	TrEdWindows();
    } else {
      @wins = map { $_->[0] } grep { $_->[1]->name eq $self->{filelist} } grep ref($_->[1]), map [$_,GetCurrentFileList($_)],
	grep { $_ != $grp }
	TrEdWindows();
    }
    EnableMinorMode('Tree_Query_Results',$_) for @wins;
  }
  if (@wins) {
    return @wins;
  } else {
    return $self->Tree_Query::TrEd::get_result_windows();
  }
}


sub get_nth_result_filename {
  my ($self,$idx)=@_;
  return $self->{current_result}[$idx];
}

sub update_label {
  my ($self)=@_;
  my $past = (($self->{past_results} ? int(@{$self->{past_results}}) : 0)
		+ ($self->{current_result} ? 1 : 0));
  ${$self->{label}} = $past
    .' of '
    .($self->{next_results} ? $past+int(@{$self->{next_results}}) : $past)
    .($self->{have_all_results} ? '' : '+');
}

sub map_nodes_to_query_pos {
  my ($self,$filename,$tree_number,$tree,$fsfile)=@_;
  return unless $self->{current_result};
  my $fn = $filename.'##'.($tree_number+1);
  my @nodes = ($tree,$tree->descendants);
  my $r = $self->{current_result};
  return {
    map {
      my @ret;
      if (defined($_->[1])) {
	if ($_->[1]=~/^\Q$fn\E\.([0-9]+)$/) {
	  @ret=($nodes[$1] => $_->[0])
	} elsif ($fsfile and $_->[1]=~/^\Q$filename\E#([^#0-9][^#]*)$/) {
	  my $n = PML::GetNodeByID($1,$fsfile);
	  @ret = ($n => $_->[0]) if $n;
	}
      }
      @ret
    } reverse # upper nodes first (optional nodes do not overwrite their parents)
      map { [$_,$r->[$_]] } 0..$#$r
  };
}

sub node_index_in_last_query {
  my ($self,$query_node)=@_;
  return unless $self->{current_result};
  return $self->{evaluator}->{query_node2pos}{$query_node};
}

sub select_matching_node {
  my ($self,$query_node)=@_;
  return unless $self->{current_result} and $self->{evaluator};
  my $idx = $self->node_index_in_last_query($query_node);
  return if !defined($idx);
  my $result = $self->{current_result}->[$idx];
  return unless defined($result);
  foreach my $win (TrEdWindows()) {
    my $fsfile = $win->{FSFile};
    next unless $fsfile;
    my $filename = $fsfile->filename;
    my $r;
    my $fn = $filename.'##'.($win->{treeNo}+1);
    if ($result =~ /\Q$fn\E\.([0-9]+)$/) {
      my $pos = $1;
      $r=$fsfile->tree($win->{treeNo});
      for (1..$pos) {
	$r=$r && $r->following();
      }
    } elsif ($result =~ /\Q$filename\E\#([^#0-9][^#]*)$/) {
      $r = PML::GetNodeByID($1,$fsfile);
      undef $r unless ($win->{Nodes} and first { $_ == $r } @{$win->{Nodes}});
    }
    if ($r) {
      EnableMinorMode('Tree_Query_Results',$win);
      SetCurrentNodeInOtherWin($win,$r);
      $Redraw='none';
      Redraw($win);
      CenterOtherWinTo($win,$r);
    }
  }
  return;
}

}
