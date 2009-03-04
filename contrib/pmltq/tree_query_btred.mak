# -*- cperl -*-
{
package Tree_Query_Btred;
use strict;

BEGIN {
  use vars qw($this $root);
  import TredMacro;
}

our $DEBUG;
#ifdef TRED
$DEBUG=1;
#endif

my $evaluator;
#ifndef TRED
my $query_tree;
my $evaluator_opts;
sub start_hook {
  use Getopt::Long ();
  my %opts;
  Getopt::Long::GetOptions(
    \%opts,
    'query|q=s',
    'plan|p',
  ) or die "Wrong options\n";
  init_search(\%opts);
}

sub init_search {
  my ($opts)=@_;
  if (defined $opts->{query}) {
    my $query=$opts->{query};
    die "Empty query\n" unless length $query;
    # FIXME: specific_relations
    my $type_mapper = $opts->{type_mapper};
    $query_tree=Tree_Query::parse_query($query,{
      specific_relations =>  $type_mapper && $type_mapper->get_specific_relations(),
    });
    DetermineNodeType($_) for $query_tree->descendants;
  } else {
    my ($query_fn,$query_id)=@ARGV;
    my $query_file = FSFile->newFSFile($query_fn,[Backends()]);
    if (ref($query_file)) {
      if ($Fslib::FSError!=0) {
	die "Error: loading query-file failed: $@ ($!)\n";
      } elsif ($query_file->lastTreeNo<0) {
	die "Error: Query file is empty\n";
      }
    }
    $query_tree = $query_file->appData('id-hash')->{$query_id};
    die "Query tree $query_fn#$query_id not found\n" unless ref $query_tree;
  }
  #  print STDERR "$query_file\n";
  #plan_query($query_tree) if $opts->{plan};
  $evaluator_opts = {plan=>$opts->{plan}};
  # print STDERR "initialized @iterators, $query_pos\n";
  # print $query_node,",",$query_tree->{id},"\n";
}

sub new_evaluator {
  $evaluator_opts->{type_mapper} ||= Tree_Query::TypeMapper->new({file=>CurrentFile()});
  Tree_Query::BtredEvaluator->new($query_tree, $evaluator_opts);
}

sub next_match {
  $evaluator ||= new_evaluator(PML::Schema());
  my $match = $evaluator->find_next_match();
  $evaluator->reset() unless $match; # prepare for next file
  return $match;
}

sub print_all_matches {
  my $match;
  $evaluator ||= new_evaluator(PML::Schema());
  while ($match = $evaluator->find_next_match()) {
    print join(" ",map { $_->{id} } @$match)."\n";
  }
  $evaluator->reset(); # prepare for next file
}

sub count_matches {
  $evaluator ||= new_evaluator(PML::Schema());
  my $limit = @_ ? int(shift()) : 100;
  my $count=0;
  $count++ while $evaluator->find_next_match({boolean => 1}) and (!$limit or $count<=$limit);
  $evaluator->reset(); # prepare for next file
  return $count;
}

#endif

# sub test {
#   # assuming the tree we get is ordered
#   my ($restart,$opts)=@_;
#   my $query_tree=$root;
#   my ($win) = grep {
#     my $fl = GetCurrentFileList($_);
#     ($fl and $fl->name eq 'Tree_Query')
#   } TrEdWindows();
#   return unless $win;
#   my $fsfile = CurrentFile($win);
#   return unless $fsfile;
#   {
#     my $cur_win = $grp;
#     $grp=$win;
#     eval {
#       print STDERR "Searching...\n" if $DEBUG;
#       $Tree_Query::btred_results=1;
#       %Tree_Query::is_match=();
#       my $one_tree = delete $opts->{one_tree};
#       if ($one_tree) {
# 	$opts->{tree}=$fsfile->tree(CurrentTreeNumber($win));
#       }
#       # $opts->{fsfile} = $fsfile;
#       $evaluator = Tree_Query::BtredEvaluator->new($query_tree,$opts) if !$evaluator or $restart;
#       #  return;
#       my $match = $evaluator->find_next_match();
#       if ($match) {
# 	%Tree_Query::is_match = map { $_ => 1 } @$match;
# 	print join(",",map { $_->{id}.": ".$_->{functor} } @$match)."\n";
# 	SetCurrentNodeInOtherWindow($win,$match->[0]);
#       }
#       print STDERR "Searching done!\n" if $DEBUG;
#       $Redraw='all';
#     };
#     $grp=$cur_win;
#   }
#   my $err = $@;
#   die $err if $err;
# }

############################################################

{

package Tree_Query::TypeMapper;
use PMLSchema;
use strict;
use warnings;

BEGIN { import TredMacro  }

sub new {
  my ($class,$opts)=@_;
  $opts||={};
  my $what = ($opts->{file}?0:1) +
             ($opts->{filelist}?0:1);
  die "Neither fsfile, nor filelist were specified!" unless $what;
  die "Options fsfile, filelist are exclusive!" if $what>1;
  my $self = bless {
    file => $opts->{file},
    filelist => $opts->{filelist},
  }, $class;
  return $self;
}

sub get_schema_for_query_node {
  my ($self,$node)=@_;
  my $decl = $self->get_type_decl_for_query_node($node);
  return $decl && $decl->schema;
}
sub get_schema_for_type {
  my ($self,$type)=@_;
  my $decl = $self->get_decl_for($type);
  return $decl && $decl->get_schema;
}

sub _get_fsfile {
  my ($self)=@_;
  my $fsfile = $self->{fsfile};
  return $fsfile if $fsfile;
  my $file = $self->{file};
  my $fl;
  if ($file) {
    if (ref($file) and UNIVERSAL::isa($file,'FSFile')) {
      $self->{file} = $file->filename;
      return $self->{fsfile} = $file;
    } else {
      return $self->{fsfile} =  first { $_->filename eq $file } GetOpenFiles();
    }
  } elsif ($fl = GetFileList($self->{filelist})) {
    my %fl;
    my @files = $fl->files;
    @fl{ @files } = ();
    return $self->{fsfile} = ((first { exists($fl{$_->filename}) } GetOpenFiles())||
				$files[0] && Open(AbsolutizeFileName($files[0],$fl->filename),{-preload=>1}));
  }
}

sub get_schema {
  my ($self)=@_;
  my $fsfile = $self->_get_fsfile || return;
  return PML::Schema($fsfile);
}

sub get_schemas {
  my ($self)=@_;
  my $fsfile = $self->_get_fsfile || return;
  return uniq map PML::Schema($_), ($fsfile,  GetSecondaryFiles($fsfile));
}

sub get_type_decl_for_query_node {
  my ($self,$node)=@_;
  return $self->get_decl_for(Tree_Query::Common::GetQueryNodeType($node));
}

sub get_decl_for {
  my ($self,$type)=@_;
  $type=~s{(/|$)}{.type$1};
  for my $schema ($self->get_schemas) {
    my $decl = $schema->find_type_by_path('!'.$type);
    return $decl if $decl;
  }
  warn "Did not find type '!$type'";
  return;
}

sub get_node_types {
  my ($self)=@_;
  return [sort map Tree_Query::Common::DeclToQueryType( $_ ), map $_->node_types, $self->get_schemas];
}

sub _find_pmlrf_relations {
  my ($self)=@_;
  my @schemas = $self->get_schemas;
  my @relations=();
  my %relations;
  for my $schema (@schemas) {
    for my $type ($schema->node_types) {
      my $type_name = Tree_Query::Common::DeclToQueryType($type);
      for my $path ($type->get_paths_to_atoms({ no_childnodes => 1 })) {
	my $decl = $type->find($path);
	$decl=$decl->get_content_decl unless $decl->is_atomic;
	if ($decl->get_decl_type == PML_CDATA_DECL and
	    $decl->get_format eq 'PMLREF') {
	  push @relations, $path;
	  $relations{$type_name}{$path}='#any';
	}
      }
    }
  }
  $self->{pmlrf_relations_hash}=\%relations;
  return $self->{pmlrf_relations} = [sort @relations];
}

sub get_pmlrf_relations {
  my ($self)=@_;
  return $self->{pmlrf_relations} || $self->_find_pmlrf_relations;
}

sub get_pmlrf_relations_hash {
  my ($self)=@_;
  my $hash = $self->{pmlrf_relations_hash};
  unless ($hash) {
    $self->_find_pmlrf_relations;
    $hash = $self->{pmlrf_relations_hash};
  }
  return $hash;
}


sub get_specific_relations {
  my ($self)=@_;
  return [qw(echild eparent a/lex.rf|a/aux.rf), 
	  @{$self->get_pmlrf_relations}];
}

my %user_defined_relations = (
  't-root' => {
  },
  't-node' => {
    'a/lex.rf|a/aux.rf' => 'a-node',
    'echild' => 't-node',
    'eparent' => 't-node',
  },
  'a-node' => {
    'echild' => 'a-node',
    'eparent' => 'a-node',
  },
);

my %known_pmlref_relations = (
  't-root' => {
    'a/lex.rf' => 'a-root',
  },
  't-node' => {
    'a/lex.rf' => 'a-node',
    'a/aux.rf' => 'a-node',
    'val_frame.rf' => 'v-frame',
    'coref_text.rf' => 't-node',
    'coref_gram.rf' => 't-node',
    'compl.rf' => 't-node',
  },
  'a-node' => {
    'p/terminal.rf' => 'english_p_terminal',
    'p/nonterminals.rf' => 'english_p_nonterminal',
  },
);

sub get_relation_target_type {
  my ($self,$node_type,$relation)=@_;
  my $pmlref_relations_hash = $self->get_pmlrf_relations_hash;
  return $known_pmlref_relations{$node_type} && $known_pmlref_relations{$node_type}{$relation}
       || $user_defined_relations{$node_type} && $user_defined_relations{$node_type}{$relation}
       || $pmlref_relations_hash->{$node_type} && $pmlref_relations_hash->{$node_type}{$relation}
	 ;
}

1;

}

############################################################

{

package Tree_Query::TrEdSearch;
use Benchmark;
use Carp;
use strict;
use warnings;
BEGIN { import TredMacro  }

use base qw(Tree_Query::TrEd Tree_Query::TypeMapper);

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
  } if $DEBUG>1;
  my $query = $opts->{query} || $root;
  $self->{query}=$query;
  $self->{evaluator} = Tree_Query::BtredEvaluator->new($query,
						      {
							type_mapper => $self,
							current_filelist => $self->{filelist} ? 1 : 0
						      });
  $self->{current_result} = undef;
  $self->{past_results}=[];
  $self->{next_results}=[];
  $self->{have_all_results}=undef;
  $self->{currentFilePos} = 0;
  $self->{currentFilelistPos} = 0;
  return $self->show_next_result;
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
    if ($self->{current_result} and !$self->{have_all_results}) {
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
	SetCurrentFileList($self->{filelist},$search_win);
	GotoFileNo($self->{current_result} ? $self->{currentFilelistPos} : 0);
	GotoTree($self->{current_result} ? $self->{currentFilePos}+1 : 1);
	print STDERR "Current filename: ", ThisAddress(),"\n" if $DEBUG > 1;
      } else {
	Open($self->{file},{-keep_related=>1});
	GotoTree($self->{current_result} ? $self->{currentFilePos}+1 : 0);
      }
      my $result;
      eval {
	$result = $self->{evaluator}->find_next_match();
	if ($result) {
	  my $result_files = $self->{evaluator}->get_result_files;
	  $self->{current_result} = [
	    map ThisAddress($result->[$_],$result_files->[$_]), 0..$#$result
	   ];
	} else {
	  $self->{have_all_results}=1;
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
  my ($self,$filename,$tree_number,$tree)=@_;
  return unless $self->{current_result};
  my $fn = $filename.'##'.($tree_number+1);
  my @nodes = ($tree,$tree->descendants);
  my $r = $self->{current_result};
  return {
    map { $_->[1]=~/^\Q$fn\E\.(\d+)$/ ? ($nodes[$1] => $_->[0]) : () } 
      reverse # upper nodes first (optional nodes do not overwrite their parents)
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
  foreach my $win (TrEdWindows()) {
    my $fsfile = $win->{FSFile};
    next unless $fsfile;
    my $fn = $fsfile->filename.'##'.($win->{treeNo}+1);
    next unless $result =~ /\Q$fn\E\.(\d+)$/;
    my $pos = $1;
    my $r=$fsfile->tree($win->{treeNo});
    for (1..$pos) {
      $r=$r && $r->following();
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

######## Private

sub claim_search_win {
  my ($self,$file)=@_;
  my $win;
  $file ||= $self->{file};
  if ($file) {
    ($win) = map { $_->[0] } grep { $_->[1]->filename eq $file } grep ref($_->[1]), map [$_,CurrentFile($_)], TrEdWindows();
  } else {
    ($win) = map { $_->[0] } grep { $_->[1]->name eq $self->{filelist} } grep ref($_->[1]), map [$_,GetCurrentFileList($_)], TrEdWindows();
  }
  unless ($win) {
    $win = SplitWindowVertically();
    my $cur_win = $grp;
    $grp=$win;
    EnableMinorMode('Tree_Query_Results',$win);
    eval {
      if ($file) {
	Open($file);
      } elsif ($self->{filelist}) {
	SetCurrentFileList($self->{filelist});
      }
    };
    $grp=$cur_win;
    die $@ if $@;
  }
  return $win;
}


}

###########################################
{
  package Tree_Query::BtredEvaluator;
  use Carp;
  use strict;
  use Scalar::Util qw(weaken);
  use List::Util qw(first);
  import TredMacro qw(SeqV uniq);
  use PMLSchema;

  my %test_relation = (
    'parent' => q($start->parent == $end),
    'child' => q($end->parent == $start),

    'depth-first-precedes' => q( $start->root==$end->root and  do{my $n=$start->following; $n=$n->following while ($n and $n!=$end); $n ? 1 : 0 }), # not very effective !!
    'depth-first-follows' => q( $start->root==$end->root and  do{my $n=$end->following; $n=$n->following while ($n and $n!=$start); $n ? 1 : 0 }), # not very effective !!
    'same-tree-as' => q($start->root==$end->root), # not very effective !!
   );

  my %test_user_defined_relation = (
    'echild' => q(do{ my $type = $node->type->get_base_type_name;
                        grep $_ == $start,
                        ($type eq 't-node.type' ? PML_T::GetEParents($end) :
                         $type eq 'a-node.type' ? PML_A::GetEParents($end,\\&PML_A::DiveAuxCP) : ()) }),
    'eparent' => q(do{ my $type = $node->type->get_base_type_name;
                        grep $_ == $end,
                        ($type eq 't-node.type' ? PML_T::GetEParents($start) :
                         $type eq 'a-node.type' ? PML_A::GetEParents($start,\\&PML_A::DiveAuxCP) : ()) }),
    'a/lex.rf|a/aux.rf' => q(grep $_ eq $end->{id}, GetANodeIDs()),
    'a/lex.rf' => q(do { my $id=$start->attr('a/lex.rf'); $id=~s/^.*?#//; $id  eq $end->{id} } ),
    'a/aux.rf' => q(grep { my $id=$_; $id=~s/^.*?#//; $id eq $end->{id} } TredMacro::ListV($start->attr('a/lex.rf'))),
    'coref_text.rf' => q(grep $_ eq $end->{id}, TredMacro::ListV($start->{'coref_text.rf'})),
    'coref_gram.rf' => q(grep $_ eq $end->{id}, TredMacro::ListV($start->{'coref_gram.rf'})),
    'compl.rf' => q(grep $_ eq $end->{id}, TredMacro::ListV($start->{'compl.rf'})),
   );


  sub new {
    my ($class,$query_tree,$opts)=@_;

    $opts ||= {};
    #######################
    # The following lexical variables may be used directly by the
    # condition subroutines
    my @conditions;
    my @iterators;
    my @sub_queries;
    my $parent_query=$opts->{parent_query};
    my $matched_nodes = $parent_query ? $parent_query->{matched_nodes} : [];
    my %have;
    my $query_pos;
    #######################


    my @debug;
    my %name2pos;
    my %name2type;
    # maps node position in a (sub)query to a position of the matching node in $matched_nodes
    # we avoid using hashes for efficiency
    my $self = bless {

      query_pos => 0,
      iterators => \@iterators,
      filter => undef,
      conditions => \@conditions,
      have => \%have,

      debug => \@debug,

      sub_queries => \@sub_queries,
      parent_query => $parent_query,
      parent_query_pos => $opts->{parent_query_pos},
      parent_query_match_pos => $opts->{parent_query_match_pos},

      matched_nodes => $matched_nodes, # nodes matched so far (incl. nodes in subqueries; used for evaluation of cross-query relations)

      type_mapper => $opts->{type_mapper},

      name2pos => \%name2pos,
      name2type => \%name2type,
      parent_pos => undef,
      pos2match_pos => undef,
      name2match_pos => undef,
      # query_nodes => [],
      results => [],
    }, $class;
    croak(__PACKAGE__."->new: missing required option: type_mapper") unless $self->{type_mapper};
    weaken($self->{parent_query}) if $self->{parent_query};
    $query_pos = \$self->{query_pos};


    my $clone_before_plan = 0;
    if (ref($query_tree)) {
      $clone_before_plan = 1;
    } else {
      $query_tree = Tree_Query::parse_query($query_tree,{
	specific_relations =>  $self->{type_mapper}->get_specific_relations(),
      });
      TredMacro::DetermineNodeType($_) for $query_tree->descendants;
    }

    my $type = $query_tree->type->get_base_type_name;
    unless ($type eq 'q-query.type' or
	    $type eq 'q-subquery.type') {
      die "Not a query tree: $type!\n";
    }
    my $roots;
    my @orig_nodes=Tree_Query::FilterQueryNodes($query_tree);
    my @query_nodes;
    my %orig2query;
    if ($opts->{no_plan}) {
      $roots = ($type eq 'q-query.type') ? [ $query_tree->children ] : [$query_tree];
      @query_nodes=@orig_nodes;
      %orig2query = map { $_ => $_ } @orig_nodes;
    } elsif ($self->{parent_query}) {
      $roots = Tree_Query::BtredPlanner::plan(
	\@orig_nodes,
	$query_tree->parent,
	$query_tree
       );
      %orig2query = map { $_ => $_ } @orig_nodes;
      @query_nodes=Tree_Query::FilterQueryNodes($query_tree); # reordered
    } else {
      if ($clone_before_plan) {
	$query_tree=FSFormat->clone_subtree($query_tree);
      }
      @query_nodes=Tree_Query::FilterQueryNodes($query_tree); # same order as @orig_nodes
      %orig2query = map { $orig_nodes[$_] => $query_nodes[$_] } 0..$#orig_nodes;
      Tree_Query::BtredPlanner::name_all_query_nodes($query_tree); # need for planning
      $roots = Tree_Query::BtredPlanner::plan(\@query_nodes,$query_tree);
      @query_nodes=Tree_Query::FilterQueryNodes($query_tree); # reordered
    }
    my $query_node;
    if (@$roots==0) {
      die "No query node!\n";
    } elsif (@$roots>1) {
      die "The query is not connected: the graph has more than one root node (@$roots)!\n";
    } else {
      ($query_node)=@$roots;
    }
    # @{$self->{query_nodes}}=@orig_nodes;
    %name2pos = map {
      my $name = lc($query_nodes[$_]->{name});
      (defined($name) and length($name)) ? ($name=>$_) : ()
    } 0..$#query_nodes;
    {
      my %node2pos = map { $query_nodes[$_] => $_ } 0..$#query_nodes;
      $self->{query_node2pos} = { map { $_=>$node2pos{$orig2query{$_}} } @orig_nodes };
      $self->{parent_pos} = [ map { $node2pos{ $_->parent  } } @query_nodes ];
    }

    {
      my @all_query_nodes = grep {$_->{'#name'} =~ /^(node|subquery)$/ } ($query_node->root->descendants);
      {
	my %node2match_pos = map { $all_query_nodes[$_] => $_ } 0..$#all_query_nodes;
	$self->{pos2match_pos} = [
	  map { $node2match_pos{ $query_nodes[$_] } } 0..$#query_nodes
	];
      }
      # we only allow refferrences to nodes in this query or some super-query
      $self->{name2match_pos} = {
	($self->{parent_query} ? (%{$self->{parent_query}{name2match_pos}}) : ()),
	map { $_ => $self->{pos2match_pos}[$name2pos{$_}] } keys %name2pos
      };

      my %node_types = map { $_=> 1 } @{$self->{type_mapper}->get_node_types};
      my $default_type = $query_node->root->{'node-type'};
      if ($default_type and !$node_types{$default_type}) {
	die "The query specifies an invalid type '$default_type' as default node type!";
      }
      for my $node (@all_query_nodes) {
	if ($node->{'node-type'}) {
	  if (!$node_types{$node->{'node-type'}}) {
	    die "The query specifies an invalid type '$node->{'node-type'}' for node: ".Tree_Query::Common::as_text($node)."\n";
	  }
	} else {
	  my $parent = $node->parent;
	  my @types =
	    $parent ?
	      (Tree_Query::Common::GetRelativeQueryNodeType(
		$parent->{'node-type'},
		$self->{type_mapper},
		SeqV($node->{relation}))
	       ) : @{$self->{type_mapper}->get_node_types};
	  if (@types == 1) {
	    $node->{'node-type'} = $types[0];
	  } elsif ($default_type) {
	    $node->{'node-type'} = $default_type;
	  } else {
	    die "Could not determine node type of node "
	      .Tree_Query::Common::as_text($node)."\n"
		."Possible types are: ".join(',',@types)." !\n";
	  }
	}
      }
      %name2type = map {
	my $name = lc($_->{name});
	(defined($name) and length($name)) ? ($name=>$_->{'node-type'}) : ()
      } @all_query_nodes;
    }

    # compile condition testing functions and create iterators
    my (@r1,@r2,@r3);
    for my $i (0..$#query_nodes) {
      my $qn = $query_nodes[$i];
      my $sub = $self->serialize_conditions($qn,{
	query_pos => $i,
	recompute_condition => \@r1, # appended in recursion
	recompute_subquery => \@r2,  # appended in recursion
	reverted_relations => \@r3,  # appended in recursion
      });
      my $conditions = eval $sub; die $@ if $@; # use the above-mentioned lexical context
      push @debug, $sub;
      push @conditions, $conditions;
      my $iterator;
      if (!$self->{parent_query} and $qn==$query_node) {
	# top-level node iterates throguh all nodes
	if ($opts->{iterator}) {
	  $iterator = $opts->{iterator};
	} elsif ($opts->{tree}) {
	  $iterator = TreeIterator->new($conditions,$opts->{tree});
 	} elsif ($opts->{current_filelist}) {
 	  $iterator = CurrentFilelistIterator->new($conditions);
	} else {
	  $iterator = CurrentFileIterator->new($conditions);
	}
      } else {
	$iterator = $self->create_iterator($qn,$conditions);
      }
      push @iterators, $iterator;
    }
    unless ($self->{parent_query}) {
      my $first = first { $_->{'#name'} eq 'node' and $_->{name} } $query_tree->children;
      my $output_opts = {
	id => $first->{name},
      };
      my ($init_code,$first_filter) = $self->serialize_filters(
	$query_tree->{'output-filters'},
	$output_opts
      );
      if ($init_code and $first_filter) {
	print "INIT CODE:\n",$init_code,"\n";
	$self->{filter} = eval $init_code; die $@ if $@;
	$self->{first_filter} = $first_filter;
      }
    }
    return $self;
  }

  sub get_results {
    my $self = shift;
    return $self->{results}
  }
  sub get_result_files {
    my $self = shift;
    return $self->{result_files}
  }
  sub run_filters {
    my $self = shift;
    if ($self->{filter}) {
      $self->{filter}->($self);
      return 1;
    } else {
      return 0;
    }
  }
  sub flush_filters {
    my $self = shift;
    if ($self->{first_filter}) {
      return $self->{first_filter}->{finish}($self->{first_filter});
    }
    return;
  }
  sub init_filters {
    my $self = shift;
    if ($self->{first_filter}) {
      return $self->{first_filter}->{init}($self->{first_filter});
    }
    return;
  }

  sub r {
    my ($self,$name)=@_;
    return unless $self->{results};
    my $pos =  $self->{name2pos}{$name};
    return unless defined $pos;
    return wantarray ? ($self->{results}[$pos],$self->{result_files}[$pos]) : $self->{results}[$pos];
  }
  *get_result_node = \&r;

#   sub get_query_nodes {
#     my $self = shift;
#     return $self->{query_nodes};
#   }

  sub reset {
    my ($self)=@_;
    $self->{query_pos} = 0;
    %{$self->{have}}=$self->{parent_query} ? %{$self->{parent_query}{have}} : ();
    $_->reset for @{$self->{iterators}};
  }

  sub create_iterator {
    my ($self,$qn,$conditions)=@_;
    	# TODO: deal with negative relations, etc.
    my ($rel) = TredMacro::SeqV($qn->{relation});
    my $relation = $rel && $rel->name;
    $relation||='child';

    print STDERR "iterator: $relation\n" if $DEBUG>1;
    my $iterator;
    if ($relation eq 'child') {
      $iterator = ChildnodeIterator->new($conditions);
    } elsif ($relation eq 'descendant') {
      my ($min,$max)=
	map { (defined($_) and length($_)) ? $_ : undef }
	map { $rel->value->{$_} }
	qw(min_length max_length);
      if (defined($min) or defined($max)) {
	print STDERR "with bounded depth ($min,$max)\n" if $DEBUG>1;
	$iterator = DescendantIteratorWithBoundedDepth->new($conditions,$min,$max);
      } else {
	$iterator = DescendantIterator->new($conditions);
      }
    } elsif ($relation eq 'parent') {
      $iterator = ParentIterator->new($conditions);
    } elsif ($relation eq 'same-tree-as') {
      $iterator = SameTreeIterator->new($conditions);
    } elsif ($relation eq 'ancestor') {
      my ($min,$max)=
	map { (defined($_) and length($_)) ? $_ : undef }
	map { $rel->value->{$_} }
	qw(min_length max_length);
      if (defined($min) or defined($max)) {
	$iterator = AncestorIteratorWithBoundedDepth->new($conditions,$min,$max);
      } else {
	$iterator = AncestorIterator->new($conditions);
      }
    } elsif ($relation eq 'user-defined') {
      my $label = $rel->value->{label};
      if ($label eq 'a/aux.rf') {
	$iterator = AAuxRFIterator->new($conditions);
      } elsif ($label eq 'a/lex.rf') {
	$iterator = ALexRFIterator->new($conditions);
	#$iterator = PMLREFIterator->new($conditions,'a/lex.rf');
      } elsif ($label eq 'a/lex.rf|a/aux.rf') {
	$iterator = ALexOrAuxRFIterator->new($conditions);
      } elsif ($label eq 'coref_text.rf') {
	$iterator = CorefTextRFIterator->new($conditions);
      } elsif ($label eq 'coref_gram.rf') {
	$iterator = CorefGramRFIterator->new($conditions);
      } elsif ($label eq 'compl.rf') {
	$iterator = ComplRFIterator->new($conditions);
      } elsif ($label eq 'echild') {
	$iterator = EChildIterator->new($conditions);
      } elsif ($label eq 'eparent') {
	$iterator = EParentIterator->new($conditions);
      } elsif (first { $_ eq $label } @{$self->{type_mapper}->get_pmlrf_relations}) {
	$iterator = PMLREFIterator->new($conditions,$label);
      } else {
	die "user-defined relation '".$label."' unknown or not implemented in BTrEd Search\n"
      }
    } else {
      die "relation ".$relation." not yet implemented\n"
    }
    if ($qn->{optional}) {
      return OptionalIterator->new($iterator);
    } else {
      return $iterator;
    }
  }

  sub serialize_filters {
    my ($self,$filters,$opts)=@_;
    return unless ref $filters;
    $opts ||= {};
    # first filter is special, it can refer to nodes
    my $prev;
    my @filters;
    {
      my $i;
      for my $f (@$filters) {
	$opts->{filter_id} = "filter_".($i++);
	push @filters, $self->serialize_filter($f,$opts);
      }
    }
    for my $filter (@filters) {
      if ($prev) {
	$prev->{output}=$filter;
      }
      $prev = $filter;
    }
    if ($prev) {
      $prev->{output}={
	init => sub {
	  print("-" x 60, "\n");
	},
	process_row => sub {
	  my ($self,$row)=@_;
	  print(join("\t",@$row)."\n");
	},
	finish => sub {
	  print("-" x 60, "\n");
	}
      };
    }
    return ($opts->{filter_init},$filters[0]);
  }

  my %aggregation_template = (
	  count => q`
            sub {
              my ($self, $i)=@_;
               $self->{aggregated}[$i]++;
            }`,
	  _DEFAULT_ => q`
            sub {
              my ($self, $i, $row)=@_;
              #<IF_LEN _FUNC_VARLIST_>
              my (_FUNC_VARLIST_) = @$row[_FUNC_COLNUMS_];
              #</IF_LEN>
              #<IF_LEN _FUNC_DEFINED_>
              if (_FUNC_DEFINED_) {
              #</IF_LEN>
                _FUNC_OP_
              #<IF_LEN _FUNC_DEFINED_>
              }
              #</IF_LEN>
            }`
	 );
	my %aggregation_init = (
	  sum => '0',
	  max => 'undef',
	  min => 'undef',
	  count => '0',
	  avg => '[undef,0]',
	  concat => '[]',
#	  ratio => '0',
	);
  my %aggregation_op = (
	  sum => q`$self->{aggregated}[$i] += _ARG_;`,
	  max => q`my $max = $self->{aggregated}[$i];
                   my $val = _ARG_;
                   $self->{aggregated}[$i] = $val if !defined($max) or $max<$val`,
	  min => q`my $min = $self->{aggregated}[$i];
                   my $val = _ARG_;
                   $self->{aggregated}[$i] = $val if !defined($min) or $val<$min`,
	  avg => q`my $avg = $self->{aggregated}[$i];
                   $self->{aggregated}[$i][0] += _ARG_;
                   $self->{aggregated}[$i][1] ++;`, # shell we count null values as 0 ?
	  concat => q`push @{$self->{aggregated}[$i]}, _ARG_;`, # shell we count null values as 0 ?
	  #	  ratio => q`$self->{aggregated}[$i] += _ARG_;`, # not sure how to implement
	 );
  my %aggregation_final = (
	  avg => q`_RESULT_ = (_RESULT_->[0] / _RESULT_->[1]);`, # shell we count null values as 0 ?
	  concat => q`_RESULT_ = join(_ARG1_, @{_RESULT_});`, # shell we count null values as 0 ?
	  #	  ratio => q`$self->{aggregated}[$i] += _ARG_;`, # not sure how to implement
	 );

  my %code_template = (
    PLAIN => q`
      	{
      	  init => sub {
	    #<IF_DISTINCT>
      	    my ($self)=@_;
      	    $self->{seen} = {};
            my $out = $self->{output};
	    $out->{init}($out);
	    #</IF_DISTINCT>
      	  },
      	  process_row => sub {
      	    my ($self,$row)= @_;
            #<IF_LEN _RET_VARLIST_>
      	    my (_RET_VARLIST_) = @$row[_RET_COLNUMS_];
            #</IF_LEN>
      	    my $out = $self->{output};
	    #<IF_DISTINCT>
      	    my @out_row = (
	      _RET_COLS_
      	    );
      	    my $key = join "\x0",map { defined $_ ? $_ : '' } @out_row;
      	    unless (exists $self->{seen}{$key}) {
      	      $self->{seen}{$key}=undef;
      	      $out->{process_row}->($out,\@out_row);
      	    }
	    #</IF_DISTINCT>
	    #<IF_NOT_DISTINCT>
	    $out->{process_row}->($out,[
	      _RET_COLS_
	    ]);
	    #</IF_NOT_DISTINCT>
      	  },
          finish => sub {
      	    my ($self)= @_;
      	    my $out = $self->{output};
	    $out->{finish}($out);
          }
      	}`,

    # Example: >> $1, $2, 2+sum($2 over $1)*max($1 over $2)
    AGGREGATE => q`
          {
	    init => sub {
	      my ($self)= @_;
	      _INIT_AGG_
	    },
	    process_row => sub {
	      my ($self,$row)= @_;
	      my $agg=0;
	      $_->($self,$agg++,$row) for @{$self->{aggregation}};
	    },
	    aggregation => [
	      _AGGREGATIONS_
	     ],
	    finish => sub {
	      my ($self)=@_;
              #<IF_LEN _RET_VARLIST_>
	      my (_RET_VARLIST_) = @{$self->{group_columns}}[_RET_COLNUMS_];
              #</IF_LEN>
	      _AGG_FINALIZE_
              #<IF_RETURN>
	       return $self->{result} = [
		 _RET_COLS_
	       ];
              #</IF_RETURN>
              #<IF_NOT_RETURN>
              my $out = $self->{output};
              $out->{process_row}->($out, [
                _RET_COLS_
              ]);
              #</IF_NOT_RETURN>
	     }
	   }`,

    GROUP => q`
        {
	  init => sub {
	    my ($self)= @_;
	    $self->{group} = {};
	  },
	  process_row => sub {
	    my ($self,$row)= @_;
            #<IF_LEN _GROUP_VARLIST_>
	    my (_GROUP_VARLIST_) = @$row[_GROUP_COLNUMS_];
            #</IF_LEN>
	    my $g = [
	      _GROUP_COLS_
	    ];
	    my $key = join "\x0",map { defined $_ ? $_ : '' } @$g;
	    my $new;
	    my $group = $self->{group}{$key} ||= ($new = {
	      key => $key,
              group_columns => $g,
	      %{$self->{grouping}}
	     });
	    $group->{init}->($group) if $new;
	    $group->{process_row}->($group,$row);
	  },
	  grouping => _TEMPLATE_AGGREGATE_,
	  finish => sub  {
            my ($self)=@_;
            #<IF_NOT_RETURN>
	    my $out = $_[0]->{output};
            $out->{init}($out);
            #</IF_NOT_RETURN>
	    #<IF_DISTINCT>
            my %seen;
	    #</IF_DISTINCT>
	    for my $group (values %{$self->{group}}) {
	      my $r = $group->{finish}->($group);
	      #<IF_DISTINCT>
              my $key = $group->{key};
              next if exists $seen{$key};
              $seen{$key}=$r;
 	      #</IF_DISTINCT>
	      %$group=();	# immediatelly cleanup group data
              #<IF_NOT_RETURN>
	      $out->{process_row}($out,$r);
              #</IF_NOT_RETURN>
	    }
            #<IF_NOT_RETURN>
	    $out->{finish}($out);
            #</IF_NOT_RETURN>
            #<IF_RETURN>
            return \%seen;
            #</IF_RETURN>
	  }
	 };
        `,
    # Example: >> $1, $2, 2+sum($2 over $1)*max($1 over $2)
  INNER_AGGREGATE =>  q`
     {
       init => sub {
         my ($self)= @_;
         $self->{init}->($_) for @{$self->{local_group}};
         $self->{saved_rows} = [];
       },
       local_group => \@local_filters,
       process_row => sub {
         my ($self,$row)= @_;
         push @{$self->{saved_rows}}, $row; # note: we could only save the columns we need
	 $_->{process_row}->($_, $row) for @{$self->{local_group}};
       },
       finish => sub {
         my ($self)=@_;
         my $saved_rows = $self->{saved_rows};
         my $row;
         my $out = $self->{output};
         my @l = map $_->{finish}->($_), @{$self->{local_group}};
         while ($row = shift @$saved_rows) {
	   #<IF_LEN _RET_VARLIST_>
	   my (_RET_VARLIST_) = @$row[_RET_COLNUMS_];
	   #</IF_LEN>
           my @keys = (_LOCAL_GROUP_KEYS_); # group-expression of n-th filter
           my (_LOCAL_VARLIST_) =
	     map $l[$_]{ $keys[$_] }[0],
	       0..$#l;
           $out->{process_row}->($out, [
	     _RET_COLS_
	   ]);
         }
       }
     }`,
  );

  # do several substitutions at once
  sub _code_substitute {
    my (undef, $map)=@_;
    my $what = join('|',reverse sort keys %$map);
    $_[0] =~ s{[ \t]*#<IF_LEN ($what)>\s*?\n(.*?)[ \t]*#</IF_LEN>\s*?\n}{
      defined($map->{$1}) and length($map->{$1}) ? $2 : ''
    }seg;
    $_[0] =~ s{($what)}{ $map->{$1} }eg;
  }
  
  sub _code_template_substitute {
    my (undef, $map)=@_;
    $_[0] =~ s{\b_TEMPLATE_([A-Z_]+)_\b}{
      _code_from_template($1, $map->{_MAP_})
    }eg;
    if ($map) {
      my $what = join('|',reverse sort keys %$map);
      $_[0] =~ s{[ \t]*#<IF_((NOT_)?($what))>\s*?\n(.*?)[ \t]*#</IF_\1>\s*?\n}{
	(($2 ? !$map->{$3} : $map->{$3}) ? $4 : '')
      }seg;
    }
  }

  sub _code_from_template {
    my ($name,$map)=@_;
    $map ||= {};
    my $code = $code_template{$name};
    _code_template_substitute($code, $map);
    return $code;
  }

  sub serialize_filter {
    my ($self, $filter, $opts)=@_;

    # $filter->{group_by}
    # $filter->{distinct}
    # $filter->{return}
    # $filter->{sort_by}

    use Data::Dumper;
    print Dumper($filter);

    my $is_first_filter = defined($opts->{column_count}) ? 0 : 1;
    my $distinct = $filter->{distinct} || 0;


    my @group_by = @{ $filter->{'group-by'} || [] };
    my @return = @{ $filter->{return} || [] };
    my @sort_by = @{ $filter->{'sort-by'} || [] };

    print "----------------\n";
    print "Serializing: g: @group_by, r: @return, s: @sort_by\n";

    my $i;

    my @foreach;
    my $foreach_idx = 0;
    my @input_columns;

    my @aggregations;
    my %return_aggregations;
    my %return_columns;
    my @return_vars;
    $i = 0;
    my @return_exp = map $self->serialize_column($_, {
	%$opts,
	var_prefix => 'v',
	foreach => ($is_first_filter && !@group_by ? \@foreach : undef),
	input_columns => ($is_first_filter && !@group_by ? \@input_columns : undef),
	columns_used => \%return_columns,
	vars_used => ($return_vars[$i++]={}),
        local_aggregations => \%return_aggregations,
	aggregations => \@aggregations,
        column_count => @group_by || $opts->{column_count},
	is_first_filter => $is_first_filter,
      }), @return;

    if (keys(%return_aggregations) and @aggregations) {
      # here we should split to two filters:
      # filter one is
      #
      # >> [ for <group> ] 
      #    [ give ] <cols_used>, <aggregations>
      # >> <return_cols_remapping_cols_to_cols_used_and_aggregations_to_extra_cols>
      #    [ sort by <sort_cols> ]
      #

      # for the first filter, we
      # must use cols_used and we must obtain unparsed aggregations
      # by preserving them from within local aggregations;
      # the first parse of local aggregations content_decl
      # can be used for this.
      # we rerun serialize_filter using only group_by
      # and the faked return

      # then we clear group_by, preserve sort_by, andg
      # we make sure that
      # _RET_VARLIST_ is serialized as the usual _RET_VARLIST
      # appended by aggregation variables
      # and _RET_COLNUMS_ is serialized as 0..$#cols_used + $#aggregations


      # and continue
      # and gener

      die "TODO";
    }

    my %group_columns;
    my @group_vars;
    my @group_by_exp = map { $self->serialize_column($_, {
	%$opts,
	var_prefix => 'g',
	columns_used => \%group_columns,
	foreach => ($is_first_filter ? \@foreach : undef),
	input_columns => ($is_first_filter ? \@input_columns : undef),
	vars_used => ($group_vars[$i++]={}),
        local_aggregations => undef, # not applicable
	aggregations => undef, # not applicable
        column_count => $opts->{column_count},
	is_first_filter => $is_first_filter,
      }) } @group_by;


    my %sort_aggregations;
    my %sort_columns;
    my @sort_vars;
    my @sort_by_exp = map $self->serialize_column($_, {
	%$opts,
	var_prefix => 'v',
	foreach => ($is_first_filter && !@group_by ? \@foreach : undef),
	input_columns => ($is_first_filter  && !@group_by ? \@input_columns : undef),
	vars_used => ($sort_vars[$i++]={}),
        local_aggregations => \%sort_aggregations,
	columns_used => \%sort_columns,
	aggregations => undef, # not applicable
        column_count => scalar @return,
	is_first_filter => $is_first_filter,
      }), @sort_by;

    my @local_filters;
    my @local_group_keys;
    for my $agg (values %return_aggregations) {
      my ($num, $name, $args, $over, $sort_by, $over_exp, $vars_used) = @$agg;
      # below we pass in something
      # that looks like a filter
      # but contains parse-trees instead of strings
      print "SUBFILTER\n";
      use Data::Dumper;
      print Dumper({
	name => $name,
	args => $args,
	over => $over,
	over_exp => $over_exp,
	vars_used => $vars_used,
       });
      $local_filters[$num] =
	$self->serialize_filter(
	  {
	    'group-by' => Fslib::List->new(
	      @$over
	     ),
	    'return' => Fslib::List->new(
	      [
	       'ANALYTIC_FUNC',
	       $name,
	       $args,
	       undef,
	       $sort_by,
	      ]
	    ),
	  },
	  {
	    is_first_filter => $is_first_filter,
	    aggregations => \@aggregations,
	    column_count => @group_by || $opts->{column_count},
	    code_map_flags => {
	      RETURN => 1,
	      DISTINCT => 1,
	    }
	  }
	);

      my $i = 0;
      my $exp = join(",\n            ",
		map {
		  my @vars_used = sort keys %{$vars_used->[$i]};
		  if (@vars_used) {
		    '(('.join(' && ',map qq{defined($_)}, @vars_used).') ? ('.$_.') : undef)'
		  } else {
		    $_
		  }
		} @$over_exp);
      $local_group_keys[$num] =
	@$over_exp > 1 ? 'join("\x0",'.$exp.')' : $exp;
    }

    my @aggregations_exp;
    my @aggregations_columns;
    my @aggregations_vars;
    if (@aggregations) {
      my $i=-1;
      @aggregations_exp = map {
	my $j = 0;
	$i++;
	[$_->[0], #name
	 [ # columns
	   map $self->serialize_column($_, {
	     %$opts,
	     var_prefix => 'v',
	     columns_used => ($aggregations_columns[$i]||={}),
	     foreach => ($is_first_filter ? \@foreach : undef),
	     input_columns => ($is_first_filter ? \@input_columns : undef),
	     vars_used => ($aggregations_vars[$i][$j++]={}),
	     local_aggregations => undef,
	     aggregations => $opts->{aggregations}, # not applicable
	     column_count => $opts->{column_count},
	     is_first_filter => $is_first_filter,
	   }), @{$_->[1]}
	 ]
	]
      } @aggregations;
    }

    $opts->{column_count} = scalar @return;
    print "COLUMN_COUNT FOR NEXT FILTER: $opts->{column_count}\n";


    # DEBUG:

    use Data::Dumper;
    print Dumper({
      filter => $filter,
      foreach => \@foreach,
      aggregations => \@aggregations_exp,
      return_exp => \@return_exp,
      return_agg => \%return_aggregations,
      group_by_exp => \@group_by_exp,
      input_agg => \%input_aggregations,
      sort_by_exp => \@sort_by_exp,
      sort_agg => \%sort_aggregations,
    });


    my @columns_used = uniq(sort keys(%return_columns));
    my @g_columns_used = uniq(sort keys(%group_columns));
    my $varlist = join(',', map '$v'.$_, @columns_used);
    my $colnums = join(',', map $_-1, @columns_used);
    my $g_varlist = join(',', map '$g'.$_, @g_columns_used);
    my $g_colnums = join(',', map $_-1, @g_columns_used);
    my $agg_varlist = join(',', map '$a'.$_, 0..$#aggregations);
    my $local_varlist = join(',',map '$l'.$_,0..$#local_filters);

    my ($agg_final,$all_agg_code,$init_agg);
    {
      my $i = 0;
      $init_agg =
	join(";         \n",
	map {
	'$self->{aggregated}['.($i++).'] = '.$aggregation_init{ $_->[0] }."; # init $_->[0](...)"
      } @aggregations_exp);

      my (@agg_code, @agg_final);
      $i = 0;
      for my $aggr (@aggregations_exp) {
	my $funcname = $aggr->[0];
	my $args = $aggr->[1];
	my $agg_code = $aggregation_template{$funcname} || $aggregation_template{_DEFAULT_} ;
	my @columns_used = uniq(sort keys(%{$aggregations_columns[$i]}));
	my $varlist = join(',', map '$v'.$_, @columns_used);
	my $colnums = join(',', map $_-1, @columns_used);
	my $defined = join(' && ', map 'defined($v'.$_.')', @columns_used);
	my $op = $aggregation_op{$funcname};
	if (defined $op) {
	  _code_substitute($op,
			   {
			     _ARG_ => $args->[0],
			   }
			  )
	}

	# no substitutions on $agg_code past this point:
	_code_substitute($agg_code,
			 {
			   _FUNC_VARLIST_ => $varlist,
			   _FUNC_COLNUMS_ => $colnums,
			   _FUNC_DEFINED_ => $defined,
			   _FUNC_OP_ => $op,
			 });

	push @agg_code, "          # $funcname(...)\n".$agg_code;

	push @agg_final, 'my $a'.$i.' = $self->{aggregated}['.$i.'];';
	if (exists $aggregation_final{$funcname}) {
	  my $final_code = $aggregation_final{$funcname};
	  my $arg1;
	  if ($funcname eq 'concat') {
	    $arg1 = $aggr->[1][1];
	    $arg1 = defined($arg1) && length($arg1) ? $arg1 : q('');
	  }
	  _code_substitute($final_code,
			   {
			     _RESULT_ => '$a'.$i,
			     _ARG1_ => $arg1,
			   });
	  push @agg_final,$final_code;
	}
	$i++;
      }

      $agg_final = join('',map { "\n              ".$_ } @agg_final);
      $all_agg_code = join(',',@agg_code);
    }

    my $cols = do {
      my $i = 0;
      join (",\n	      ",
	    map {
	      my @vars_used = sort keys %{$return_vars[$i++]};
	      if (@vars_used) {
		'(('.join(' && ',map qq{defined($_)}, @vars_used).') ? ('.$_.') : undef)'
	      } else {
		$_
	      }
	    } @return_exp)
    };
    my $group_cols = do {
      my $i = 0;
      join (",\n	      ",
	    map {
	      my @vars_used = sort keys %{$group_vars[$i++]};
	      if (@vars_used) {
		'(('.join(' && ',map qq{defined($_)}, @vars_used).') ? ('.$_.') : undef)'
	      } else {
		$_
	      }
	    } @group_by_exp)
    };
    my $local_group_keys = join (",\n	      ",@local_group_keys);

    my $output_filter;
    # first without any inner aggregations
    my $code;
    if (@group_by) {
      # use group_by template
      if (!keys(%input_aggregations) and !@local_filters) {
	$code = _code_from_template('GROUP', {
	  _MAP_ => { RETURN => 1 },
	  RETURN => 0,
	  DISTINCT => $distinct,
	  %{$opts->{code_map_flags}||{}},
	});
      } else {
	die "TODO";
      }
    } elsif (@aggregations and !keys(%return_aggregations)) {
      # use the direct template
      $code = _code_from_template('AGGREGATE', {
	RETURN => 0,
	DISTINCT => $distinct,
	%{$opts->{code_map_flags}||{}},
      });
    } elsif (!@aggregations and keys(%return_aggregations)) {
      $code = _code_from_template('INNER_AGGREGATE', {
	RETURN => 0,
	DISTINCT => $distinct,
	%{$opts->{code_map_flags}||{}},
      });
    } elsif (@aggregations and keys(%return_aggregations)) {
      die "TODO";
    } else {
      # no aggregations at all
      $code = _code_from_template('PLAIN', {
	RETURN => 0,
	DISTINCT => $distinct,
	%{$opts->{code_map_flags}||{}},
      });
    }

    # below, user-data may be involved, we must do a one step-substitution
    _code_substitute($code,
		     {
		       _AGGREGATIONS_ => $all_agg_code,
		       _RET_VARLIST_ => $varlist,
		       _GROUP_VARLIST_ => $g_varlist,
		       _RET_COLNUMS_ => $colnums,
		       _GROUP_COLNUMS_ => $g_colnums,
		       _AGG_FINALIZE_ => $agg_final,
		       _INIT_AGG_ => $init_agg,
		       _GROUP_COLS_ => $group_cols,
		       _RET_COLS_ => $cols,
		       _LOCAL_VARLIST_ => $local_varlist,
		       _LOCAL_GROUP_KEYS_ => $local_group_keys,
		     }
		    );

    $code = "#line 1 ".$opts->{filter_id}."\n".$code;
    {
      my $i = 0;
      printf("%3s\t%s",$i++,$_."\n") for split /\n/,$code;
    }
    $output_filter = eval $code; die $@ if $@;



    # filter_init:
    if ($is_first_filter) {
      my $code = q`
         $first_filter->{process_row}->($first_filter, [
           _RET_COLS_
         ]);
      `;
      # no substitutions past this point!
      _code_substitute($code,
		       {
			 _RET_COLS_ => join (",\n           ",@input_columns),
		       }
		      );
      # now we simulate a left join
      {
	my @wrap_l=[0,'sub {'];
	my @wrap_r=([0,'}']);
	my $i=0;
	my $indent=0;
	foreach my $f (@foreach) { # he?
	  if ($f->[0]==1) {
	    push @wrap_l,
	      [$indent,
	       ($i && $f=~'$var'.($i-1)) ? qq`my \@var$i = defined(\$var`.($i-1).qq`) ? $f->[1] : ();` :
		 qq`my \@var$i = $f->[1];`
		],
	      [$indent,qq`foreach my \$var$i (\@var$i ? \@var$i : (undef)) {`];
	    unshift @wrap_r, [$i,qq`}`];
	    $indent++;
	  } elsif ($f->[0]==0) {
	    push @wrap_l, [$indent,
			   ($i && $f=~'$var'.($i-1)) ? qq`my \$var$i = defined(\$var`.($i-1).qq`) ? $f->[1] : undef;` :
			     qq`my \$var$i = $f->[1];`];
	  }
	  $i ++;
	}
	$code = join('',
		     map { ('  ' x ($_->[0]+10)).$_->[1]."\n" }
		       (
			 @wrap_l,
			 [$indent,$code],
			 @wrap_r,
			)
		      );
      }
      print "$code\n";
      $opts->{filter_init} = $code;
    }

    return $output_filter;

    # >> $1, 1 + sum(2*$1+$3), max($2+1)
    # or
    # >> $1, count()
    # ERROR (mixing aggregation and non-aggregation)
    #
    #
    # TODO:
    # 1) combination of for ... give and ...(... over ...)
    #
    #  >> for $a.functor, $b.functor give
    #    $1,$2, ratio(count() over $1)
    #
    # is same as
    #
    #  >> for $a.functor, $b.functor give
    #    $1 & $2, count() div sum(count() over $1)
    #
    # and can be rewritten as:
    #
    #   >> for $a.functor, $b.functor give
    #     $1,$2,count()
    #   >> $1 & $2, ($3 div sum($3 over $1))
    #
    # 2) sort
    # 3) distinct
    #
    # >> $1, $2, 2+sum($2 over $1)*max($1 over $2)
    # INNER_AGGREGATE:
    # $output_filter = {
    #    init => sub {
    #      my ($self)= @_;
    #      $_->{init}->($_) for @{$self->{local_group}};
    #      $_->{saved_rows} = [];
    #    },
    #    local_group => [
    #        {
    #          compile_filter('for $1 give sum($2)'),
    #          finish => {
    #            my %r;
    #            for my $key (keys %{$self->{group}}) {
    #              my $group = $self->{group}{$key};
    #              $r{$key} = $group->finish;
    #              %$group=(); # immediatelly cleanup group data
    #            }
    #            return \%r;
    #          }
    #        }
    #        {
    #          compile_filter('for $2 give max($1)'),
    #          finish => # see above
    #        }
    #    ],
    #    saved_rows => [],
    #    process_row => sub {
    #      my ($self,$row)= @_;
    #      my ($v1,$v2) = @$row[0,1];
    #      push @{$self->{saved_rows}}, [$v1,$v2]; # only save the columns we need
    #
    #      my $group1 = $self->{local_group}[0]; # for $1 give sum($2)
    #      $group1->process_row($group1, $row);
    #      
    #      my $group2 = $self->{local_group}[1]; # for $2 give max($1)
    #      $group2->process_row($group2, $row);
    #    }
    #    finish => sub {
    #      my ($self)=@_;
    #      my $saved_rows = $self->{saved_rows};
    #      my $saved;
    #      my $output = $self->{output};
    #      my @local_group_results = map $_->finish, @{$self->{local_group}};
    #      while ($saved = shift @$saved_rows) {
    #        my ($v1,$v2) = @$saved; # recover saved columns
    #        my $key1 = $1;
    #        my $key2 = $2;
    #        my $l1 = $local_group_results[0]{ $key1 };
    #        my $l2 = $local_group_results[1]{ $key2 };
    #        $output->process_row($output, [ $v1, $v2, 
    #                                        defined($l1) && defined($l2) ? 2+$l1*$l2 : undef ]);
    #      }
    #    }
    #
    # GROUP:
    # >> for $1,substr($2,3,1),$3 give $1, sum(2*$1+$3), max($2+1)
    #
    # $output_filter = {
    #    init => sub {
    #      my ($self)= @_;
    #      $self->{group} = {};
    #    }
    #    process_row => sub {
    #      my ($self,$row)= @_;
    #      my ($v1,$v2,$v3) = @$row[0,1,2];
    #      my $g = [$v1, defined($v2) ? substr($v2,3,1) : undef, $v3];
    #      my $key = join "\x0",map { defined $_ ? $_ : '' } @$g;
    #      my $new;
    #      my $group = $self->{group}{$key} ||= ($new = {
    #        key => $g,
    #        %{$self->{grouping}}
    #      });
    #      $group->init() if $new;
    #      $group->process_row($group,$row);
    #    },
    #    grouping => {
    #      init => sub {
    #        my ($self)= @_;
    #        $self->{aggregated}[0] = 0;     # init sum()
    #        $self->{aggregated}[1] = undef; # init max()
    #      },
    #      process_row => sub {
    #        my ($self,$row)= @_;
    #        my $agg=0;
    #        $_->($self,$agg++,$row) for @{$self->{aggregation}};
    #      },
    #      aggregation => [
    #      # 0: sum(2*$1+$3)
    #        sub {
    #          my ($self, $i, $row)=@_;
    #          my ($v1,$v3) = @$row[0,2];
    #          $self->{aggregated}[$i] += ((2*$v1)+$v3) if defined($v1) and defined($v3);
    #          return;
    #        },
    #      # 1: max($2+1)
    #        sub {
    #          my ($self, $i, $row)=@_;
    #          my ($v2) = @$row[1];
    #          my $max = $self->{aggregated}[$i];
    #          $self->{aggregated}[$i] = $v2 if !defined($max) or (defined($v2) and $max<$v2+1);
    #          return;
    #        },
    #      ]
    #      finish => {
    #        my ($self)=@_;
    #        my ($v1) = @{$self->{key}}[0];
    #        my ($a1,$a2) = @{$self->{aggregated}};
    #        return $self->{result} = [
    #          $v1,
    #          defined($a1) ? 1 + $a1 : undef,
    #          defined($a2) ? $a2 : undef,
    #        ];
    #      }
    #    }
    #    finish => {
    #      my $out = $_[0]->{output};
    #      for my $group (values %{$self->{group}}) {
    #        my $r = $group->finish;
    #        %$group=(); # immediatelly cleanup group data
    #        $out->{process_row}($out,$r);
    #      }
    #    }
    # }
    #
    # AGGREGATE:
    # >> 1 + sum(2*$1+$3), max($2+1)
    #
    # this is basically equivalent to
    # >> for 1 return 1 + sum(2*$1+$3), max($2+1)
    # but optimized
    #
    # PLAIN:
    #  >> distinct 1 + $3, $2
    #
    #
  }

  sub serialize_column {
    my ($self,$column,$opts)=@_;
    $opts||={};
    my $pt;
    if (ref($column)) {
      $pt = $column;
    } else {
      # column is a PT:
      $pt = Tree_Query::parse_column_expression($column); # $pt stands for parse tree
      die "Invalid column expression '$column'" unless defined $pt;
    }
    return $self->serialize_expression_pt($pt,{
      %$opts,
      output_filter => 1,
      expression=>$column,
    });
  }

  sub serialize_conditions {
    my ($self,$qnode,$opts)=@_;
    my $conditions = $self->serialize_element({
      %$opts,
      type => $qnode->{'node-type'},
      name => 'and',
      condition => $qnode,
    });

    my $pos = $opts->{query_pos};
    my $match_pos = $self->{pos2match_pos}[$pos];
    my $optional;
    if ($qnode->{optional}) {
      my $parent_pos = $self->{parent_pos}[$pos];
      if (!defined $parent_pos) {
	die "Optional node cannot at the same time be the head of a subquery!";
      }
      $optional = '$matched_nodes->['.$self->{pos2match_pos}[$parent_pos].']';
    }
    if ($conditions=~/\S/) {
      if (defined $optional) {
	$conditions='('.$optional.'==$node or '.$conditions.')';
      }
    } else {
      $conditions=undef
    }

    print STDERR "CONDITIONS[$pos/$match_pos]: $conditions\n" if $DEBUG;
    my $check_preceding = '';

    my $recompute_cond = $opts->{recompute_condition}[$match_pos];
    if (defined $recompute_cond) {
      $check_preceding = join('', map {"\n   and ".
	   '$conditions['.$_.']->($matched_nodes->['.$self->{pos2match_pos}[$_].'],'
	   .'$iterators['.$_.']->file,'
	   .'1) '
     } sort { $a<=>$b } keys %$recompute_cond);
    }
    if (length $check_preceding) {
      $check_preceding = "\n".
	'  and ($backref or '.
	  '($matched_nodes->['.$match_pos.']=$node) # a trick: make it appear as if this node already matched!'."\n".
	    $check_preceding.
	')';
    }
    my $nodetest = '$node and ($backref or '
      .(defined($optional) ? $optional.'==$node or ' : '')
      .'!exists($have{$node}))';
    my $type_name = quotemeta($qnode->{'node-type'});
    my $sub = qq(#line 0 "query-node/${match_pos}"\n)
      . 'sub { my ($node,$fsfile,$backref)=@_; '."\n  "
       .$nodetest
       .(defined($type_name) && length($type_name) ? "\n and ".q[$node->type->get_decl_path =~ m{^\!].$type_name.q[(?:\.type)$}] : ())
       .(defined($conditions) ? "\n  and ".$conditions : '')
       . $check_preceding
       ."\n}";
    print STDERR "SUB: $sub\n" if $DEBUG;
    return $sub;
  }

  sub serialize_element {
    my ($self,$opts)=@_;
    my ($name,$node)=map {$opts->{$_}} qw(name condition);
    my $pos = $opts->{query_pos};
    my $match_pos = $self->{pos2match_pos}[$pos];
    if ($name eq 'test') {
      my %depends_on;
      my $foreach = [];
      my $left = $self->serialize_expression({%$opts,
					      foreach => $foreach,
					      depends_on => \%depends_on,
					      expression=>$node->{a}
					     }); # FIXME: quoting
      my $right = $self->serialize_expression({%$opts,
					       foreach => $foreach,
					       depends_on => \%depends_on,
					       expression=>$node->{b}
					      }); # FIXME: quoting
      my $operator = $node->{operator};
      my $negate = $operator=~s/^!// ? 1 : 0;
      if ($operator eq '=') {
	if ($right=~/^(?:\d*\.)?\d+$/ or $left=~/^(?:\d*\.)?\d+$/) {
	  $operator = '==' unless $negate;
	} else {
	  $operator = $negate ? 'ne' : 'eq';
	}
      } elsif ($operator eq '~') {
	$operator = $negate ? '!~' : '=~';
      }
      my $condition;
      if ($operator eq '~*') {
	$condition='do{ my $regexp='.$right.'; '.$left.($negate ? '!~' : '=~').' /$regexp/i}';
      } elsif ($operator eq 'in') {
	# TODO: 'first' is actually pretty slow, we should use a disjunction
	# but splitting may be somewhat non-trivial in such a case
	# - postponing till we know exactly how a tree-query term may look like
	$condition='do{ my $node='.$left.'; '.($negate ? '!' : '').'grep $_ eq '.$left.', '.$right.'}';
	# #$condition=$left.' =~ m{^(?:'.join('|',eval $right).')$}';
	# 	$right=~s/^\s*\(//;
	# 	$right=~s/\)\s*$//;
	# 	my @right = split /,/,$right;
	# 	$condition='do { my $node='.$left.'; ('.join(' or ',map { '$node eq '.$_ } @right).')}';
      } else {
	$condition='('.$left.' '.$operator.' '.$right.')';
      }

      my @wrap_l = ([0,q`do {`],[0,q` my $reslt;`]);
      my @wrap_r = ([0,q` $reslt`],[0,q`}`]);
      my $negate = 0;
      for my $i (0..$#$foreach) {
	if ($foreach->[$i][0]==2) {
	  $negate=!$negate;
	  unshift @wrap_r, [$i,q`$reslt = !$reslt;`];
	} elsif ($foreach->[$i][0]==1) {
	  push @wrap_l, [$i,qq`foreach my \$var$i ($foreach->[$i][1]) {`],
	                [$i,qq` if (defined \$var$i) {`] # although we might probably assume that list values are defined
			  ;
	  unshift @wrap_r, [$i,qq`  last if \$reslt;`],
	                   [$i,qq` }`],
			   [$i,qq`}`];
	} else {
	  push @wrap_l, [$i,qq`my \$var$i = $foreach->[$i][1];`],
	                [$i,qq`if (defined \$var$i) {`];
	  unshift @wrap_r, [$i,qq`}`];
	}
      }
      $condition = join('',
			map { ('  ' x ($_->[0]+10)).$_->[1]."\n" }
			  (
			    @wrap_l,
			    [$#$foreach,qq`  \$reslt = ($condition) ? `.($negate ? '0:1;' : '1:0;')],
			    @wrap_r
			   )
			 );

      my $target_match_pos = TredMacro::max($match_pos,keys %depends_on);
      my $target_pos = TredMacro::Index($self->{pos2match_pos},$target_match_pos);
      if (defined $target_pos) {
	# target node in the same sub-query
	if ($target_pos<=$pos) {
	  return $condition;
	} elsif ($target_pos>$pos) {
	  $opts->{recompute_condition}[$target_match_pos]{$pos}=1;
	  return ('( $$query_pos < '.$target_pos.' ? '.int(!$opts->{negative}).' : '.$condition.')');
	}
      } else {
	# this node is referred to from some super-query
	if ($target_match_pos > $self->{parent_query_match_pos}) {
	  # we need to postpone the evaluation of the whole sub-query up-till $matched_nodes->[$target_match_pos] is known
	  $self->{postpone_subquery_till}=$target_match_pos if ($self->{postpone_subquery_till}||0)<$target_match_pos;
	}
	return $condition;
      }
    } elsif ($name =~ /^(?:and|or|not)$/) {
      my $negative = $opts->{negative} ? 1 : 0;
      if ($name eq 'not') {
	$negative=!$negative;
      }
      my @c =grep {defined and length}
	map {
	  $self->serialize_element({
	    %$opts,
	    negative => $negative,
	    name => $_->{'#name'},
	    id => $node->{name},
	    # type => $node->{'node-type'},
	    condition => $_,
	  })
	} grep { $_->{'#name'} ne 'node' } $node->children;
      return () unless @c;
      if ($name eq 'not') {
	return 'not('.join("\n  and ",@c).')';
      } else {
	return '('.join("\n  $name ",@c).')';
      }
    } elsif ($name eq 'subquery') {
      my $subquery = ref($self)->new($node, {
	type_mapper => $self->{type_mapper},
	parent_query => $self,
	parent_query_pos => $pos,
	parent_query_match_pos => $match_pos,
      });
      push @{$self->{sub_queries}}, $subquery;
      my $sq_pos = $#{$self->{sub_queries}};
      my @occ = map {
	(length($_->{min}) || length($_->{max})) ?
	  ((length($_->{min}) ? $_->{min} : undef),
	   (length($_->{max}) ? $_->{max}+1 : undef)) : (1,undef)
      } TredMacro::AltV($node->{occurrences});
      my $occ_list =
	TredMacro::max(map {int($_)} @occ)
	    .','.join(',',(map { defined($_) ? $_ : 'undef' } @occ));
      my $condition = q`(($backref or $matched_nodes->[`.$match_pos.q`]=$node) and `. # trick: the subquery may ask about the current node
	qq/\$sub_queries[$sq_pos]->test_occurrences(\$node,$occ_list))/;
      my $postpone_subquery_till = $subquery->{postpone_subquery_till};
      if (defined $postpone_subquery_till) {
	print "postponing subquery till: $postpone_subquery_till\n" if $DEBUG;
	my $target_pos = TredMacro::Index($self->{pos2match_pos},$postpone_subquery_till);
	if (defined $target_pos) {
	  # same subquery, simply postpone, just like when recomputing conditions
	  # my $postpone_pos = $postpone_subquery_till;
	  $opts->{recompute_condition}[$postpone_subquery_till]{$pos}=1;
	  return ('( $$query_pos < '.$target_pos.' ? '.int(!$opts->{negative}).' : '.$condition.')');
	} else {
	  print "other subquery\n" if $DEBUG;
	  # otherwise postpone this subquery as well
	  $self->{postpone_subquery_till}=$postpone_subquery_till if $postpone_subquery_till>($self->{postpone_subquery_till}||0);
	  return $condition;
	}
      } else {
	return $condition;
      }
    } elsif ($name eq 'ref') {
      my ($rel) = TredMacro::SeqV($node->{relation});
      return unless $rel;
      my $target = lc( $node->{target} );
      my $relation = $rel->name;
      my $expression;
      my $label='';
      if ($relation eq 'user-defined') {
	$label = $rel->value->{label};
	$expression = $test_user_defined_relation{$label};
	if (!defined $expression) {
	  if (first { $_ eq $label } @{$self->{type_mapper}->get_pmlrf_relations}) {
	    return $self->serialize_element(
	      {
		%$opts,
		name => 'test',
		condition => {
		  '#name' => 'test',
		  a => '$'.$target.'.id',
		  b => $label,
		  operator => '=',
		},
	      });
	  } else {
	    die "User-defined relation '$label' not supported test!\n";
	  }
	}
      } else {
	if ($relation eq 'descendant' or $relation eq 'ancestor') {
	  my ($min,$max)=
	    map { (defined($_) and length($_)) ? $_ : undef }
	    map { $rel->value->{$_} }
	    qw(min_length max_length);
	  my ($START,$END)=($relation eq 'ancestor') ? ('$start','$end') : ('$end','$start');
	  $expression = 'do { my $n='.$START.'; '.
	    ((defined($min) or defined($max)) ? 'my $l=0; ' : '').
	      'while ($n and $n!='.$END.(defined($max) ? ' and $l<'.$max : ''). ') { $n=$n->parent; '.
		((defined($min) or defined($max)) ? '$l++;' : '').
	      ' }'.
	      ' ($n and $n!='.$START.' and $n=='.$END.(defined($min) ? ' and '.$min.'<=$l' : '').') ? 1 : 0}';
	} elsif ($relation eq 'order-precedes' or
		   $relation eq 'order-follows') {
	  my ($attr1,$attr2) =
	    # map {
	    #   defined($_) ? (m{/} ? qq(attr(q{$_})) : qq({$_})) : undef
	    # }
	    map {
	      if (defined) {
		my $decl = $self->{type_mapper}->get_decl_for($_);
		if ($decl->get_decl_type == PML_ELEMENT_DECL) {
		  $decl = $decl->get_content_decl;
		}
		my ($m)=$decl->find_members_by_role('#ORDER');
		defined($m) && $m->get_name
	      } else { undef }
	    } ($opts->{type}, $self->{name2type}{$target});
	  die "Could not determine ordering attribute for node '$opts->{id}'\n"
	    unless defined $attr1;
	  die "Could not determine ordering attribute for node '$target'\n"
	    unless defined $attr2;
	  # $expression = qq(\$start->$attr1)
	  #               .($relation eq 'order-precedes' ? ' < ' : ' > ')
	  #               .qq(\$end->$attr2 )
	  return $self->serialize_element(
	    {
	      %$opts,
	      name => 'test',
	      condition => {
	  	'#name' => 'test',
	  	a => $attr1,
	  	b => '$'.$target.'.'.$attr2,
	  	operator => ($relation eq 'order-precedes' ? ' < ' : ' > '),
	      },
	    });
	} else {
	  $expression = $test_relation{$relation};
	}
	die "Relation '$relation' not supported test!\n" unless defined $expression;
      }
      my $target_pos = $self->{name2pos}{$target};
      my $target_match_pos = $self->{name2match_pos}{$target};
      my $condition = q/ do{ my ($start,$end)=($node,$matched_nodes->[/.$target_match_pos.q/]); /.$expression.q/ } /;
      if (defined $target_pos) {
	# target node in the same sub-query
	if ($target_pos<$pos) {
	  return $condition;
	} elsif ($target_pos>$pos) {
	  $opts->{recompute_condition}[$target_match_pos]{$pos}=1;
	  return ('( $$query_pos < '.$target_pos.' ? '.int(!$opts->{negative}).' : '.$condition.')');
	} else {
	  # huh, really?
	  return q/ do{ my ($start,$end)=($node,$node); /.$expression.q/ } /;
	}
      } elsif (defined $target_match_pos) {
	# this node is matched by some super-query
	if ($target_match_pos > $self->{parent_query_match_pos}) {
	  # we need to postpone the evaluation of the whole sub-query up-till $matched_nodes->[$target_pos] is known
	  $self->{postpone_subquery_till}=$target_match_pos if ($self->{postpone_subquery_till}||0)<$target_match_pos;
	}
	return $condition;
      } else {
	die "Node '$target' does not exist or belongs to a sub-query and cannot be referred from relation $relation $label at node no. $match_pos!\n";
      }
    } else {
      die "Unknown element $name ";
    }
  }

  sub serialize_target {
    my ($self,$target,$opts)=@_;
    if ($target eq $opts->{id} and !$opts->{output_filter}) {
      return '$node';
    }
    my $target_match_pos = $self->{name2match_pos}{$target};
    if (defined $target_match_pos) {
      $opts->{depends_on}{$target_match_pos}=1;
      return '$matched_nodes->['.$target_match_pos.']';
    } else {
      my $pos = $opts->{query_pos};
      my $match_pos = $self->{pos2match_pos}[$pos];
      die "Node '$target' does not exist or belongs to a sub-query and cannot be referred from expression $opts->{expression} of node no. $match_pos!\n";
    }
  }

  sub serialize_expression_pt { # pt stands for parse tree
    my ($self,$pt,$opts)=@_;
    my $this_node_id = $opts->{id};
    if (ref($pt)) {
      my $type = shift @$pt;
      if ($type eq 'EVERY') {
	if ($opts->{output_filter}) {
	  die "Cannot use quantifier '*' in output filter: '$opts->{expression}'"
	}
	push @{$opts->{foreach}}, [2];
	return $self->serialize_expression_pt($pt->[0],$opts);
      } elsif ($type eq 'ATTR' or $type eq 'REF_ATTR') {
	my ($node,$node_type);
	if ($opts->{output_filter} and defined($opts->{column_count})) {
	  die "Attribute reference cannot be used in output filter columns whose input is not the body of the query: '$opts->{expression}'"
	}
	if ($type eq 'REF_ATTR') {
	  my $target = lc($pt->[0]);
	  $pt=$pt->[1];
	  die "Error in attribute reference of node $target in expression $opts->{expression} of node '$this_node_id'"
	    unless shift(@$pt) eq 'ATTR'; # not likely
	  $node=$self->serialize_target($target,$opts);
	  $node_type = $self->{name2type}{$target};
	} else {
	  $node='$node';
	  $node_type = $opts->{type};
	}
	# Below we resolve the attribute path according to the PML schema
	# we use $opts->{foreach} array to store information
	# about wrapper loops to be generated; elements of the foreach array are of the form:
	# [type, expression]
	# where type==2 and expressoin is undef for the primitive FORALL quantificator '*'
	#       type==1 if expression produces a list (to be wrapped with a foreach + if defined)
        #       type==0 if expression produces at most one value (to be wrapped with an if defined)
	my $attr=join('/',@$pt);
	$attr=~s{\bcontent\(\)}{#content}g; # translate from PML-TQ notation to PML notation
	my $type_decl = $self->{type_mapper}->get_decl_for($node_type);
	my $ret;
	if (!$type_decl) {
	  die "Cannot resolve attribute path $attr on an unknown node type '$opts->{type}'\n";
	  # where follows a possible fallback:
	  $attr = (($attr=~m{/}) ? $node.qq`->attr(q($attr))` : $node.qq[->{q($attr)}]);
	  $ret = qq{ $attr };
	} else {
	  my $decl = $type_decl;
	  my $foreach = $opts->{foreach} ||= [];
	  my $pexp=$node;
	  for my $step (@$pt) {
	    $step = '#content' if $step eq '[]';
	    my $decl_is = $decl->get_decl_type;
	    if ($decl_is == PML_STRUCTURE_DECL or $decl_is == PML_CONTAINER_DECL) {
	      my $m = $decl->get_member_by_name($step);
	      if (defined $m) {
		$decl=$m->get_content_decl;
	      } else {
		$m = $decl->get_member_by_name($step.'.rf');
		if ($m and ($m->get_role||'') eq '#KNIT') {
		  $decl=$m->get_knit_content_decl;
		} elsif ($m and ($m->get_content_decl->get_role||'') eq '#KNIT') {
		  $decl=$m->get_content_decl;
		} else {
		  die "Error while compiling attribute path $attr for objects of type '$opts->{type}': didn't find member '$step'\n" unless defined($m);
		}
	      }
	      #
	      # value
	      #
	      push @$foreach, [0,$pexp.'->{qq('.quotemeta($step).')}'];
	      $pexp = '$var'.$#$foreach;
	    } elsif ($decl_is == PML_SEQUENCE_DECL) {
	      my $e = $decl->get_element_by_name($step) || die "Error while compiling attribute path $attr for objects of type '$opts->{type}': didn't find element '$step'\n";
	      $decl = $e->get_content_decl;
	      push @$foreach, [1,$pexp.'->values(qq('.quotemeta($step).'))'];
	      $pexp = '$var'.$#$foreach;
	    } elsif ($decl_is == PML_LIST_DECL) {
	      $decl = $decl->get_knit_content_decl;
	      if ($step =~ /^\[(\d+)\]$/) {
		push @$foreach, [0,$pexp.'->[$1]'];
		$pexp = '$var'.$#$foreach;
	      } else {
		push @$foreach, [1,'@{'.$pexp.'}'];
		$pexp = '$var'.$#$foreach;
		redo;
	      }
	    } elsif ($decl_is == PML_ALT_DECL) {
	      $decl = $decl->get_content_decl;
	      push @$foreach, [1,'AltV('.$pexp.')'];
	      $pexp = '$var'.$#$foreach;
	      redo;
	    } else {
	      die "Error while compiling attribute path $attr for objects of type '$opts->{type}': Cannot apply location step '$step' to an atomic type '".$decl->get_decl_path."'!\n";
	    }
	  }
	  $ret = '$var'.$#$foreach;
	}
	if ($opts->{output_filter}) {
	  return $self->serialize_column_node_ref($ret,$opts);
	} else {
	  return $ret;
	}
      } elsif ($type eq 'ANALYTIC_FUNC') {
	my $name = shift @$pt;
	die "The analytic function ${name}() can only be used in an output filter expression!\n"
	  unless $opts->{'output_filter'};
	my ($args,$over,$sort) = @$pt;
	$args||=[];
	if ($name eq 'concat') {
	  die "The analytic function $name takes one or two arguments concat(STR, SEPARATOR?) in the output filter expression $opts->{expression}; got @$args!\n" if @$args==0 or @$args>2;
	  if (@$args==2) {
	    unless (defined($args->[1]) and !ref($args->[1]) and $args->[1]!~/^\$/) {
	      die "The second argument to concat(STR, SEPARATOR?) must be a literal string or number in $opts->{expression}!\n";
	    }
	  }
	} elsif (@$args>1) {
	  die "The analytic function $name takes at most one arguments in the output filter expression $opts->{expression}!\n";
	} elsif (@$args==0) {
	  if ($opts->{column_count} and !$opts->{is_first_filter}) {
	    $args=['$1'];
	  } else {
	    $args=['0'];
	  }
	}
	if ($over and @$over) {
	  if ($opts->{local_aggregations}) {
	    #
	    # we now compile the columns just to
	    # determine a key
	    # so that we can merge two aggregations into one
	    # and to obtain variables used in individual clauses
	    #
	    my @vars;
	    my $i = -1;
	    my @cols = map {
	      $i++;
	      my $j = 0;
	      [map {
		my $ppt = Fslib::CloneValue($_);
		 $self->serialize_expression_pt($ppt,{
		   output_filter => 1,
		   var_prefix => 'v',
		   expression => $opts->{expression},
		   column_count => $opts->{column_count},
		   columns_used => $opts->{columns_used},
		   aggregations => $opts->{aggregations},
		   is_first_filter => $opts->{is_first_filter},
		   vars_used => ($vars[$i][$j++]={}),
		   local_aggregations => undef,
		 })} @{$_||[]}
	      ],
	    } ($args,$over,$sort);
	    my $key = $name.':'.join(';',map join(',',@$_), @cols);
	    my $num;
	    if (exists $opts->{local_aggregations}{ $key }) {
	      $num = $opts->{local_aggregations}{ $key }[0];
	    } else {
	      $num = scalar keys %{$opts->{local_aggregations}};
	      $opts->{local_aggregations}{ $key } = [
		$num,
		$name,
		$args,
		$over,
		$sort,
		$cols[1], # over (local group cols)
		$vars[1], # variables used in over
	       ];
	    }
	    my $var = '$l'.$num;
	    $opts->{vars_used}{$var}=1;
	    return $var;
	  } else {
	    die "Cannot use analytic function $name with an 'over' clause in this context in the output filter expression $opts->{expression}!\n";
	  }
	} else {
	  if (!defined $opts->{aggregations}) {
	    die "Cannot use analytic function $name without an 'over' clause in this context in the output filter expression $opts->{expression}!\n";
	  }
	  my $num = scalar @{ $opts->{aggregations} };
	  push @{ $opts->{aggregations} }, [$name,$args];
	  my $var = '$a'.$num;
	  $opts->{vars_used}{$var}=1;
	  return $var;
	}
      } elsif ($type eq 'FUNC') {
	my $name = $pt->[0];
	my $args = $pt->[1];
	my $id;
	if ($name=~/^(?:descendants|lbrothers|rbrothers|sons|depth|name)$/) {
	  my $node;
	  if ($args and @$args==1 and !ref($args->[0]) and $args->[0]=~s/^\$//) {
	    $node=$self->serialize_target($args->[0],$opts);
	  } elsif ($args and @$args) {
	    die "Wrong arguments for function ${name}() in expression $opts->{expression} of node '$this_node_id'!\nUsage: ${name}(\$node?)\n";
	  } else {
	    $node=$self->serialize_target($this_node_id,$opts);
	  }
	  my $ret = ($name eq 'descendants') ? qq{ scalar(${node}->descendants) }
	       : ($name eq 'lbrothers')   ? q[ do { my $n = ].$node.q[; my $i=0; $i++ while ($n=$n->lbrother); $i } ]
	       : ($name eq 'rbrothers')   ? q[ do { my $n = ].$node.q[; my $i=0; $i++ while ($n=$n->rbrother); $i } ]
	       : ($name eq 'depth_first_order') ? q[ do { my $n = ].$node.q[; my $r=$n->root; my $i=0; $i++ while ($n!=$r and $r=$r->following); $i } ]
	       : ($name eq 'sons')        ? qq{ scalar(${node}->children) }
    	       : ($name eq 'depth')       ? qq{ ${node}->level }
    	       : ($name eq 'name')       ? qq{ ${node}->{'#name'} }
	       : die "Tree_Query internal error while compiling expression: should never get here!";

	  if ($opts->{output_filter}) {
	    die "Cannot use function '$name' at this point of an output filter: '$opts->{expression}'\n"
	      if defined($opts->{column_count});
	    return $self->serialize_column_node_ref($ret,$opts);
	  } else {
	    return $ret;
	  }
	} elsif ($name=~/^(?:lower|upper|length)$/) {
	  if ($args and @$args==1) {
	    my $func = $name eq 'lower' ? 'lc'
	             : $name eq 'upper' ? 'uc'
		     : 'length';
	    return $func.'('
	      .  $self->serialize_expression_pt($args->[0],$opts)
		. ')';
	  } else {
	    die "Wrong arguments for function ${name}() in expression $opts->{expression} of node '$this_node_id'!\nUsage: ${name}(string)\n";
	  }
	} elsif ($name eq 'substr') {
	  if ($args and @$args>1 and @$args<4) {
	    return 'substr('
	      .  join(',', map { $self->serialize_expression_pt($_,$opts) } @$args)
	      . ')';
	  } else {
	    die "Wrong arguments for function substr() in expression $opts->{expression} of node '$this_node_id'!\nUsage: substr(string,from,length?)\n";
	  }

	} elsif ($name eq 'replace') {
	  if ($args and @$args==3) {
	    my @args = map { $self->serialize_expression_pt($_,$opts) } @$args;
	    return 'do{ my ($str,$from,$to) = (' .join(',', @args).'); $str=~s/\Q$from/$to/g; $str }';
	  } else {
	    die "Wrong arguments for function ${name}() in expression $opts->{expression} of node '$this_node_id'!\nUsage: $name(string,target,replacement)\n"
	  }
	} elsif ($name eq 'tr') {
	  if ($args and @$args==3) {
	    my @args = map { $self->serialize_expression_pt($_,$opts) } @$args;
	    return 'do{ my ($str,$from,$to) = (' .join(',', @args).'); $from=~s{/}{\\/}g; $to=~s{/}{\\/}g; eval qq{$str=~tr/$from/$to/}; $str; }';
	  } else {
	    die "Wrong arguments for function ${name}() in expression $opts->{expression} of node '$this_node_id'!\nUsage: $name(string,from_chars,to_chars)\n"
	  }
	} elsif ($name eq 'match') {
	  die "match() NOT YET IMPLEMENTED!\n";
	} elsif ($name eq 'substitute') {
	  die "substitue() NOT YET IMPLEMENTED!\n";
	}
      } elsif ($type eq 'EXP') {
	my $out.='(';
	while (@$pt) {
	  $out.=$self->serialize_expression_pt(shift @$pt,$opts);
	  if (@$pt) {		# op
	    my $op = shift @$pt;
	    if ($op eq 'div') {
	      $op='/'
	    } elsif ($op eq 'mod') {
	      $op='%'
	    } elsif ($op eq '&') {
	      $op=' . '
	    } elsif ($op !~ /[-+*]/) {
	      die "Urecognized operator '$op' in expression $opts->{expression} of node '$this_node_id'\n";
	    }
	    $out.=$op;
	  }
	}
	$out.=')';
	return $out;
      } elsif ($type eq 'SET') {
	return '('
	  .  join(',', map { $self->serialize_expression_pt($_,$opts) } @$pt)
	  . ')';
      }
    } else {
      if ($pt=~/^[-0-9']/) {	# literal
	return qq( $pt );
      } elsif ($pt=~s/^(['"])(.*)\1$/$2/s) { # literal string
	$pt=~s/\\([^\\])/$1/sg;
	$pt=~s/'/\\'/sg;
	$pt=q{'}.$pt.q{'};
      } elsif ($pt=~s/^\$//) {	# a plain variable
	if ($pt eq '$') {
	  my $ret = $self->serialize_target($this_node_id,$opts);
	  if ($opts->{output_filter}) {
	    die "Cannot use node reference '$$' at this point of an output filter: '$opts->{expression}'\n"
	      if defined($opts->{column_count});
	    return $self->serialize_column_node_ref($ret,$opts);
	  } else {
	    return $ret;
	  }
	} elsif ($pt =~ /^[1-9]\d*$/) { #column reference
	  die "Column reference \$$pt can only be used in an output filter; error in expression '$opts->{expression}' of node '$this_node_id'\n"
	    unless $opts->{'output_filter'};
	  die "Column reference \$$pt used at position where there is yet no column to refer to\n"
	    unless defined $opts->{'column_count'};
	  die "Column reference \$$pt used at position where there are only $opts->{'column_count'} columns\n"
	    if $pt > $opts->{'column_count'};
	  my $var = '$'.$opts->{var_prefix}.$pt;
	  $opts->{columns_used}{$pt}=1;
	  $opts->{vars_used}{$var}=1;
	  return $var;
	} else {
	  my $ret = $self->serialize_target($pt,$opts);
	  if ($opts->{output_filter}) {
	    die "Cannot use node reference '$pt' at this point of an output filter: '$opts->{expression}'\n"
	      if defined($opts->{column_count});
	    return $self->serialize_column_node_ref($ret,$opts);
	  } else {
	    return $ret;
	  }
	}
      } else {			# unrecognized token
	die "Token '$pt' not recognized in expression $opts->{expression} of node '$this_node_id'\n";
      }
    }
  }

  sub serialize_column_node_ref {
    my ($self, $ret, $opts)=@_;
    push @{$opts->{input_columns}},$ret;
    my $i = scalar @{$opts->{input_columns}};
    my $var = '$'.$opts->{var_prefix}.$i;
    $opts->{columns_used}{ $i }=1;
    $opts->{vars_used}{$var}=1;
    return $var;
  }

  sub serialize_expression {
    my ($self,$opts)=@_;
    my $pt = Tree_Query::parse_expression($opts->{expression}); # $pt stands for parse tree
    die "Invalid expression '$opts->{expression}' on node '$opts->{id}'" unless defined $pt;
    return $self->serialize_expression_pt($pt,$opts);
  }

  sub test_occurrences {
    my ($self,$seed,$test_max) = (shift,shift,shift);
    $self->reset();
    my $count=0;
    print STDERR "<subquery>\n" if $DEBUG>1;
    while ($self->find_next_match({boolean => 1, seed=>$seed})) {
      $count++;
      last unless $count<=$test_max;
      $self->backtrack(0); # this is here to count on DISTINCT
      # roots of the subquery (i.e. the node with occurrences specified).
    }
    my ($min,$max_plus1)=@_;
    my $ret=0;
    while (@_) {
      ($min,$max_plus1)=(shift,shift);
      if ((!defined($min) || $count>=$min) and
	    (!defined($max_plus1) || $count<$max_plus1)) {
	$ret=1;
	last;
      }
    }
    print "occurrences: >=$count ($ret)\n" if $DEBUG > 1;
    print STDERR "</subquery>\n" if $DEBUG > 1;
    $self->reset() if $count;
    return $ret;
  }

  sub backtrack {
    my ($self,$pos)=@_;
    my $query_pos = \$self->{query_pos}; # a scalar reference
    return unless $$query_pos >= $pos;

    my $iterators = $self->{iterators};
    my $matched_nodes = $self->{matched_nodes};
    my $pos2match_pos = $self->{pos2match_pos};
    my $have = $self->{have};
    my $iterator;
    my $node;
    while ($pos<$$query_pos) {
      $node = delete $matched_nodes->[$pos2match_pos->[$$query_pos]];
      delete $have->{$node} if $node;
      $$query_pos--;
    }
    return 1;
  }
  sub find_next_match {
    my ($self,$opts)=@_;
    $opts||={};
    my $iterators = $self->{iterators};
    my $parent_pos = $self->{parent_pos};
    my $query_pos = \$self->{query_pos}; # a scalar reference
    my $matched_nodes = $self->{matched_nodes};
    my $pos2match_pos = $self->{pos2match_pos};
    my $have = $self->{have};

    my $iterator = $iterators->[$$query_pos];
    my $node = $iterator->node;
    if ($node) {
      delete $have->{$node};
      # print STDERR ("iterate $$query_pos $iterator: $self->{debug}[$$query_pos]\n") if $DEBUG;
      $node
	= $matched_nodes->[$pos2match_pos->[$$query_pos]]
	  = $iterator->next;
      $have->{$node}=1 if $node;
    } elsif ($$query_pos==0) {
      # first
      # print "Starting subquery on $opts->{seed}->{id} $opts->{seed}->{t_lemma}.$opts->{seed}->{functor}\n" if $opts->{seed} and $DEBUG;
      $node
	= $matched_nodes->[$pos2match_pos->[$$query_pos]]
	  = $iterator->start( $opts->{seed}, $opts->{fsfile} );
      $have->{$node}=1 if $node;
    }
    while (1) {
      if (!$node) {
	if ($$query_pos) {
	  # backtrack
	  $matched_nodes->[$pos2match_pos->[$$query_pos]]=undef;
	  $$query_pos--;	# backtrack
	  print STDERR ("backtrack to $$query_pos\n") if $DEBUG > 1;
	  $iterator=$iterators->[$$query_pos];

	  $node = $iterator->node;
	  delete $have->{$node} if $node;

	  #print STDERR ("iterate $$query_pos $iterator: $self->{debug}[$$query_pos]\n") if $DEBUG;
	  $node
	    = $matched_nodes->[$pos2match_pos->[$$query_pos]]
	      = $iterator->next;
	  $have->{$node}=1 if $node;
	  next;
	} else {
	  print STDERR "no match\n" if $DEBUG > 1;
	  return;		# NO RESULT
	}
      } else {
	print STDERR ("match $node->{id} [$$query_pos,$pos2match_pos->[$$query_pos]]: $node->{afun}.$node->{t_lemma}.$node->{functor}\n") if $DEBUG > 1;

	if ($$query_pos<$#$iterators) {
	  $$query_pos++;
	  $iterator = $iterators->[ $parent_pos->[$$query_pos] ];
	  my ($seed,$fsfile) = ($iterator->node, $iterator->file);
	  $iterator = $iterators->[$$query_pos];
	  $node
	    = $matched_nodes->[$pos2match_pos->[$$query_pos]]
	      = $iterator->start($seed,$fsfile);
	  #print STDERR ("restart $$query_pos $iterator from $seed->{t_lemma}.$seed->{functor} $self->{debug}[$$query_pos]\n") if $DEBUG;
	  $have->{$node}=1 if $node;
	  next;

	} else {
	  print STDERR ("complete match [bool: $opts->{boolean}]\n") if $DEBUG > 1;
	  # complete match:
	  if ($opts->{boolean}) {
	    return 1;
	  } else {
	    $self->{result_files}=[map { $_->file } @$iterators];
	    return $self->{results}=[map { $_->node } @$iterators];
	  }
	}
      }
    }
    return;
  }


}
#################################################
{
  package Tree_Query::BtredPlanner;
  use strict;
  use vars qw(%weight %reverse);

  %weight = (
    'user-defined:echild' => 5,
    'user-defined:eparent' => 2,
    'user-defined:a/lex.rf|a/aux.rf' => 2,
    'user-defined:a/lex.rf' => 1,
    'user-defined:a/aux.rf' => 2,
    'user-defined:coref_text.rf' => 1,
    'user-defined:coref_gram.rf' => 1,
    'user-defined:compl.rf' => 1,
    'descendant' => 30,
    'ancestor' => 8,
    'parent' => 0.5,
    'child' => 10,
    'order-precedes' => 10000,
    'order-follows' => 10000,
    'depth-first-precedes' => 1000,
    'depth-first-follows' => 1000,
    'same-tree-as' => 40,
   );

  %reverse = (
    'user-defined:echild' => 'user-defined:eparent',
    'user-defined:eparent' => 'user-defined:echild',
    'descendant' => 'ancestor',
    'ancestor' => 'descendant',
    'parent' => 'child',
    'child' => 'parent',
    'same-tree-as' => 'same-tree-as',
    'order-precedes' => 'order-follows',
    'order-follows' => 'order-precedes',
    'depth-first-precedes' => 'depth-first-follows',
    'depth-first-follows' => 'depth-first-precedes',
   );

  sub name_all_query_nodes {
    my ($tree)=@_;
    my @nodes = grep { $_->{'#name'} =~ /^(?:node|subquery)$/ } $tree->descendants;
    my $max=0;
    my %name2node = map {
      my $n=lc($_->{name});
      $max=$1+1 if $n=~/^n(\d+)$/ and $1>=$max;
      (defined($n) and length($n)) ? ($n=>$_) : ()
    } @nodes;
    my $name = 'n0';
    for my $node (@nodes) {
      my $n=lc($node->{name});
      unless (defined($n) and length($n)) {
	$node->{name}= $n ='n'.($max++);
	$name2node{$n}=$node;
      }
    }
    return \%name2node;
  }
  sub weight {
    my ($rel)=@_;
    my $name = $rel->name;
    if ($name eq 'user-defined') {
      $name.=':'.$rel->value->{label};
    }
    my $w = $weight{$name};
    return $w if defined $w;
    warn "do not have weight for edge: '$name'\n";
    return;
  }
  sub reversed_rel {
    my ($ref)=@_;
    my ($rel)=TredMacro::SeqV($ref->{relation});
    my $name = $rel->name;
    if ($name eq 'user-defined') {
      $name.=':'.$rel->value->{label};
    }
    my $rname = $reverse{$name};
    if (defined $rname) {
      my $rev;
      if ($rname =~s/^user-defined://) {
	$rev = Fslib::Seq::Element->new('user-defined', Fslib::CloneValue($rel->value));
	$rev->value->{label}=$rname;
      } else {
	$rev = Fslib::Seq::Element->new(
	  $rname,
	  Fslib::CloneValue($rel->value)
	   );
      }
      $rev->value->{reversed}=$ref;
      return $rev;
    } else {
      return;
    }
  }
  sub plan_query {
    my ($query_tree)=@_;
    $query_tree||=$TredMacro::root;
    name_all_query_nodes($query_tree);
    my @query_nodes=Tree_Query::FilterQueryNodes($query_tree);
    plan(\@query_nodes,$query_tree);
  }

  sub plan {
    my ($query_nodes,$query_tree,$query_root)=@_;
    die 'usage: plan(\@nodes,$query_tree,$query_node?)' unless
      ref($query_nodes) eq 'ARRAY' and $query_tree;
    my %node2pos = map { $query_nodes->[$_] => $_ } 0..$#$query_nodes;
    my %name2pos = map {
      my $name = lc($query_nodes->[$_]->{name});
      (defined($name) and length($name)) ? ($name=>$_) : ()
    } 0..$#$query_nodes;
    my $root_pos = defined($query_root) ? $node2pos{$query_root} : undef;

    require Graph;
    require Graph::ChuLiuEdmonds;
    my @edges;
    my @parent;
    my @parent_edge;
    for my $i (0..$#$query_nodes) {
      my $n = $query_nodes->[$i];
      print "$i: $n->{name}\n" if $DEBUG > 1;
      my $parent = $n->parent;
      my $p = $node2pos{$parent};
      $parent[$i]=$p;
      # turn node's relation into parent's extra-relation
      if (defined $p) {
	my ($rel) = TredMacro::SeqV($n->{relation});
	$rel||=Fslib::Seq::Element->new('child', Fslib::Container->new());
	$parent_edge[$i]=$rel;
	delete $n->{relation};
	my $ref = TredMacro::NewSon($parent);
	$ref->{'#name'} = 'ref';
	TredMacro::DetermineNodeType($ref);
	$ref->{relation}=Fslib::Seq->new([$rel]);
	$ref->{target} = $n->{name};
      }
    }
    for my $i (0..$#$query_nodes) {
      my $n = $query_nodes->[$i];
      for my $ref (grep { $_->{'#name'} eq 'ref' } $n->children) {
	my $target = lc( $ref->{target} );
	my ($rel)=TredMacro::SeqV($ref->{relation});
	next unless $rel;
	my $t = $name2pos{$target};
	my $no_reverse;
	my $tn = $query_nodes->[$t];
	my $tnp=$tn->parent;
	if ($n->{optional} or $tn->{optional} or ($tnp and $tnp->{optional})) {
	  # only direct edges can go in and out of an optional node
	  # and only direct edge can go to a child of an optional node
	  next unless $rel==$parent_edge[$t];
	  $no_reverse=1;
	}
	if (defined $t and $t!=$i) {
	  push @edges,[$i,$t,$ref,weight($rel)] unless defined($root_pos) and $t==$root_pos;
	  unless ($no_reverse or (defined($root_pos) and $i==$root_pos)) {
	    my $reversed = reversed_rel($ref);
	    if (defined $reversed) {
	      push @edges,[$t,$i,$reversed,weight($reversed)];
	    }
	  }
	}
      }
    }
    undef @parent_edge; # not needed anymore
    my $g=Graph->new(directed=>1);
    $g->add_vertex($_) for 0..$#$query_nodes;
    my %edges;
    for my $e (@edges) {
      my $has = $g->has_edge($e->[0],$e->[1]);
      my $w = $e->[3]||100000;
      if (!$has or $g->get_edge_weight($e->[0],$e->[1])>$w) {
	$edges{$e->[0]}{$e->[1]}=$e->[2];
	$g->delete_edge($e->[0],$e->[1]) if $has;
	$g->add_weighted_edge($e->[0],$e->[1], $w);
      }
    }
    my $mst=$g->MST_ChuLiuEdmonds();
#ifdef TRED
#    TredMacro::ChangingFile(1);
#endif
    for my $qn (@$query_nodes) {
      $qn->cut();
    }
    my $last_ref=0;
    my @roots;
    for my $i (0..$#$query_nodes) {
      my $qn = $query_nodes->[$i];
      my $p=undef;
      if ($mst->in_degree($i)==0) {
	$qn->paste_on($query_tree);
	push @roots,$qn;
      } else {
	my ($e) = $mst->edges_to($i);
	$p=$e->[0];
	$qn->paste_on($query_nodes->[$p]);
      }

      # now turn the selected extra-relation into relation
      # of $qn
      if (defined $p) {
 	my $parent = $query_nodes->[$p];
	my $ref = $edges{$p}{$i};
	my $rel;
	if (UNIVERSAL::isa($ref,'Fslib::Seq::Element')) {
	  $rel = $ref;
	  $ref = delete $rel->value->{reversed};
	} else {
	  ($rel) = TredMacro::SeqV($ref->{relation});
	}
	TredMacro::DeleteLeafNode($ref);
	delete $qn->{'relation'};
	TredMacro::AddToSeq($qn,'relation',$rel);
      }
    }
    return \@roots;
  }
}
#################################################
{
  package Tree_Query::Iterator;
  use strict;
  use constant CONDITIONS=>0;
  use Carp;
  sub new {
    my ($class,$conditions)=@_;
    croak "usage: $class->new(sub{...})" unless ref($conditions) eq 'CODE';
    return bless [$conditions],$class;
  }
  sub conditions { return $_[0]->[CONDITIONS]; }
  sub start {}
  sub next {}
  sub node {}
  sub reset {}
}
#################################################
{
  package FSFileIterator;
  use strict;
  use Carp;
  use base qw(Tree_Query::Iterator);
  use constant CONDITIONS=>0;
  use constant FILE=>1;
  use constant TREE_NO=>2;
  use constant NODE=>3;
  sub new {
    my ($class,$conditions,$fsfile)=@_;
    croak "usage: $class->new(sub{...})" unless ref($conditions) eq 'CODE';
    return bless [$conditions,$fsfile],$class;
  }
  sub start  {
    my ($self,$fsfile)=@_;
    $self->[TREE_NO]=0;
    if ($fsfile) {
      $self->[FILE]=$fsfile;
    } else {
      $fsfile=$self->[FILE];
    }
    my $n = $self->[NODE] = $self->[FILE]->tree(0);
    return ($n && $self->[CONDITIONS]->($n,$fsfile)) ? $n : ($n && $self->next);
  }
  sub next {
    my ($self)=@_;
    my $conditions=$self->[CONDITIONS];
    my $n=$self->[NODE];
    my $fsfile=$self->[FILE];
    while ($n) {
      $n = $n->following || $fsfile->tree(++$self->[TREE_NO]);
      last if $conditions->($n,$fsfile);
    }
    return $self->[NODE]=$n;
  }
  sub node {
    return $_[0]->[NODE];
  }
  sub file {
    return $_[0]->[FILE];
  }
  sub reset {
    my ($self)=@_;
    $self->[NODE]=undef;
  }
}
#################################################
{
  package CurrentFileIterator;
  use strict;
  use base qw(Tree_Query::Iterator);
  BEGIN {
    import TredMacro qw($this $root);
  }
  use constant CONDITIONS=>0;
  use constant NODE=>1;
  sub start  {
    my ($self)=@_;
    # TredMacro::GotoFileNo(0);
    TredMacro::GotoTree(0);
    $this=$root;
    $self->[NODE]=$this;
    return ($this && $self->[CONDITIONS]->($this,TredMacro::CurrentFile())) ? $this : ($this && $self->next);
  }
  sub next {
    my ($self)=@_;
    my $conditions=$self->[CONDITIONS];
    my $n=$self->[NODE];
    my $fsfile=TredMacro::CurrentFile();
    while ($n) {
      $n = $n->following || (TredMacro::NextTree() && $this);
      last if $conditions->($n,$fsfile);
    }
    return $self->[NODE]=$n;
  }
  sub file {
    return TredMacro::CurrentFile();
  }
  sub node {
    return $_[0]->[NODE];
  }
  sub reset {
    my ($self)=@_;
    $self->[NODE]=undef;
  }
}
#################################################
{
  package CurrentFilelistIterator;
  use strict;
  use base qw(Tree_Query::Iterator);
  BEGIN {
    import TredMacro qw($this $root $grp);
  }
  use constant CONDITIONS=>0;
  use constant NODE=>1;
  sub start  {
    my ($self)=@_;
    # TredMacro::GotoFileNo(0);
    TredMacro::GotoTree(0);
    $this=$root;
    $self->[NODE]=$this;
    my $fsfile = $grp->{FSFile};
    return ($this && $self->[CONDITIONS]->($this,$fsfile)) ? $this : ($this && $self->next);
  }
  sub next {
    my ($self)=@_;
    my $conditions=$self->[CONDITIONS];
    my $n=$self->[NODE];
    my $fsfile = $grp->{FSFile};
    while ($n) {
      $n = $n->following
	|| (TredMacro::NextTree() && $this ) 
	||  (TredMacro::NextFile() && ($fsfile=$grp->{FSFile}) && $this)
	  ;
      TredMacro::FPosition();
      last if $conditions->($n,$fsfile);
    }
    return $self->[NODE]=$n;
  }
  sub node {
    return $_[0]->[NODE];
  }
  sub file {
    return $grp->{FSFile};
  }
  sub reset {
    my ($self)=@_;
    $self->[NODE]=undef;
  }
}
#################################################
{
  package TreeIterator;
  use strict;
  use Carp;
  use base qw(Tree_Query::Iterator);
  use constant CONDITIONS=>0;
  use constant TREE=>1;
  use constant NODE=>2;
  use constant FILE=>3;
  sub new  {
    my ($class,$conditions,$root,$fsfile)=@_;
    croak "usage: $class->new(sub{...})" unless ref($conditions) eq 'CODE';
    return bless [$conditions,$root,$fsfile],$class;
  }
  sub start  {
    my ($self)=@_;
    my $root = $self->[NODE] = $self->[TREE];
    return ($root && $self->[CONDITIONS]->($root,$self->[FILE])) ? $root : ($root && $self->next);
  }
  sub next {
    my ($self)=@_;
    my $conditions=$self->[CONDITIONS];
    my $n=$self->[NODE];
    my $fsfile=$self->[FILE];
    while ($n) {
      $n = $n->following;
      last if $conditions->($n,$fsfile);
    }
    return $self->[NODE]=$n;
  }
  sub node {
    return $_[0]->[NODE];
  }
  sub file {
    return $_[0]->[FILE];
  }
  sub reset {
    my ($self)=@_;
    $self->[NODE]=undef;
  }
}
#################################################
{
  package SameTreeIterator;
  use strict;
  use Carp;
  use base qw(TreeIterator);
  sub new  {
    my ($class,$conditions)=@_;
    croak "usage: $class->new(sub{...})" unless ref($conditions) eq 'CODE';
    return bless [$conditions],$class;
  }
  sub start  {
    my ($self,$root,$fsfile)=@_;
    $root=$root->root if $root;
    $self->[TreeIterator::NODE] = $self->[TreeIterator::TREE] = $root;
    $self->[TreeIterator::FILE]=$fsfile;
    return ($root && $self->[TreeIterator::CONDITIONS]->($root,$fsfile)) ? $root : ($root && $self->next);
  }
}
#################################################
{
  package OptionalIterator;
  use strict;
  use base qw(Tree_Query::Iterator);
  use constant CONDITIONS=>0;
  use constant ITERATOR=>1;
  use constant NODE=>2;
  use constant FILE=>3;
  use Carp;
  sub new {
    my ($class,$iterator)=@_;
    croak "usage: $class->new($iterator)" unless UNIVERSAL::isa($iterator,'Tree_Query::Iterator');
    return bless [$iterator->conditions,$iterator],$class;
  }
  sub start  {
    my ($self,$parent,$fsfile)=@_;
    $self->[NODE]=$parent;
    $self->[FILE]=$fsfile;
    return $parent ? ($self->[CONDITIONS]->($parent,$fsfile) ? $parent : $self->next) : undef;
  }
  sub next {
    my ($self)=@_;
    my $n = $self->[NODE];
    if ($n) {
      $self->[NODE]=undef;
      return $self->[ITERATOR]->start($n,$self->[FILE]);
    }
    return $self->[ITERATOR]->next;
  }
  sub node {
    my ($self)=@_;
    return $self->[NODE] || $self->[ITERATOR]->node;
  }
  sub file {
    return $_[0]->[FILE];
  }
  sub reset {
    my ($self)=@_;
    $self->[NODE]=undef;
    $self->[FILE]=undef;
    $self->[ITERATOR]->reset;
  }
}
#################################################
{
  package ChildnodeIterator;
  use strict;
  use base qw(Tree_Query::Iterator);
  use constant CONDITIONS=>0;
  use constant NODE=>1;
  use constant FILE=>2;
  sub start  {
    my ($self,$parent,$fsfile)=@_;
    if ($fsfile) {
      $self->[FILE]=$fsfile;
    } else {
      $fsfile=$self->[FILE];
    }
    my $n = $self->[NODE]=$parent->firstson;
    return ($n && $self->[CONDITIONS]->($n,$fsfile)) ? $n : ($n && $self->next);
  }
  sub next {
    my ($self)=@_;
    my $conditions=$self->[CONDITIONS];
    my $n=$self->[NODE]->rbrother;
    my $fsfile = $self->[FILE];
    $n=$n->rbrother while ($n and !$conditions->($n,$fsfile));
    return $self->[NODE]=$n;
  }
  sub node {
    return $_[0]->[NODE];
  }
  sub file {
    return $_[0]->[FILE];
  }
  sub reset {
    my ($self)=@_;
    $self->[NODE]=undef;
    $self->[FILE]=undef;
  }
}
#################################################
{
  package DescendantIterator;
  use strict;
  use base qw(Tree_Query::Iterator);
  use constant CONDITIONS=>0;
  use constant NODE=>1;
  use constant TOP=>2;
  use constant FILE=>3;

  sub start  {
    my ($self,$parent,$fsfile)=@_;
    if ($fsfile) {
      $self->[FILE]=$fsfile;
    } else {
      $fsfile=$self->[FILE];
    }
    my $n= $parent->firstson;
    $self->[NODE]=$n;
    $self->[TOP]=$parent;
    return ($n && $self->[CONDITIONS]->($n,$fsfile)) ? $n : ($n && $self->next);
  }
  sub next {
    my ($self)=@_;
    my $conditions=$self->[CONDITIONS];
    my $top = $self->[TOP];
    my $n=$self->[NODE]->following($top);
    my $fsfile=$self->[FILE];
    $n=$n->following($top) while ($n and !$conditions->($n,$fsfile));
    return $self->[NODE]=$n;
  }
  sub node {
    return $_[0]->[NODE];
  }
  sub file {
    return $_[0]->[FILE];
  }
  sub reset {
    my ($self)=@_;
    $self->[NODE]=undef;
    $self->[TOP]=undef;
    $self->[FILE]=undef;
  }
}

#################################################
{
  package DescendantIteratorWithBoundedDepth;
  use strict;
  use base qw(Tree_Query::Iterator);
  use Carp;
  use constant CONDITIONS=>0;
  use constant MIN=>1;
  use constant MAX=>2;
  use constant DEPTH=>3;
  use constant NODE=>4;
  use constant FILE=>5;

  sub new {
    my ($class,$conditions,$min,$max)=@_;
    croak "usage: $class->new(sub{...})" unless ref($conditions) eq 'CODE';
    $min||=0;
    return bless [$conditions,$min,$max],$class;
  }
  sub start  {
    my ($self,$parent,$fsfile)=@_;
    $self->[FILE]=$fsfile;
    my $n=$parent->firstson;
    $self->[DEPTH]=1;
    $self->[NODE]=$n;
    return ($self->[MIN]<=1 and $self->[CONDITIONS]->($n,$fsfile)) ? $n : ($n && $self->next);
  }
  sub next {
    my ($self)=@_;
    my $min = $self->[MIN];
    my $max = $self->[MAX];
    my $depth = $self->[DEPTH];
    my $conditions=$self->[CONDITIONS];
    my $n = $self->[NODE];
    my $fsfile=$self->[FILE];
    my $r;
    SEARCH:
    while ($n) {
      if ((!defined($max) or ($depth<$max)) and $n->firstson) {
	$n=$n->firstson;
	$depth++;
      } else {
	while ($n) {
	  if ($depth == 0) {
	    undef $n;
	    last SEARCH;
	  }
	  if ($r = $n->rbrother) {
	    $n=$r;
	    last;
	  } else {
	    $n=$n->parent;
	    $depth--;
	  }
	}
      }
      if ($n and $min<=$depth and $conditions->($n,$fsfile)) {
	$self->[DEPTH]=$depth;
	return $self->[NODE]=$n;
      }
    }
    return $self->[NODE]=undef;
  }
  sub file {
    return $_[0]->[FILE];
  }
  sub node {
    return $_[0]->[NODE];
  }
  sub reset {
    my ($self)=@_;
    $self->[NODE]=undef;
    $self->[FILE]=undef;
  }
}
#################################################
{
  package ParentIterator;
  use strict;
  use base qw(Tree_Query::Iterator);
  use constant CONDITIONS=>0;
  use constant NODE=>1;
  use constant FILE=>2;
  sub start  {
    my ($self,$node,$fsfile)=@_;
    $self->[FILE]=$fsfile;
    my $n = $node->parent;
    return $self->[NODE] = ($n && $self->[CONDITIONS]->($n,$fsfile)) ? $n : undef;
  }
  sub next {
    return $_[0]->[NODE]=undef;
  }
  sub node {
    return $_[0]->[NODE];
  }
  sub file {
    return $_[0]->[FILE];
  }
  sub reset {
    my ($self)=@_;
    $self->[NODE]=undef;
    $self->[FILE]=undef;
  }
}
#################################################
{
  package AncestorIterator;
  use strict;
  use base qw(Tree_Query::Iterator);
  use constant CONDITIONS=>0;
  use constant NODE=>1;
  use constant FILE=>2;
  sub start  {
    my ($self,$node,$fsfile)=@_;
    $self->[FILE]=$fsfile;
    my $n = $node->parent;
    $self->[NODE]=$n;
    return ($n && $self->[CONDITIONS]->($n,$fsfile)) ? $n : ($n && $self->next);
  }
  sub next {
    my ($self)=@_;
    my $conditions=$self->[CONDITIONS];
    my $n=$self->[NODE]->parent;
    my $fsfile = $self->[FILE];
    $n=$n->parent while ($n and !$conditions->($n,$fsfile));
    return $_[0]->[NODE]=$n;
  }
  sub node {
    return $_[0]->[NODE];
  }
  sub file {
    return $_[0]->[FILE];
  }
  sub reset {
    my ($self)=@_;
    $self->[NODE]=undef;
    $self->[FILE]=undef;
  }
}
#################################################
{
  package AncestorIteratorWithBoundedDepth;
  use strict;
  use base qw(Tree_Query::Iterator);
  use Carp;
  use constant CONDITIONS=>0;
  use constant MIN=>1;
  use constant MAX=>2;
  use constant NODE=>3;
  use constant DEPTH=>4;
  use constant FILE=>5;
  sub new  {
    my ($class,$conditions,$min,$max)=@_;
    croak "usage: $class->new(sub{...})" unless ref($conditions) eq 'CODE';
    $min||=0;
    return bless [$conditions,$min,$max],$class;
  }
  sub start  {
    my ($self,$node,$fsfile)=@_;
    my $min = $self->[MIN]||1;
    my $max = $self->[MAX];
    $self->[FILE]=$fsfile;
    my $depth=0;
    $node = $node->parent while ($node and ($depth++)<$min);
    $node=undef if defined($max) and $depth>$max;
    $self->[NODE]=$node;
    $self->[DEPTH]=$depth;
    return ($node && $self->[CONDITIONS]->($node,$fsfile)) ? $node : ($node && $self->next);
  }
  sub next {
    my ($self)=@_;
    my $conditions=$self->[CONDITIONS];
    my $max = $self->[MAX];
    my $depth = $self->[DEPTH]+1;
    return $_[0]->[NODE]=undef if ($depth>$max);
    my $n=$self->[NODE]->parent;
    my $fsfile = $self->[FILE];
    while ($n and !$conditions->($n,$fsfile)) {
      $depth++;
      if ($depth<=$max) {
	$n=$n->parent;
      } else {
	$n=undef;
      }
    }
    return $_[0]->[NODE]=$n;
  }
  sub node {
    return $_[0]->[NODE];
  }
  sub file {
    return $_[0]->[FILE];
  }
  sub reset {
    my ($self)=@_;
    $self->[NODE]=undef;
    $self->[FILE]=undef;
  }
}

#################################################
{
  package ALexRFIterator;
  use strict;
  use base qw(Tree_Query::Iterator);
  use constant CONDITIONS=>0;
  use constant NODE=>1;
  use constant FILE=>2;
  sub start  {
    my ($self,$node,$fsfile)=@_;
    my $lex_rf = $node->attr('a/lex.rf');
    my $refnode;
    $self->[FILE] = $fsfile;
    if (defined $lex_rf) {
      # $lex_rf=~s/^.*?#//;
      $refnode=PML_T::GetANodeByID($lex_rf,$fsfile);
    }
    return $self->[NODE] = $self->[CONDITIONS]->($refnode,$self->file) ? $refnode : undef;
  }
  sub next {
    return $_[0]->[NODE]=undef;
  }
  sub node {
    return $_[0]->[NODE];
  }
  sub file {
    return PML_T::AFile($_[0]->[FILE]);
  }
  sub reset {
    my ($self)=@_;
    $self->[NODE]=undef;
    $self->[FILE]=undef;
  }
}
#################################################
{
  package SimpleListIterator;
  use strict;
  use base qw(Tree_Query::Iterator);
  use constant CONDITIONS=>0;
  use constant NODES=>1;
  use constant FILE=>2;
  use constant FIRST_FREE=>3; # number of the first constant free for user
  sub start  {
    my ($self,$node,$fsfile)=@_;
    $self->[FILE]=$fsfile;
    my $nodes = $self->[NODES] = $self->get_node_list($node);
    my $n = $nodes->[0];
    return ($n && $self->[CONDITIONS]->(@$n)) ? $n->[0] : ($n->[0] && $self->next);
  }
  sub next {
    my ($self)=@_;
    my $nodes = $self->[NODES];
    my $conditions=$self->[CONDITIONS];
    shift @{$nodes};
    my $n;
    while (($n = $nodes->[0]) and !$conditions->(@$n)) {
      shift @{$nodes};
    }
    return $nodes->[0][0];
  }
  sub node {
    my ($self)=@_;
    my $n = $self->[NODES][0];
    return $n && $n->[0];
  }
  sub file {
    my ($self)=@_;
    my $n = $self->[NODES][0];
    return $n && $n->[1];
  }
  sub reset {
    my ($self)=@_;
    $self->[NODES]=undef;
    $self->[FILE]=undef;
  }
  sub get_node_list {
    return [];
  }
}

#################################################
{
  package AAuxRFIterator;
  use strict;
  use base qw(SimpleListIterator);
  sub get_node_list  {
    my ($self,$node)=@_;
    my $a_file = PML_T::AFile($self->[SimpleListIterator::FILE]);
    return [map {
      my $n = PML_T::GetANodeByID($_,$self->[SimpleListIterator::FILE]);
      defined $n ? [$n,$a_file] : ()
    } TredMacro::ListV($node->attr('a/aux.rf'))];
  }
}
#################################################
{
  package PMLREFIterator;
  use strict;
  use base qw(SimpleListIterator);
  use constant ATTR => SimpleListIterator::FIRST_FREE;
  use Carp;
  sub new {
    my ($class,$conditions,$attr)=@_;
    croak "usage: $class->new(sub{...},\$attr)" unless (ref($conditions) eq 'CODE' and defined $attr);
    my $self = SimpleListIterator->new($conditions);
    $self->[ATTR]=$attr;
    bless $self, $class; # reblessing
    return $self;
  }
  sub get_node_list  {
    my ($self,$node)=@_;
    my $fsfile = $self->[SimpleListIterator::FILE];
    return [map {
      my $id = $_;
      $id=~s/^(.*)?#//;
      if ($1) {
	my $ref_fs = $fsfile->appData('ref')->{$1};
	my $n = $ref_fs && PML::GetNodeByID($id,$ref_fs);
	$ref_fs && $n ? [$n, $ref_fs] : ();
      } else {
	my $n = PML::GetNodeByID($id,$fsfile);
	$n ? [$n, $fsfile] : ()
      }
    } PMLInstance::get_all($node,$self->[ATTR])];
  }
}
#################################################
{
  package ALexOrAuxRFIterator;
  use strict;
  use base qw(SimpleListIterator);
  sub get_node_list  {
    my ($self,$node)=@_;
    my $fsfile = $self->[SimpleListIterator::FILE];
    my $a_file = PML_T::AFile($fsfile);
    return [ $a_file ? map [$_,$a_file ], PML_T::GetANodes($node,$fsfile) : () ];
  }
}
#################################################
{
  package CorefTextRFIterator;
  use strict;
  use base qw(SimpleListIterator);
  sub get_node_list  {
    my ($self,$node)=@_;
    my $fsfile = $self->[SimpleListIterator::FILE];
    return [map {
      my $n = PML::GetNodeByID($_);
      $n ? [ $n, $fsfile ] : ()
    } TredMacro::ListV($node->attr('coref_text.rf'))];
  }
}
#################################################
{
  package CorefGramRFIterator;
  use strict;
  use base qw(SimpleListIterator);
  sub get_node_list  {
    my ($self,$node)=@_;
    my $fsfile = $self->[SimpleListIterator::FILE];
    return [map {
      my $n = PML::GetNodeByID($_);
      $n ? [ $n, $fsfile ] : ()
    } TredMacro::ListV($node->attr('coref_gram.rf'))];
  }
}
#################################################
{
  package ComplRFIterator;
  use strict;
  use base qw(SimpleListIterator);
  sub get_node_list  {
    my ($self,$node)=@_;
    my $fsfile = $self->[SimpleListIterator::FILE];
    return [map {
      my $n = PML::GetNodeByID($_);
      $n ? [ $n, $fsfile ] : ()
    } TredMacro::ListV($node->attr('compl.rf'))];
  }
}
#################################################
{
  package EParentIterator;
  use strict;
  use base qw(SimpleListIterator);
  sub get_node_list  {
    my ($self,$node)=@_;
    my $type = $node->type->get_base_type_name;
    my $fsfile = $self->[SimpleListIterator::FILE];
    return [
      map [ $_,$fsfile ],
      ($type eq 't-node.type' ?
	 PML_T::GetEParents($node) :
	     $type eq 'a-node.type' ?
	       PML_A::GetEParents($node,\&PML_A::DiveAuxCP) :
		   ())
	   ];
  }
}
#################################################
{
  package EChildIterator;
  use strict;
  use base qw(SimpleListIterator);
  sub get_node_list  {
    my ($self,$node)=@_;
    my $type = $node->type->get_base_type_name;
    my $fsfile = $self->[SimpleListIterator::FILE];
    return [
      map [ $_,$fsfile ],
      ($type eq 't-node.type' ?
	      PML_T::GetEChildren($node) :
		  $type eq 'a-node.type' ?
		    PML_A::GetEChildren($node,\&PML_A::DiveAuxCP) :
			())
		       ];
  }
}
#################################################


=comment on implementation on top of btred search engine

1. find in the query graph an oriented sceleton tree, possibly using
Kruskal and some weighting rules favoring easy to follow types of
edges (relations) with minimum number of potential target nodes
(e.g. parent, ancestor a/lex.rf are better than child, descendant or
a/aux.rf, and far better then their negated counterparts).

2. Order sibling nodes of this tree by similar algorithm so that all
relations between these nodes go from right bottom to left top (using
reversing where possible) and the result is near optimal using similar
weighting as above. This may be done only for relations not occuring
in condition formulas.

3. For each relation between nodes that occurs in a condition formula,
assume that the relation is or is not satisfied so that the truth
value of the condition is not decreased (whether to take the formula
negatively or positively is probably easy to compute since we may
eliminate all negations of non-atomic subformulas and then aim for
TRUE value of the respective literal; that is, we only count the
number of negations on the path from the root of the expression to the
predicate representing the relational constraint and assume TRUE for
even numbers and FALSE for odd numbers).

The actual truth values of these relations will be verified only after
all query nodes have been matched (or maybe for each node as soon as
all nodes it refers to have been matched).

4. The query context consists of:

- the node in the query-tree being matched (current query node)

- association of the previously matched query nodes with result node iterators

- information about unresolved relational constraints on already
  matched nodes

5. the search starts by creating an initial query context and a simple
iterator for the root query node matches

6. in each step one of the following cases occurs:

- the iterator for the current query node is empty
  -> backtrack: return to the state of the context of the previous query node
     and iterate the associated iterator
  -> fail if there is no previous query node

- the iterator returns a node:

  - check relational constraints depending on this node.
    If any of them invalidates the condition on an already matched node,
    itereate and repeat 6

  - if there is a following query node, make it the current query node
    and repeat 6

  - otherwise: we have a complete match. Return the match, back-track
    the context to the root-node and iterate the root-node iterator.
    Then repeat 6.

Note: #occurrences are to be implemented as sub-queries that are
processed along with other conditions within the simple iterators.
The relation predicates from these sub-queries to the out-side trees
are treated as predicate relations in complex relations and are only
resolved as soon as all required query nodes are matched.

=cut

}
1;
