# -*- cperl -*-
{
package Tree_Query_Btred;

use strict;
use Tree_Query::Common;
use Tree_Query::TypeMapper;
use Tree_Query::BtredEvaluator;

use vars qw($this $root $DEBUG);
BEGIN {
  import TredMacro;
  *DEBUG = \$Tree_Query::BtredEvaluator::DEBUG;
  *ALL_SUBQUERIES_LAST = \$Tree_Query::BtredEvaluator::ALL_SUBQUERIES_LAST;
  *ORDER_SIBLINGS=\$Tree_Query::BtredEvaluator::ORDER_SIBLINGS;
}

#ifdef TRED
$DEBUG=4 if $::tredDebug;
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
    $query_tree=Tree_Query::Common::parse_query($query,{
      user_defined_relations => ($type_mapper && $type_mapper->get_user_defined_relations()),
      pmlrf_relations => ($type_mapper && $type_mapper->get_pmlrf_relations()),
    });
    DetermineNodeType($_) for $query_tree->descendants;
  } else {
    my ($query_fn,$query_id)=@ARGV;
    my $query_file = Treex::PML::Factory->createDocumentFromFile($query_fn, {
      backends => [Backends()]
    });
    if (ref($query_file)) {
      if ($Treex::PML::FSError!=0) {
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
  $evaluator_opts->{type_mapper} ||= Tree_Query::TypeMapper->new({fsfile=>CurrentFile()});
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

###########################################

}
1;
