# -*- cperl -*-
#
# remove non-extension version of Tree_Query

BEGIN {
  if (exists &Tree_Query::NewQuery) {
    print STDERR "NOTE: tredlib/contrib/tree_query was obsoleted by pmltq extension\n";
    for my $pkg (qw(
		     Tree_Query::Common
		     Tree_Query::NG2PMLTQ
		     Tree_Query::TrEdSearch
		     Tree_Query::BtredEvaluator
		     Tree_Query::BtredPlanner
		     Tree_Query::Iterator
		     Tree_Query_Btred
		     TrEd::PMLTQ::UserAgent
		     Tree_Query::HTTPSearch
		     Tree_Query::SQLSearch
		     Tree_Query::Grammar
		     Tree_Query)) {
      no strict qw(refs);
      %{$pkg."::"}=();
      my $file = $pkg; $file=~s{::}{/}g;
      delete $INC{$file.'.pm'};
    }
  }
}
eval {
  delete $grp->{framegroup}{minorModes}{Tree_Query_Results} if ref $grp;
};
print STDERR $@ if $@;
