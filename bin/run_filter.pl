#!/usr/bin/perl
use strict;
our $filters;
our $filter;
my $first = 1;

use Getopt::Long;
my %opts = (
  'compiled-filter' => 'filter.pl',
);
GetOptions(
   \%opts,
   "compiled-filter|c=s",
   "if-exists|e",
) || die "Command-line syntax error!";


use POSIX qw(ceil floor);
sub round {
  my ($value, $precision) = @_;
  my $rounding = ($value >= 0 ? 0.5 : -0.5);
  my $decimalscale = 10**int($precision || 0);
  my $scaledvalue = int($value * $decimalscale + $rounding);
  return $scaledvalue / $decimalscale;
}
# sub round {
#   my ($num, $digits)=@_;
#   $digits = int $digits;
#   return sprintf("%.${digits}f", $num);
# }
sub trunc {
  my ($self, $num, $digits) = @_;
  $digits = int $digits;
  my $decimalscale = 10**abs($digits);
  if ($digits >= 0) {
    return int($num * $decimalscale) / $decimalscale;
  } else {
    return int($num / $decimalscale) * $decimalscale;
  }
}

my $output_filter = {
  init => sub { },
  process_row => sub {
    my ($self,$row)=@_;
    print(join("\t",@$row)."\n");
  },
  finish => sub { }
};

while (<>) {
  chomp;
  unless ($filter) {
    if (!$opts{'if-exists'} or -f $opts{'compiled-filter'}) {
      unless (do $opts{'compiled-filter'}) {
	open my $fh, "<", $opts{'compiled-filter'} ||
	  die "Cannot open $opts{'compiled-filter'}: $!";
	print STDERR $_ while <$fh>;
	close $fh;
	print STDERR "\n";
	die "Running filter $opts{'compiled-filter'} failed!";
      }
      my @filters = map {
	my @local_filters = map eval, @{$_->{local_filters_code}};
	my $sub = eval($_->{code});
	die $@ if $@;
	$sub
      } @$filters;

      # connect filters
      my $prev;
      for my $filter (@filters) {
	$prev->{output}=$filter if $prev;
	$prev = $filter;
      }
      if ($prev) {
	$prev->{output} = $output_filter;
      }
      $filter = $filters[0];
      $filter->{init}->($filter);
    } else {
      $filter = $output_filter;
    }
  }
  $filter->{process_row}->($filter,[split /\t/,$_]);
}
$filter->{finish}->($filter) if $filter;
