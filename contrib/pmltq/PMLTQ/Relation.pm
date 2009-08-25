package PMLTQ::Relation;
# pajas@ufal.ms.mff.cuni.cz 21 srp 2009

use 5.006;
use strict;
use warnings;
use Carp;
use File::Spec;
use File::Glob qw(:glob);

my %user_defined;
my %start_to_target_type_map;

# autoloading of relation modules
for my $dir (@INC) {
  next if ref $dir;
  for my $module (glob(File::Spec->catfile($dir,'PMLTQ','Relation','*.pm'))) {
    my $return = do $module;
    unless ($return) {
      if ($@) {
	warn "Failed to load PMLTQ::Relation submodule $module: $@\n";
      } elsif (!defined $return) {
	warn "Failed to compile PMLTQ::Relation submodule $module: $!\n";
      } elsif (!$return) {
	warn "PMLTQ::Relation submodule $module did not return a true value.\n";
      }
    }
  }
}

sub import {
  my $class=shift;
  for my $def (@_) {
    my $name = $def->{name};
    $user_defined{ $def->{start_node_type}.':'.$name } = $def;
    $start_to_target_type_map{ $def->{start_node_type} }{ $name } = $def->{target_node_type};
  }
}

sub create_iterator {
  my ($class,$node_type,$label) = (shift,shift,shift);
  my $rel = $user_defined{ $node_type.':'.$label };
  if ($rel and exists($rel->{iterator_class})) {
    $rel->{iterator_class}->new(@_);
  } else {
    return;
  }
}

sub iterator_weight {
  my ($class,$node_type,$label) = @_;
  my $rel = $user_defined{$node_type.':'.$label};
  return unless $rel;
  return $rel && $rel->{iterator_weight};
}

sub relations_for_node_type {
  my ($class, $start_type)=@_;
  my $map = $start_to_target_type_map{$start_type};
  return $map ? [sort keys %$map] : [];
}

sub target_type {
  my ($class, $start_type,$label)=@_;
  my $rel = $start_to_target_type_map{$start_type};
  return $rel && $rel->{$label};
}

sub reversed_relation {
  my ($class, $start_type, $name)=@_;
  my $rel =  $user_defined{$start_type.':'.$name};
  return $rel && $rel->{reversed_relation};
}

sub test_code {
  my ($class, $start_type, $name)=@_;
  my $rel =  $user_defined{$start_type.':'.$name};
  return $rel && $rel->{test_code};
  return undef;
}

#################################################
{
  package PMLTQ::Relation::Iterator;
  use strict;
  use constant CONDITIONS=>0;
  use Carp;
  sub new {
    my ($class,$conditions)=@_;
    croak "usage: $class->new(sub{...})" unless ref($conditions) eq 'CODE';
    return bless [$conditions],$class;
  }
  sub clone {
    my ($self)=@_;
    return bless [$self->[CONDITIONS]], ref($self);
  }
  sub conditions { return $_[0]->[CONDITIONS]; }
  sub set_conditions { $_[0]->[CONDITIONS]=$_[1]; }
  sub start {}
  sub next {}
  sub node {}
  sub reset {}
}

#################################################
{
  package PMLTQ::Relation::SimpleListIterator;
  use strict;
  use base qw(PMLTQ::Relation::Iterator);
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
  sub start_file {
    my ($self)=@_;
    return $self->[FILE];
  }
}


1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

PMLTQ::Relation - Perl extension for blah blah blah

=head1 SYNOPSIS

   use PMLTQ::Relation;
   blah blah blah

=head1 DESCRIPTION

Stub documentation for PMLTQ::Relation, 
created by template.el.

It looks like the author of the extension was negligent
enough to leave the stub unedited.

Blah blah blah.

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

