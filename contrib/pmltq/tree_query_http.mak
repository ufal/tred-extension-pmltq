# -*- cperl -*-
################
### HTTP interface to pmltq server
################

#### TrEd interface to Tree_Query::Evaluator
{

{
package TrEd::PMLTQ::UserAgent;
  use base qw(LWP::UserAgent);
  sub credentials {
    shift; # self
    return $IOBackend::lwp_user_agent->credentials(@_);
  }
  sub get_basic_credentials {
    return;
  }
}

package Tree_Query::HTTPSearch;
use base qw(Tree_Query::TrEd);
use Benchmark;
use Carp;
use strict;
use warnings;
use Scalar::Util qw(weaken);
use HTTP::Request::Common;
use File::Temp;
use Encode;
use PMLSchema;
use URI;

use vars qw($VERSION $MIN_SERVER_VERSION);
$VERSION = "0.2";
$MIN_SERVER_VERSION = "0.3";
my $ua = $IOBackend::lwp_user_agent; #TrEd::PMLTQ::UserAgent->new();

#use LWP::UserAgent;

BEGIN { import TredMacro  }

our %DEFAULTS = (
  row_limit => 5000,
  limit => 100,
  timeout => 30,
);

$Tree_Query::HTTPSearchPreserve::object_id=0; # different NS so that TrEd's reload-macros doesn't clear it
# my $ua = $IOBackend::lwp_user_agent;
#$ua = IOBackend->new;
#$ua->agent("TrEd/1.0 ");

sub new {
  my ($class,$opts)=@_;
  $opts||={};
  my $self = bless {
    object_id =>  $Tree_Query::HTTPSearchPreserve::object_id++,
    config => {
      pml => $opts->{config_pml},
    },
    query => undef,
    query_nodes => undef,
    results => undef,
    limit=>undef,
    spinbox_timeout=>undef,
  }, $class;
  $self->init($opts->{config_file},$opts->{config_id}) || return;
  my $ident = $self->identify;
  {
    my $tb;
    ($tb, $self->{label}) = Tree_Query::CreateSearchToolbar($ident);
    $tb->Label(-text=>"Timeout:")->pack(-side=>'left',-padx=>10);
    my $b = $tb->Spinbox(
      -background=>'white',
      -widt=>3,
      -from => 10,
      -to => 300,
      -increment=>5,
      -textvariable => \$self->{spinbox_timeout},
     )->pack(-side => 'left', );
    AttachTooltip($b,'Select timeout in seconds.');
  }
  $self->{on_destroy} = MacroCallback(
    sub {
      DestroyUserToolbar($ident);
      for my $win ($self->get_result_windows) {
	CloseFileInWindow($win);
	CloseWindow($win);
      }
      ChangingFile(0);
    });
  return $self;
}

sub toolbar {
  my ($self)=@_;
  GetUserToolbar($self->identify);
}

sub DESTROY {
  my ($self)=@_;
  # warn "DESTROING $self\n";
  RunCallback($self->{on_destroy}) if $self->{on_destroy};
}

sub identify {
  my ($self)=@_;
  my $ident= "HTTPSearch-".$self->{object_id};
  if ($self->{config}{data}) {
    my $cfg = $self->{config}{data};
    $ident.=' ';
    if ($cfg->{id}) {
      $ident.=$cfg->{id};
    } else {
      $ident.=$cfg->{username}.'@' if $cfg->{username};
      $ident.=$cfg->{url};
    }
  }
  return $ident;
}


sub search_first {
  my ($self, $opts)=@_;
  $opts||={};
  my $query = $self->{query} = $opts->{query} || $root;
  my $query_id = (ref($query) && $query->{id}) || '';

  $self->{last_query_nodes} = [Tree_Query::Common::FilterQueryNodes($query)];
  $query = Tree_Query::Common::as_text($query,{
    resolve_types=>1,
    no_filters => $opts->{no_filters},
  }) if ref($query);
  my ($limit, $row_limit) =
    $opts->{count} ? (0,1) :
    map { $opts->{$_}||$self->{config}{pml}->get_root->get_member($_) } qw(limit row_limit);
  for (qw(limit row_limit)) {
    $opts->{$_} = $DEFAULTS{$_} unless defined($opts->{$_}) and length($opts->{$_});
    $opts->{$_} = int($opts->{$_});
  }

  if ($opts->{count}) {
    $query.="\n>> count()";
  }
  my $timeout = int($opts->{timeout}||$self->{spinbox_timeout}) || $DEFAULTS{timeout};
  my $t0 = new Benchmark;

  my $tmp = File::Temp->new( TEMPLATE => 'pmltq_XXXXX',
			     TMPDIR => 1,
			     UNLINK => 1,
			     SUFFIX => '.txt' );
  $self->update_label('Query in progress, please wait....');
  $self->{current_result}=undef;
  my $res = $self->request(query => [
    query => $query,
    format => 'text',
    limit => $limit,
    row_limit => $row_limit,
    timeout => $timeout,
   ], $tmp->filename);
  binmode $tmp, ':utf8';
  $self->{limit}=$limit;
  my $t1 = new Benchmark;
  my $time = timestr(timediff($t1,$t0));
  unless ($opts->{quiet}) {
    print STDERR "$query_id\t".$self->identify."\t$time\n";
  }
  $self->update_label('');
  unless ($res->is_success) {
    if ($res->code() eq '500') {
      ErrorMessage("Error reported by PML-TQ server:\n\n".$res->content."\n");
    } else {
      ErrorMessage($res->status_line."\n".$res->content."\n");
    }
    return;
  }
  $t0 = new Benchmark;
  my $results = [ map { chomp; [ split /\t/, $_ ] }
		  <$tmp>
#		    split /\r?\n/, Encode::decode_utf8($res->content,0) 
		];
#  unlink $tmp;
  close $tmp;
  $t1 = new Benchmark;
  print STDERR "Decoding results took ",timestr(timediff($t1,$t0)),"\n";
  my $matches = @$results;
  if ($matches) {
    my $returns_nodes=$res->header('Pmltq-returns-nodes');
    $limit=$row_limit unless $returns_nodes;
    my $how_many = (($limit and $matches==$limit) ? '>=' : '').
      $matches.($returns_nodes ? ' match'.($matches>1?'es':'') : ' row'.($matches>1?'s':''));
    return $results unless
      (!$returns_nodes and $matches<200) or
	QuestionQuery('Results',
		      $how_many,
		      'Display','Cancel') eq 'Display';
    unless ($returns_nodes) {
      my $res = Tree_Query::ShowResultTable("Results ($how_many)",$results,$query_id);
      return;
    }
    {
      $self->update_label('Preparing results ...');
      my @wins = grep { IsMinorModeEnabled('Tree_Query_Results',$_) } TrEdWindows();
      unless (@wins>0) {
	@wins = (SplitWindowVertically({no_init => 1, no_redraw=>1,no_focus=>0}));
	EnableMinorMode('Tree_Query_Results',$wins[0]);
      }
      $self->{results}=$results;
      $self->{current_result_no}=0;
      my $cur_res = $self->{current_result}=[$self->idx_to_pos($results->[0])];
      for my $win (@wins) {
	SetMinorModeData('Tree_Query_Results','index',undef,$win);
      }
      my @context=($this,$root,$grp);
      for my $res_win (@wins) {
	CloseFileInWindow($res_win);
	$grp=$res_win;
	SetCurrentWindow($grp);
	SetCurrentStylesheet(STYLESHEET_FROM_FILE);
      }
      ($this,$root,$grp)=@context;
      $self->show_result('current');
      $self->update_label;
      SetCurrentWindow($grp);
    }
  } else {
    QuestionQuery('Results','No results','OK');
  }
  return $results;
}

sub current_query {
  my ($self)=@_;
  return $self->{query};
}

sub resolve_path {
  my ($self,$path)=@_;
  return undef unless defined $path;
  my $cfg = $self->{config}{data};
  my $url = $cfg->{url};
  $url.='/' unless $url=~m{^https?://.+/};
  return qq{${url}data/$path};
}


sub map_nodes_to_query_pos {
  my ($self,$filename,$tree_number,$tree)=@_;
  return unless $self->{current_result};
  my $fn = $filename.'##'.($tree_number+1);
  my @nodes = ($tree,$tree->descendants);
  my $r = $self->{current_result};
  return {
    map { (defined($_->[1]) and $_->[1]=~/^\Q$fn\E\.(\d+)$/) ? ($nodes[$1] => $_->[0]) : () } 
      reverse # upper nodes first (optional nodes do not overwrite their parents)
      map { [$_,$self->resolve_path($r->[$_])] } 0..$#$r
  };
}

sub node_index_in_last_query {
  my ($self,$query_node)=@_;
  return unless $self->{current_result};
  return Index($self->{last_query_nodes},$query_node);
}

sub select_matching_node {
  my ($self,$query_node)=@_;
  return unless $self->{current_result};
  my $idx = Index($self->{last_query_nodes},$query_node);
  return if !defined($idx);
  my $result = $self->{current_result}->[$idx];
  $result = $self->resolve_path($result);
  foreach my $win (TrEdWindows()) {
    my $fsfile = $win->{FSFile};
    next unless $fsfile;
    my $fn = $fsfile->filename.'##'.($win->{treeNo}+1);
    next unless $result =~ /\Q$fn\E\.(\d+)$/;
    my $pos = $1;
    my $r=$fsfile->tree($win->{treeNo});
    for (1..$pos) {
      $r=$r->following();
    }
    if ($r) {
      EnableMinorMode('Tree_Query_Results',$win);
      SetCurrentNodeInOtherWin($win,$r);
      CenterOtherWinTo($win,$r);
    }
  }
  return;
}

sub get_node_types {
  my ($self)=@_;
  my $res = $self->request('nodetypes',[format=>'text']);
  unless ($res->is_success) {
    ErrorMessage($res->status_line, "\n");
    return;
  }
  return [ split /\r?\n/, Encode::decode_utf8($res->content,1) ];
}

sub configure {
  my ($self)=@_;
  my $config = $self->{config}{pml};
  GUI() && edit_config('Edit configuration',
		       $config->get_root,
		       $config->get_schema->get_root_decl->get_content_decl,
		      ) || return;
  $config->save();
  return 1;
}

sub reconfigure {
  my ($self)=@_;
  my $cfg = $self->{config}{pml};
  undef $self->{config}{pml};
  return $self->init($cfg->get_filename,$self->{config}{id}) if $cfg;
  return;
}

sub get_schema_for_query_node {
  my ($self,$node)=@_;
  my $type = Tree_Query::Common::GetQueryNodeType($node);
  return $self->get_schema($self->get_schema_name_for($type));
}

sub get_schema_for_type {
  my ($self,$type)=@_;
  return $self->get_schema($self->get_schema_name_for($type));
}

sub get_type_decl_for_query_node {
  my ($self,$node)=@_;
  return $self->get_decl_for(Tree_Query::Common::GetQueryNodeType($node));
}

sub get_decl_for {
  my ($self,$type)=@_;
  return unless $type;
  return $self->{type_decls}{$type} ||= Tree_Query::Common::QueryTypeToDecl($type,$self->get_schema($self->get_schema_name_for($type)));
}

sub get_specific_relations {
  my ($self)=@_;
  return $self->{specific_relations} if $self->{specific_relations};
  my $res = $self->request('relations',[format=>'text']);
  unless ($res->is_success) {
    ErrorMessage($res->status_line, "\n");
    return [];
  }
  return $self->{specific_relations}=[ split /\r?\n/, Encode::decode_utf8($res->content,1) ];
}

sub get_relation_target_type {
  my ($self,$node_type,$relation)=@_;
  my $rels = $self->{specific_relation_map};
  unless ($rels) {
    my $res = $self->request('relation_target_types',[format=>'text']);
    unless ($res->is_success) {
      ErrorMessage($res->status_line, "\n");
      return;
    }
    $self->{specific_relation_map} = $rels = {};
    for my $line (split /\r?\n/, Encode::decode_utf8($res->content,1)) {
      my ($type,$rel,$target)=split /:/,$line,3;
      $rels->{$type}{$rel}=$target;
    }
  }
  return $rels->{$node_type}{$relation};
}


#########################################
#### Private API

sub edit_config {
  my ($title,$data,$type,$focus)=@_;
  ToplevelFrame()->TrEdNodeEditDlg({
    title => $title,
    type => $type,
    object => $data,
    search_field => 0,
    focus => $focus,
    no_sort=>1,
    password_map => {
      password=>1,
      'configurations/http/password' => 1,
      'configurations/dbi/password' => 1,
    },
  });
}

sub get_schema_name_for {
  my ($self,$type)=@_;
  if ($self->{schema_types}{$type}) {
    return $self->{schema_types}{$type};
  }
  my $res = $self->request('type',[
    type => $type,
    format=>'text'
  ]);
  unless ($res->is_success) {
    die "Couldn't resolve schema name for type $type: ".$res->status_line."\n";
  }
  my $name = Encode::decode_utf8($res->content,1);
  $name=~s/\r?\n$//;
  return $self->{schema_types}{$type} = $name || die "Did not find schema name for type $type\n";
}

sub get_schema {
  my ($self,$name)=@_;
  return unless $name;
  if ($self->{schemas}{$name}) {
    return $self->{schemas}{$name};
  }
  my $res = $self->request('schema',[
    name => $name,
   ]);
  unless ($res->is_success) {
    die "Failed to obtain PML schema $name ".$res->status_line."\n";;
  }
  return $self->{schemas}{$name} = PMLSchema->new({string => Encode::decode_utf8($res->content,1)})
    || die "Failed to obtain PML schema $name\n";
}

sub request {
  my ($self,$type,$data,$out_file)=@_;
  my $cfg = $self->{config}{data};
  my $user = $cfg->{username};
  my $password = $cfg->{password};
  my $url = $cfg->{url};
  $url.='/' unless $url=~m{^https?://.+/};
  if (ref $data) {
    $data = [ map { Encode::_utf8_off($_); $_ } @$data ];
  } elsif (defined $data) {
    Encode::_utf8_off($data);
  }
  Encode::_utf8_off($url);
  Encode::_utf8_off($type);
  #  $ua->set_cfg($cfg);
  $ua->credentials(URI->new($url)->host_port,'PMLTQ',$user,$password)
    if (grep { defined && length } $password, $user)==2;
  my $res = eval {
    $ua->request(POST(qq{${url}${type}}, $data),$out_file ? $out_file : ());
  };
  if ($res and $res->is_error and $res->code == 401) {
    # unauthorized
    # Got authorization error 401, maybe the nonce is stale, let's try again...
    $res = eval {
      $ua->request(POST(qq{${url}${type}}, $data),$out_file ? $out_file : ());
    };
  }
  
  confess($@) if $@;
  return $res;
}

sub init {
  my ($self,$config_file,$id)=@_;
  $self->load_config_file($config_file) || return;
  my $configuration = $self->{config}{data};
  my $cfgs = $self->{config}{pml}->get_root->{configurations};
  my $cfg_type = $self->{config}{type};
  if (GUI() and !$id) {
    my @opts = ((map { $_->{id} } map $_->value, grep $_->name eq 'http', SeqV($cfgs)));
    unless (@opts) {
      my $cfg = Fslib::Struct->new();
      edit_config('Edit connection',$cfg,$cfg_type,'id') || return;
      $cfgs->push_element('http',$cfg);
      $self->{config}{pml}->save();
      push @opts, $cfg->{id};
    }

    my @sel= $configuration ? $configuration->{id} : @opts ? $opts[0] : ();
    ListQuery('Select connection',
			 'browse',
			 \@opts,
			 \@sel,
	      {
		label => { -text=> qq{Select from previously configured server connections\nor create a new one.} },
		buttons => [
		  {
		    -text => 'New',
		    -command => [sub {
				   my $l = pop @_;
				   my $cfg = Fslib::Struct->new();
				   edit_config('Edit connection',$cfg,$cfg_type,'id') || return;
				   $cfgs->push_element('http',$cfg);
				   $self->{config}{pml}->save();
				   $l->insert('end',$cfg->{id});
				   $l->see('end');
				   $l->selectionClear(0,'end');
				   $l->activate('end');
				   $l->selectionSet('end');
				 }],
		  },
		  {
		    -text => 'Clone',
		    -command => [sub {
				   my $l = pop @_;
				   my $id = $l->get('active');
				   return unless $id;
				   my $cfg = first { $_->{id} eq $id } map $_->value, grep $_->name eq 'http', SeqV($cfgs);
				   return unless $cfg;
				   $cfg = Fslib::CloneValue($cfg);
				   $cfg->{id}=undef;
				   edit_config('Edit connection',$cfg,$cfg_type,'id') || return;
				   $cfgs->push_element('http',$cfg);
				   $self->{config}{pml}->save();
				   $l->insert('end',$cfg->{id});
				   $l->see('end');
				   $l->selectionClear(0,'end');
				   $l->activate('end');
				   $l->selectionSet('end');
				 }],
		  },
		  {
		    -text => 'Edit',
		    -command => [sub {
				   my $l = pop @_;
				   my $id = $l->get('active');
				   if ($id) {
				     my $cfg = first { $_->{id} eq $id } map $_->value, grep $_->name eq 'http', SeqV($cfgs);
				     edit_config('Edit connection',$cfg,$cfg_type,'url') || return;
				     $self->{config}{pml}->save();
				     if ($cfg->{id} ne $id) {
				       $l->insert('active',$cfg->{id});
				       $l->delete('active');
				     }
				   }
				 }],
		  },
		  {
		    -text => 'Remove',
		    -command => [sub {
				   my $l = pop @_;
				   my $id = $l->get('active');
				   if ($id and
				       QuestionQuery('Delete connection',
						     qq{Really delete connection '$id'?},
						     'Delete','Cancel') eq 'Delete') {
				     $l->delete('active');
				     my $cfg = first { $_->{id} eq $id } map $_->value, grep $_->name eq 'http', SeqV($cfgs);
				     if ($cfg) {
				       $cfgs->delete_value($cfg);
				       $self->{config}{pml}->save();
				     }
				   }
				 }],
		  },
		 ],
	      }
	     ) || return;
    ($id) = @sel;
  }
  return unless $id;
  my $cfg;
#   if ($id eq ' CREATE NEW CONNECTION ') {
#     $cfg = Fslib::Struct->new();
#     GUI() && edit_config('Edit connection',$cfg,$cfg_type,'id') || return;
#     $cfgs->push_element('http',$cfg);
#     $self->{config}{pml}->save();
#     $id = $cfg->{id};
#   } else {
    $cfg = first { $_->{id} eq $id } map $_->value, grep $_->name eq 'http', SeqV($cfgs);
    die "Didn't find configuration '$id'" unless $cfg;
#  }
  $self->{config}{id} = $id;
  unless (defined $cfg->{url}) {
    if (GUI()) {
      edit_config('Edit connection',$cfg,$cfg_type,'url') || return;
    } else {
      die "The configuration $id does not specify a URL\n";
    }
    $self->{config}{pml}->save();
  }
  $self->{config}{data} = $cfg;
  $self->{spinbox_timeout}=int($self->{config}{pml}->get_root->get_member('timeout')) || $DEFAULTS{timeout};
  $self->check_server_version;
}


sub prepare_results {
  my ($self,$dir)=@_;
  my $no = $self->{current_result_no};
  if ($dir eq 'prev') {
    if ($no>0) {
      $self->{current_result_no} = --$no;
      $self->{current_result}=[$self->idx_to_pos($self->{results}[$no])];
    }
  } elsif ($dir eq 'next') {
    if ($no<$#{$self->{results}}) {
      $self->{current_result_no} = ++$no;
      $self->{current_result}=[$self->idx_to_pos($self->{results}[$no])];
    }
  }
}

sub have_results {
  my ($self) = @_;
  ($self->{current_result} and $self->{last_query_nodes}
     and @{$self->{current_result}} and @{$self->{last_query_nodes}}) ? 1 : 0;
}

sub get_nth_result_filename {
  my ($self,$idx)=@_;
  $self->resolve_path($self->{current_result}[$idx])
}




sub update_label {
  my ($self,$text)=@_;
  if (defined $text) {
    ${$self->{label}}=$text;
  } else {
    my $no = $self->{current_result_no}+1;
    my $limit = $self->{limit}||0;
    my $matches = $self->{results} ? @{$self->{results}} : 0;
    ${$self->{label}} = qq{$no of $matches}.($matches==$limit ? '+' : '');
  }
  my $tb=$self->toolbar;
  $tb->update if $tb;
  return;
}

sub check_server_version {
  my ($self,$server_version)=@_;
  my $res = $self->request('version',
			   [ client_version=>$VERSION,
			     format=>'text',
			    ]);
  unless ($res->is_success) {
    die "Failed to connect to server (the server is incompatible or down)!\n".$res->status_line."\n";;
  }
  my $v = $res->content;
  $v=~s/\r?\n$//;
  if ($v!~/^(IN)?COMPATIBLE\s+(\S+)/ or $1) {
    die "Server requires a newer version of this client; please upgrade the 'pmltq' TrEd extension!!\n";
  } elsif (PMLSchema::cmp_revisions($MIN_SERVER_VERSION,$2)>0) {
    die "Server is too old for this client; please ask your PML-TQ server administrator to upgrade!\n";
  }
  return 1;
}

sub idx_to_pos {
  my ($self,$idx_list)=@_;
  my @res;
  for my $ident (@$idx_list) {
    unless ($ident =~ m{//}) {
      my $res = $self->request('node',
			       [ idx=>$ident,
				 format=>'text',
				]);
      unless ($res->is_success) {
	die "Failed to resolve $ident!\n".$res->status_line."\n";;
      }
      my $f = $res->content;
      $f=~s/\r?\n$//;
      print "$f\n";
      push @res, $f;
    } else {
      push @res, undef;
    }
  }
  return @res;
}

sub load_config_file {
  my ($self,$config_file)=@_;
  if (!$self->{config}{pml} or ($config_file and
				$config_file ne $self->{config}{pml}->get_filename)) {
    if ($config_file) {
      die "Configuration file '$config_file' does not exist!" unless -f $config_file;
      $self->{config}{pml} = PMLInstance->load({ filename=>$config_file });
    } else {
      $config_file ||= FindInResources('treebase.conf');
      if (-f $config_file) {
	$self->{config}{pml} = PMLInstance->load({ filename=>$config_file });
      } else {
	my $tred_d = File::Spec->catfile($ENV{HOME},'.tred.d');
	mkdir $tred_d unless -d $tred_d;
	$config_file = File::Spec->catfile($tred_d,'treebase.conf');
	$self->{config}{pml} = PMLInstance->load({ string => $DEFAULTS{pmltq_config},
					      filename=> $config_file});
	$self->{config}{pml}->save();
      }
    }
  }
  $self->{config}{type} = $self->{config}{pml}->get_schema->get_type_by_name('http-config.type')->get_content_decl;
  return $self->{config}{pml};
}

sub get_results {
  my $self = shift;
  return $self->{results} || [];
}

sub get_query_nodes {
  my $self = shift;
  return $self->{query_nodes};
}


my ($userlogin) = (getlogin() || ($^O ne 'MSWin32') && getpwuid($<) || 'unknown');
$DEFAULTS{pmltq_config} = <<"EOF";
<pmltq_config xmlns="http://ufal.mff.cuni.cz/pdt/pml/">
  <head>
    <schema href="treebase_conf_schema.xml"/>
  </head>
  <limit>$DEFAULTS{limit}</limit>
  <row_limit>$DEFAULTS{row_limit}</row_limit>
  <timeout>$DEFAULTS{timeout}</timeout>
  <configurations>
<!--
    <http id="localhost">
      <url>http://localhost:8121/</host>
      <username>$userlogin</username>
      <password></password>
    </http>
-->
  </configurations>
</pmltq_config>
EOF

} # HTTP
