# -*- cperl -*-
################
### HTTP interface to pmltq server
################

#### TrEd interface to PMLTQ::Evaluator
{

  {

    package TrEd::PMLTQ::UserAgent;
    use base qw(LWP::UserAgent);

    sub credentials {
      shift;    # self
      return $Treex::PML::IO::lwp_user_agent->credentials(@_);
    }

    sub get_basic_credentials {
      return;
    }
  }

  package PMLTQ::HTTPSearch;
  use base qw(PMLTQ::TrEd);
  use Benchmark;
  use Carp;
  use strict;
  use warnings;
  use Scalar::Util qw(weaken);
  use HTTP::Request::Common;
  use File::Temp;
  use Encode;
  use Treex::PML::Schema;
  use URI;
  use URI::WithBase;
  use JSON;

  my $JSON = JSON->new->utf8;
  my $API_VERSION = 'v1';

  use vars qw($VERSION $MIN_SERVER_VERSION);
  $VERSION            = "0.3";
  $MIN_SERVER_VERSION = "0.4";
  my $ua = $Treex::PML::IO::lwp_user_agent;    #TrEd::PMLTQ::UserAgent->new();

  #use LWP::UserAgent;

  BEGIN { import TredMacro }

  our %DEFAULTS = (
    row_limit => 5000,
    limit     => 100,
    timeout   => 30,
  );

  $PMLTQ::HTTPSearchPreserve::object_id = 0;    # different NS so that TrEd's reload-macros doesn't clear it

  # my $ua = $Treex::PML::IO::lwp_user_agent;
  #$ua = Treex::PML::IO::UserAgent->new;
  #$ua->agent("TrEd/1.0 ");

  sub new {
    my ( $class, $opts ) = @_;
    $opts ||= {};
    my $self = bless {
      object_id       => $PMLTQ::HTTPSearchPreserve::object_id++,
      config          => { pml => $opts->{config_pml}, },
      query           => undef,
      query_nodes     => undef,
      results         => undef,
      limit           => undef,
      spinbox_timeout => undef,
    }, $class;
    $self->init( $opts->{config_file}, $opts->{config_id} ) || return;
    my $ident = $self->identify;
    {
      my $tb;
      ( $tb, $self->{label} ) = PMLTQ::CreateSearchToolbar($ident);
      $tb->Label( -text => "Timeout:" )->pack( -side => 'left', -padx => 10 );
      my $b = $tb->Spinbox(
        -background   => 'white',
        -widt         => 3,
        -from         => 10,
        -to           => 300,
        -increment    => 5,
        -textvariable => \$self->{spinbox_timeout},
      )->pack( -side => 'left', );
      AttachTooltip( $b, 'Select timeout in seconds.' );
    }
    $self->{on_destroy} = MacroCallback(
      sub {
        DestroyUserToolbar($ident);
        for my $win ( $self->get_result_windows ) {
          CloseFileInWindow($win);
          CloseWindow($win);
        }
        ChangingFile(0);
      } );
    return $self;
  }

  sub toolbar {
    my ($self) = @_;
    GetUserToolbar( $self->identify );
  }

  sub DESTROY {
    my ($self) = @_;

    # warn "DESTROING $self\n";
    RunCallback( $self->{on_destroy} ) if $self->{on_destroy};
  }

  sub identify {
    my ($self) = @_;
    my $ident = "HTTPSearch-" . $self->{object_id};
    if ( $self->{config}{data} ) {
      my $cfg = $self->{config}{data};
      $ident .= ' ';
      my $title = Treex::PML::Instance::get_data( $cfg, 'cached_description/title' );
      if ($title) {
        $ident .= $title;
      }
      else {
        $ident .= $cfg->{username} . '@' if $cfg->{username};
        $ident .= $cfg->{url};
      }
    }
    return $ident;
  }

  sub search_first {
    my ( $self, $opts ) = @_;
    $opts ||= {};
    my $query = $self->{query} = $opts->{query} || $root;
    my $query_id = ( ref($query) && $query->{id} ) || '';

    $self->{last_query_nodes}
      = [ PMLTQ::Common::FilterQueryNodes($query) ];
    $query = PMLTQ::Common::as_text(
      $query,
      { resolve_types => 1,
        no_filters    => $opts->{no_filters},
      } ) if ref($query);
    my ( $limit, $row_limit )
      = $opts->{count}
      ? ( 0, 1 )
      : map { $opts->{$_} || $self->{config}{pml}->get_root->get_member($_) } qw(limit row_limit);

    for (qw(limit row_limit)) {
      $opts->{$_} = $DEFAULTS{$_}
        unless defined( $opts->{$_} )
        and length( $opts->{$_} );
      $opts->{$_} = int( $opts->{$_} );
    }

    if ( $opts->{count} ) {
      $query .= "\n>> count()";
    }
    my $timeout = int( $opts->{timeout} || $self->{spinbox_timeout} )
      || $DEFAULTS{timeout};
    $self->update_label('Query in progress, please wait....');
    $self->{current_result} = undef;
    my ($res, $content) = $self->request_post(
      query => {
        query     => $query,
        limit     => $limit,
        #row_limit => $row_limit,
        timeout   => $timeout,
      }
    );
    $self->{limit} = $limit;

    unless ( $opts->{quiet} ) {
      print STDERR "$query_id\t" . $self->identify . "\n";
    }
    $self->update_label('');
    unless ( $res->is_success ) {
      if ( $res->code() eq '500' ) {
        ErrorMessage( "Error reported by PML-TQ server:\n\n" . $res->content . "\n" );
      }
      else {
        ErrorMessage( $content->{error} . "\n" );
      }
      return;
    }

    my $matches = $content->{results};
    if ($matches) {
      my $returns_nodes = $content->{names};
      $limit = $row_limit unless $returns_nodes;
      my $how_many
        = ( ( $limit and $matches == $limit ) ? '>=' : '' )
        . $matches
        . (
        $returns_nodes
        ? ' match' . ( $matches > 1 ? 'es' : '' )
        : ' row' . ( $matches > 1 ? 's' : '' ) );
      unless ($returns_nodes) {
        if ( @$matches >= 1000 ) {
          my $ans = QuestionQuery( 'Results', $how_many, 'Display', 'Save to File', 'Cancel' );
          if ( $ans eq 'Save to File' ) {
            PMLTQ::SaveResults( $matches, $query_id );
            return $matches;
          }
          elsif ( $ans eq 'Cancel' ) {
            return $matches;
          }
        }
        PMLTQ::ShowResultTable( "Results ($how_many)", $matches, $query_id );
        return;
      }
      {
        $self->update_label('Preparing results ...');
        my @wins
          = grep { IsMinorModeEnabled( 'PMLTQ_Results', $_ ) } TrEdWindows();
        unless ( @wins > 0 ) {
          @wins = ( SplitWindowVertically( { no_init => 1, no_redraw => 1, no_focus => 0 } ) );
          EnableMinorMode( 'PMLTQ_Results', $wins[0] );
        }
        $self->{results}           = $matches;
        $self->{current_result_no} = 0;
        my $cur_res = $self->{current_result} = [ $self->idx_to_pos( $matches->[0] ) ];
        for my $win (@wins) {
          SetMinorModeData( 'PMLTQ_Results', 'index', undef, $win );
        }
        my @context = ( $this, $root, $grp );
        for my $res_win (@wins) {
          CloseFileInWindow($res_win);
          $grp = $res_win;
          SetCurrentWindow($grp);
          SetCurrentStylesheet(STYLESHEET_FROM_FILE);
        }
        ( $this, $root, $grp ) = @context;
        $self->show_result('current');
        $self->update_label;
        SetCurrentWindow($grp);
      }
    }
    else {
      QuestionQuery( 'Results', 'No results', 'OK' );
    }
    return $matches;
  }

  sub current_query {
    my ($self) = @_;
    return $self->{query};
  }

  sub resolve_path {
    my ( $self, $path ) = @_;
    return undef unless defined $path;
    my $url = $self->_request_url('data');
    return $url->as_string . '/' . $path;
  }

  sub map_nodes_to_query_pos {
    my ( $self, $filename, $tree_number, $tree, $fsfile ) = @_;
    return unless $self->{current_result};
    my $fn = $filename . '##' . ( $tree_number + 1 );
    my $is_treex = $tree && $tree->isa('Treex::Core::Bundle');
    my @nodes = ( $tree, $tree->descendants );
    my $r = $self->{current_result};

    my %map;
    for my $res (
      reverse    # upper nodes first (optional nodes do not overwrite their parents)
      map { [ $_, $self->resolve_path( $r->[$_] ) ] } 0 .. ( @$r - 1 ) )
    {
      if ( defined( $res->[1] ) ) {
        eval {
          my $result = $self->{results}->[ $self->{current_result_no} ];
          my ($id) = $result->[ $res->[0] ] =~ m/@(.*)$/;
          $map{$id} = $res->[0] if $id;
        };
      }
    }
    return \%map;

    # use Data::Dumper;
    # print Dumper($r);

    # my %map;
    # for my $res (reverse # upper nodes first (optional nodes do not overwrite their parents)
    #        map { [$_,$self->resolve_path($r->[$_])] } 0..(@$r-1) ) {
    #     if (defined($res->[1])) {
    #         if ($is_treex) {
    #             my $id;
    #             eval {
    #                 my $result = $self->{results}->[$self->{current_result_no}];
    #                 ($id) = $result->[$res->[0]] =~ m/@(.*)$/;
    #                 #$tree->get_document->get_node_by_id($id)
    #             };
    #             $map{$id} = $res->[0] if $id;
    #         } elsif ($res->[1]=~/^\Q$fn\E\.([0-9]+)$/) {
    #             $map{$nodes[$1]->{id}} = $res->[0] if $nodes[$1] and $nodes[$1]->{id};
    #         } elsif ($fsfile and $res->[1]=~/^\Q$filename\E#([^#0-9][^#]*)$/) {
    #             #my $n = PML::GetNodeByID($1,$fsfile);
    #             $map{$1} = $res->[0] if $1;
    #         }
    #     }
    # }
    # return \%map;
  }

  sub node_index_in_last_query {
    my ( $self, $query_node ) = @_;
    return unless $self->{current_result};
    return Index( $self->{last_query_nodes}, $query_node );
  }

  sub select_matching_node {
    my ( $self, $query_node ) = @_;
    return unless $self->{current_result};
    my $idx = Index( $self->{last_query_nodes}, $query_node );
    return if !defined($idx);
    my $result = $self->{current_result}->[$idx];
    $result = $self->resolve_path($result);
    foreach my $win ( TrEdWindows() ) {
      my $fsfile = $win->{FSFile};
      next unless $fsfile;
      my $filename = $fsfile->filename;
      my $r;
      my $fn = $filename . '##' . ( $win->{treeNo} + 1 );
      if ( $result =~ /\Q$fn\E\.([0-9]+)$/ ) {
        my $pos = $1;
        $r = $fsfile->tree( $win->{treeNo} );
        for ( 1 .. $pos ) {
          $r = $r && $r->following();
        }
      }
      elsif ( $result =~ /\Q$filename\E\#([^#0-9][^#]*)$/ ) {
        $r = PML::GetNodeByID( $1, $fsfile );
        undef $r
          unless ( $win->{Nodes}
          and first { $_ == $r } @{ $win->{Nodes} } );
      }
      if ($r) {
        EnableMinorMode( 'PMLTQ_Results', $win );
        SetCurrentNodeInOtherWin( $win, $r );
        CenterOtherWinTo( $win, $r );
      }
    }
    return;
  }

  sub get_node_types {
    my ( $self, $schema_name ) = @_;
    my ($res, $content) = $self->request_api(
      'node-types',
      [ ( defined($schema_name)
          ? ( layer => $schema_name )
          : ()
        ),
      ] );
    unless ( $res->is_success ) {
      ErrorMessage( $content->{error}, "\n" ) if $content;
      ErrorMessage( $res->status_line, "\n" ) unless $content;
      return;
    }
    return $content->{types};
  }

  sub configure {
    my ($self) = @_;
    my $config = $self->{config}{pml};
    GUI()
      && edit_config( 'Edit configuration', $config->get_root, $config->get_schema->get_root_decl->get_content_decl, )
      || return;
    $config->save();
    return 1;
  }

  sub reconfigure {
    my ($self) = @_;
    my $cfg = $self->{config}{pml};
    undef $self->{config}{pml};
    return $self->init( $cfg->get_filename, $self->{config}{id} ) if $cfg;
    return;
  }

  sub get_schema_for_query_node {
    my ( $self, $node ) = @_;
    my $type = PMLTQ::Common::GetQueryNodeType($node);
    return $self->get_schema( $self->get_schema_name_for($type) );
  }

  sub get_schema_for_type {
    my ( $self, $type ) = @_;
    return $self->get_schema( $self->get_schema_name_for($type) );
  }

  sub get_type_decl_for_query_node {
    my ( $self, $node ) = @_;
    return $self->get_decl_for( PMLTQ::Common::GetQueryNodeType($node) );
  }

  sub get_decl_for {
    my ( $self, $type ) = @_;
    return unless $type or $type =~ /:\*$/;
    return $self->{type_decls}{$type}
      ||= PMLTQ::Common::QueryTypeToDecl( $type, $self->get_schema( $self->get_schema_name_for($type) ) );
  }

  sub get_user_defined_relations {
    my ( $self, $type ) = @_;
    if ($type) {
      return $self->{type_user_defined_relations}{$type}
        if $self->{type_user_defined_relations}
        and $self->{type_user_defined_relations}{$type};
      my ($res, $content) = $self->request_api(
        'relations',
        [ category => 'implementation',
          type     => $type,
        ] );
      unless ( $res->is_success ) {
        ErrorMessage( $res->status_line, "\n" );
        return [];
      }
      return $self->{type_user_defined_relations}{$type} = $content->{relations};
    }
    else {
      return $self->{user_defined_relations}
        if $self->{user_defined_relations};
      my ($res, $content) = $self->request_api(
        'relations',
        [ category => 'implementation' ] 
      );
      unless ( $res->is_success ) {
        ErrorMessage( $res->status_line, "\n" );
        return [];
      }
      return $self->{user_defined_relations} = $content->{relations};
    }
  }

  sub get_pmlrf_relations {
    my ( $self, $type ) = @_;
    if ($type) {
      return $self->{type_pmlrf_relations}{$type}
        if $self->{type_pmlrf_relations}
        and $self->{type_pmlrf_relations}{$type};
      my ($res, $content) = $self->request_api(
        'relations',
        [ category => 'pmlrf',
          type     => $type,
        ] );
      unless ( $res->is_success ) {
        ErrorMessage( $res->status_line, "\n" );
        return [];
      }
      return $self->{type_pmlrf_relations}{$type} = $content->{relations};
    }
    else {
      return $self->{pmlrf_relations} if $self->{pmlrf_relations};
      my ($res, $content) = $self->request_api(
        'relations',
        [ category => 'pmlrf',
          ( defined($type) ? ( type => $type ) : () ) ] );
      unless ( $res->is_success ) {
        ErrorMessage( $res->status_line, "\n" );
        return [];
      }
      return $self->{pmlrf_relations} = $content->{relations};
    }
  }

  sub get_specific_relations {
    my ( $self, $type ) = @_;
    if ($type) {
      return $self->{type_specific_relations}{$type}
        if $self->{type_specific_relations}
        and $self->{type_specific_relations}{$type};
      my ($res, $content) = $self->request_api(
        'relations',
        [ type   => $type ] 
      );
      unless ( $res->is_success ) {
        ErrorMessage( $res->status_line, "\n" );
        return [];
      }
      return $self->{type_specific_relations}{$type} = $content->{relations};
    }
    else {
      return $self->{specific_relations} if $self->{specific_relations};
      my ($res, $content) = $self->request_api(
        'relations',
        [ format => 'text',
          ( defined($type) ? ( type => $type ) : () ) ] );
      unless ( $res->is_success ) {
        ErrorMessage( $res->status_line, "\n" );
        return [];
      }
      return $self->{specific_relations} = $content->{relations};
    }
  }

  sub get_relation_target_type {
    my ( $self, $node_type, $relation, $category ) = @_;
    my $map_name;
    if ( $category eq 'implementation' ) {
      $map_name = 'user_defined_relation_map';
    }
    elsif ( $category eq 'pmlrf' ) {
      $map_name = 'pmlrf_relation_map';
    }
    else {
      $map_name = 'specific_relation_map';
    }
    my $rels = $self->{$map_name};
    unless ( $rels and $rels->{$node_type} ) {
      my ($res, $content) = $self->request_api(
        'relation-target-types',
        [ category => ( $category || '' ),
          type     => $node_type,
        ] );
      unless ( $res->is_success ) {
        ErrorMessage( $res->status_line, "\n" );
        return;
      }
      my $R = {};
      $self->{$map_name}{$node_type} = $R;
      for my $item ( @{$content->{map}} ) {
        my ( $type, $rel, $target ) = @$item;
        $R->{$rel} = $target if $type eq $node_type;
      }
      return $R->{$relation};
    }
    return $rels->{$node_type}{$relation};
  }

#########################################
#### Private API

  sub edit_config {
    my ( $title, $data, $type, $focus, $top ) = @_;
    $top ||= ToplevelFrame();
    $top->TrEdNodeEditDlg(
      { enable_callback => sub {
          my ($path) = @_;
          return 0 if $path =~ m{^/?cached_|^/?id$};
          return 1;
        },
        title        => $title,
        type         => $type,
        object       => $data,
        search_field => 0,
        focus        => $focus,
        no_sort      => 0,
        password_map => {
          password                       => 1,
          'configurations/http/password' => 1,
          'configurations/dbi/password'  => 1,
        },
      } );
  }

  sub get_schema_name_for {
    my ( $self, $type ) = @_;
    if ( $self->{schema_types}{$type} ) {
      return $self->{schema_types}{$type};
    }
    my ($res, $content) = $self->request_api( 'type', [ type => $type ] );
    unless ( $res->is_success ) {
      die "Couldn't resolve schema name for type $type: " . $res->status_line . "\n";
    }

    return $self->{schema_types}{$type} = $content->{name}
      || die "Did not find schema name for type $type\n";
  }

  sub get_schema {
    my ( $self, $name ) = @_;
    return unless $name;
    if ( $self->{schemas}{$name} ) {
      return $self->{schemas}{$name};
    }
    my ($res, $content) = $self->request_file( 'schema', [ name => $name ] );
    unless ( $res->is_success ) {
      die "Failed to obtain PML schema $name " . $res->status_line . "\n";
    }
    return $self->{schemas}{$name} = Treex::PML::Schema->new( { string => $content } )
      || die "Failed to obtain PML schema $name\n";
  }

  sub _request_url {
    my ( $self, @type ) = @_;

    my $cfg      = $self->{config}{data};
    my $url      = URI::WithBase->new('/', $cfg->{url});
    my $treebank = $cfg->{treebank};
    $url->path_segments($API_VERSION, 'treebanks', $treebank, @type);
    #print STDERR "$url\n";

    return $url->abs;
  }

  sub _decode_responce {
    my ( $self, $res ) = @_;

    my $json = $res->decoded_content;
    confess ($json) unless ($res->is_success);
    return $json ? $JSON->decode($json) : undef;
  }

  sub request_api {
    my ( $self, $type, $data, $out_file ) = @_;
    
    my $cfg = $self->{config}{data};
    $data ||= [];

    my $url = $self->_request_url($type);
    $url->query_form(@$data);

    if (wantarray) {
      my ($res, $content) = $self->request_server( $cfg, $url->as_string, $out_file );
      return ($res, $content);
    }
    else {
      my $res = $self->request_server( $cfg, $url->as_string, $out_file );
      return $res;
    }
  }

  sub request_post {
    my ( $self, $type, $data, $out_file ) = @_;

    my $cfg = $self->{config}{data};
    my $url = $self->_request_url($type);

    my $req = HTTP::Request->new(POST => $url->as_string);
    $req->content_type('application/json');
    $req->content($JSON->encode($data)) if $data;

    my $res = eval { $ua->request( $req, $out_file ? $out_file : () ); };
    confess($@) if $@;

    if (!$out_file and wantarray)  {
      return ($res, $self->_decode_responce($res));
    }
    return $res;
  }

  sub request_file {
    my ( $self, $type, $data, $out_file ) = @_;

    my $res = $self->request_api( $type, $data, $out_file );
    if (!$out_file and wantarray)  {
      return ($res, $res->decoded_content);
    }
    return $res;
  }

  sub request_server {
    my ( $self, $cfg, $url, $out_file ) = @_;

    my $user     = $cfg->{username} || '';
    my $password = $cfg->{password} || '';

    #print STDERR "$user:$password @ $url";

    $ua->credentials( URI->new($url)->host_port, 'PMLTQ', $user, $password )
      if ( grep { defined && length } $password, $user ) == 2;

    my $res = eval { $ua->get( "${url}", $out_file ? (':content_file' => $out_file) : () ); };
    confess($@) if $@;
    
    if (!$out_file and wantarray)  {
      return ($res, $self->_decode_responce($res));
    }
    return $res;
  }

  sub _cfg_label {
    my ($cfg) = @_;
    return $cfg->{id} . " : "
      . ( Treex::PML::Instance::get_data( $cfg, 'cached_description/title' ) || '' );
  }

  sub _cfg_id_from_label {
    my ($label) = @_;
    return unless $label;
    $label =~ s/ : .*//s;
    return $label;
  }

  sub init {
    my ( $self, $config_file, $id ) = @_;
    $self->load_config_file($config_file) || return;
    my $configuration = $self->{config}{data};

    my $cfgs = ( $self->{config}{pml}->get_root->{configurations} ||= Treex::PML::Factory->createSeq );
    my $cfg_type = $self->{config}{type};
    if ( GUI() and !$id ) {
      require Tk::QueryDialog;
      my @confs = ( map $_->value, grep $_->name eq 'http', SeqV($cfgs) );

      unless (@confs) {
        my $cfg = Treex::PML::Factory->createStructure();
        $cfg->{id} = $self->_new_cfg_id($cfgs);
        my $valid = 0;
        do {
          edit_config( 'Edit connection', $cfg, $cfg_type, 'url' )
            || return;

          $valid = (grep {defined length} ($cfg->{url}, $cfg->{treebank})) == 2;
          unless ($valid) {
            ErrorMessage('Please enter at least server url and treebank name to continue');
          }
        }
        until ($valid);
        
        $cfgs->push_element( 'http', $cfg );
        push @confs, $cfg;
        if ( _update_service_info( $self, $cfg, 0, undef ) ) {
          if ( _add_related_service( $self, $cfgs, $cfg, undef ) ) {
            @confs = ( map $_->value, grep $_->name eq 'http', SeqV($cfgs) );
          }
        }
        else {
          $self->{config}{pml}->save();
        }
      }

      my $d = ToplevelFrame()->DialogBox(
        -title   => 'Select Connection',
        -buttons => [ 'Select', 'Add Service', 'New URL', 'Info', 'Edit', 'Remove', 'Close' ],
      );
      $d->add( 'Label', -text => 'Select, create, or modify connections to PML-TQ servers:', )
        ->pack( -expand => 1, -fill => 'x', -pady => 10 );
      my $hlist = $d->Scrolled(
        'HList',
        -scrollbars => 'osoe',
        -columns    => 3,
        -itemtype   => 'text',
        -selectmode => 'browse',
        -width      => 0,
        -height     => ( @confs > 10 ? scalar(@confs) : 10 ),
      )->pack( -expand => 1, -fill => 'both' );
      eval { $d->configure( -focus => $hlist ); };
      $self->_add_cfg_items_to_hlist( $hlist, \@confs, $configuration && $configuration->{id} );
      $hlist->bind( '<Alt-KeyPress>',     sub { } );
      $hlist->bind( '<Meta-KeyPress>',    sub { } );
      $hlist->bind( '<Control-KeyPress>', sub { } );
      $hlist->bind(
        '<KeyPress>',
        [ sub {
            my ( $hlist, $K ) = @_;
            return unless $K =~ /^[a-z]$/i;
            my $anchor = $hlist->info('anchor') || return;
            my $id = $hlist->info( next => $anchor );
            for ( 0, 1 ) {
              while ( $id and $id ne $anchor ) {
                return unless $id;
                if ( $id =~ /^\Q$K/ ) {
                  $hlist->see($id);
                  $hlist->selectionClear();
                  $hlist->selectionSet($id);
                  $hlist->anchorSet($id);
                  Tk->break;
                  return;
                }
                $id = $hlist->info( next => $id );
              }
              ($id) = $hlist->info('children');
            }
            Tk->break;
            return;
          },
          Tk::Ev("K")
        ],
      );
      $d->BindEscape();
      $d->BindButtons();
      $d->Subwidget('B_Add Service')->configure(
        -command => [
          sub {
            my ( $self, $cfgs, $cfg_type, $hlist ) = @_;
            if ( $hlist->info('anchor') ) {
              _add_related_service( $self, $cfgs, undef, $hlist );
            }
            else {
              _new_service_url( $self, $cfgs, $cfg_type, $hlist );
            }
          },
          $self,
          $cfgs,
          $cfg_type,
          $hlist
        ],
      );
      $d->Subwidget('B_New URL')->configure( -command => [ \&_new_service_url, $self, $cfgs, $cfg_type, $hlist ], );
      $d->Subwidget('B_Info')->configure(
        -command => [
          sub {
            my ( $self, $l ) = @_;
            my $cfg = $l->info( 'data', $l->info('anchor') );
            return unless $cfg;
            return _update_service_info( $self, $cfg, 1, $l );
          },
          $self,
          $hlist
        ],
      );
      $d->Subwidget('B_Edit')->configure(
        -command => [
          sub {
            my ( $self, $cfgs, $cfg_type, $l ) = @_;
            my $id = $l->info('anchor');
            if ($id) {
              my $cfg = $l->info( 'data', $id );
              return unless $cfg;
              edit_config( 'Edit connection', $cfg, $cfg_type, 'url', $l->toplevel )
                || return;
              if ( _update_service_info( $self, $cfg, 0, $l ) ) {
                _add_related_service( $self, $cfgs, $cfg, $l );
              }
              else {
                $self->_add_cfg_item( $l, $id, $cfg );
                $self->{config}{pml}->save();
              }
            }
          },
          $self,
          $cfgs,
          $cfg_type,
          $hlist
        ] );
      $d->Subwidget('B_Remove')->configure(
        -command => [
          sub {
            my ( $self, $cfgs, $l ) = @_;
            my $id    = $l->info('anchor');
            my $cfg   = $l->info( 'data', $id );
            my $title = Treex::PML::Instance::get_data( $cfg, 'cached_description/title' )
              || '';
            if (
              $id
              and $l->QuestionQuery(
                -title   => 'Remove connection',
                -label   => qq{Really remove connection $title ($id)?},
                -buttons => [ 'Remove', 'Cancel' ],
              ) eq 'Remove'
              )
            {
              my $next = $l->info( 'next' => $id )
                || $l->info( 'prev' => $id );
              $l->delete( entry => $id );
              $l->anchorSet($next) if $next;
              $cfgs->delete_value($cfg);
              $self->{config}{pml}->save();
            }
          },
          $self,
          $cfgs,
          $hlist
        ],
      );
      return unless $d->Show() eq 'Select';
      $id = $hlist->info('anchor');
      $d->destroy;
    }

    return unless $id;
    my $cfg;

    $cfg = first { $_->{id} eq $id } map $_->value, grep $_->name eq 'http', SeqV($cfgs);
    die "Didn't find configuration '$id'" unless $cfg;
    $self->{config}{id} = $id;
    unless ( defined $cfg->{url} ) {
      if ( GUI() ) {
        edit_config( 'Edit connection', $cfg, $cfg_type, 'url' )
          || return;
      }
      else {
        die "The configuration $id does not specify a URL\n";
      }
      $self->{config}{pml}->save();
    }
    $self->{config}{data} = $cfg;
    $self->{spinbox_timeout} = int( $self->{config}{pml}->get_root->get_member('timeout') )
      || $DEFAULTS{timeout};
    #$self->check_server_version;
  }

  sub _new_service_url {
    my ( $self, $cfgs, $cfg_type, $l ) = @_;
    my $cfg = Treex::PML::Factory->createStructure();
    $cfg->{id} = $self->_new_cfg_id($cfgs);
    edit_config( 'Edit connection', $cfg, $cfg_type, 'url', $l->toplevel )
      || return;
    $cfgs->push_element( 'http', $cfg );
    my $id = $cfg->{id};
    $self->_add_cfg_item( $l, $id, $cfg );
    $l->see($id);
    $l->anchorSet($id);

    if ( _update_service_info( $self, $cfg, 0, $l ) ) {
      _add_related_service( $self, $cfgs, $cfg, $l );
    }
    else {
      $self->{config}{pml}->save();
    }
  }

  sub _add_cfg_items_to_hlist {
    my ( $self, $hlist, $confs, $anchor ) = @_;
    for my $c (@$confs) {
      $self->_add_cfg_item( $hlist, $c->{id}, $c );
    }
    if ( $anchor and $hlist->info( 'exists', $anchor ) ) {
      $hlist->anchorSet($anchor);
    }
    elsif (@$confs) {
      $hlist->anchorSet( $confs->[0]->{id} );
    }
  }

  sub _add_cfg_item {
    my ( $self, $hlist, $item, $cfg ) = @_;
    my @cols = map Treex::PML::Instance::get_data( $cfg, $_ ), qw(id cached_description/title url);
    my $exists = $hlist->info( 'exists', $item );
    if ( $item ne $cfg->{id} ) {
      if ( $exists and $hlist->info( 'exists', $cfg->{id} ) ) {
        $hlist->delete( entry => $item );
        $item = $cfg->{id};
        $hlist->anchorSet($item);
      }
      elsif ($exists) {
        $hlist->add( $cfg->{id}, -data => $cfg, -after => $item );
        $hlist->delete( entry => $item );
        $item = $cfg->{id};
        $hlist->anchorSet($item);
        $exists = 0;
      }
    }
    elsif ($exists) {
      $hlist->entryconfigure( $item, -data => $cfg );
    }
    else {
      $hlist->add( $item, -data => $cfg );
    }
    if ($exists) {
      for my $i ( 0 .. $#cols ) {
        $hlist->itemConfigure( $item, $i, -text => $cols[$i] );
      }
    }
    else {
      for my $i ( 0 .. $#cols ) {
        $hlist->itemCreate( $item, $i, -text => $cols[$i] );
      }
    }
  }

  sub _update_service_info {
    my ( $self, $cfg, $show, $l ) = @_;
    my $top = $l ? $l->toplevel : ToplevelFrame();
    my $anchor = $l && $l->info('anchor');

    my $url      = URI::WithBase->new('/', $cfg->{url});
    my $treebank = $cfg->{treebank};
    $url->path_segments($API_VERSION, 'treebanks', $treebank, 'metadata');
    my ($res, $content) = $self->request_server( $cfg, $url->abs->as_string );

    unless ($res->is_success) {
      ErrorMessage( "Failed to connect to server to retrieve basic information about the treebank:\n\n"
          . $res->status_line
          . "\n" );
      _show_service_info( $self, $cfg, $top )
        if $show and ref( $cfg->{cached_description} );
      return;
    }
    if ( $content ) {
      _update_cached_info( $self, $cfg, $content );
      $self->{config}{pml}->save();
      if ($anchor) {

        # update info
        $self->_add_cfg_item( $l, $anchor, $cfg );
      }
      _show_service_info( $self, $cfg, $top ) if $show;
      return 1;
    }
    return;
  }

  sub _update_cached_info {
    my ( $self, $cfg, $service_info ) = @_;
    $cfg->{cached_description} ||= Treex::PML::Factory->createStructure();
    foreach my $key (qw(title description homepage)) {
      $cfg->{cached_description}{$key} = $service_info->{$key};
    }
  }

  sub _show_service_info {
    my ( $self, $cfg, $widget ) = @_;
    my $d = $widget->toplevel->DialogBox(
      -title   => 'Information About PML-TQ Search Service',
      -buttons => [qw[OK]],
    );
    my $f = $d->add(
      'Frame',
      -relief      => 'sunken',
      -borderwidth => 2,
    )->pack( -expand => 1, -fill => 'both', -padx => 7, -pady => 7 );
    my $s = $cfg->{cached_description} || {};
    $f->Label(
      -text       => $s->{title},
      -wraplength => '500p',
      -font       => 'C_bold',
      -anchor     => 'nw',
      -takefocus  => 1,
      -justify    => 'left'
    )->pack( -fill => 'x', -padx => 3 );
    $f->Label(
      -text       => "id: " . $cfg->{id},
      -wraplength => '500p',
      -font       => 'C_bold',
      -anchor     => 'nw',
      -takefocus  => 1,
      -justify    => 'left'
    )->pack( -fill => 'x', -padx => 3 );
    if ( $s->{description} ) {
      $f->Label(
        -text       => $s->{description},
        -wraplength => '500p',
        -font       => 'C_normal',
        -foreground => 'black',
        -anchor     => 'nw',
        -justify    => 'left'
      )->pack( -fill => 'x', -padx => 3 );
    }
    if ( $s->{homepage} ) {
      my $link = $f->Label(
        -text       => $s->{homepage},
        -font       => 'C_small',
        -foreground => 'blue',
        -anchor     => 'ne',
        -justify    => 'right'
      );
      $link->bind( $link, '<1>', [ sub { \&main::open_url_in_browser( $_[1] ) }, $s->{homepage} ] );
      $link->pack( -fill => 'x', -padx => 3 );
    }
    $d->BindEscape();
    $d->Show;
  }

  sub _add_related_service {
    my $l = pop @_;
    my ( $self, $cfgs, $cfg ) = @_;
    my $id;
    if ($cfg) {
      $id = $cfg->{id};
    }
    else {
      $id = $l->info('anchor');
      $cfg = defined($id) && $l->info( 'data', $id );
    }
    return unless $cfg and $id;
    my %enabled;
    for my $c ( map $_->value, grep $_->name eq 'http', SeqV($cfgs) ) {
      $enabled{ $c->{url} } = 1 if $c->{url};
    }

    my $url = URI::WithBase->new('/', $cfg->{url});
    $url->path_segments($API_VERSION, 'treebanks');
    my ($res, $content) = $self->request_server( $cfg, $url->abs->as_string );
    unless ( $res->is_success ) {
      ErrorMessage( "Failed to connect to server:\n\n" . $res->status_line . "\n" );
      return;
    }
    my @services;
    my %services;
    for my $service ( @$content ) {
      if ( $service->{name} ) {
        push @services, ( $services{$service->{name}} = $service );
        if ( exists $service->{access} ) {

          # deselect servers the user is not authorized to connect to
          $enabled{ $service->{name} } = 0 unless $service->{access};
        }
      }
    }
    return unless @services;
    my $top = $l ? $l->toplevel : ToplevelFrame();
    my $d = $top->DialogBox(
      -title   => 'Select service',
      -buttons => [qw[OK Cancel]],
    );
    my $f0 = $d->add(
      'Frame',
      -relief      => 'sunken',
      -borderwidth => 2,
    )->pack( -expand => 1, -fill => 'both', -padx => 7, -pady => 7 );
    my $pane = $f0->Scrolled(
      'Pane',
      -borderwidth => 0,
      -background  => '#cccccc',
      -scrollbars  => 'oe',
      -width       => '610p',
      -height      => '500',
      -sticky      => 'we'
    )->pack( -expand => 1, -fill => 'both' );
    my @b;
    my $highlight_color = '#959ca5';
    my $background      = 'white';
    my %is_link;

    foreach my $s (@services) {
      my $f = $pane->Frame(
        -background  => $background,
        -borderwidth => 1,
        -relief      => 'flat',
        -takefocus   => 0,
      );
      $f->bindtags( [ $f, ref($f), $f->toplevel, 'all' ] );
      my $b = $f->Checkbutton(
        -variable => \$enabled{ $s->{url} },
        -state    => (
          ( exists( $s->{access} ) and $s->{access} == 0 )
          ? 'disabled'
          : 'normal'
        ),
        -highlightthickness => 1,
        -highlightcolor     => $highlight_color,
        -activebackground   => $background,
        -borderwidth        => 2,
        -background         => $background,
        -text               => $s->{title},
        -wraplength         => '500p',
        -font               => 'C_bold',
        -anchor             => 'nw',
        -takefocus          => 1,
        -justify            => 'left'
      )->pack( -fill => 'x', -padx => 3 );
      $b->bindtags( [ $b, ref($b), $b->toplevel, 'all' ] );
      push @b, $b;
      if ( $s->{description} ) {
        $f->Label(
          -background => $background,
          -text       => $s->{description},
          -wraplength => '500p',
          -font       => 'C_normal',
          -foreground => 'black',
          -anchor     => 'nw',
          -justify    => 'left'
        )->pack( -fill => 'x', -padx => 3 );
      }
      if ( $s->{homepage} ) {
        my $link = $f->Label(
          -background => $background,
          -text       => $s->{homepage},
          -font       => 'C_small',
          -foreground => 'blue',
          -anchor     => 'ne',
          -justify    => 'right'
        );
        $is_link{$link} = 1;
        $link->bind( $link, '<1>', [ sub { \&main::open_url_in_browser( $_[1] ) }, $s->{homepage} ] );
        $link->pack( -fill => 'x', -padx => 3 );
      }
      $f->pack(
        -expand => 1,
        -fill   => 'both',
        -padx   => 0,
        -pady   => 0.5
      );
    }
    foreach my $i ( 0 .. $#b ) {
      my $b = $b[$i];
      $b->bind( $b, '<Control-Return>', sub { $d->{selected_button} = 'OK'; Tk->break } );
      $b->bind( '<Down>', [ sub { shift; $pane->see( $_[0]->parent ); $_[0]->focus }, $b[ $i + 1 ] ] ) if ( $i < $#b );
      $b->bind( '<Up>',   [ sub { shift; $pane->see( $_[0]->parent ); $_[0]->focus }, $b[ $i - 1 ] ] ) if ( $i > 0 );
      $b->bind(
        '<Prior>',
        sub {
          my ($w)   = @_;
          my $frame = $w->parent->parent;
          my $y     = $w->parent->y;
          my $h     = $pane->height;
          for my $b ( reverse $frame->children ) {
            my $dist = ( $y - $b->y );
            if ( $dist >= $h ) {
              my ($c) = grep { $_->isa('Tk::Checkbutton') } $b->children;
              $c->focus if $c;
              Tk->break;
            }
          }
        } );
      $b->bind(
        '<Next>',
        sub {
          my ($w)   = @_;
          my $frame = $w->parent->parent;
          my $y     = $w->parent->y;
          my $h     = $pane->height;
          for my $b ( $frame->children ) {
            my $dist = ( $b->y - $y );
            if ( $dist >= $h ) {
              my ($c) = grep { $_->isa('Tk::Checkbutton') } $b->children;
              $c->focus if $c;
              Tk->break;
            }
          }
        } );
      $b->bind(
        '<Home>',
        [ sub {
            shift;
            $pane->yview(qw(moveto 0));
            $_[0]->focus;
          },
          $b[0] ] );
      $b->bind(
        '<End>',
        [ sub {
            shift;
            $pane->yview(qw(moveto 1));
            $_[0]->focus;
          },
          $b[-1] ] );
      for my $w ( $b->parent, $b->parent->children ) {
        $pane->BindMouseWheelVert( '', $w );
        $w->bind( '<1>', [ $b, 'focus' ] ) unless $is_link{$w};
      }
      $b->bind(
        '<FocusIn>',
        sub {
          for my $w ( $_[0]->parent, $_[0]->parent->children ) {
            $w->configure( -background => $highlight_color );
            eval { $w->configure( -activebackground => $highlight_color, -activeforeground => 'white' ); };
            if ( eval { $w->cget('-foreground') eq 'black' } ) {
              $w->configure( -foreground => 'white' );
            }
          }
          $pane->see( $_[0]->parent );
        } );
      $b->bind(
        '<FocusOut>',
        sub {
          for my $w ( $_[0]->parent, $_[0]->parent->children ) {
            $w->configure( -background => $background );
            eval { $w->configure( -activebackground => $background, -activeforeground => 'black' ); };
            if ( eval { $w->cget('-foreground') eq 'white' } ) {
              $w->configure( -foreground => 'black' );
            }
          }
          $pane->see( $_[0]->parent );
        } );
    }
    $b[0]->focus;
    $d->BindButtons;
    $d->BindEscape();
    $d->BindReturn( $d, 1 );
    $pane->BindMouseWheelVert( '', $pane );
    return if $d->Show ne 'OK';

    foreach my $c ( map $_->value, grep $_->name eq 'http', SeqV($cfgs) ) {
      my $name = $c->{treebank};
      if ( !$name ) {

        # prune broken entries
        $cfgs->delete_value($c);
      }
      elsif ( exists( $enabled{$name} ) ) {
        if ( $enabled{$name} ) {
          delete $enabled{$name};

          # update cached values:
          _update_cached_info( $self, $c, $services{$name} )
            if $services{$name};
        }
        else {
          $cfgs->delete_value($c);
        }
      }
    }
    my $ids_ref;
    foreach my $s (@services) {
      if ( $enabled{ $s->{name} } ) {
        my $new_id;
        ( $new_id, $ids_ref ) = $self->_new_cfg_id( $cfgs, undef, $ids_ref );
        my $c = Treex::PML::Factory->createStructure(
          { id       => $new_id,
            url      => $cfg->{url},
            treebank => $s->{name},
            username => $cfg->{username},
            password => $cfg->{password},
          },
          1
        );
        $cfgs->push_element( 'http', $c );
        _update_cached_info( $self, $c, $s );
      }
    }
    $self->{config}{pml}->save();
    if ($l) {
      $l->delete('all');
      $self->_add_cfg_items_to_hlist( $l, [ map $_->value, grep $_->name eq 'http', SeqV($cfgs) ], $id );
    }
    return 1;
  }

  sub _new_cfg_id {
    my ( $self, $cfgs, $new_id, $ids_ref ) = @_;

    unless ( defined $ids_ref ) {
      $ids_ref = { map { $_->{id} => 1 } map $_->value, SeqV($cfgs) };
    }
    if ( !$new_id or exists $ids_ref->{$new_id} ) {
      $new_id = 'a';
      $new_id++ while exists $ids_ref->{$new_id};
      $ids_ref->{$new_id} = 1;
    }
    return wantarray ? ( $new_id, $ids_ref ) : $new_id;
  }

  sub prepare_results {
    my ( $self, $dir ) = @_;
    my $no = $self->{current_result_no};
    if ( $dir eq 'prev' ) {
      if ( $no > 0 ) {
        $self->{current_result_no} = --$no;
        $self->{current_result}
          = [ $self->idx_to_pos( $self->{results}[$no] ) ];
      }
    }
    elsif ( $dir eq 'next' ) {
      if ( $no < $#{ $self->{results} } ) {
        $self->{current_result_no} = ++$no;
        $self->{current_result}
          = [ $self->idx_to_pos( $self->{results}[$no] ) ];
      }
    }
  }

  sub have_results {
    my ($self) = @_;
    (       $self->{current_result} and $self->{last_query_nodes}
        and @{ $self->{current_result} }
        and @{ $self->{last_query_nodes} } ) ? 1 : 0;
  }

  sub get_nth_result_filename {
    my ( $self, $idx ) = @_;
    $self->resolve_path( $self->{current_result}[$idx] );
  }

  sub update_label {
    my ( $self, $text ) = @_;
    if ( defined $text ) {
      ${ $self->{label} } = $text;
    }
    else {
      my $no      = $self->{current_result_no} + 1;
      my $limit   = $self->{limit} || 0;
      my $matches = $self->{results} ? @{ $self->{results} } : 0;
      ${ $self->{label} }
        = qq{$no of $matches} . ( $matches == $limit ? '+' : '' );
    }
    my $tb = $self->toolbar;
    $tb->update if $tb;
    return;
  }

  sub check_server_version {
    my ( $self, $server_version ) = @_;
    my $res = $self->request_api(
      'version',
      [ client_version => $VERSION,
        format         => 'text',
      ] );
    unless ( $res->is_success ) {
      die "Failed to connect to server (the server is incompatible, down, or you are not authorized):\n\n"
        . $res->status_line . "\n";
    }
    my $v = $res->content;
    $v =~ s/\r?\n$//;
    if ( $v !~ /^(IN)?COMPATIBLE\s+(\S+)/ or $1 ) {
      die "Server requires a newer version of this client; please upgrade the 'pmltq' TrEd extension!!\n";
    }
    elsif ( Treex::PML::Schema::cmp_revisions( $MIN_SERVER_VERSION, $2 ) > 0 ) {
      die "Server is too old for this client; please ask your PML-TQ server administrator to upgrade!\n";
    }
    return 1;
  }

  sub idx_to_pos {
    my ( $self, $idx_list ) = @_;
    my @res;
    for my $ident (@$idx_list) {
      unless ( $ident =~ m{//} ) {
        my ($res, $content) = $self->request_api( 'node', [ idx => $ident ] );
        unless ( $res->is_success ) {
          die "Failed to resolve $ident!\n" . $res->status_line . "\n";
        }
        push @res, $content->{node};
      }
      else {
        push @res, undef;
      }
    }
    return @res;
  }

  sub load_config_file {
    my ( $self, $config_file ) = @_;
    if (
      !$self->{config}{pml}
      or (  $config_file
        and $config_file ne $self->{config}{pml}->get_filename ) )
    {
      if ($config_file) {
        die "Configuration file '$config_file' does not exist!"
          unless -f $config_file;
        $self->{config}{pml}
          = Treex::PML::Instance->load( { filename => $config_file } );
      }
      else {
        $config_file ||= FindInResources('treebase.conf');
        if ( -f $config_file ) {

          # we need this extension's resource paths precede TrEd's resource paths
          # since old versions of TrEd include old version of treebase_conf_schema.xml
          my @paths = Treex::PML::ResourcePaths();
          eval {
            Treex::PML::AddResourcePathAsFirst( CallerDir( File::Spec->catdir( '..', 'resources' ) ) );
            $self->{config}{pml}
              = Treex::PML::Instance->load( { filename => $config_file } );
          };
          Treex::PML::SetResourcePaths(@paths);
          die $@ if $@;
        }
        else {
          my $tred_d = File::Spec->catfile( $ENV{HOME}, '.tred.d' );
          mkdir $tred_d unless -d $tred_d;
          $config_file = File::Spec->catfile( $tred_d, 'treebase.conf' );
          $self->{config}{pml} = Treex::PML::Instance->load(
            { string   => $DEFAULTS{pmltq_config},
              filename => $config_file
            } );
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

  $DEFAULTS{pmltq_config} = <<"EOF";
<pmltq_config xmlns="http://ufal.mff.cuni.cz/pdt/pml/">
  <head>
    <schema href="treebase_conf_schema.xml"/>
  </head>
  <limit>$DEFAULTS{limit}</limit>
  <row_limit>$DEFAULTS{row_limit}</row_limit>
  <timeout>$DEFAULTS{timeout}</timeout>
  <configurations>
  </configurations>
</pmltq_config>
EOF

}    # HTTP
