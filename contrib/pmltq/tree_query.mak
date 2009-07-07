# -*- cperl -*-

#include <contrib/pml/PML.mak>

package Tree_Query;
{
use strict;

use vars qw($this $root);
BEGIN {
  import TredMacro;
  import PML qw(&SchemaName);
}

use PMLSchema qw(:constants);
use File::Spec;
use Benchmark ':hireswallclock';

use lib CallerDir();

use Tree_Query::Common qw(:all);
use Tree_Query::NG2PMLTQ qw(ng2pmltq);

our $VALUE_LINE_MODE = 0;
our @SEARCHES;
our $SEARCH;

register_reload_macros_hook(sub{
  undef @SEARCHES;
  undef $SEARCH;
});

Bind 'Tree_Query->NewQuery' => {
  context => 'TredMacro',
  key => 'Shift+F3',
  menu => '__* Start Tree Query',
};
Bind 'Tree_Query->NewQuery' => {
  key => 'Shift+F3',
  menu => '__* Start Tree Query',
};
our $ng_string;

my $insert_key='Insert';
if ($^O eq 'darwin') {
  # there is no Insert key on Mac
  $insert_key = 'i';
}

my @TOOLBAR_BINDINGS = (
  {
    command => sub {
      new_tree_after();
      $root->{id}=new_tree_id();
    },
    key => 'Ctrl+n',
    menu => 'Create a new query tree',
    toolbar => ['New query', 'filenew' ],
  },
  {
    command => sub {
      my $res = EditBoxQuery('NetGraph Import', $ng_string,
			     'Insert or paste a query in the syntax of NetGraph:');
      return unless defined $res and length $res;
      $ng_string = $res;
      my $string = ng2pmltq($ng_string);
      new_tree_after();
      $root->{id}=new_tree_id();
      $root->{description} = 'Imported from NetGraph';
      unless (EditQuery($root,{string=>$string})) {
	DeleteNode();
	ChangingFile(0);
      }
    },
    menu => 'Import query from NetGraph',
    toolbar => ['Import', 'netgraph_client' ],
  },
  '---',
  {
    command => 'SelectSearch',
    key => 'c',
    menu => , 'Select file(s) to search or connect to a search server',
    changing_file => 0,
    toolbar => ['Connect' , 'connect_creating' ],
  },
  {
    command => sub {
      unless ($SEARCH) {
	SelectSearch()||return;
      }
      if ($SEARCH->configure) {
	for (@SEARCHES) {
	  eval { $_->reconfigure } ; # various searches may share the same config file
	  ErrorMessage("$@") if $@;
	}
      }
    },
    key => 'C',
    menu => 'Configure search engine',
    changing_file => 0,
    toolbar => ['Configure' , 'configure' ],
  },
  '---',
#  "\n",
  {
    command => sub { $this=$root; EditSubtree() },
    key => 'Ctrl+e',
    menu => 'Edit complete query',
    toolbar => ['Edit query',  'edit_file'],
  },
  {
    command => 'EditNodeConditions',
    key => 'e',
    menu => 'Edit part of the query corresponding to the current node',
    toolbar => ['Edit node','edit_node' ],
  },
  {
    command => 'EditSubtree',
    key => 'E',
    menu => 'Edit part of the query corresponding to the current subtree',
    toolbar => ['Edit subtree', 'edit_subtree' ],
   },
  {
    command => sub { $this=$root;
		     ChangingFile(0);
		     init_id_map($root,1);
		     Redraw() if $TredMacro::FileChanged==1;
		     EditSubtree();
		   },
    key => 'f',
    menu => 'Edit output filters',
    toolbar => ['Filters','filter' ],
  },
  '---',

  {
    command => 'cut_to_clipboad',
    key => 'Ctrl+Delete',
    menu => 'Cut subtree to clipboard',
    toolbar => ['Cut',  'editcut'],
   },
  {
    command => 'copy_to_clipboad',
    key => 'Ctrl+'.$insert_key,
    menu => 'Copy subtree to clipboard',
    toolbar => ['Copy', 'editcopy'],
  },
  {
    command => sub { PasteClipboardWithRename(); copy_to_clipboad() },
    key => 'Shift+'.$insert_key,
    menu => 'Paste subtree from clipboard',
    toolbar => ['Paste', 'editpaste'],
  },
  {
    command => 'paste_as_new_tree',
    key => 'Ctrl+Shift+'.$insert_key,
    menu => 'Paste as new tree',
    toolbar => ['Paste New Tree', 'editpaste'],
  },
  '---',
  {
    command =>  sub {
      ChangingFile(0);
      my $qn = first { $_->{'#name'} =~ /^(?:node|subquery)$/ } ($this,$this->ancestors);
      if ($qn) {
	$qn->{'.unhide'}=!$qn->{'.unhide'};
	$this=$qn;
      }
    },
    key => 'h',
    menu => 'Expand/hide auxiliary and logical nodes for current node',
    toolbar => ['(Un)Expand','toggle_hide_subtree' ],
  },
  {
    command =>  sub {
      ToggleHiding();
      unless (HiddenVisible()) {
	delete $_->{'.unhide'} for $root->descendants;
      }
    },
    key => 'H',
    menu => 'Expand/hide auxiliary and logical nodes for all nodes',
    changing_file => 0,
    toolbar => ['(Un)Expand all','toggle_hide_all' ],
  },

  "\n",

  {
   command => 'AddNode',
   key => $insert_key,
   menu => 'Create a new query node',
   toolbar => ['Add node', 'add_node'],
  },
  '---',
  {
    command => 'AddNOT',
    key => '!',
    toolbar => ['NOT','not' ],
    menu => 'Negate a condition',
  },
  {
    command => 'AddAND',
    key => 'a',
    menu => 'Add AND condition (under an OR or NOT)',
    toolbar => ['AND','and' ],
  },
  {
    command => 'AddOR',
    key => 'o',
    menu => 'Add OR condition',
    toolbar => ['OR','or' ],
  },
  {
    command =>  'NewTest',
    key => '=',
    menu => 'Add a equality test',
    toolbar => ['Equality','test_equality' ],
  },
  {
    command =>  sub { NewTest('~')   },
    key => '~',
    menu => 'Add a regexp test',
    toolbar => ['Regexp','test_regexp' ],
  },
  '---',
  {
    command => sub {
      ChangingFile(0);
      return unless $this->{'#name'}=~/^(?:node|subquery)$/;
      my $prev_name = $this->{name};
      $this->{name} = GetNodeName($this);
      EDIT: while (EditAttribute($this,'name')) {
	if ($this->{name}) {
	  my $n = $root->firstson;
	  while ($n) {
	    if ($n!=$this and 
		  $n->{'#name'} =~ /^(node|subquery)$/ and
		    $n->{name} eq $this->{name}) {
	      QuestionQuery("Wrong name",
			    "Name $this->{name} is already taken!",
			    "Edit name","Cancel");
	      $this->{name} = $prev_name;
	      next EDIT;
	    }
	    $n = $n->following;
	  }
	}
	ChangingFile(1);
	if ($prev_name and $this->{name} ne $prev_name) {
	  RenameNode($this,$prev_name);
	}
	return;
      }
      $this->{name} = $prev_name;
    },
    key => '$',
    menu => 'Change node name',
    toolbar => ['Name','name_node' ],
  },
  {
    command => sub {
      ChangingFile(0);
      return unless $this and $this->parent and $this->{'#name'} =~ /^(node|subquery)$/;
      $this->{'node-type'} ? EditAttribute($this,'node-type') : AssignType($this)
    },
    key => 'T',
    menu => 'Try automatically set the node type based on parent-node type',
    toolbar => ['Type','node_type' ],
  },
  {
    command => 'AssignRelation',
    key => 'r',
    changing_file => 0,
    menu => 'Edit relation of the current node to its parent',
    toolbar => ['Relation','relation' ],
  },
  {
    command => 'CreateRelationToNamed',
    key => 'R',
    changing_file => 0,
    menu => 'Create relation from the current node to a given node',
    toolbar => ['Add Rel','add_relation' ],
  },
  '---',
  {
    command =>  sub {
      ChangingFile(0);
      unless ($this->parent and $this->parent->parent and $this->{'#name'} eq 'node') {
	QuestionQuery('Sorry',"Cannot make this node optional!",'Ok');
	return;
      }
      $this->{optional}=!$this->{optional};
      ChangingFile(1);
    },
    key => '?',
    menu => 'Toggle optional',
    toolbar => ['Optional','optional_node' ],
  },
  {
    command =>  sub {
      ChangingFile(0);
      my $orig_name = $this->{'#name'};
      if ($orig_name eq 'node' and
	    $this->parent and $this->parent->parent and $this->parent->{'#name'} =~ /^(?:node|subquery)$/) {
	if ($this->{optional}) {
	  QuestionQuery('Sorry',"Cannot set occurrences for an optional node!",'Ok');
	  return;
	}
	$this->set_type(undef);
	$this->{'#name'}='subquery';
	DetermineNodeType($this);
      }
      unless ($this->{'#name'} eq 'subquery') {
	QuestionQuery('Sorry',"Cannot set occurrences for this node!",'Ok');
	return;
      }
      if (not (AltV($this->{'occurrences'}))) {
	$this->{occurrences}=Fslib::Struct->new({min=>1});
      }
      local $main::sortAttrs=0;
      if (EditAttribute($this,'occurrences')) {
	if (! defined first { defined && length } map { ($_->{min},$_->{max}) } AltV($this->{occurrences})) {
	  $this->set_type(undef);
	  $this->{'#name'}='node';
	  delete $this->{occurrences};
	  DetermineNodeType($this);
	}
	ChangingFile(1);
      } elsif ($orig_name ne $this->{'#name'}) {
	$this->{'#name'}=$orig_name;
	DetermineNodeType($this);
	if ($orig_name eq 'node') {
	  delete $this->{occurrences};
	}
      }
    },
    key => 'x',
    menu => 'Edit occurrences on a subquery-node',
    toolbar => ['Occurrences','subquery' ],
  },
  '---',
  {
    command => 'DeleteNode',
    key => 'Delete',
    menu => 'Delete current node (pasting its children on its parent)',
    toolbar => ['Delete node', 'delete_node' ],
  },
  {
    command =>  'DeleteSubtree',
    key => 'Shift+Delete',
    menu => 'Delete current subtree',
    toolbar => ['Delete subtree', 'delete_subtree'],
  },
  '---',
  {
    command =>  sub { my $new = new_rbrother();
		      if ($new and $new->{'#name'} =~ /^(node|subquery)$/) {
			( $new->parent ? AssignRelation($new) : AssignType($new) ) || DeleteLeafNode($new);
		      }
		    },
    key => 'Alt+Right',
    menu => 'New right brother node',
    toolbar => ['Brother', 'new_rbrother' ],
  },
  {
    command =>  sub { my $new = new_son();
		      if ($new and $new->{'#name'} =~ /^(node|subquery)$/) {
			( $new->parent ? AssignRelation($new) : AssignType($new) ) || DeleteLeafNode($new);
		      }
		    },
    key => 'Alt+Down',
    menu => 'New son node',
    toolbar => ['Son', 'new_son' ],
  },

  {
    command =>  sub {
      my $child = $this;
      return unless $child->parent;
      my $new = new_parent($child);
      if ($new and $new->{'#name'} =~ /^(node|subquery)$/) {
	$new->{relation} = $child->{relation};
	undef $child->{relation};
	AssignType($new);
	AssignRelation($child);
      }
    },
    key => 'Alt+Up',
    menu => 'Insert a new node between the current node and its parent',
    toolbar => ['Parent', 'new_parent' ],
  },
 );

DeclareMinorMode 'Tree_Query_Results' => {
  abbrev => 'reslt',
  priority_bindings => {
    'n' => sub { $SEARCH && $SEARCH->show_next_result; ChangingFile(0); },
    'p' => sub { $SEARCH && $SEARCH->show_prev_result; ChangingFile(0); },
  },
  pre_hooks => {
    root_style_hook => sub {
      map_results($root);
    },
  },
  post_hooks => {
    node_style_hook => sub {
      my ($node,$styles)=@_;
      my $m=$Tree_Query::is_match{$node};
      if (defined $m) {
	AddStyle($styles,'Oval',-fill => '#'.$Tree_Query::colors[$m]);
	AddStyle($styles,'Node',-addwidth=>3);
	AddStyle($styles,'Node',-addheight=>3);
      }
    },
    get_value_line_hook => sub {
      my ($fsfile,$no)=@_;
      map_results($fsfile->tree($no),$fsfile->filename,$no,$fsfile);
      if (!defined $_[-1]) {
	# value line not supplied by hook, we provide the standard one
	$_[-1] = $grp->treeView->value_line($fsfile,$no,1,1,$grp);
      }
      my $vl =  $_[-1];
      if (ref $vl) {
	my $m;
	for my $item (@$vl) {
	  for (@$item[1..$#$item]) {
	    if (defined($m=$Tree_Query::is_match{$_})) {
	      # print "match: $m $_\n";
	      @$item = $item->[0],grep !/^-foreground => /, @$item[1..$#$item];
	      push @$item,'-foreground => #'.$Tree_Query::colors[$m];
	      last;
	    }
	  }
	}
      }
      # return $vl;
    },
  }
};

Bind({command=>'Undo',
      key => 'Ctrl+z',
      menu=>'Undo',
      changing_file => 0,
    });
Bind({command=>'Redo',
      key => 'Ctrl+Z',
      menu=>'Redo',
      changing_file => 0,
    });

Bind({
  command => sub {
    ChangingFile(0);
    return unless !$this->parent || $this->{'#name'}=~/^(?:node|subquery)$/;
    EditAttribute($this,'node-type') && ChangingFile(1);
  },
  key => 't',
  menu => 'Edit node type',
});
Bind({
    command =>  sub { my $new = new_lbrother();
		      if ($new and $new->{'#name'} =~ /^(node|subquery)$/) {
			( $new->parent ? AssignRelation($new) : AssignType($new) ) || DeleteLeafNode($new);
		      }
		    },
    key => 'Alt+Left',
    menu => 'New left brother node',
    # toolbar => ['Left brother','new_lbrother' ],
  });

for (grep ref, @TOOLBAR_BINDINGS) {
  Bind($_);
}

Bind AutoNameAllNodes => {
  key => 'A',
  menu => 'Automatically assign a name to all nodes',
};

Bind AddAND => {
  key => 'a',
  menu => 'Add AND condition (under an OR or NOT)',
};

Bind AddOR => {
  key => 'o',
  menu => 'Add OR condition',
};

Bind sub {
  for (reverse sort_children_by_node_type($this)) {
    CutPaste($_,$this);
  }
} => {
  key => 's',
  menu => "Sort node's children by type"
};

Bind 'Search' => {
  key => 'space',
  menu => 'Run query',
  changing_file => 0,
};

Bind sub { Search({no_filters=>1}) } => {
  key => 'Shift+space',
  menu => 'Run query without output filters',
  changing_file => 0,
};

Bind sub { Search({no_filters=>1, count=>1 }) } => {
  key => 'Ctrl+space',
  menu => 'Count matches',
  changing_file => 0,
};

Bind sub {  $SEARCH && $SEARCH->show_current_result } => {
  key => 'm',
  menu => 'Show Match',
  changing_file => 0,
};

Bind sub { $SEARCH && $SEARCH->show_next_result } => {
  key => 'n',
  menu => 'Show Next Match',
  changing_file => 0,
};

Bind sub { $SEARCH && $SEARCH->show_prev_result } => {
  key => 'p',
  menu => 'Show Previous Match',
  changing_file => 0,
};

Bind sub { RenewStylesheets(); $Redraw='stylesheet'; } => {
  key => 'y',
  menu => 'Renew Tree_query Stylesheet',
  changing_file => 0,
};

Bind 'fix_netgraph_query' => {
  key => 'f',
  menu => 'Attempt to fix a NetGraph query',
};

Bind sub { $VALUE_LINE_MODE=!$VALUE_LINE_MODE } => {
  key => 'v',
  menu => 'Toggle value line mode (TreeQuery/SQL)',
  changing_file => 0,
};

our @colors = qw(
66B032 ffff93740000 4a6d0133c830 b9f30175f2f0 0392CE ffffe1c90000
9655c9b94496 fef866282da3 007FFF C154C1 CC7722 FBFB00
00A86B fef8b3ca5b88 CCCCFF 8844AA 987654 F0E68C
BFFF00 E68FAC 00FFFF FFAAFF 996515 f3f6bdcb15f4
ADDFAD FFCBA4 007BA7 CC99CC B1A171 dddd00
6B8E23 FF8855 9BDDFF FF00FF 654321 FFFACD
00FF00 FF2400 1560BD 997A8D cd0da2373d4f FFFF77
D0EA2B b7ce1c6b0d0c E2F9FF  c1881d075743  0247FE
);


 my %schema_map = (
#   't-node' => PMLSchema->new({filename => 'tdata_schema.xml',use_resources=>1}),
#   'a-node' => PMLSchema->new({filename => 'adata_schema.xml',use_resources=>1}),
);

#define no_extra_edit_menu_bindings
#include <contrib/support/extra_edit.inc>
#undefine no_extra_edit_menu_bindings

#include <contrib/support/arrows.inc>

#unbind-key Alt+N
#unbind-key Alt+Up
#unbind-key Alt+Down
#unbind-key Alt+Left
#unbind-key Alt+Right
#remove-menu New tree
#unbind-key Alt+T
#remove-menu Trim (remove all but current subtree)

# Setup context
unshift @TredMacro::AUTO_CONTEXT_GUESSING, sub {
  SchemaName() eq 'tree_query' ? __PACKAGE__ : undef ;
};

my %icons;
sub icon {
  require Tk::PNG;
  my ($name)=@_;
  return unless GUI();
  if (ref $icons{$name}) {
    return $icons{$name};
  }
  my $file = File::Spec->catfile(CallerDir(),'icons',$name.'.png');
  if (-r $file) {
    return $icons{$name} = ToplevelFrame()->Photo('pmltq_'.$name,-format => 'png', -file => $file);
  } else {
    return $icons{$name} = main::icon($grp->{framegroup}, $name)
  }
}

sub allow_switch_context_hook {
  return 'stop' if SchemaName() ne 'tree_query';
}

#ifdef TRED
sub macros_reloaded_hook {
  switch_context_hook(__PACKAGE__,__PACKAGE__); # renew toolbar
}
#endif

sub get_status_line_hook {
  return 'To create an additional edge (relation), drag a start node over the target node using mouse and hold CTRL before releasing.';
}

sub pre_switch_context_hook {
  my ($prev,$new,$win)=@_;
  return if $prev eq $new;
  return if $grp==$win and $grp!=CurrentWindow();
  if (first { $win!=$_ and CurrentContextForWindow($_) eq 'Tree_Query' } TrEdWindows()) {
    DisableUserToolbar('Tree_Query');
  } else {
    DestroyUserToolbar('Tree_Query');
  }
}

# Setup stylesheet
sub switch_context_hook {
 my ($prev,$new)=@_;
 CreateStylesheets();
 SetCurrentStylesheet('Tree_Query'),Redraw()
   if GetCurrentStylesheet() ne 'Tree_Query'; #eq STYLESHEET_FROM_FILE();

 if (exists &GetUserToolbar) {
 if (GetUserToolbar('Tree_Query')) {
#    unless (UserToolbarVisible('Tree_Query')) {
#      ShowUserToolbar('Tree_Query');
#    }
   EnableUserToolbar('Tree_Query');
 } else {
   my $tb = NewUserToolbar('Tree_Query');
   my $frame = $tb->Frame()->pack(qw(-fill x));	#qw(-side top -expand 1 -fill both));
   $frame->packPropagate;
   for my $binding (@TOOLBAR_BINDINGS) {
     if (ref($binding)) {
       my $but = $binding->{toolbar};
       return unless $but;
       my $button = $frame->Button(
	 -command => MacroCallback($binding),
	 -padx => 2,
	 -font    =>'C_small',
	 -borderwidth => 0,
	 -takefocus=>0,
	 -relief => $main::buttonsRelief,
	 $but->[1] ? (-compound => 'top',
		      -image => icon($but->[1]),
		      -text  => $but->[0],
		     ) :
		       (
			 -text  => $but->[0]
			)
		      )->pack(-side=>'left',-padx=>5);
       my $tooltip = $binding->{menu} || '';
       $tooltip .= qq{ ($binding->{key})} if $binding->{key};
       AttachTooltip($button,$tooltip) if $tooltip;
     } elsif ($binding eq '---') {
       $frame->Frame(-bd => 2, -width => 2, -relief => 'groove')
	 ->pack(-side=> 'left', -padx => '3', -fill => 'y', -pady => 3);
     } elsif ($binding eq "\n") {
       $frame = $tb->Frame()->pack(qw(-fill x)); #qw(-side top -expand 1 -fill both));
       $frame->packPropagate;
     }
   }
 }
}
}
# sub file_reloaded_hook {
#   FileAppData('noautosave',1);
# }

sub RenewStylesheets {
  DeleteStylesheet('Tree_Query');
  CreateStylesheets();
  SetCurrentStylesheet('Tree_Query');
  SaveStylesheets();
}

sub CreateStylesheets{
  unless(StylesheetExists('Tree_Query')){
    SetStylesheetPatterns(<<'EOF','Tree_Query',1);
context:  Tree_Query
hint: 
rootstyle:#{balance:1}#{Node-textalign:center}#{NodeLabel-halign:center}
rootstyle: #{vertical:0}#{nodeXSkip:40}#{skipHiddenLevels:1}
rootstyle: #{NodeLabel-skipempty:1}#{CurrentOval-width:3}#{CurrentOval-outline:red}
node: <? !$this->parent ? "Tree Query" : () ?>
node: <?length($${id}) ? ' #{blue(}${id}#{)} ' : '' 
?><? 
  $this->{'#name'} =~ /^(node|subquery)$/ ?
   ( length($${node-type}) 
        ? $${node-type}.' ' : $root->{'node-type'} ? '#{brown(}${node-type='.$root->{'node-type'}.'}#{)}' : '#{red(}${node-type=AMBIGUOUS TYPE}#{)}' ) : () 
?>#{darkblue}<?length($${name}) ? '$'.$${name}.' ' : '' ?>
label:#{darkgreen}<?
  my $occ = Tree_Query::occ_as_text($this);
  length $occ ? '#{-coords:n-10,n}#{-anchor:e}${occurrences='.$occ.'x}' : ""
?><? $${optional} ? '#{-coords:n-10,n}#{-anchor:e}${optional=?}'  : q()
?>

node: #{brown(}${description}

node:<?
  ($this->{'#name'} =~ /^(?:and|or|not)$/) ? uc($this->{'#name'}) : '' 
?>${a}${target}
node:<?
  if (($this->{'#name'}=~/^(?:node|subquery)$/) and !$this->{'.unhide'} and !TredMacro::HiddenVisible() ) {
    join("\n",map { Tree_Query::as_text($_,{indent=>'  ',wrap =>1,no_description=>1}) } 
       grep {
	my $f;
	not(
	  $_->{'#name'} eq 'ref' or
	  ($_->{'#name'} eq 'not' and $f=$_->firstson and $f->{'#name'} eq 'ref' and !$f->rbrother)) }
       grep { $_->{'#name'} !~ /^(?:node|subquery|ref)$/ } $this->children)
  } elsif ($this->{'#name'} eq 'test') {
    '${operator}'
  } elsif ($this->{'#name'} eq '' and !$this->parent) {
     my $filters = Tree_Query::as_text($this,{no_childnodes=>1, indent=>'  ',wrap =>1,no_description=>1});
     $filters=~s/([ \t]*>>)/Output filters:\n$1/; $filters
  }
?>
node:${b}
style: <? 
  my $name = $this->{'#name'};
  if ($this->parent->parent) {
    if ($name =~ /^(?:node|subquery|ref)$/) {
      my $rel=Tree_Query::Common::rel_as_text($this) || 'child';
      my $hint = $rel;
      my $dash = Tree_Query::arrow_dash($rel);
      $rel=~s/{.*//;
      my $color = Tree_Query::arrow_color($rel);
      my $arrow = Tree_Query::arrow($rel);
      "#{Line-arrow:$arrow}#{Line-dash:$dash}#{Line-hint:$hint}".
      (defined($color) ? "#{Line-fill:$color}" : '').
      ($name eq 'ref' and defined($color) ? "#{Oval-outline:$color}#{Oval-fill:$color}" : '').
      '#{Line-tag:relation}'
    }
  } else {
    '#{Line-coords:n,n,n,n}'
  }
?>
style: <? if ($this->parent) {
    if ($this->parent->{'#name'} eq 'or') {
      '#{Line-dash:8,2}'
     }
  } else {
     '#{Node-shape:rectangle}#{Node-surroundtext:1}#{Oval-fill:white}#{NodeLabel-valign:center}'
  }
?>
xlabel:<?
   if ($this->{'#name'} eq 'node'
      and !(grep { ($_->{'#name'}||'node') ne 'node' } $this->ancestors)) {
      '#{-clear:0}#{-coords:n,n}#{-anchor:center}'.$${color}
   }
?>
style:<?
   my $name = $this->{'#name'};
   if ($name eq 'node' and Tree_Query::Common::IsMemberNode($this,$Tree_Query::SEARCH)) {
    ( $this->{'.unhide'} ? '#{Node-shape:polygon}#{Node-polygon:-4,4,4,4,0,-4}' : '#{Node-shape:rectangle}' ).
     '#{Oval-fill:gray}#{Line-width:1}'
   } elsif ($name eq 'node'
      and !(grep { ($_->{'#name'}||'node') ne 'node' } $this->ancestors)) {
     my $color = Tree_Query::NodeIndexInLastQuery($this);
    ( $this->{'.unhide'} ? '#{Node-shape:polygon}#{Node-polygon:-8,8,8,8,0,-8}' : '' ).
     (defined($color) ? '#{Oval-fill:#'.$Tree_Query::colors[$color].'}' : '').
     '#{Line-arrowshape:14,20,4}'
   } elsif ($name eq 'node') {
    ( $this->{'.unhide'} ? '#{Node-shape:polygon}#{Node-polygon:-8,8,8,8,0,-8}' : '' ).
     '#{Node-fill:brown}#{Line-arrowshape:14,20,4}'
   } elsif ($name eq 'test') {
    '#{NodeLabel-dodrawbox:yes}#{Line-fill:lightgray}#{Node-shape:rectangle}#{Oval-fill:gray}'
   } elsif ($name eq 'subquery') {
    ( $this->{'.unhide'} ? '#{Node-shape:polygon}#{Node-polygon:-4,4,4,4,0,-4}' : 
                                     '#{Node-shape:oval}' ).'#{Line-arrowshape:14,20,4}'
   } elsif ($name eq 'ref') {
      '#{Node-shape:rectangle}'
   } elsif ($name =~ /^(?:or|and|not)$/) {
      '#{Node-shape:rectangle}#{Node-surroundtext:1}#{NodeLabel-valign:center}#{Oval-fill:cyan}'
   } else {
     '${Oval-fill:black}'
   }
?>
EOF
  }
}

sub DefaultQueryFile {
  return $ENV{HOME}.'/.tred.d/queries.pml';
}

sub new_tree_id {
  use POSIX;
  POSIX::strftime('q-%y-%m-%d_%H%M%S', localtime());
}

sub NewQuery {
  my ($class,$opts)=@_;
  $opts||={};
  my $id = new_tree_id();
  my $filename = ($opts->{query_file} || DefaultQueryFile());
  my $fl = first { $_->name eq 'Tree Queries' } TrEdFileLists();
  unless ($fl) {
    $fl = Filelist->new('Tree Queries');
    $fl->add(0,$filename);
    AddNewFileList($fl);
  }
  if (CurrentFile() and $root and GetCurrentFileList() && GetCurrentFileList()->name ne 'Tree Queries'
	and PML::SchemaName() ne 'tree_query') {
    my $win = SplitWindowVertically({no_init => 1, no_redraw=>1,no_focus=>0,ratio=>-0.5});
    # SetCurrentWindow($win);
    $grp=$win;
    $Redraw='all';
  }
  unless (-f $filename) {
    unless (-d main::dirname($filename)) {
      mkdir main::dirname($filename);
    }
    my $fsfile = PMLInstance->load({
      filename => $filename,
      config   => $PMLBackend::config,
      string   => <<"END" })->convert_to_fsfile();
<?xml version="1.0" encoding="utf-8"?>
<tree_query xmlns="http://ufal.mff.cuni.cz/pdt/pml/">
 <head>
  <schema href="tree_query_schema.xml" />
 </head>
 <q-trees>
 </q-trees>
</tree_query>
END
    push @main::openfiles, $fsfile;
    SetCurrentFileList($fl->name);
    ResumeFile($fsfile);
  } else {
    SetCurrentFileList($fl->name);
    Open($filename);
  }
  my @trees = GetTrees();
  if (@trees) {
    GotoTree($opts->{tree_no} || scalar(@trees));
  }
  if (!@trees or !$opts->{tree_no} && $root && $root->children) {
    DetermineNodeType(NewTreeAfter());
    $root->{id}=$id if $root;
  }

  ChangingFile(0);
  if ($opts->{new_search}) {
    _NewSearch(Tree_Query::TrEdSearch->new($opts->{new_search}),$opts->{preserve_search});
  } else {
    return SelectSearch() unless $opts->{no_select};
  }
  return;
}


sub get_query_node_schema {
  my ($node)=@_;
  my $qn = first { $_->{'#name'} =~ /^(?:node|subquery)$/ } ($node,$node->ancestors);
  my $table = ($qn && $qn->{'node-type'})||$node->root->{'node-type'};
  return unless $table;
  return $schema_map{$table};
}

sub attr_validate_hook {
  my ($txt,$attr_path,$node)=@_;
  if ($node->{'#name'} eq 'test' and $attr_path eq 'a' or $attr_path eq 'b') {
    eval { query_parser()->parse_flat_expression($txt) };
    return $@ ? 0 : 1
  }
  return 1;
}

sub attr_choices_hook {
  my ($attr_path,$node,undef,$editor)=@_;
  return unless UNIVERSAL::isa($node,'FSNode');
  if ($node->{'#name'} eq 'ref' and $attr_path eq 'target') {
    return [
      grep { defined && length }
      map $_->{name},
      grep $_->{'#name'} =~ /^(?:node|subquery)$/, $node->root->descendants
    ];
  } elsif (!$node->parent or $node->{'#name'} =~ /^(?:node|subquery)$/) {
    if ($attr_path eq 'node-type') {
      if ($SEARCH and UNIVERSAL::can($SEARCH,'get_node_types')) {
	return $SEARCH->get_node_types;
      }
      return [sort keys %schema_map];
    }
  } elsif ($node->{'#name'} eq 'test') {
    if ($attr_path eq 'a') {
      my $type = $SEARCH && $SEARCH->get_type_decl_for_query_node($node);
      if ($type) {
	my @res = sort map { my $t = $_; $t=~s{#content}{[]}g; $t } $type->get_paths_to_atoms({ no_childnodes => 1 });
	return @res ? \@res : ();
      }
    } elsif ($attr_path eq 'b') {
      if (UNIVERSAL::isa($SEARCH,'Tree_Query::SQLSearch')) {
	my $name = $editor->get_current_value('a');
	if ($name and $name=~m{^(?:\$[[:alpha:]_][[:alnum:]_/\-]*\.)?([[:alpha:]_][[:alnum:]_/\-]*)$}) {
	  my $attr = $1;
	  my $table = Tree_Query::Common::GetQueryNodeType($node,$SEARCH);
	  return unless $table=~m{^[[:alpha:]_][[:alnum:]_/\-]*$};
	  if ($attr=~s{^(.*)/}{}) {
	    my $t=$1;
	    $t=~s{/}{_}g;
	    $table=$table.'_'.$t;
	  }
	  my $sql = <<SQL;
SELECT * FROM (
  SELECT "$attr" FROM "${table}"
  WHERE "$attr" IS NOT NULL
  GROUP BY "$attr"
  ORDER BY count(1) DESC
) WHERE ROWNUM<100
ORDER BY "$attr"
SQL
	  #my $sql = qq(SELECT DISTINCT "$attr" FROM ${table} ORDER BY "$attr");
	  print "$sql\n";
	  my $results = eval { $SEARCH->{evaluator}->run_sql_query($sql,{ MaxRows=>100, RaiseError=>1, Timeout => 10 }) };
	  print $@;
	  return if $@;
	  my @res= sort map qq('$_->[0]'),@$results;
	  return @res ? \@res : ();
	}
      } elsif (UNIVERSAL::can($SEARCH,'get_type_decl_for_query_node')) {
	my $name = $editor->get_current_value('a');
	if ($name and $name=~m{^(?:\$([[:alpha:]_][[:alnum:]_\-]*)\.)?([[:alpha:]_][[:alnum:]_/\-]*)$}) {
	  my $var = $1;
	  my $attr = $2;
	  if ($var) {
	    $node=first {
	      $_->{'name'} eq $var and
	      $_->{'#name'}=~/^(?:node|subquery)$/
	    } $node->root->descendants;
	  }
	  return unless $node;
	  my $decl = $SEARCH->get_type_decl_for_query_node($node);
	  if ($decl) {
	    $decl = $decl->find($attr);
	    my $decl_is = $decl->get_decl_type;
	    while ($decl_is == PML_ALT_DECL or
		   $decl_is == PML_LIST_DECL) {
	      $decl = $decl->get_content_decl;
	      $decl_is = $decl->get_decl_type;
	    }
	    if ($decl_is == PML_CHOICE_DECL or
		$decl_is == PML_CONSTANT_DECL) {
	      return [sort map { $_=~/\D/ ? qq{"$_"} : $_ } $decl->get_values];
	    }
	  }
	}
      }
    }
  }
  return;
}

my %id;
my %name2node_hash;
sub init_id_map {
  my ($tree,$assign_names)=@_;
  # assign_names == 1 means actually set name for the nodes in the current subquery
  # assign_names == 2 means actually set name for all nodes

  my @nodes = grep { $_->{'#name'} =~ /^(?:node|subquery)$/ } $tree->descendants;

  my %main_query_nodes;
  if (defined($assign_names) and $assign_names==1) {
    %main_query_nodes;
    @main_query_nodes{ Tree_Query::Common::FilterQueryNodes($tree) } = ();
  }
  %id = map {
    my $n=$_->{name};
    (defined($n) and length($n)) ? ($_=>$n) : ()
  } @nodes;
  %name2node_hash = map {
    my $n=$_->{name};
    (defined($n) and length($n)) ? ($n=>$_) : ()
  } @nodes;
  my $id = 'n0';
  my %occup; @occup{values %id}=();
  for my $n (@nodes) {
    unless (defined $id{$n} and length $id{$n}) {
      $id++ while exists $occup{$id}; # just for sure
      $id{$n}=$id; # generate id;
      if (defined($assign_names) and ($assign_names==2 or exists $main_query_nodes{$n})) {
	$n->{name}=$id;
	ChangingFile(1);
      }
      $occup{$id}=1;
      $name2node_hash{$id}=$n;
    }
  };
}

sub GetNodeName {
  my ($node)=@_;
  croak(q{GetNodeName: #name!~'^(node|subquery)$'}) unless defined($node) and $node->{'#name'} =~ /^(node|subquery)$/;
  if (defined($node->{name})) {
    return $node->{name}
  } else {
    my $i=0;
    $i++ while (exists $name2node_hash{"n$i"});
    my $name = "n$i";
    $node->set_attr('name',$name);
    $name2node_hash{$name}=$node;
    return $name;
  }
}

sub CreateRelationToNamed {
  my $current = $this;
  return unless $current and $current->{'#name'} =~ m/^(node|subquery|ref)$/;
  AutoNameAllNodes();
  Redraw();
  my %nodes;
  for my $n (grep { $_->{'#name'} =~ /^(node|subquery)$/  and $_->{name}} $root->descendants) {
    if (cmp_subquery_scope($current,$n)>=0) {
      $nodes{$n->{name}}=$n;
    }
  }
  my @sel;
  ListQuery('Select node',
	    'browse',
	    [sort keys %nodes],
	    \@sel,
	    {
	      label => { -text=> qq{Select target node} },
	    }
	   ) || return;
  my $target = $nodes{$sel[0]};
  if ($target) {
    EditRelationFromTo($current,$target);
  }
}

sub AssignRelation {
  shift unless ref $_[0];
  my $node = $_[0] || $this;
  return unless
    $node and
    $node->parent and ($node->{'#name'} =~ /^(node|subquery)$/ or ($node->{'#name'} eq 'ref' and $node->{target}));
  unless ($node->parent->parent) {
    delete $node->{relation};
    return 1;
  }

  my ($rel) = map _rel_name($_,'%2$s (%1$s)'), SeqV($node->{relation});
  my @sel=($rel||'child');
  my $node_type = Tree_Query::Common::GetQueryNodeType($node->parent);
  my $relations = GetRelationTypes($node->parent,$SEARCH,1);
  if ($SEARCH && $node_type) {
    @$relations = grep {
      @{[GetRelativeQueryNodeType($node_type, $SEARCH, CreateRelation($_))]}>0
    } @$relations;
  }
  # unshift @$relations,'member';
  return unless @$relations;
  if (@$relations == 1) {
    @sel=@$relations
  } else {
    ListQuery('Select relation',
	      'browse',
	      $relations,
	      \@sel,
	      {
		label => { -text=> qq{Select relation of the current node to its parent} },
	      }
	     ) || return;
  }
  SetRelation($node,$sel[0]) if @sel;
  #if (@sel and $sel[0] eq 'descendant' or $sel[0] eq 'ancestor') {
  #  local $main::sortAttrs=0;
  #  EditAttribute($node,'relation/[1]'.$sel[0]) || return;
  #}
  AssignType($node) || return;
#  } elsif (EditAttribute($node,'relation')) {
#    AssignType($node);
    ChangingFile(1);
#  }
  return 1;
}

sub AssignType {
  shift unless ref $_[0];
  my $node = $_[0] || $this;
  return 1 unless $SEARCH;
  return 1 if ( $node->parent && $node->{'#name'}!~/^(?:node|subquery)$/ );
  my @types;
  if (Tree_Query::Common::IsMemberNode($node,$SEARCH)) {
    if ($SEARCH) {
      my $ptype = Tree_Query::Common::GetQueryNodeType($node->parent,$SEARCH);
      @types = Tree_Query::Common::GetMemberPaths($ptype, $SEARCH);
    }
  } else {
    @types = Tree_Query::Common::GetQueryNodeType($node,$SEARCH);
  }
  if (@types <= 1) {
    $node->{'node-type'} = $types[0];
  } elsif (@types) {
    my @sel=$types[0];
    ListQuery('Select node type',
	      'browse',
	      \@types,
	      \@sel,
	      {
		label => { -text=> qq{Select type of the node} },
	      }
	     ) || return;
    $node->{'node-type'}=$sel[0];
  }
  return 1;
}


sub PasteClipboardWithRename {
  my $n = $root;
  my %names;
  while ($n) {
    if ( $n->{'#name'} =~ /^(?:node|subquery)$/ 
	 and $n->{name} ) {
      $names{$n->{name}}=$n;
    }
    $n=$n->following;
  }

  $n = $TredMacro::nodeClipboard;
  while ($n) {
    if ( $n->{'#name'} =~ /^(?:node|subquery)$/ and $n->{name} ) {
      my $i = 0;
      my $name = $n->{name};
      while (exists $names{$name}) {
	$name = $n->{name}.'_'.($i++);
      }
      RenameNode($n,$n->{name},$name) if $i;
    }
    $n = $n->following;
  }
  paste_from_clipboad();
}

sub RenameNode {
  my ($node, $old_name,$new_name)=@_;
  if ($new_name) {
    $node->{name} = $new_name;
  } else {
    $new_name = GetNodeName($node);
  }
  my $top = $node;
  # find current scope top
  if ($top->{'#name'} ne 'subquery') {
    $top=$top->parent while $top->parent and $top->parent->{'#name'} eq 'node';
    $top = $top->parent if $top->parent and !$top->parent->parent;
  }
  my $n = $top;
  while ($n) {
    if ($n->{'#name'} eq 'ref') {
      $n->{target} = $new_name if $n->{target} eq $old_name;
    } elsif ($n->{'#name'} eq 'test') {
      for my $attr (qw(a b)) {
	$n->{$attr} = RenameInExpression($n->{$attr}, $old_name,$new_name);
      }
    }
    $n=$n->following($top);
  }
  if ($top and !$top->parent) {
    my ($filter)= ListV($top->{'output-filters'});
    for my $attr (qw(return group-by)) {
      if ($filter->{$attr}) {
	for (@{$filter->{$attr}}) {
	  $_ = RenameInExpression($_, $old_name,$new_name);
	}
      }
    }
  }
}

sub RenameInExpression {
  my ($val, $old_name,$new_name)=@_;
  $val =~ s{(['](?:[^'\\]+|\\.)*[']|["](?:[^"\\]+|\\.)*["])|\$\Q$old_name\E}{
    $1 ? $1 : '$'.$new_name
  }ge;
  return $val;
}

#include "ng.inc"

# given to nodes or their IDs ($id and $ref)
# returns 0 if both belong to the same subquery
# returns 1 if $id is in a subquery nested in a subtree of $ref
# (and hence $ref can be referred to from $id)
# returns -1 otherwise

my %arrow = (
  # relation => 'first|last|both|none', # defaults to first
  member => 'none',
);

# TODO: allow these colors to be defined by user
my %color = (
  'member' => '#aaa',
  'child' => 'black',
  'parent' => '#779',
  'descendant' => 'black',
  'ancestor' => '#779',
#  'descendant' => 'blue',
#  'ancestor' => 'lightblue',
  'same-tree-as' => '#eea',
  'same-document-as' => '#eec',
  'depth-first-precedes' => 'red3',
  'depth-first-follows' => 'red4',
  'order-precedes' => 'orange',
  'order-follows' => 'orange3',

  'a/lex.rf' => 'violet',
  'a/aux.rf' => 'thistle',
  'a/lex.rf|a/aux.rf' => 'tan',
  'p/terminal.rf' => 'navy',
  'p/nonterminals.rf' => 'darkgreen',
  'val_frame.rf' => 'cyan',
  'coref_text' => '#4C509F',
  'coref_text.rf' => '#4C509F',
  'coref_gram' => '#C05633',
  'coref_gram.rf' => '#C05633',
  'compl' => '#629F52',
  'compl.rf' => '#629F52',
  'eparent' => 'green',
  'echild' => '#a6d052', # color[0]
);

my %dash = (
  'normal' => '',
  '!' => '', #'8,6',
  '{' => '12,4',
  'ancestor' => '12,4',
  'descendant' => '12,4',
  '!{' => '12,4', #'12,3,4,3',
  '!ancestor' => '12,4', #'12,3,4,3',
  '!descendant' => '12,4', #'12,3,4,3',
);
my $free_arrow_color = 1; # color[0] taken by echild
my %assigned_colors;
sub arrow_color {
  my $rel = shift;
  my $color = $color{$rel} || $assigned_colors{$rel};
  return $color if defined $color;
  $assigned_colors{$rel} = $color = '#'.$colors[$free_arrow_color];
  $free_arrow_color=($free_arrow_color+1) % scalar(@colors);
  return $color;
}
sub arrow {
  my $rel = shift;
  return $arrow{$rel} || 'first';
}
sub arrow_dash {
  my ($rel,$node) = @_;
  if ($rel=~s/\{.*//) {
    return $dash{'{'};
  } else {
    return $dash{$rel} || '';
  }
}

sub get_nodelist_hook {
  my ($fsfile,$tree_no,$prevcurrent,$show_hidden)=@_;
  return unless $fsfile;
  my $tree = $fsfile->tree($tree_no);
  return unless $tree;

  my @nodes=($tree);
  my $node = $tree->firstson;
  my $next;
  my $current = $tree;
  while ($node) {
    $current=$node if $node==$prevcurrent;
    if ($node->{'#name'} =~ /^(?:node|subquery)$/) {
      push @nodes,$node;
      $next=$node->following;
    } else {
      if ($show_hidden or
	    do {
	      my $p = $node->parent;
	      while ($p) { last if $p->{'#name'} =~ /^(?:node|subquery)$/; $p = $p->parent; }
	      $p && $p->{'.unhide'} ? 1 : 0
	    }) {
	push @nodes,$node;
	$next=$node->following;
      } else {
	$next=$node->following_right_or_up;
      }
    }
    $node=$next;
  }
  return [\@nodes,$current];
}

my (%legend,%main_query);
our $no_legend;
our $stop_img;
sub root_style_hook {
  my ($root,$styles,$Opts)=@_;
  DrawArrows_init();
  init_id_map($root);
  %legend=();
  my @nodes = GetDisplayedNodes();
  my $hv = HiddenVisible();
  %main_query = map { $_=>1 } Tree_Query::Common::FilterQueryNodes($root);
  return if $no_legend;
  # icon('process-stop');
  for my $node (@nodes) {
    my @refs;
    my $qn = first { $_->{'#name'} =~ /^(?:node|subquery)$/ } ($node,$node->ancestors);
    my $showHidden = $qn->{'.unhide'} || $hv;
    @refs = ($node) if $node->{'#name'} =~ /^(?:node|subquery|ref)$/ and
      $node->parent and $node->parent->parent;
    unless ($showHidden) {
      push @refs, grep { $_->{'#name'} eq 'ref' }
	map { $_->{'#name'} eq 'not' ? $_->children : $_ }
	  $node->children;
    }
    for my $n (@refs) {
      my $rel=Tree_Query::Common::rel_as_text($n);
      $rel||='child' if $n==$node;
      $rel=~s/{\d*,\d*}/{n,m} (transitive)/;
      next unless $rel;
      if ($n!=$node and $n->parent->{'#name'} eq 'not') {
	$legend{'! '.$rel}=1
      } else {
	$legend{$rel}=1
      }
    }
  }
#  use Data::Dumper;
#  print Dumper(\%legend);
  my $tv = $grp->treeView;
  my $fh=$tv->getFontHeight;
  $tv->realcanvas->delete('legend');
  my $scale=$tv->scale_factor();
  $Opts->{baseYPos}+= $scale*(20 + $fh * (keys(%legend)
					    +
					  ($SEARCH ? 0 : 3)
					    +
					  ($root && $root->firstson ? 0 : 3)
					 ) );
}
sub after_redraw_hook {
  return unless $root;
  DrawArrows_cleanup();
  return if $no_legend;
  return if $SEARCH and !keys(%legend) and ($root and $root->firstson);
  my $tv = $grp->treeView;
  my $scale=$tv->scale_factor();
  my $c=$tv->realcanvas;
  my $fh = $grp->treeView->getFontHeight;
  my $y=$scale * 10;
  my $hint='';
  if (!$SEARCH) {
    $hint .= qq{NO SEARCH ENGINE SELECTED!\nEditing features will be limited. Press 'c' to select a search engine.\n}
  }
  unless ($root and $root->firstson) {
    $hint .= qq{\n} if $hint;
    $hint .= qq{QUERY IS EMPTY!\nPressing '$insert_key' to create the first query node, or 'e' to open the query editor!\n}
  }
  if (length $hint) {
    chomp $hint;
    $c->createText($scale * 15, $y,
		   -font => ($tv->get_scaled_font || $tv->get_font),
		   -text=> $hint,
		   -anchor=>'nw',
		   -tags=>['legend','text_item'] );
    $y+=$scale * (2+($hint=~y/\n/\n/)) * $fh;
  }
  my $lw = $grp->treeView->get_lineWidth;
  $lw=2 if $lw<2;

  my @l = sort { $a->[2] cmp $b->[2] or $a->[1] cmp $b->[1] or $a->[3] cmp $b->[3] }
         map { [$_,(/^(!?\s*)([^\s\{]+)(\{)?/)] } keys %legend;
  for my $legend (@l) {
    my ($r,$negate,$name,$transitive) = @$legend;
    $transitive||='';
    my $color = arrow_color($name);
    my $width = $r eq 'member' ? $scale : $lw*$scale;
    $c->createLine($scale * 75, $y, $scale * 15, $y,
		   -fill => $color,
		   
		   (-dash => [split ',',($negate ? $dash{'!'.$name.$transitive}||$dash{'!'.$transitive} : $dash{$name.$transitive}||$dash{$transitive}||'')]),
		   -arrow => $arrow{$name}||'first',
		   -arrowshape => [14,20,4],
		   -tags => ['scale_width','legend']
		  );
    if ($negate) {
      for my $i (1..2) {
	my $x = $scale * 5 + 20*$scale*$i;
	for my $sign (-1,1) {
	  $c->createLine( $x-3,
			  $y-3*$sign,
			  $x+3,
			  $y+3*$sign,
			  -fill=>$color,
			  -width => $width,
			  -tags => ['scale_width','legend']
			 );
	}
      }
    }
    $c->createText($scale * 85, $y, -font => ($tv->get_scaled_font || $tv->get_font),
		   # -fill => arrow_color($name),
		   -text=> $r, -anchor=>'w', -tags=>['legend','text_item'] );
    $y+=$scale * $fh;
  }
  my @b = $c->bbox('legend');
  $c->lower(
    $c->createRectangle(
      $b[0]-$scale * 5,$b[1],$b[2]+ $scale * 5,$b[3],
      -fill => 'lightyellow',
      -tags=>['legend'],
  ),'legend');

  %legend=();
}

sub node_style_hook {
  my ($node,$styles) = @_;
  my $i=0;
  my @refs;
  my $qn = first { $_->{'#name'} =~ /^(?:node|subquery)$/ } ($node,$node->ancestors);
  my $showHidden = $qn->{'.unhide'} || HiddenVisible();
  my $lw = $grp->treeView->get_lineWidth;
  $lw=2 if $lw<2;
  my $is_member_node = Tree_Query::Common::IsMemberNode($node,$SEARCH);
  if ($main_query{$node}) {
    if ($is_member_node) {
      AddStyle($styles,'Node',
	       -addheight=>3,
	       -addwidth=>3,
	      );
    } else {
      AddStyle($styles,'Node',
	       -addheight=>7,
	       -addwidth=>7,
	      );
      AddStyle($styles,'Line',
	       -width=>1+$lw,
	      );
    }
  } elsif (!$is_member_node and 
	   $node->{'#name'} =~ /^(?:node|subquery)$/) {
    AddStyle($styles,'Node',
	     -addheight=>1,
	     -addwidth=>1);
    AddStyle($styles,'Line',
	     -width=>1+$lw,
	    );
  }
  if ($showHidden) {
    @refs=($node) if $node->{'#name'} eq 'ref';
  } else {
    @refs=grep { $_->{'#name'} eq 'ref' }
      map { $_->{'#name'} eq 'not' ? $_->children : $_ }
      $node->children;
  }
  my $width = $showHidden ? $lw-1 : $lw;
  for my $ref (@refs) {
    DrawArrows($node,$styles, [
      map {
	my $name = $_->name;
	$name = $_->value->{label} if $name eq 'user-defined';
	my $target = $ref->{target};
	my $negate = ($node!=$ref && $ref->parent->{'#name'} eq 'not') ? 1 : 0;
	my $text = Tree_Query::Common::rel_as_text($ref);
	$text = '! '.$text if $negate;
	my $color = $showHidden ? 'gray' : arrow_color($name);
	scalar {
	  -target => $name2node_hash{$target},
	  -fill   => $color,
 	  (-decoration => $negate ?
	     qq{shape=line;force=45%;start=30;step=30;stop=-30;repeat=1000;coords=-3,-3,3,3;tag=scale_width,line;fill=$color;width=$width;rotate=1}.
	     qq{|shape=line;force=45%;start=30;step=30;stop=-30;repeat=1000;coords=3,-3,-3,3;tag=scale_width,line;fill=$color;width=$width;rotate=1}
	       : ''),
	  (-dash   => $negate ?
	     (($text=~/{/) ? $dash{'!'.$1}||$dash{'!{'} : $dash{'!'.$text}||$dash{'!'})
	     : (($text=~/{/) ? $dash{$1}||$dash{'{'} : $dash{$text}||'')),
	  -raise => 8+16*(++$i),
	  -hint => $text,
	  -tag => 'relation:'.$ref,
	}
      } SeqV($ref->attr('relation'))
     ], {
       -arrow => 'last',
       -arrowshape => '14,20,4',
       -width => $width,
       -smooth => 1,
     });
  }
}

sub get_value_line_hook {
  my ($fsfile,$treeNo)=@_;
  return unless $fsfile;
  my $tree = $fsfile->tree($treeNo);
  return unless $tree;
  init_id_map($tree);
  return $VALUE_LINE_MODE == 0 ?
    make_string_with_tags(tq_serialize($tree,{arrow_colors=>\%color}),[]) :
      UNIVERSAL::isa($SEARCH,'Tree_Query::SQLSearch') ? 
	  ($SEARCH->{evaluator} ? $SEARCH->{evaluator}->build_sql($tree,{format=>1})
	     : 'NO EVALUATOR')
	     : 'PLEASE SELECT SQL SEARCH';
}

sub line_click_hook {
  my ($node,$tag,$button, $double,$modif, $ev)=@_;
  if ($node and $double and $button eq '1' and !$modif) {
    if ($tag =~ /^relation:(.*)$/) {
      for my $n (grep { "$_" eq $1 }  $node, $node->children) {
	local $main::sortAttrs=0;
	EditAttribute($n,'relation');
	Redraw();
	last;
      }
    } elsif ($tag eq 'relation') {
      local $main::sortAttrs=0;
      EditAttribute($node,'relation');
      Redraw();
    }
  }
}
sub node_release_hook {
  my ($node,$target,$mod)=@_;
  return unless $target;
  # print STDERR "NODE_RELEASE_HOOK: $mod, $node->{'#name'}, $target->{'#name'}, $node->{optional}\n";
  my $old_parent;
  if ($node->{'#name'} =~ /^(node|subquery)$/) {
    $old_parent = $node->parent;
    $old_parent=$old_parent->parent while ($old_parent and $old_parent->{'#name'} !~ /^(node|subquery)$/);
  }
  if (defined($mod) and $mod =~ /^(Control|Control-3|-2)$/) {
    if (EditRelationFromTo($node,$target)) {
      TredMacro::Redraw_FSFile_Tree();
      ChangingFile(1);
    } else {
      return 'stop';
    }
  } elsif (!$mod and $node->{'#name'} eq 'node' and $target->{'#name'} =~ /^(and|or|not)$/ and !$node->{optional}) {
    $this->set_type(undef);
    $node->{'#name'}='subquery';
    DetermineNodeType($node);
    if (eval { CutPasteWithRelations($node,$old_parent,$target) }) {
      Redraw();
    } else {
      $this->set_type(undef);
      $node->{'#name'}='node';
      DetermineNodeType($node);
    }
    warn $@ if $@;
    return 'stop';
  } elsif ($node->{'#name'} =~ /^(subquery|node)$/) {
    if (eval { CutPasteWithRelations($node,$old_parent,$target) }) {
      Redraw();
    }
    warn $@ if $@;
    return 'stop';
  } elsif (!$mod and $target->{'#name'} =~ /^(ref|test)$/) {
    return 'stop';
  }
  return;
}

sub current_node_change_hook {
  my ($node,$prev)=@_;
  return unless $SEARCH;
  return $SEARCH->select_matching_node($node);
}

############################################################
# Helper routines

sub _rel_name {
  my ($rel,$fmt)=@_;
  ($rel)=SeqV($rel) if ref($rel) eq 'Fslib::Seq';
  return unless ref $rel;
  $fmt ||= q{%s:%s};
  my $rel_name = $rel->name;
  if ($rel_name eq 'user-defined') {
    return sprintf($fmt,$rel->value->{category}||$rel_name,$rel->value->{label});
  }
  return $rel_name;
}


# # convert the main relation to a ref to old_parent
# # and find a ref to new_parent and use it as the main relation
sub CutPasteWithRelations {
  my ($node,$old_parent,$new_parent)=@_;

  if ($old_parent and $old_parent->parent and !$node->{relation}) {
    SetRelation($node,'child');
  }

  # find all relations pointing to this node:
  my @revert;
  my @delete;
  for my $ref (grep { $_->{'#name'} eq 'ref' and $_->{'target'} eq $node->{name} } $node->root->descendants) {
    next unless SeqV($ref->{relation});
    my $is_reversible = isReversible($ref->{relation});
    my $ref_node = $ref;
    my $start = $ref->parent;
    if ($start->{'#name'} !~ /^(node|subquery)$/) {
      if ($start->{'#name'} eq 'not'
	  and !$ref->lbrother and !$ref->rbrother
	  and $start->parent
	  and $start->parent->{'#name'} =~ /^(node|subquery)$/
	 ) {
	$ref_node = $start;
	$start = $start->parent;
      } else {
	$start=$start->parent while $start and $start->{'#name'} !~ /^(node|subquery)/;
	$is_reversible=0;
      }
    }
    next unless $start;
    my $cmp = cmp_subquery_scope($start,$new_parent);
    if ($cmp<0) {
      if ($is_reversible) {
	push @revert,[$ref_node,$start,$ref];
      } else {
	push @delete,[$ref_node,$start,$ref];
      }
    }
  }
  if (@delete) {
    my $answer = QuestionQuery(
       "Scope checking",
       "The node is connected by some non-revertable relations with nodes "
      ."from a different scope. These relations will be dropped if the node is pasted:\n\n"
      .join("\n", map { "\t" . _rel_name($_->[2]->{relation},'%2$s')
			.($_->[1]->{name} ? " from ".$_->[1]->{name} : '')
		      } @delete)."\n",
       "Drop relation and Paste",
       "Ignore relations and Paste",
       "Cancel"
     );
    if ($answer =~ /Ignore/) {
      ChangingFile(1);
      CutPastePreserveOrder($node,$new_parent);
      return 1;
    } elsif ($answer !~ /Paste/) {
      return 0;
    }
  }

  # reverse incomming relations
  for my $r (@revert) {
    my ($ref_node,$start,$ref) = @$r;
    ChangingFile(1);
    ReverseRelation($ref->{relation});
    $ref->{target} = GetNodeName($start);
    CutPastePreserveOrder($ref_node,$node);
  }

  # drop irreversibe incomming relations
  for my $r (@delete) {
    ChangingFile(1);
    DeleteLeafNode($r->[0]);
  }

 my ($ref_from_new_parent) = grep { $_->{'#name'} eq 'ref' and $_->{'target'} eq $node->{name} } $new_parent->children;
 my $ref_to_new_parent;
  if (!$ref_from_new_parent) {
    ($ref_to_new_parent)  = grep { $_->{'#name'} eq 'ref' and $_->{'target'} eq $new_parent->{name} } $node->children;
    if ($ref_to_new_parent) {
      undef $ref_to_new_parent unless isReversible($ref_to_new_parent->{relation});
    }
  }


  my $forget_relation;
  # convert the main relation to a ref to old_parent
  if ($old_parent) {
    my $cmp = cmp_subquery_scope($old_parent,$new_parent);
    if ($cmp<0) {
      if (SeqV($node->{relation})) {
	if (isReversible($node->{relation})) {
	  ChangingFile(1);
	  $forget_relation = 1;
	  my $ref = NewSon($node);
	  $ref->{'#name'}='ref';
	  DetermineNodeType($ref);
	  $ref->{target} = GetNodeName($old_parent);
	  $ref->{relation} = ReverseRelation(delete $node->{relation});
	} else {
	  return (QuestionQuery(
	    "Pasting to a subquery",
	    "The node and its old parent were connected by relation "._rel_name($node->{relation})."\n".
	    "that cannot be reversed. This relation will be dropped after paste",
	    "Drop relation and Paste",
	    "Cancel") =~ /Paste/ ? 1 : 0);
	}
      }
    } elsif($cmp>=0) {
      my $answer;
      if (!$ref_from_new_parent and !$ref_to_new_parent) {
	my $rel_name = _rel_name($node->{relation},'%2$s');
  	if ($old_parent and $rel_name) {
	  if ($new_parent and $new_parent->parent) {
	    $answer = 'Use';
	  } else {
	    $answer = 'Make';
	  }
# 	  $answer = QuestionQuery(
# 	    "Preserve relations",
# 	    "Do you want to preserve relation $rel_name to old parent as a reference?",
# 	    "Make a reference",
# 	    "Use as relation to new parent",
# 	    "Forget",
# 	    "Cancel");
# 	  return if $answer eq 'Cancel';
 	} else {
 	  $answer = 'Forget';
 	}
      } else {
	$answer = 'Make';
      }
      if ($answer =~ /Forget/) {
	$forget_relation = 1;
      } elsif ($answer =~ /Make/) {
	ChangingFile(1);
	my $relation = delete $node->{relation};
	$forget_relation = 1;
	my $ref = NewSon($old_parent);
	$ref->{'#name'}='ref';
	DetermineNodeType($ref);
	$ref->{target} = GetNodeName($node);
	$ref->{relation} = $relation;
      }
    }
  }



  # find a ref to new_parent and use it as the main relation
  #
  {
    my $ref = $ref_from_new_parent || $ref_to_new_parent;
    if ($ref) {
      ChangingFile(1);
      $node->{relation} = delete $ref->{relation};
      ReverseRelation($node->{relation}) if $ref_to_new_parent;
      DeleteLeafNode($ref);
      CutPastePreserveOrder($node,$new_parent);
      DetermineNodeType($node);
      return 1;
    }
 }

  ChangingFile(1);
  CutPastePreserveOrder($node,$new_parent);
  AssignRelation($node) if $forget_relation;
  return 1;
}

sub CutPastePreserveOrder {
  my ($node,$new_parent)=@_;
  my $n = $new_parent->root;
  my $last; # left-nost child of new_parent that precedes node
  while ($n && $n!=$node) {
    $last = $n if $n->parent == $new_parent;
    $n=$n->following;
  }
  if ($last) {
    return CutPasteAfter($n,$last);
  } else {
    return CutPaste($n,$new_parent);
  }
}

sub isReversible {
  my ($relation)=@_;
  my ($rel) = SeqV($relation);
  my $rel_name = _rel_name($rel);
  my $reversed_name = Tree_Query::Common::reversed_relation($rel_name);
  return $reversed_name || undef;
}

sub ReverseRelation {
  my ($relation)=@_;
  croak("Cannot call ReverseRelation() on undefined value") unless $relation;
  my ($rel)=SeqV($relation);
  my $reversed_name = Tree_Query::Common::reversed_relation(_rel_name($rel));
  if ($reversed_name) {
    if ($reversed_name=~s/^(user-defined|implementation|pmlrf)://) {
      $rel->set_name('user-defined');
      $rel->value->{label}=$reversed_name;
      $rel->value->{category}=$1 unless $1 eq 'user-defined';
    } else {
      $rel->set_name($reversed_name);
    }
  }
  return $relation;
}


sub EditRelationFromTo {
  my ($node,$target)=@_;
  my $type = $node->{'#name'};
  my $target_is = $target->{'#name'};
  return unless $target_is =~/^(?:node|subquery)$/
    and $type =~/^(?:node|subquery|not|and|or|ref)$/;
  return if cmp_subquery_scope($node,$target)<0;
  my @sel =
    map _rel_name($_->{relation},'%2$s (%1$s)'),
    grep { $_->{target} eq $target->{name} }
      ($type eq 'ref' ? $node : (grep $_->{'#name'} eq 'ref', $node->children));
  my $node_type;
  my $relations;
  if ($type =~/^(?:node|subquery)$/) {
    $node_type = Tree_Query::Common::GetQueryNodeType($node,$SEARCH);
    $relations = GetRelationTypes($node,$SEARCH,0);
  } else {
    my $n = $node->parent;
    $n=$n->parent while $n->{'#name'} =~/^(?:node|subquery)$/;
    $node_type = Tree_Query::Common::GetQueryNodeType($n,$SEARCH);
    $relations = GetRelationTypes($n,$SEARCH,0);
  }
  my $target_type = Tree_Query::Common::GetQueryNodeType($target,$SEARCH);
  if ($SEARCH && $node_type && $target_type) {
    @$relations = grep {
      first { $_ eq $target_type } @{[GetRelativeQueryNodeType($node_type, $SEARCH, CreateRelation($_))]}
    } @$relations;
  }
  return unless @$relations;
  ListQuery('Select relations',
	    ($type eq 'ref' ? 'browse' : 'multiple'),
	    $relations,
	    \@sel,
	    {
	      label => { -text=> qq{Select query-node relations to add or preserve} },
	    }
	   ) || return;
  init_id_map($node->root);
  if ($type eq 'ref') {
    $node->{target}=GetNodeName($target);
    SetRelation($node,$sel[0]) if @sel;
    return 1 if @sel
  } else {
    AddOrRemoveRelations($node,$target,\@sel,{-add_only=>0});
    return 1;
  }
  return;
}

# note: you have to call init_id_map($root); first!
sub AddOrRemoveRelations {
  my ($node,$target,$types,$opts)=@_;
  my $target_name = GetNodeName($target);
  my %types = map { $_=> 1 } @$types;
  my @refs =
    grep { $_->{target} eq $target_name }
    grep { $_->{'#name'} eq 'ref' } $node->children;
  for my $ref (@refs) {
    my ($rel)=SeqV($ref->{relation});
    if ($rel) {
      my $rel_name = _rel_name($rel,'%2$s (%1$s)');
      if ($opts->{-add_only}) {
	delete $types{$rel_name}; # already have it
      } elsif (!$types{$rel_name}) {
	DeleteLeafNode($ref);
      } else {
	delete $types{$rel_name}; # already have it
      }
    }
  }
  my @new;
  for my $type (grep { $types{$_} } @$types) {
    my $ref = NewSon($node);
    $ref->{'#name'}='ref';
    DetermineNodeType($ref);
    $ref->{target}=$target_name;
    my ($name,$value);
    SetRelation($ref,$type);
    push @new,$ref;
  }
  return @new;
}



our %is_match;
our $btred_results;
our @last_results;

# determine which nodes are part of the current result
sub map_results {
  return unless $SEARCH;
  my ($root,$filename,$tree_number,$fsfile)=@_;
  $filename = FileName() unless defined $filename;
  $tree_number = CurrentTreeNumber() unless defined $tree_number;
  $fsfile = CurrentFile() unless defined $fsfile;
  my $map = $SEARCH->map_nodes_to_query_pos($filename,$tree_number,$root,$fsfile);
  %is_match = defined($map) ? %$map : ();
}

sub NodeIndexInLastQuery {
  if ($SEARCH) {
    return $SEARCH->node_index_in_last_query(@_);
  }
  return;
}

sub GetSearch {
  my ($ident)=@_;
  return first { $_->identify eq $ident }  @SEARCHES;
}

sub Search {
  shift unless ref($_[0]);
  my $opts=$_[0] || {};
  ChangingFile(0);
  unless ($SEARCH) {
    SelectSearch() || return;
  }

  unless ($this->parent) {
    $this=first { $_->{'#name'} eq 'node' } $this->children;
  } else {
    my $non_node = first { $_->parent and $_->{'#name'} ne 'node' } reverse ($this, $this->ancestors);
    $this=$non_node->parent if $non_node;
  }

  if (UNIVERSAL::isa($SEARCH,'Tree_Query::SQLSearch')) {
    $SEARCH->search_first({%$opts});
  } else {
    $SEARCH->search_first($opts);
  }
}

sub __this_module_path {
  return [caller(0)]->[1]
}

sub SelectSearch {
  my @sel;
  require Tk::DialogReturn;
  my $d = ToplevelFrame()->DialogBox(
    -title => 'Select search target',
    -cancel_button=>'Cancel',
    -buttons => ['Cancel'],
  );
  $d->add('Label', -text=>'Which data source are you going to query?')->pack(-pady => 10);
  my $f = $d->add('Frame')->pack(qw(-expand 1 -fill both));
  $d->BindEscape;
  my ($vol,$dir)=File::Spec->splitpath(__this_module_path());
  my @b;
  for my $b (['Files (local)' => 'file', 0, 0, 0], # grid row, grid column, underline
#	     ['List of files (local)' => 'filelist',0,1,5],
	     ['Treebank (server)' => 'remote-db',0,1,0],
#	     ['Database (server)' => 'local-db',1,1,0],
	    ) {
    my $but = $f->Button(
	    -compound=>'top',
	    -text => $b->[0],
	    -underline => $b->[4],
	    -image => icon($b->[1]),
	    -highlightthickness => 3,
	    -command => [sub { $_[0]->{selected_button}=$_[1] },$d,$b->[0]],
	   );
    $but->grid(-column => $b->[3], -row => $b->[2]);
    $but->bind($d,'<Return>'=>'Invoke');
    push @b,$but;
  }
  for (0..$#b) {
    $b[$_]->bind('<Right>',[$b[ ($_+1) % (0+@b) ],'focus']);
    $b[$_]->bind('<Left>',[$b[ $_-1 ],'focus']);
#    $b[$_]->bind('<Down>',[$b[ [2,3,1,0]->[$_] ],'focus']);
#    $b[$_]->bind('<Up>',[$b[ [3,2,0,1]->[$_] ],'focus']);
  }

  $d->configure(-focus=>$b[0]);
  my $preserve=0;
  if ($SEARCH) {
    $d->add('Checkbutton',-variable => \$preserve,-underline=>0,-text=>'Preserve current search')
      ->pack();
  }
  $d->BindButtons;
  my $choice = $d->Show;
  return if $choice eq 'Cancel';
  my $file;
  my $particular_trees=0;
  if ($choice=~/^File/) {
    my @sel;
    my $filelist_no='A';
    my $file_no=0;
    ListQuery('Search through...',
	      'browse',
	      [
		#  (map { $_->identify } @SEARCHES),
		map((($file_no++).'. ')."File: ".$_, uniq(map $_->filename, grep ref, map(CurrentFile($_), TrEdWindows()), GetOpenFiles())),
		map((($filelist_no++).'. ')."List: ".$_->name #." (".$_->file_count." files)", grep $_->file_count
		      , TrEdFileLists())
	       ],
	      \@sel,
	      {
		label => { -text=> qq{Select a file or file-list to search through} },
		init => sub {
		  my ($d,$l) = @_;
		  my $f = $d->add('Frame')->pack(-expand => 1, -fill=>'x');
		  $f->Checkbutton(-text=>'Particular Trees Only',
				  -variable =>\$particular_trees,
				 )->pack(-side=>'left');
		  $f->Button(-text=>'Add List',
			     -command => MacroCallback(sub {
			        my $f = $d->getOpenFile(
				  -filetypes=> 
				    [["Filelists",           ['.fl']],
				     ["All files",           ['*','*.*']]
				    ],
				  -title=> "Load filelist ...");
				return unless ($f and -f $f);
				my $fl = Filelist->new(undef, $f);
				$fl->load();
				if (AddNewFileList($fl)) {
				  $l->insert('end',"List: ".$fl->name);
				  $l->see('end');
				  $l->selectionClear(0,'end');
				  $l->selectionSet('end');
				  $l->activate('end');
				}
			      })
			    )->pack(-side=>'right');
		  $f->Button(-text=>'Add File',
			     -command => MacroCallback(sub {
			        my $f = $d->getOpenFile(
				  -filetypes => \@main::open_types,
				  -title=> "Load file ...");
				return unless ($f and -f $f);
				my $idx=0;
				my $last=$l->index('end');
				$idx++ while ($idx<$last and $l->get($idx)=~/^(\d+)\. File:/);
				$l->insert($idx,"$idx. File: ".$f);
				$l->see($idx);
				$l->selectionClear(0,'end');
				$l->selectionSet($idx);
				$l->activate($idx);
			      })
			    )->pack(-side=>'right');
		},
	      }
	     ) || return;
    return unless @sel;
    $file = $sel[0];
  }
  my $S;
  # my $S = GetSearch($sel);
  #  unless ($S) {
  if ($choice =~ /^Database/) {
    $S=Tree_Query::SQLSearch->new();
  } elsif ($choice =~ /^Treebank/) {
    $S=Tree_Query::HTTPSearch->new();
  } elsif ($choice =~ /^File/) {
    if ($file=~s/^(\S+. )?File: //) {
      $S=Tree_Query::TrEdSearch->new({file => $file, particular_trees=>$particular_trees});
    } elsif ($file=~s/^(\S+. )?List: //) {
      $file =~ s/ \([^\)]* files\)$//g;
      $S=Tree_Query::TrEdSearch->new({filelist => $file, particular_trees=>$particular_trees});
    }
    #    } elsif ($choice =~ /^List/) {
    #      $S=Tree_Query::TrEdSearch->new({filelist => $file});
  }
  if ($S) {
    _NewSearch($S,$preserve);
  }
  # TODO
  #
  return $SEARCH;
}

sub _NewSearch {
  my ($S,$preserve) = @_;
  if (!$preserve and $SEARCH) {
    DestroyUserToolbar($SEARCH->identify);
    @SEARCHES = grep { $_!=$SEARCH } @SEARCHES;
    $SEARCH=undef;
  }
  my $ident = $S->identify;
  @SEARCHES = grep { $_->identify ne $ident } @SEARCHES;
  push @SEARCHES, $S;
  return SetSearch($S);
}

sub SetSearch {
  my ($s) = @_;
  my $prev=$SEARCH && $SEARCH->identify;
  my $ident = $s && $s->identify;
  $SEARCH=$s;
  if (!defined($prev) or $ident ne $prev) {
    for my $name (grep defined, $ident,$prev) {
      my $tb = GetUserToolbar($name);
      next unless $tb;
      my $lab = first { ref($_) eq 'Tk::Label' } $tb->children;
      $lab->configure(-font=> $name eq $ident ? 'C_small_bold' : 'C_small' ) if $lab;
    }
  }
}

sub CreateSearchToolbar {
  my ($ident)=@_;
  RemoveUserToolbar($ident);
  my $tb = NewUserToolbar($ident);
  for my $but (['Query' =>
		    sub{
		      my $s = GetSearch($ident);
		      if ($s) {
			SetSearch($s);
			$s->search_first;
		      }
		      ChangingFile(0);
		    },
		'search_filter',
		'Perform the query: show first match/output (space)',
	       ],
	       ['Search' =>
		  sub{
		    my $s = GetSearch($ident);
		    if ($s) {
		      SetSearch($s);
		      $s->search_first({no_filters=>1});
		    }
		    ChangingFile(0);
		  },
		'search',
		'Find first match, ignore output filters (Shift+space)',
	       ],
	       ['Count' =>
		    sub{
		      my $s = GetSearch($ident);
		      if ($s) {
			SetSearch($s);
			$s->search_first({ no_filters=>1, count=>1 });
		      }
		      ChangingFile(0);
		    },
		'search_count',
		'Count occurrences (Ctrl+space)',
	       ],
	       ['Previous match' =>
		      sub{
			my $s = GetSearch($ident);
			if ($s) {
			  SetSearch($s);
			  $s->show_prev_result;
			}
			ChangingFile(0);
		      },
		'search_previous',
		'Show previous match (p)',
	       ],
	       ['This match' =>
		    sub{
		      my $s = GetSearch($ident);
		      if ($s) {
			SetSearch($s);
			$s->show_current_result;
		      }
		      ChangingFile(0);
		    },
		'apply',
		'Show corresponding node in the current match (m)',
	       ],
	       ['Next match' =>
		    sub{
		      my $s = GetSearch($ident);
		      if ($s) {
			SetSearch($s);
			$s->show_next_result;
		      }
		      ChangingFile(0);
		    },
		'search_next',
		'Show next match (n)',
	       ],
	      ) {
    my $b = $tb->Button(-text  => $but->[0],
		-command => MacroCallback($but->[1]),
		-padx => 2,
		-font    =>'C_small',
		-borderwidth => 0,
		-takefocus=>0,
		-relief => $main::buttonsRelief,
		-image => icon($but->[2]),
		-compound => 'top',
	       )->pack(-side=>'left',-padx => 5);
    AttachTooltip($b,$but->[3]);
  }
  my $l = $tb->Label(-text=>$ident,-font=>'C_small')->pack(-side=>'left');
  $l->bind('<1>',
	   MacroCallback(
	     sub{
	       my $s = GetSearch($ident);
	       if ($s) {
		 SetSearch($s);
		 $s->show_current_result;
	       }
	       ChangingFile(0);
	     }),
	  );
  my $b = $tb->Button(-text=>'x',
	      -font => 'C_small',
		-takefocus=>0,
	      -relief => $main::buttonsRelief,
	      -borderwidth=> $main::buttonBorderWidth,
	      -image => icon('16x16/remove'),
	      -command => MacroCallback([sub {
					   my $ident=shift;
					   DestroyUserToolbar($ident);
					   my ($s) = grep { $_->identify eq $ident } @SEARCHES;
					   @SEARCHES = grep { $_ != $s } @SEARCHES;
					   $SEARCH = undef if $SEARCH and $SEARCH == $s;
					   ChangingFile(0);
					 },$ident])
	     )->pack(-side=>'right');
  AttachTooltip($b,'Close this search.');
  my $label;
  $tb->Label(-textvariable=>\$label,-font=>'C_small')->pack(-side=>'right',-padx => 5);
  return ($tb,\$label);
}

sub EditNodeConditions {
  EditQuery($this,{no_childnodes=>1})
}
sub EditSubtree {
  EditQuery($this)
}

our ($match_node_re,$variable_re,$relation_re,$rel_length_re);
$match_node_re  = qr/\[((?:(?> [^][]+ )|(??{ $match_node_re }))*)\]/x;
$variable_re = qr/\$[[:alpha:]_][[:alnum:]_]*/;
$relation_re = qr/descendant|ancestor|child|parent|descendant|ancestor|depth-first-precedes|depth-first-follows|order-precedes|order-follows|same-tree-as|same-document-as/;
$rel_length_re='(?:\{[0-9]*,[0-9]*\})';


sub _find_type_in_query_string {
  my ($context,$rest)=@_;
  my ($type,$var);
  my $user_defined = Tree_Query::Common::user_defined_relations_re($SEARCH);
  $user_defined.='|' if length $user_defined;
  my $pmlrf_re = Tree_Query::Common::pmlrf_relations_re($SEARCH);

  if ($context =~ /(${variable_re})\.$/) {
    $var = $1;
    if (($context.$rest) =~ m{^(.*)\bmember(?:\s+|\s*::\s*)?(${PMLSchema::CDATA::Name}(?:/${PMLSchema::CDATA::Name})*)\s+\Q$var\E\s*:=\s*\[}s) {
      $type = $2;
      my ($prec) = _find_type_in_query_string($1,substr($context.$rest,length($1)));
      $type = $prec.'/'.$type
    } elsif (($context.$rest)=~/(?:(${user_defined}${relation_re})${rel_length_re}(?:\s+|\s*::\s*))?(${PMLSchema::CDATA::Name})\s+\Q$var\E\s*:=\s*\[/) {
      $type = $2;
    } elsif (($context.$rest)=~/(?:(${pmlrf_re})${rel_length_re}?(?:\s+|\s*->\s*))?(${PMLSchema::CDATA::Name})\s+\Q$var\E\s*:=\s*\[/) {
      $type = $2;
    }
  } else {
    $context = reverse $context;
    my $depth = 0;
    while (length $context) {
      my $prev = $context;
      $context =~ s/^[^]["']+|"(?:[^"\\]+|\\.)*"|'(?:[^'\\]+|\\.)*'//;
      if ($context=~s/^\[\s*//) {
	last if $depth==0;
	$depth--;
      }
      $depth++ if $context=~s/^\]\s*//;
      return if $prev eq $context; # not a context for inserting anything
    }
    return unless $context =~ /\S/;
    $context=reverse $context;
    if ($context =~ m{^(.*)\bmember\s+(${PMLSchema::CDATA::Name}(?:/${PMLSchema::CDATA::Name})*)(?:\s+|$)(?:(${variable_re})\s*:=)?$}s) {
      $type = $2;
      my ($prec) = _find_type_in_query_string($1,substr($context.$rest,length($1)));
      $type = $prec.'/'.$type
    } elsif ($context =~ /(?:(${user_defined}${relation_re})(?:\s+|\s*::\s*))?(?:(${PMLSchema::CDATA::Name})(?:\s+|$))(?:(${variable_re})\s*:=)?$/) {
      $type = $2;
    } elsif ($context =~ /(?:(${pmlrf_re})(?:\s+|\s*->\s*))?(?:(${PMLSchema::CDATA::Name})(?:\s+|$))(?:(${variable_re})\s*:=)?$/) {
      $type = $2;
    }
  }
  return ($type,$var);
}

sub _find_element_names_for_decl {
  my ($decl)=@_;
  my %names;
  $decl->get_schema->for_each_decl(
    sub {
      my ($d)=@_;
      if ($d->get_decl_type == PML_ELEMENT_DECL
	    and $d->get_content_decl == $decl) {
	$names{ $d->get_name } = 1;
      }
    }
   );
  return [sort keys %names];
}

sub _editor_offer_values {
  my ($ed,$operator,$qn) = @_;
  my @sel;
  my $context= $ed->get('0.0','insert');
  my $eq;
  if ($SEARCH) {
    if ($context=~s{(?:(${variable_re}\.)?
                       (${PMLSchema::CDATA::Name}(?:/${PMLSchema::CDATA::Name})*)|
                       (name)\(\s*(${variable_re})?\s*\)
                    )
                    ((?:\s*!?\s*=?|\s*!\s*in|\s+in)\s*)$
                   }{$1}x) {
      my ($var,$attr,$is_name,$name_var) = ($1,$2,$3,$4);
      $eq=$5;
      my ($type) = _find_type_in_query_string($context,
					      $ed->get('insert','end'));
      $type=Tree_Query::Common::GetQueryNodeType($qn->parent,$SEARCH).$type if $qn && $type=~m{^/};
      my $decl = $SEARCH->get_decl_for($type);
      my @values;
      if ($is_name) {
	@values = @{_find_element_names_for_decl($decl)};
      } elsif ($decl and ($decl = $decl->find($attr))) {
	my $decl_is = $decl->get_decl_type;
	while ($decl_is == PML_ALT_DECL or
		 $decl_is == PML_LIST_DECL) {
	  $decl = $decl->get_content_decl;
	  $decl_is = $decl->get_decl_type;
	}
	if ($decl_is == PML_CHOICE_DECL or
	      $decl_is == PML_CONSTANT_DECL) {
	  @values = $decl->get_values;
	}
      }
      if (@values) {
	$sel[0] = $values[0] unless $operator eq 'in';
	unless (ListQuery(
	  'Select value',
	  $operator eq 'in' ? 'multiple' : 'browse',
	  \@values,
	  \@sel,
	  {
	    top => $ed->toplevel,
	  }
	 )) {
	  $ed->focus;
	  $ed->delete('insert -'.length($eq).' chars','insert');
	  if ($eq =~ /!/) {
	    $ed->Insert(' !'.$operator.' ');
	  } else {
	    $ed->Insert(' '.$operator.' ');
	  }
	  return;
	}
      }
    }
  }
  if (!defined($eq) and $context =~ m{((?:\s*!?\s*=?|\s*!\s*in|\s+in)\s*)$}) {
    $eq = $1;
  };
  $ed->focus;
  @sel = map { $_=~/\D/ ? qq{"$_"} : $_ } @sel;
  $ed->delete('insert -'.length($eq).' chars','insert');
  if (defined($eq) and $eq =~ /!/) {
    $ed->Insert(' !');
  } else {
    $ed->Insert(' ');
  }
  if ($operator eq 'in') {
    $ed->Insert(q(in { ).join(', ',@sel).q( } ));
  } else {
    $ed->Insert($operator.' '.(@sel ? $sel[0] : ''))
  }
}

sub _assign_shortcuts {
  my ($labels)=@_;
  my %used;
  my %map;
  my @unassigned = @$labels;
  my $i=0;
  while (@unassigned) {
    my @next;
    for (@unassigned) {
      if (length($_) > $i) {
	my $ul = lc substr($_, $i, 1);
	if ($ul !~ /[[:alnum:]]/ or exists $used{$ul}) {
	  push @next,$_;
	} else {
	  $used{$ul}=1;
	  $map{$_}=$i;
	}
      }
    }
    @unassigned = @next;
    $i++;
  }
  return \%map;
}

sub EditQuery {
  my ($node,$opts)=@_;

  $opts||={};

  my $no_childnodes = ($node->{'#name'} =~ /^(node|subquery)$/ and $opts->{no_childnodes}) ? 1 : 0;
  my $string = $opts->{string} || as_text($node,{no_childnodes=>$no_childnodes});
  my $result;
  # {
  #   my $t0 = new Benchmark;
  #   my $t1 = new Benchmark;
  #   my $time = timestr(timediff($t1,$t0));
  #   print "creating parser took: $time\n";
  # }
  eval { require Tk::TextUndo; };
  my $qopts={
    -widget => ['TextUndo', -background => 'white'],
    $node->parent ? (-cursor => 'end - 3 chars') : (),
	     -height => 16,
	     -init => sub {
	       my ($d,$ed)=@_;
	       my $f = $d->add('Frame')->pack(-side=>'top',-expand => 'no');
	       for (
		 qw(? 3x),
		 ['Relation' => undef, #[ map { /^(\S+)/ } @{GetRelationTypes($this)} ],
		  {
		    -command => [
		      sub {
			my ($ed,$qn)=@_;
			my ($node_type) = _find_type_in_query_string($ed->get('0.0','insert'),
								     $ed->get('insert','end'));
			$node_type=Tree_Query::Common::GetQueryNodeType($qn->parent,$SEARCH).$node_type if $node_type=~m{^/};
			my $relations;
			if ($SEARCH and defined $node_type and length $node_type) {
			  $relations =
			    [ grep {
			      @{[GetRelativeQueryNodeType($node_type,
							  $SEARCH,
							  CreateRelation($_))]}>0
							} @{GetRelationTypes($this,$SEARCH,1)}
						       ];
			} else {
			  $relations = GetRelationTypes($this,$SEARCH,1);
			}
			#unshift @$relations,'member';
			return unless @$relations;
			my @sel=['child'];
			if (ListQuery('Select relation',
				  'browse',
				  $relations,
				      \@sel)) {
			  my $sel = $sel[0];
			  $sel=~s/\s.*//;
			  $ed->Insert($sel.' ');
			}
			$ed->focus;
		      },$ed,$node
		     ],
		  }
		 ],
		 ['Type' => undef,
		  {
		    -state => $SEARCH ? 'normal' : 'disabled',
		    -command => [
		      sub {
			my ($ed,$qn)=@_;
			my $prev=$ed->get('0.0','insert');
			my ($node_type) = _find_type_in_query_string($prev,
								     $ed->get('insert','end'));
			$node_type=Tree_Query::Common::GetQueryNodeType($qn->parent,$SEARCH).$node_type if $node_type=~m{^/};
			my $relation = 'child';
			my $user_defined = Tree_Query::Common::user_defined_relations_re($SEARCH);
			my $pmlrf_re = Tree_Query::Common::pmlrf_relations_re($SEARCH);
			if ($prev=~/(${relation_re}|member)${rel_length_re}?\s*(?:::)?$/) {
			  $relation = $1;
			} elsif ($user_defined and $prev=~/(${user_defined})${rel_length_re}?\s*(?:::)?$/) {
			  $relation = $1.' (implementation)';
			} elsif ($pmlrf_re and $prev=~/(${pmlrf_re})${rel_length_re}?\s*(?:->)?$/) {
			  $relation = $1.' (pmlrf)';
			}
			my @types;
			if ($relation eq 'member') {
			  @types = Tree_Query::Common::GetMemberPaths($node_type, $SEARCH);
			} elsif($node_type) {
			  @types = GetRelativeQueryNodeType($node_type,$SEARCH,CreateRelation($relation));
			} else {
			  @types = @{$SEARCH->get_node_types};
			}
			if (@types==1) {
			  $ed->Insert($types[0].' ');
			} elsif (@types) {
			  my @sel=[$types[0]];
			  if (ListQuery('Select node type', 'browse', \@types, \@sel)) {
			    $ed->Insert($sel[0].' ');
			  }
			}
			$ed->focus;
		      },$ed,$node
		     ],
                  }
		 ],
		 ['Member' => undef,
		  {
		    -state => $SEARCH ? 'normal' : 'disabled',
		    -command => [
		      sub {
			my ($ed,$qn)=@_;
			my $prev=$ed->get('0.0','insert');
			my ($node_type) = _find_type_in_query_string($prev,
								     $ed->get('insert','end'));
			$node_type=Tree_Query::Common::GetQueryNodeType($qn->parent,$SEARCH).$node_type if $node_type=~m{^/};
			my @types = Tree_Query::Common::GetMemberPaths($node_type, $SEARCH);
			if (@types==1) {
			  $ed->Insert(' member '.$types[0].' [  ]');
			  $ed->SetCursor('insert - 2 chars');
			} elsif (@types) {
			  my @sel=[$types[0]];
			  if (ListQuery('Select node type', 'browse', \@types, \@sel)) {
			    $ed->Insert(' member '.$sel[0].' [  ]');
			    $ed->SetCursor('insert - 2 chars');
			  }
			}
			$ed->focus;
		      },$ed,$node
		     ],
                  }
		 ],
		 ['$n :=',undef, {
		   -command => [
		     sub {
			 my ($ed,$tree)=@_;
			 my $string = $ed->get('0.0','end');
			 my %seen = map { $_=> 1 } ($string =~ m{\$([[:alpha:]_][[:alnum:]_]*)}g);
			 while ($tree) {
			   $seen{$tree->{name}} = 1 if $tree->{'#name'}=~/node|subquery/;
			   $tree = $tree->following;
			 }
			 my $x = 'a';
			 while ($seen{$x}) {
			   $x++;
			 }
			 # replace any previous assignment
			 if ($ed->get('0.0','insert') =~ /(\$[[:alpha:]_][[:alnum:]_]*\s*:=\s*)$/s) {
			   $ed->delete('insert - '.length($1).' chars','insert');
			 }
			 $ed->Insert('$'.$x.' := ');
			 if ($ed->get('insert','end') !~ /^\s*\[/s) {
			   $ed->Insert('[  ]');
			   $ed->SetCursor('insert - 2 chars');
			 }
		       },$ed,$node->root
		   ],
		 } ],
		 '[ ]',
		 "\n",
		 ['Attribute',undef,
		  {
		    -state => $SEARCH ? 'normal' : 'disabled',
		    -underline => 5,
		    -command =>
		      [sub {
			 my ($ed,$qn)=@_;
			 my $context = $ed->get('0.0','insert');
			 my ($type,$var) = _find_type_in_query_string($context,
								      $ed->get('insert','end'));
			 if (defined $type and length $type) {
			   $type=Tree_Query::Common::GetQueryNodeType($qn->parent,$SEARCH).$type if $type=~m{^/};
			   my $decl = $SEARCH->get_decl_for($type);
			   if ($decl) {
			     my @res = map { my $t = $_; $t=~s{#content}{[]}g; $t } $decl->get_paths_to_atoms({ no_childnodes => 1 });
			     if (@{ _find_element_names_for_decl($decl) }) {
			       unshift @res, 'name()';
			     }
			     if (@res) {
			       my @sel=$res[0];
			       if (ListQuery('Select attribute'.
					       ($var ? ' for '.$var : ()),'browse',
					     \@res,
					     \@sel,
					     {
					       top=>$ed->toplevel,
					       list=>{ -exportselection => 0 }
					     }
					    )) {
				 $ed->deleteSelected;
				 $context = $ed->get('0.0','insert');
				 if (!$var and $context=~/[[:alnum:]"'}\)\]]\s*$/ and
				       $context!~/\b(and|or|div|mod|sort\s*by|over|give|for)\s*$/) {
				   $ed->Insert(',');
				 }
				 $ed->Insert(($var ? '' :' ').$sel[0].' ');
			       }
			     }
			   }
			 }
			 $ed->focus;
		       },$ed,$node]
		     }
		 ],
		 ['!','!'],
		 ['= (equals)',undef,{-command => [\&_editor_offer_values,$ed,'=',$node]}],
		 ['in { ... }',undef,{-command => [\&_editor_offer_values,$ed,'in',$node]}],
		 ['~ (regexp)' => '~'],
		 qw|< >|,
		 ['Function' => [#map { $_.'()' }
				 sort
				   qw(
				       descendants(#NODE?#)
				       lbrothers(#NODE?#)
				       rbrothers(#NODE?#)
				       sons(#NODE?#)
				       depth_first_order(#NODE?#)
				       depth(#NODE?#)
				       lower(#STR#)
				       upper(#STR#)
				       length(#STR#)
				       substr(#STR#,OFFSET,LEN?),
				       tr(#STR#,CHARS_TO_REPLACE,REPLACEMENT_CHARS)
				       replace(#STR#,SUBSTR,REPLACEMENT)
				       ciel(#NUM#)
				       floor(#NUM#)
				       round(#NUM#,PLACES?)
				       trunc(#NUM#,PLACES?)
				       percnt(#NUM#)
				       name(#NODE?#)
				       file(#NODE?#)
				       tree_no(#NODE?#)
				       address(#NODE?#)
				       substitute(#STR#,REGEXP,REPLACEMENT,FLAGS?)
				       match(#STR#,REGEXP,FLAGS?)
				    )
				  ]],
		 "\n",
		 qw|, and or () !()|,
		 [q|"..."| => q|""|],
		 [q|'...'|=> q|''|],
		 qw|+ - * / ^ $|,
		 ['& (concat)' => '&'],
		 "\n",
		 ($node->parent
		    ? ()
		    : (qw| >> |,
                       [q|Grouping: for/give/sort by| => qq|for ...\n    give distinct ...\n    sort by ...|],
		       ['Aggregation function' => [map { $_.'()' }
						  sort
						    qw( min max sum avg count ratio concat )
						   ]]),
		 ),
		) {
		 if ($_ eq "\n") {
		   $f = $d->add('Frame')->pack(-side=>'top');
		   next;
		 }
		 my ($label,$value,$opts)=ref($_) ? @$_ : ($_,$_);
		 $opts||={};
		 if (ref($value) eq 'ARRAY') {
		   my $menubutton = $f->Menubutton(
		     -text => $label,
		     -underline => 0,
		     -relief => 'raised',
		     -direction => 'right',
		     %$opts,
		    )->pack(-side=>'left');
		   my $menu = $menubutton->menu(-tearoff => 0,-font=>'C_small');
		   $menubutton->configure(-menu => $menu);
		   my $ul = _assign_shortcuts($value);
		   for (@$value) {
		     $menubutton->command(-label => $_,
					  (defined $ul->{$_} ? (-underline => $ul->{$_}) : ()),
					  -command =>
					    [ sub {
						my ($ed,$text)=@_;
						$ed->deleteSelected;
						my $context = $ed->get('0.0','insert');
						if ($context=~/[[:alnum:]"'}\)\]]\s*$/ and
						    $context!~/\b(and|or|div|mod|sort\s*by|over|give|for)\s*$/) {
						  $ed->Insert(',');
						}
						if ($text=~s/#(.*)#(.*)/$1$2/) {
						  $ed->Insert($text);
						  $ed->SetCursor('insert -'.length($2).' chars');
						  $ed->tagAdd('sel','insert-'.length($1).' chars','insert');
						} else {
						  $ed->Insert($text);
						}
					      },
					      $ed,' '.$_.' ' ]
					 );
		   }
		 } else {
		   $f->Button(-text => $label,
#			      ($label=~/([[:alpha:]])/ ? (-underline => $-[0]) : ()),
			      defined($value) ? ( -command => [ sub { $_[0]->Insert($_[1]=~/\$|\^/ ? $_[1] : ' '.$_[1].' ');
						      $_[0]->SetCursor('insert -2 chars') if $_[1]=~/["'[({]/;
						    }, $ed, $value] ) : (),
			      %$opts,
			       )->pack(-side=>'left');
		 }
	       }
	       $d->BindButtons($d);
	     },
	   };
  my $description;
  while ( defined ($string = EditBoxQuery('Edit query node', $string, '',$qopts)) ) {
    my $t0 = new Benchmark;
    eval {
      my $opts = {
	user_defined_relations => ($SEARCH && $SEARCH->get_user_defined_relations()),
	pmlrf_relations => ($SEARCH && $SEARCH->get_pmlrf_relations()),
      };
      if (!$node->parent) {
	$description='';
	$description.=$1."\n" while ($string=~s{^\s*(?:#[ \t]*([^\n]*)\n)}{});
	chomp $description;
	$result=parse_query($string,$opts);
      } elsif ($node->{'#name'} eq 'node') {
	$result=parse_node($string,$opts);
      } else {
	$result=parse_conditions($string,$opts); # returns ARRAY
      }
    };
    #  {
    #   my $t1 = new Benchmark;
    #   my $time = timestr(timediff($t1,$t0));
    #   print "parsing query took: $time\n";
    # }
    last unless $@;
    if (ref($@) eq 'Tree_Query::ParserError' ) {
      $qopts->{-cursor} = $@->line.'.end';
    }
    ErrorMessage("$@");
  }
  return unless $string;
  {
    my $t0 = new Benchmark;
    if ($node->parent) {
      my @c;
      if ($no_childnodes) {
	@c=map CutNode($_), grep { $_->{'#name'} eq 'node' } reverse $node->children;
      }
      if (ref($result) eq 'ARRAY') {
	$_->paste_after($node) for @$result;
	DetermineNodeType($_) for map { ($_,$_->descendants) } @$result;
	eval {
	  Tree_Query::Common::CompleteMissingNodeTypes($SEARCH,$_) for @$result
	} if $SEARCH;
  	$result=$result->[0];
      } else {
	$result->paste_after($node);
	DetermineNodeType($_) for ($result,$result->descendants);
	eval { Tree_Query::Common::CompleteMissingNodeTypes($SEARCH,$result) }
	  if $SEARCH;
	$result->{'.unhide'}=1 if $node->{'.unhide'};
      }
      $this=$result if $node==$this;
      DeleteSubtree($node);
      PasteNode($_,$result) for @c;
    } else {
      $node->{'output-filters'}=CloneValue($result->{'output-filters'});
      $node->{'description'}=$description if defined($description);
      DeleteSubtree($_) for $node->children;
      CutPaste($_,$node) for reverse $result->children;
      DetermineNodeType($_) for ($node->descendants);
      eval { Tree_Query::Common::CompleteMissingNodeTypes($SEARCH,$node) } if $SEARCH;
    }
    # {
    #   my $t1 = new Benchmark;
    #   my $time = timestr(timediff($t1,$t0));
    #   print "postprocessing took: $time\n";
    # }
  }
  return 1;
}

sub NewTest {
  my ($op)=@_;
  $op='=' if $op !~ /^[=~]$/;
  my $new;
  my $node=$this;
  ChangingFile(0);
  if ($node->{'#name'}=~/^(?:node|subquery|and|or|not)$/) {
    $new=NewSon();
  } elsif ($node->{'#name'}=~/^(?:test|ref)$/) {
    $new=NewRBrother();
  } else {
    return;
  }
  $new->{'#name'}='test';
  $new->{operator}=$op;
  DetermineNodeType($new);
  local $main::sortAttrs=0;
  if (EditAttribute($new,undef,undef,'a')) {
#     $node->{'.unhide'}=1;
    $this=$new;
  } else {
    DeleteLeafNode($new);
    $this=$node;
    return;
  }
  ChangingFile(1);
}

sub AddNode {
  my $node=$this;
  return unless $node;
  my $new;
  if (!$node->parent or $node->{'#name'} =~ /^(?:node|subquery)/) {
    $new = NewSon();
    $new->{'#name'}='node';
    DetermineNodeType($new);
  } elsif ($node->{'#name'}=~/^(?:test|ref)/) {
    $new = NewRBrother();
    $new->{'#name'}=$node->{'#name'};
    DetermineNodeType($new);
  } else {
    $new = NewSon();
    DetermineNodeType($new);
  }
  if ($new and $new->{'#name'} =~ /^(?:node|subquery)/) {
    my $ok = 1;
#    if (Tree_Query::Common::IsMemberNode($node,$SEARCH)) {
#      SetRelation($new,'member');
#      $ok=AssignType($new);
#    } else {
      $ok = ($node->parent ? AssignRelation($new) : AssignType($new));
#    }
    unless ($ok) {
      DeleteLeafNode($new);
      $this=$node;
    }
  }
}

sub DeleteNode {
  my $node=$this;
  return unless $node;
  unless ($node->parent) {
    return if (!$node->firstson
      and (QuestionQuery("Really delete tree?",
			 "Do you want to delete the whole query tree?",
			 "Delete","Cancel") ne 'Delete'));
    DestroyTree();
  }
  my $p = $node->parent && $node->parent->parent;
  if ($node->{'#name'} =~ /^(?:node|subquery)/) {
    my $name = $node->{name};
    DeleteSubtree($_) for grep { !($_->{'#name'} eq 'node' or ($p && $_->{'#name'} eq 'subquery')) }
      $node->children;

    # delete all ref-nodes pointing to $node
    for my $ref (grep { $_->{'#name'} eq 'ref' and $_->{target} eq $name  } $node->root->descendants) {
      # if the ref node is an only child of a negation, delete the negation as well
      $ref = $ref->parent if $ref->parent and $ref->parent->{'#name'} eq 'not'
	and $ref->parent->children == 1;
      DeleteSubtree($ref);
    }
  }
  delete_node_keep_children($node);
}

sub AddNOT {
  my $node=$this;
  ChangingFile(0);
  return unless $node->parent;
  if ($node->{'#name'} =~ /^(node|subquery|and|or)$/) {
    my $not = NewSon();
    $not->{'#name'}='not';
    DetermineNodeType($not);
    $this=$not;
    $node->{'.unhide'}=1;
  } elsif ($node->parent->{'#name'} eq 'not' and
	     !$node->lbrother and !$node->rbrother) {
    delete_node_keep_children($node->parent);
  } else {
    my $not = NewParent();
    $not->{'#name'}='not';
    DetermineNodeType($not);
    $this=$node;
    ChangingFile(1);
  }
}

sub AddAND {
  my $node=$this;
  ChangingFile(0);
  return unless $node->parent;
  my $and;
  if ($node->{'#name'} eq 'or') {
    $and = NewSon();
    $this=$and;
  } elsif ($node->parent->{'#name'} eq 'or') {
    $and = NewParent();
    $this=$node;
  } else {
    return;
  }
  $and->{'#name'}='and';
  DetermineNodeType($and);
  ChangingFile(1);
}

sub AddOR {
  my $node=$this;
  ChangingFile(0);
  return unless $node->parent and $node->{'#name'} ne 'or';
  my $or;
  if ($node->{'#name'} =~ /^(?:node|and|not|subquery)/) {
    $or=NewSon();
    $this=$or;
    $node->{'.unhide'}=1;
  } else {
    $or = NewParent();
    $this=$node;
  }
  $or->{'#name'}='or';
  DetermineNodeType($or);
  ChangingFile(1);
}

sub AutoNameAllNodes {
  ChangingFile(0);
  init_id_map($root,2);
}

### Displaying results

sub ShowResultTable {
  my ($title,$results,$query_id)=@_;
  $title||="Results";
  $title.=" for query $query_id" if $query_id;
  my $d = ToplevelFrame()->Toplevel(
    -title=> $title,
  );
  $d->withdraw;
  $d->BindButtons;
  $d->bind($d,'<Escape>'=> [sub { shift; shift->destroy(); },$d]);
  my $t= $d->Scrolled('ROText', qw/-relief sunken -borderwidth 2 -height 20 -scrollbars oe/);
  $t->insert('0.0', join('',map { join("\t",@$_)."\n" } @$results));
  $t->pack(qw/-side top -expand yes -fill both/);
  eval {
    $t->TextSearchLine(-parent => $d,
		       -label=>'S~earch',
		       -prev_img =>icon('16x16/up'),
		       -next_img =>icon('16x16/down'),
		      )->pack(qw(-fill x));
  };
  print STDERR $@ if $@;
  $t->Subwidget('xscrollbar')->configure(qw(-takefocus 0));
  $t->Subwidget('yscrollbar')->configure(qw(-takefocus 0));
  $t->BindMouseWheelVert();

  my $bottom=$d->Frame()->pack(qw/-side bottom -fill x/);
  $bottom->Button(-text=> 'Close',
		  -command=> [sub { shift->destroy; },$d])
    ->pack(-side=> 'left', -expand=> 1,  -padx=> 1, -pady=> 1);
  $bottom->Button(
    -text=>'Save To File',
    -command => [
      sub {
	my $d=shift;
	my $filename = main::get_save_filename(
	  $d,
	  -filetypes=>[["CSV",['.csv','.txt']],
		       ["All files",['*','*.*']],
		      ],
	  -title => "Save results as ...",
	  -initialfile=> ($query_id ? "results_for_".$query_id.".txt" : 'results.txt'),
	 );
	return unless defined($filename) and length($filename);
	my $backup;
	if (-f $filename) {
	  $backup=1 if rename $filename, $filename.'~';
	}
	if (open my $fh, '>:utf8', $filename) {
	  for (@$results) {
	    print $fh join("\t",@$_)."\n";
	  }
	  close $fh;
	} else {
	  TrEd::Basics::errorMessage($d,'Cannot write to '.$filename.': '.$!);
	  if ($backup) {
	    rename $filename.'~', $filename;
	  }
	}
      },$d])
    ->pack(-side=> 'left', -expand=> 1,  -padx=> 1, -pady=> 1);
  $t->focus();
  $d->Popup;
}


} # use strict
1;
