package File::Rename;

use strict;
BEGIN { eval { require warnings; warnings->import } }

package File::Rename::Options;

use Getopt::Long ();

eval{ Getopt::Long::Configure qw(
	posix_default
	no_ignore_case
); 1 } or warn $@;

sub GetOptions {
    my @expression;
    Getopt::Long::GetOptions(
	'-v|verbose'	=> \my $verbose,
	'-n|nono'	=> \my $nono,
	'-f|force'	=> \my $force,
	'-h|?|help'	=> \my $help,
	'-m|man'	=> \my $man,
	'-V|version'	=> \my $version,
	'-e=s'		=> \@expression,
	'-E=s'		=>
	    sub {
		my(undef, $e) = @_;
		$e .= ';'; 
		push @expression, $e; 
	    },
    ) or return;

    my $options = {
	verbose 	=> $verbose,
	no_action	=> $nono,
	over_write	=> $force,
	show_help	=> $help,
	show_manual	=> $man,
	show_version	=> $version,
    };
    return $options if $help or $man or $version;
	 
    if( @expression ) {
	$options->{_code} = join "\n", @expression;
    }
    else { 
	return unless @ARGV;
	$options->{_code} = shift @ARGV;
    } 
    return $options;
}
 
package File::Rename;

use base qw(Exporter);
use vars qw(@EXPORT_OK $VERSION);

@EXPORT_OK = qw( rename );
$VERSION = '0.20';

sub rename_files {
    my $code = shift;
    my $options = shift;
    _default(\$options); 
    for (@_) {
        my $was = $_;
	$code->();
    	if( $was eq $_ ){ }		# ignore quietly
    	elsif( -e $_ and not $options->{over_write} ) { 
		warn  "$was not renamed: $_ already exists\n"; 
	}
    	elsif( $options->{no_action} ) { 
		print "rename($was, $_)\n";
	}
    	elsif( &{ defined &CORE::GLOBAL::rename ? \&CORE::GLOBAL::rename : \&CORE::rename }($was,$_)) { 
		print "$was renamed as $_\n" if $options->{verbose}; 
	}
    	else { 	warn  "Can't rename $was $_: $!\n"; }
    }
}

sub rename_list { 
    my($code, $options, $fh, $file) = @_;
    _default(\$options); 
    print "Reading filenames from ",
	( defined $file ?		$file 
	: defined *{$fh}{SCALAR} and
	  defined ${*{$fh}{SCALAR}} ?	${*{$fh}{SCALAR}}
	:	 			"file handle ($fh)"
	),
	"\n" if $options->{verbose};
    chop(my @file = <$fh>); 
    rename_files $code, $options,  @file;
}

sub rename { 
    my($argv, $code, $verbose) = @_;
    if( ref $code ) {
	if( 'HASH' eq ref $code ) {
	    require Carp;
	    if(defined $verbose ) {
		Carp::carp(<<CARP);
File::Rename::rename: third argument ($verbose) ignored
CARP
	    } 
	    $verbose = $code;
	    $code = delete $verbose->{_code}
	    	or Carp::carp(<<CARP);
File::Rename::rename: no _code in $verbose
CARP

	}	
    } 
    unless( ref $code ) {
	if( my $eval = eval <<CODE ) 
sub {
#line 1
$code
#line
}
CODE
	{	
	    $code = $eval;
	} 
	else {
	    my $error = $@;
	    $error =~ s/\(eval\s+\d+\)/\(user-supplied code\)/g;
	    $error =~ s/\s+line\s+1\b//g unless $code =~ /\n/;
	    $error =~ s/\"[^#"]*\#line\s+1\n/"/;
	    $error =~ s/\n\#line\n[^#"]*\"/"/;
	    $error =~ s/\s*\z/\n/;
	    die $error;
	}
    }
    if( @$argv ) { rename_files $code, $verbose, @$argv }
    else { rename_list $code, $verbose, \*STDIN, 'STDIN' }
}

sub _default {
    my $ref = shift;
    return if ref $$ref;
    my $verbose = $$ref;
    $$ref = { verbose => $verbose }
}

1;

__END__

=head1 NAME

File::Rename - Perl extension for renaming multiple files

=head1 SYNOPSIS

  use File::Rename qw(rename);		# hide CORE::rename
  rename @ARGV, sub { s/\.pl\z/.pm/ }, 1;

  use File::Rename;
  File::Rename::rename @ARGV, '$_ = lc';

=head1 DESCRIPTION

=over 4

=item C<rename( FILES, CODE [, VERBOSE])>

rename FILES using CODE,
if FILES is empty read list of files from stdin

=item C<rename_files( CODE, VERBOSE, FILES)>

rename FILES using CODE

=item C<rename_list( CODE, VERBOSE, HANDLE [, FILENAME])>

rename a list of file read from HANDLE, using CODE

=back

=head2 OPTIONS

=over 8

=item FILES

List of files to be renamed,
for C<rename> must be an array

=item CODE

Subroutine to change file names,
for C<rename> can be a string,
otherside a code reference

=item VERBOSE

Flag for printing names of files successfully renamed,
optional for C<rename>

=item HANDLE

Filehandle to read file names to be renames

=item FILENAME (Optional)

Name of file that HANDLE reads from

=back

=head2 HASH

Either CODE or VERBOSE can be a HASH of options.

If CODE is a HASH, VERBOSE is ignored 
and CODE is supplied by the B<_code> key.

Other options are 

=over 16

=item B<verbose>

As VERBOSE above, provided by B<-v>.

=item B<no_action>

Print names of files to be renamed, but do not rename
(i.e. take no action), provided by B<-n>.

=item B<over_write>

Allow files to be over-written by the renaming, provided by B<-f>. 

=item B<show_help>

Print help, provided by B<-h>.

=item B<show_manual> 

Print manual page, provided by B<-m>.

=item B<show_version> 

Print version number, provided by B<-V>.

=back

=head2 EXPORT

None by default.

=head1 ENVIRONMENT

No environment variables are used.

=head1 SEE ALSO

mv(1), perl(1), rename(1)

=head1 AUTHOR

Robin Barker <RMBarker@cpan.org>

=head1 Acknowledgements

Based on code from Larry Wall.

Options B<-e>, B<-f>, B<-n> suggested
by more recent code written by Aristotle Pagaltzis.

=head1 DIAGNOSTICS

Errors from the code argument are not trapped.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2004, 2005, 2006, 2011 by Robin Barker

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut

