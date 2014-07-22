package UKIRT::Archive::WrapDR;

=head1 NAME

UKIRT::Archive::WrapDR - UKIRT archive data reduction wrapping subroutines

=cut

use strict;
use warnings;

use File::Spec;
use IO::Dir;
use IO::File;

use JSA::WrapDR qw/run_pipeline/;

use parent qw/Exporter/;
our @EXPORT_OK = qw/combine_pipeline_logs run_ukirt_pipeline/;

our $VERSION = '0.001';

=head1 SUBROUTINES

=over 4

=item combine_pipeline_logs

Given two the names of two log files, append the second to the first and
then delete it.  This should deal with trimming the HTML to produce a
file which isn't more invalid than the input files.  It assumes the
exact output format seen at the time of writing.

=cut

sub combine_pipeline_logs {
    my ($log_cal, $log_sci) = @_;

    my $fh_cal = new IO::File($log_cal, 'r+');
    my $fh_sci = new IO::File($log_sci, 'r');

    # Go back before the HTML footer in the first log.
    $fh_cal->seek(-22, SEEK_END);

    # Skip over header of the second log.
    undef while <$fh_sci> !~ /^(?:&nbsp;)*System&nbsp;description:/
          and not $fh_sci->eof();

    print $fh_cal $_ foreach <$fh_sci>;

    $fh_cal->close();
    $fh_sci->close();

    unlink $log_sci;
}


=item run_ukirt_pipeline

Run the ORAC-DR pipeline appropriately for UKIRT data.

    run_ukirt_pipeline($oracinst, $outdir, \@files, $drparameters);

Returns the name of the log file written by ORAC-DR, or undef
if this cannot be uniquely determined.

=cut

sub run_ukirt_pipeline {
    my ($oracinst, $outdir, $files, $drparameters) = @_;

    # Ensure $drparamers is not undefined before appending to it.
    $drparameters //= '';

    # Switch on skiperror mode for UKIRT.
    $drparameters .= ' --skiperror';

    # Remove leading spaces as otherwise they get interpreted as
    # a blank recipe name.
    $drparameters =~ s/^\s+//;

    # Determine where the logs will be written.  If ORAC_LOGDIR is not
    # set, then set it now so that we always get the full and non-hidden
    # filename.
    $ENV{'ORAC_LOGDIR'} = $outdir unless exists $ENV{'ORAC_LOGDIR'};
    my $logdir = $ENV{'ORAC_LOGDIR'};

    # See what logs are already present.  Unfortunately we can't
    # use JSA::Files::scan_dir because that only works on the current
    # directory.
    my $dir = new IO::Dir($logdir);
    my %existing = map {$_ => 1} $dir->read();
    $dir->close();

    run_pipeline(1, $oracinst, undef, $outdir,
                $files, $drparameters, {batch => 0});

    # See what logs have been newly written.
    my @logs;
    $dir = new IO::Dir($logdir);
    while (defined (my $log = $dir->read())) {
        next unless $log =~ /^oracdr_[-_a-zA-Z0-9]*\.html$/;
        push @logs, $log unless exists $existing{$log};
    }
    $dir->close();

    # Return the log file name if we were able uniquely to determine it.
    return File::Spec->catfile($logdir, $logs[0]) if 1 == scalar @logs;
    return undef;
}

1;

__END__

=back

=head1 COPYRIGHT

Copyright (C) 2014 Science and Technology Facilities Council.
All Rights Reserved.

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful,but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 59 Temple
Place,Suite 330, Boston, MA  02111-1307, USA

=cut
