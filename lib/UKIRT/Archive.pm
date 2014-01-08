package UKIRT::Archive;

=head1 NAME

UKIRT::Archive - UKIRT archive support module

=cut

use strict;
use warnings;

use DateTime;
use File::Copy;
use File::Spec;
use Number::Interval;

use JSA::Convert qw/ndf2fits/;
use JSA::Files qw/merge_pngs looks_like_drthumb/;
use JSA::Headers qw/update_fits_product/;
use JSA::Headers::Starlink qw/update_fits_headers add_fits_comments/;
use JSA::Logging qw/log_warning/;

use parent qw/Exporter/;
our @EXPORT_OK = qw/convert_ukirt_products
                    convert_ukirt_filename
                    match_observation_numbers
                    ukirt_filename_is_archival
                    ukirt_file_is_product/;

our $VERSION = '0.001';

=head1 SUBROUTINES

=over 4

=item convert_ukirt_products

Convert ORAC-DR products into FITS files suitable for ingestion by
CADC.

Analagous to C<JSA::Convert::convert_dr_files> which appears to be
too JCMT-specific to be re-used for UKIRT.

=cut

sub convert_ukirt_products {
    my ($dpid, $href) = @_;

    my $dpdate = DateTime->now()->datetime();

    my @pngs = ();

    foreach my $file (sort keys %$href) {
        my $header = $href->{$file};

        if (ukirt_file_is_product($file, $header)) {
            # Convert ORAC-DR product to FITS.

            # TODO: update provenance?
            # See JSA::Convert::convert_dr_files call to
            # prov_update_parent_path for JSA example.

            # TODO: set WCS attributes?
            # See JSA::Convert::convert_dr_files call to
            # set_wcs_attrib, but that subroutine sets
            # the third axis to FREQ for JCMT.

            update_fits_headers($file, {
                dpid => $dpid,
                dpdate => $dpdate,
                instream => 'UKIRT',
                mode => '', # Dummy value to avoid uninitialized warnings
            });

            add_fits_comments($file, cadc_ukirt_acknowledgement());

            my $outfile = convert_ukirt_filename($file);
            ndf2fits($file, $outfile);

            # Fix product names in extensions.  Is this necessary
            # for UKIRT?
            update_fits_product($outfile);

        }
        elsif (looks_like_drthumb($file)) {
            # Rename PNG thumbnails and add to the thumbnail list.

            my $outfile = convert_ukirt_filename($file);
            copy($file, $outfile);
            push @pngs, $outfile;
        }
    }

    # Combine thumbnail images.
    merge_pngs(@pngs);
}

=item convert_ukirt_filename

Rename a file to follow the naming convention for reduced UKIRT
data in the CADC archive.

TODO: add proper naming rules.

=cut

sub convert_ukirt_filename {
    my $file = shift;

    $file =~ s/\.sdf$/.fits/;

    return 'UKIRT_' . $file;
}

=item match_observation_numbers

Separate a list of files into those which do and do not match the given
list of observation numbers.

    my ($files_cal, $files_sci) = match_observation_numbers(\@files, $obs_cal)

This can be used to get lists of calibration and science files given a list
of the calibration observation numbers.

=cut

sub match_observation_numbers {
    my ($files, $obs_cal) = @_;

    my @files_cal;
    my @files_sci;

    my @cal = map {
        my ($min, $max) = /:/ ? (split /:/) : ($_, $_);
        new Number::Interval(Min => $min, Max => $max,
                             IncMin => 1, IncMax => 1);
    } split ',', $obs_cal;

    foreach my $file (@$files) {
        # Could use ORAC-DR classes to handle the filenames but we expect
        # UKIRT raw data filenames to match this pattern.
        if ($file =~ /^[a-z][0-9]{8}_([0-9]{5})\./) {
            my $obs = $1;
            $obs =~ s/^0*//;
            if (scalar grep {$_->contains($obs)} @cal) {
                push @files_cal, $file;
            }
            else {
                push @files_sci, $file;
            }
        }
        else {
            log_warning('Could not determine observation number for file ' .
                        $file);
            push @files_sci, $file;
        }
    }

    return (\@files_cal, \@files_sci);
}

=item ukirt_filename_is_archival

Check whether the given filename follows the naming convention for
reduced UKIRT data in the CADC archive.

Analagous to C<JSA::Files::looks_like_cadcfile>.

=cut

{
    # List of filename patterns.  A file is considered archival
    # if it matches any of these patterns.
    my @ukirt_archival_patterns = (
        qr/^UKIRT_.*\.(fits|png)$/,
    );

    sub ukirt_filename_is_archival {
        my $filename = basename(shift);

        foreach my $pattern (@ukirt_archival_patterns) {
            return 1 if $filename =~ $pattern;
        }

        return 0;
    }
}

=item ukirt_file_is_product

Determine whether a data file is a product which should be prepared
for ingestion.

TODO: add proper naming checks.

=cut

{
    # Hash of filename patterns an header check subroutines.  Files
    # will be determined to be products if the file name matches
    # a pattern and the associated subroutine returns a true value
    # given the header.
    my %ukirt_product_patterns = (
        qr/^g[kcimfu].*\.sdf$/ => sub {return 1;},
    );

    sub ukirt_file_is_product {
        my ($file, $header) = @_;

        while (my ($pattern, $filter) = each %ukirt_product_patterns) {
            return 1 if $file =~ $pattern && $filter->($header);
        }

        return 0;
    }
}

=back

=head2 Local Convenience Subroutines

=over 4

=item basename

File component of the given pathname.  Simplified version
of C<JSA::Files::_strip_path>, but we can't use that since
it is a private subroutine.

=cut

sub basename {
    my (undef, undef, $file) = File::Spec->splitpath(shift);
    return $file;
}

=item cadc_ukirt_acknowledgement

Return a reference to a CADC and UKIRT acknowledgement text.

=cut

{
    my $acknowledgement_text = undef;
    sub cadc_ukirt_acknowledgement {
        unless (defined $acknowledgement_text) {
            my @text = <DATA>;
            chomp @text;
            $acknowledgement_text = \@text;
        }
        return $acknowledgement_text;
    }
}

1;

=back

=head1 COPYRIGHT

Copyright (C) 2013 Science and Technology Facilities Council.
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

__DATA__

ACKNOWLEDGEMENTS:
If you have used CADC facilities and products (such as these data)
for your research, please include the following acknowledgement:

"This research used the facilities of the Canadian Astronomy Data
Centre operated by the the National Research Council of Canada with
the support of the Canadian Space Agency."

The following acknowledgement should appear in any published paper
containing data obtained with the UKIRT:

"The United Kingdom Infrared Telescope is operated by the Joint
Astronomy Centre on behalf of the Science and Technology Facilities
Council of the U.K."

In addition, any paper containing data obtained during UKIRT
Service observing should mention the Service Programme either in
the observations section, the acknowledgements, or both. e.g.,

"some of the data reported here were obtained as part of the UKIRT
Service Programme"

Any paper containing data obtained using IRPOL2 should also include
the following (or similar) acknowledgment:

"We thank the Department of Physical Sciences, University of
Hertfordshire, for providing IRPOL2 for the UKIRT."
