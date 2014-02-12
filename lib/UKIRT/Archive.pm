package UKIRT::Archive;

=head1 NAME

UKIRT::Archive - UKIRT archive support module

=cut

use strict;
use warnings;

use DateTime;
use File::Copy;
use File::Spec;
use File::Temp qw/tempfile/;
use Number::Interval;

use JSA::Convert qw/ndf2fits/;
use JSA::Files qw/looks_like_drthumb/;
use JSA::Headers qw/update_fits_product/;
use JSA::Headers::Starlink qw/update_fits_headers add_fits_comments/;
use JSA::Logging qw/log_warning/;

use parent qw/Exporter/;
our @EXPORT_OK = qw/convert_ukirt_products
                    match_observation_numbers
                    ukirt_filename_is_archival/;

our $VERSION = '0.001';

=head1 SUBROUTINES

=over 4

=item convert_ukirt_products

Convert ORAC-DR products into FITS and PNG files suitable for ingestion by
CADC.

Analagous to C<JSA::Convert::convert_dr_files> which appears to be
too JCMT-specific to be re-used for UKIRT.

=cut

sub convert_ukirt_products {
    my ($dpid, $href) = @_;

    my $dpdate = DateTime->now()->datetime();

    my %pngs = ();

    foreach my $file (sort keys %$href) {
        my $header = $href->{$file};

        if (ukirt_file_is_product($file)) {
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
                fitsmod_extra => ['D OBSERVER'],
            });

            add_fits_comments($file, cadc_ukirt_acknowledgement());

            my $outfile = convert_ukirt_filename($file, $header);
            next unless defined $outfile;
            ndf2fits($file, $outfile);

            # Fix product names in extensions.
            update_fits_product($outfile);

        }
        elsif (looks_like_drthumb($file)) {
            # Add to the thumbnail list for later consideration.

            $pngs{$file} = $header;
        }
    }

    # Combine thumbnail images.
    merge_ukirt_pngs(\%pngs);
}

=item convert_ukirt_filename

Rename a file to follow the naming convention for reduced UKIRT
data in the CADC archive.

TODO: add proper naming rules.

=cut

sub convert_ukirt_filename {
    my $file = shift;
    my $header = shift;

    my ($inst, $date, $obs, $suffix) = split_ukirt_filename($file);
    return undef unless defined $inst;

    my $product = $header->value('PRODUCT');
    return undef unless (defined $product) && ($product =~ /^[-_A-Za-z0-9]+$/);

    return sprintf('UKIRT_%s_%s_%05d_%s.fits', $inst, $date, $obs, $product);
}

=item convert_ukirt_preview_filename

Converts filenames for preview thumbnails.

=cut

sub convert_ukirt_preview_filename {
    my $file = shift;
    my $header = shift;

    my $keywords = $header->{'Keywords'};
    return undef unless defined $keywords;
    my $product = undef;

    foreach (split /[;,]/, $keywords) {
        s/^\s+//;
        s/\s+$//;

        if (s/^jsa:productID=//) {
            $product = $_;
            last;
        }
    }
    return undef unless (defined $product) && ($product =~ /^[-_A-Za-z0-9]+$/);

    my ($inst, $date, $obs, $suffix) = split_ukirt_filename($file);
    return undef unless defined $inst;

    return undef unless $suffix =~ /(?:rimg|rsp)_(?:vector_)?(\d+)$/;
    my $size = $1;

    return sprintf('UKIRT_%s_%s_%05d_%s_preview_%s.png', $inst, $date, $obs, $product, $size);
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

=item merge_ukirt_pngs

Preview PNG image merging routine.  Similar to JSA::Files::merge_pngs but
using the CAOM-2 convention (rsp on the left, rimg on the right, and
no padding when one of these is missing).  It also adds handling for
cases where only the rimg is present.

=cut

sub merge_ukirt_pngs {
    my $files = shift;
    my %newfiles = ();

    my $montage = '/usr/bin/montage';
    my $composite = '/usr/bin/composite';
    my $have_montage = -e $montage;
    my $have_composite = -e $composite;

    while (my ($file, $header) = each %$files) {
        # Determine which new file this should be merged into.

        my $newname = convert_ukirt_preview_filename($file, $header);
        next unless defined $newname;

        if (exists $newfiles{$newname}) {
            push @{$newfiles{$newname}}, $file;
        }
        else {
            $newfiles{$newname} = [$file];
        }
    }

    while (my ($new, $old) = each %newfiles) {
        my @rimg = grep {/_rimg_/} @$old;
        my @rsp = grep {/_rsp_/} @$old;

        foreach my $type (\@rimg, \@rsp) {
            next
                if 2 > scalar @$type;
            die 'Too many PNG files of the same type for ' . $new
                if 2 < scalar @$type;

            my @bg = grep {! /_vector_/} @$type;
            my @vc = grep {/_vector_/} @$type;

            die 'Could not identify background and vector for ' . $new
                unless (1 == scalar @bg) and (1 == scalar @vc);

            if ($have_composite) {
                my (undef, $tmp) = tempfile('merge_XXXX', OPEN => 0,
                    DIR => File::Spec->curdir(), SUFFIX => '.png');

                system("$composite -compose lighten $vc[0] $bg[0] $tmp");

                @$type = ($tmp);
            }
            else {
                # Vectors are white and the previous will probably be shown on
                # a while background, so the background image is probably
                # better if we can't merge them.

                @$type = ($bg[0]);
            }
        }

        unless (@rimg or @rsp) {
            # It's an error if the files we previously matched are no longer
            # in our lists.

            die 'No rimg or rsp PNG files remaining for ' . $new;
        }
        elsif (@rimg and not @rsp) {
            # If there is only one file then just copy it.  Note that the @rimg
            # and @rsp arrays should have just one entry now.  They're arrays
            # because we needed to call grep in array context.

            copy($rimg[0], $new);
        }
        elsif (@rsp and not @rimg) {
            # Same as above, but rsp only.

            copy($rsp[0], $new);
        }
        else {
            # If there are both types then merge if possible.

            if ($have_montage) {
                $new =~ /preview_(\d+)/;
                my $size = $1;
                system("$montage $rsp[0] $rimg[0] -tile 2x1 -geometry ${size}x${size}+0+0 $new");
            }
            else {
                # Don't have montage available, so just pick one. Always rimg for now
                # but maybe should default to rsp for spectrometers.

                copy($rimg[0], $new);
            }
        }
    }
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

=cut

{
    # Filename suffix list.  Note that there will also be a check that
    # the PRODUCT header is present.
    my @ukirt_product_patterns = (
        # Polarimetry
        'StokesI',
        'StokesQ',
        'StokesU',
        'StokesV',
        'polangle',
        'polintensity',
        'polpercent',

        # Imaging
        'mos',

        # Spectroscopy
        'aws',
        'dbs',
        'dbsi',
        'fc',
        'fci',

        # IFU
        'cube_fc',
        'im_fc',
        'sp_fc',
        'im',
        'sp',
        'cube_dbs',
    );

    sub ukirt_file_is_product {
        my $file = shift;

        my ($inst, $date, $obs, $suffix) = split_ukirt_filename($file);
        return 0 unless defined $inst;

        # There are some products without suffices.
        return 1 unless defined $suffix;

        return 1 if grep {$_ eq $suffix} @ukirt_product_patterns;

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

=item split_ukirt_filename

Returns a list of components of a UKIRT reduced file name.

    my ($inst, $date, $obs, $suffix) = split_ukirt_filename($filename);

Returns undef on failure to match the filename.

=cut

{
    my %ukirt_instruments = (
        k => 'CGS3',
        c => 'CGS4',
        i => 'IRCAM3',
        m => 'Michelle',
        f => 'UFTI',
        u => 'UIST',
    );

    sub split_ukirt_filename {
        my $file = shift;

        if ($file =~ /^g([u])(\d{8})_(\d+)_([-_A-Za-z0-9]+)\.(?:sdf|png)$/) {
            return ($ukirt_instruments{$1}, $2, $3, $4);
        }
        elsif ($file =~ /^g([u])(\d{8})_(\d+).sdf$/) {
            # Nastily-named no-extension product?
            return ($ukirt_instruments{$1}, $2, $3, undef);
        }
        else {
            return undef;
        }
    }
}

1;

=back

=head1 COPYRIGHT

Copyright (C) 2013-2014 Science and Technology Facilities Council.
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
