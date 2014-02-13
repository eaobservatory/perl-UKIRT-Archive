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

use Astro::WaveBand;
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
    my %sdfs = ();

    foreach my $file (sort keys %$href) {
        my $header = $href->{$file};

        if (ukirt_file_is_product($file)) {
            $sdfs{$file} = $header;

        }
        elsif (looks_like_drthumb($file)) {
            $pngs{$file} = $header;
        }
    }

    # Convert ORAC-DR products to FITS.
    convert_ukirt_sdfs($dpid, $dpdate, \%sdfs);

    # Combine thumbnail images.
    merge_ukirt_pngs(\%pngs);
}

=item convert_ukirt_filename

Return the new name of a file to following the naming convention for reduced
UKIRT data in the CADC archive.

=cut

sub convert_ukirt_filename {
    my $file = shift;
    my $header = shift;
    my $include_suffix = shift;
    my $include_mos = shift;

    my ($inst, $date, $obs, $suffix) = split_ukirt_filename($file);
    return undef unless defined $inst;

    my $product = $header->value('PRODUCT');
    return undef unless (defined $product) && ($product =~ /^[-_A-Za-z0-9]+$/);

    return sprintf('UKIRT_%s_%s_%05d_%s.fits', $inst, $date, $obs, $product)
        unless $include_suffix;

    $suffix =~ s/_mos// unless $include_mos;

    return sprintf('UKIRT_%s_%s_%05d_%s_%s.fits', $inst, $date, $obs, $product,
        $suffix);
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

=item convert_ukirt_sdfs

Convert ORAC-DR-produced SDF files into FITS files for ingestion by CADC.

=cut

sub convert_ukirt_sdfs {
    my $dpid = shift;
    my $dpdate = shift;
    my $sdfs = shift;

    my %new_name = ();
    my %new_name_count = ();

    # Go through the list of files, rejecting those for which
    # we cannot determine a new file name, and make a list of the
    # new files.  Also record how many times each new name appears.
    foreach my $file (sort keys %$sdfs) {
        my $outfile = convert_ukirt_filename($file, $sdfs->{$file}, 0, 0);
        next unless defined $outfile;

        $new_name{$file} = $outfile;
        $new_name_count{$outfile} ++;
    }

    # Go through the files a second time to actually perform the
    # conversion.
    while (my ($file, $outfile) = each %new_name) {
        # Are we trying to produce more than one file with this
        # new file name?  If so, then re-generate the name, but
        # including the original file suffix in order to
        # distinguish them.  This is to allow there to be planes
        # in CAOM-2 (corresponding to one product ID) containing
        # more than one file.  Also try to leave out a _mos part
        # of the suffix, to improve the file names for multi-color
        # imaging.

        if ($new_name_count{$outfile} > 1) {
            $outfile = convert_ukirt_filename($file, $sdfs->{$file}, 1, 0);
            $new_name_count{$outfile} ++;

            if ($new_name_count{$outfile} > 1) {
                $outfile = convert_ukirt_filename($file, $sdfs->{$file}, 1, 1);
                $new_name_count{$outfile} ++;
            }
        }

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

        ndf2fits($file, $outfile);

        # Fix product names in extensions.
        update_fits_product($outfile);
    }
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

my $montage = '/usr/bin/montage';
my $have_montage = -e $montage;
my $composite = '/usr/bin/composite';
my $have_composite = -e $composite;

sub merge_ukirt_pngs {
    my $files = shift;
    my %newfiles = ();

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
            next if 2 > scalar @$type;

            my ($instrument, undef, undef, undef) = split_ukirt_filename($type->[0]);

            if (2 == scalar @$type and 1 == scalar grep {/_vector_/} @$type) {
                # One vector plot, and another image onto which to overlay it.

                @$type = merge_ukirt_png_vec_bg(@$type);
            }
            elsif ((scalar @$type) == (scalar grep {/_\d+_(.+)_(?:rimg|rsp)/
                    and Astro::WaveBand::has_filter($instrument, $1)} @$type)) {
                # All images are of different filters (multi-color mode).

                @$type = merge_ukirt_png_multicolor($instrument, @$type);
            }
            else {
                # This error is no longer fatal because having the
                # thumbnails isn't essential to the data reduction
                # process.

                print "WARNING: found too many thumbnails of the same type\n";
                print ' for ' . $new . " and can not merge them.\n";
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
                die 'Can not determine size' unless $new =~ /preview_(\d+)/;
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

sub merge_ukirt_png_vec_bg {
    my @bg = grep {! /_vector_/} @_;
    my @vc = grep {/_vector_/} @_;

    # This subroutine should only be called if there is one
    # map and one vector plot, so raise an error if they
    # can not be identified.
    die 'Could not identify background and vector from: ' . join(' ', @_)
        unless (1 == scalar @bg) and (1 == scalar @vc);

    if ($have_composite) {
        my (undef, $tmp) = tempfile('merge_XXXXXX', OPEN => 0,
            DIR => File::Spec->curdir(), SUFFIX => '.png');

        system("$composite -compose lighten $vc[0] $bg[0] $tmp");

        return $tmp;
    }
    else {
        # Vectors are white and the previews will probably be shown on
        # a while background, so the background image is probably
        # better if we can't merge them.

        return $bg[0];
    }
}

sub merge_ukirt_png_multicolor {
    my $instrument = shift;

    # Sort thumbnails into wavelength order.
    my %png = map {
        die 'Filter name vanished from thumbnail name'
            unless /_\d+_(.+)_(?:rimg|rsp)/;
        Astro::WaveBand->new(Instrument => $instrument,
                             Filter => $1)->wavelength() => $_;
    } @_;
    my @png = @png{sort keys %png};

    # In case we don't have composite available, check there is at
    # least one thumbnail, and return it.
    die 'Lost the multi-colour thumbnails while sorting' unless scalar @png;
    return $png[0] unless $have_composite;

    if (1 == scalar @png) {
        die 'Attempting to merge only one multi-color thumbnail';
    }
    elsif (2 == scalar @png) {
        die 'Can not determine size' unless $png[0] =~ /_(\d+)\.png/;
        my $size = $1;

        my (undef, $tmp) = tempfile('merge_XXXXXX', OPEN => 0,
            DIR => File::Spec->curdir(), SUFFIX => '.png');

        system("$composite -compose copyBlue $png[0] -size ${size}x${size} canvas:black $tmp");

        my (undef, $tmp2) = tempfile('merge_XXXXXX', OPEN => 0,
            DIR => File::Spec->curdir(), SUFFIX => '.png');

        system("$composite -compose copyRed $png[1] $tmp $tmp2");

        return $tmp2;
    }
    else {
        # We have 3 or more images, but we can't make a RGB composite of more
        # than three -- therefore just merge the first three we have.

        my (undef, $tmp) = tempfile('merge_XXXXXX', OPEN => 0,
            DIR => File::Spec->curdir(), SUFFIX => '.png');

        system("$composite -compose copyBlue $png[0] $png[1] $tmp");

        my (undef, $tmp2) = tempfile('merge_XXXXXX', OPEN => 0,
            DIR => File::Spec->curdir(), SUFFIX => '.png');

        system("$composite -compose copyRed $png[2] $tmp $tmp2");

        return $tmp2;
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
        'I',
        'Q',
        'U',
        'V',
        'TH',
        'PI',
        'P',

        # Spectro-polarimetry
        'sp-I',
        'sp-Q',
        'sp-U',
        'sp-V',
        'sp-TH',
        'sp-PI',
        'sp-P',

        # Imaging
        'mos',

        # Spectroscopy
        'dif', # (also IFU)
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
        return 0 unless defined $inst
                    and defined $suffix;

        return 1 if grep {$_ eq $suffix} @ukirt_product_patterns;

        # Special case for multi-color "mos" files.  We need to check
        # if we have a valid filter name to avoid matching other
        # files ending *_mos which we don't want.
        if ($suffix =~ /(.+)_mos/) {
            return 1 if Astro::WaveBand::has_filter($inst, $1);
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
