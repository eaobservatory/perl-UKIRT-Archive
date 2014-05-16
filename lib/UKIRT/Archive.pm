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

use Astro::FITS::CFITSIO qw/:constants/;
use Astro::WaveBand;
use JSA::Convert qw/ndf2fits/;
use JSA::Files qw/looks_like_drthumb/;
use JSA::Headers qw/update_fits_product/;
use JSA::Headers::Starlink qw/update_fits_headers add_fits_comments/;
use JSA::Logging qw/log_warning/;
use JSA::Starlink qw/prov_update_parent_path/;

use parent qw/Exporter/;
our @EXPORT_OK = qw/convert_ukirt_products
                    convert_ukirt_logs
                    match_observation_numbers
                    ukirt_filename_is_raw
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
    my %catalogs = ();

    foreach my $file (sort keys %$href) {
        my $header = $href->{$file};

        if (ukirt_file_is_product($file)) {
            $sdfs{$file} = $header;
        }
        elsif (ukirt_file_is_catalog_product($file)) {
            $catalogs{$file} = $header;
        }
        elsif (looks_like_drthumb($file)) {
            $pngs{$file} = $header;
        }
    }

    # Convert ORAC-DR products to FITS.
    convert_ukirt_sdfs($dpid, $dpdate, \%sdfs);

    # Rename ORAC-DR catalog product FITS files.
    convert_ukirt_catalogs($dpid, $dpdate, \%catalogs);

    # Combine thumbnail images.
    merge_ukirt_pngs(\%pngs);
}

=item convert_ukirt_logs

Rename the log files which we want to keep so that they have suitable
file names for storage in the archive.

Takes a complete list of files found in the directory because the
JSA::Headers::read_headers subroutine eliminates files (such as logs)
without headers.  We also need to have a look a the product filename
in order to determine the instrument and date to use in the new
filenames.  (Assuming that UKIRT data is processed as a whole night,
otherwise we will just have the last log for data reduced on a given night.)

=cut

sub convert_ukirt_logs {
    my $files = shift;
    my %inst = ();
    my %date = ();
    my @log = ();

    # Look through the files and make a list of logs to process,
    # and count the number of files for each instrument and date.
    foreach my $file (@$files) {
        if (ukirt_filename_is_log($file)) {
            push @log, $file;
        }
        elsif (ukirt_file_is_product($file)) {
            my ($inst, $date, undef, undef) = split_ukirt_filename($file);
            next unless defined $inst and defined $date;
            $inst{$inst} ++;
            $date{$date} ++;
        }
    }

    # Determine the instrument and date with which to label the logs.
    # (The most common amongst the files specified.)
    return unless %inst;
    my $inst = [sort {$inst{$b} <=> $inst{$a}} keys %inst]->[0];
    my $date = [sort {$date{$b} <=> $date{$a}} keys %date]->[0];

    # Finally copy the logs we want to keep.
    foreach my $file (@log) {
        my $outfile = convert_ukirt_log_filename($inst, $date, $file);
        next unless defined $outfile;

        copy($file, $outfile);
    }
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

    return lc(sprintf('ukirt_%s_%s_%05d_%s.fits', $inst, $date, $obs,
        $product)) unless $include_suffix;

    $suffix =~ s/_mos// unless $include_mos;

    return lc(sprintf('ukirt_%s_%s_%05d_%s_%s.fits', $inst, $date, $obs,
        $product, $suffix));
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

        if (s/^jsa:product=//) {
            $product = $_;
            last;
        }
    }
    return undef unless (defined $product) && ($product =~ /^[-_A-Za-z0-9]+$/);

    my ($inst, $date, $obs, $suffix) = split_ukirt_filename($file);
    return undef unless defined $inst;

    return undef unless $suffix =~ /(?:rimg|rsp)_(?:vector_)?(\d+)$/;
    my $size = $1;

    return lc(sprintf('ukirt_%s_%s_%05d_%s_preview_%s.png', $inst, $date,
        $obs, $product, $size));
}

=item convert_ukirt_log_filename

Converts filenames for log files.

=cut

sub convert_ukirt_log_filename {
    my $inst = shift;
    my $date = shift;
    my $file = shift;

    my $suffix = undef;

    if ($file =~ /^log\.([a-z]+)$/) {
        $suffix = 'log_' . $1;
    }

    return undef unless defined $suffix;

    return lc(sprintf('ukirt_%s_%s_%s.txt', $inst, $date, $suffix));
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
    my %new_name_unique = ();

    # Go through the list of files, rejecting those for which
    # we cannot determine a new file name, and make a list of the
    # new files.  Also record how many times each new name appears.
    foreach my $file (sort keys %$sdfs) {
        my $outfile = convert_ukirt_filename($file, $sdfs->{$file}, 0, 0);
        next unless defined $outfile;

        $new_name{$file} = $outfile;
        $new_name_count{$outfile} ++;
    }

    # Go through the files a second time to make sure each file has a
    # unique new name.
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

        $new_name_unique{$file} = $outfile;
    }

    # Actually perform the conversion.  We now have all the filename mappings
    # so we can update the provenance.
    while (my ($file, $outfile) = each %new_name_unique) {
        prov_update_parent_path(
            $file,
            sub {
                # Supposedly test that the file is a valid product.  But we
                # already checked all these files, so just return true.
                return 1;
            },
            sub {
                # Call-back to test whether a filename is suitable for the
                # archive.  Can't just give a \&ukirt_filename_is_archival
                # as we need to remove the directory name from the path.
                my $file = basename(shift);
                return ukirt_filename_is_archival($file);
            },
            sub {
                # Call-back to check whether a provenance entry should
                # be kept.  Yes if it's already an archival file, a raw
                # file, or it's one we're currently converting to FITS.
                my $file = basename(shift);
                $file =~ s/\.I\d+//;
                return ukirt_filename_is_archival($file)
                    || ukirt_filename_is_raw($file)
                    || exists $new_name_unique{$file};
            },
            sub {
                # Call-back to convert provenance entry filenames to
                # their archival form.  We can just use the hash of
                # filename conversions.  This routine does not get
                # called for archival files.
                my $file = basename(shift);
                $file =~ s/\.I\d+//;
                return $file if ukirt_filename_is_raw($file);
                return $new_name_unique{$file};
            },
            # Enable strict provenance checking.
            1,
        );

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

=item convert_ukirt_catalogs

Convert ORAC-DR-produced catalog files into FITS files for ingestion by CADC.

=cut

sub convert_ukirt_catalogs {
    my $dpid = shift;
    my $dpdate = shift;
    my $catalogs = shift;

    while (my ($file, $header) = each %$catalogs) {
        my $product = $header->value('PRODUCT');
        next unless (defined $product) && ($product =~ /^[-_A-Za-z0-9]+$/);

        my ($inst, $date, $obs, $suffix) = split_ukirt_filename($file);
        next unless defined $inst;

        my $outfile = lc(sprintf('ukirt_%s_%s_%05d_%s_catalog.fits',
                              $inst, $date, $obs, $product));

        copy($file, $outfile);

        # Add FITS headers similarly to how it's done for sdf
        # files by update_fits_headers().

        my $status = 0;
        my $fits = Astro::FITS::CFITSIO::open_file($outfile, READWRITE,
                                                   $status);
        if ($status) {
            print "WARNING: failed to open catalog to update header\n";
        }
        else {
            $fits->update_key(TSTRING, 'INSTREAM', 'UKIRT',
                              'Source of input stream', $status);
            $fits->update_key(TSTRING, 'DPDATE', $dpdate,
                              'Data processing date', $status);
            $fits->update_key(TSTRING, 'DPRCINST', $dpid,
                              'Data processing recipe instance ID', $status);
            $fits->write_comment($_, $status)
                foreach @{cadc_ukirt_acknowledgement()};
            $fits->close_file($status);
        }

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
                    and has_filter_fuzzy($instrument, $1)} @$type)) {
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
        my $filter = has_filter_fuzzy($instrument, $1);
        die 'Filter name suddenly became invalid'
            unless defined $filter;
        Astro::WaveBand->new(Instrument => $instrument,
                             Filter => $filter)->wavelength() => $_;
    } @_;
    my @png = @png{sort {$a <=> $b} keys %png};

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
        qr/^ukirt_.*\.(?:fits|png|txt)$/,
    );

    sub ukirt_filename_is_archival {
        my $filename = basename(shift);

        foreach my $pattern (@ukirt_archival_patterns) {
            return 1 if $filename =~ $pattern;
        }

        return 0;
    }
}

=item ukirt_filename_is_log

Test whether the given filename is a log file which we would like
to save in the archive.

=cut

{
    my @ukirt_log_patterns = (
        qr/^log\.group$/,
    );

    sub ukirt_filename_is_log {
        my $filename = basename(shift);

        foreach my $pattern (@ukirt_log_patterns) {
            return 1 if $filename =~ $pattern;
        }

        return 0;
    }
}

=item ukirt_filename_is_raw

Check whether the given filename follows the naming convention for
raw UKIRT.

Analagous to C<JSA::Files::looks_like_rawfile>.

=cut

{
    # List of filename patterns.  A file is considered archival
    # if it matches any of these patterns.
    my @ukirt_raw_patterns = (
        qr/^[a-z][0-9]{8}_[0-9]{5}\.(?:sdf|fits)$/,
    );

    sub ukirt_filename_is_raw {
        my $filename = basename(shift);

        foreach my $pattern (@ukirt_raw_patterns) {
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
        'arc',
        'standard',

        # IFU
        'cube_fc',
        'im_fc',
        'sp_fc',
        'cube',
        'im',
        'sp',
        'cube_dbs',
        'im_dbs',
        'sp_dbs',
    );

    sub ukirt_file_is_product {
        my $file = shift;

        return 0 unless $file =~ /\.sdf$/;

        my ($inst, $date, $obs, $suffix) = split_ukirt_filename($file);
        return 0 unless defined $inst
                    and defined $suffix;

        return 1 if grep {$_ eq $suffix} @ukirt_product_patterns;

        # Special case for multi-color "mos" files.  We need to check
        # if we have a valid filter name to avoid matching other
        # files ending *_mos which we don't want.
        if ($suffix =~ /(.+)_mos/) {
            return 1 if has_filter_fuzzy($inst, $1);
        }

        return 0;
    }
}

sub ukirt_file_is_catalog_product {
    my $file = shift;

    return 0 unless $file =~ /\.FIT$/;

    my ($inst, $date, $obs, $suffix) = split_ukirt_filename($file);
    return 0 unless defined $inst
                and defined $suffix;

    # Accept any catalog.  Note that we will also check that there
    # is a product header later.

    return 1;
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

=item has_filter_fuzzy

Check whether an instrument has a given filter when the name of the filter
has been mangled by ORAC-DR for use in a file name by applying these
replacements:

    $hdsfilter =~ s/\./p/g;
    $hdsfilter =~ tr/\(\)\[\]/d/;

If the filter is found, return the un-mangled filter name, otherwise
return undef.

=cut

sub has_filter_fuzzy {
    my $instrument = shift;
    my $hdsfilter = shift;

    # Expand the potential filter name into the list of all possible
    # pre-replacement strings.

    my @poss = ('');

    foreach my $char (split //, $hdsfilter) {
        if ($char eq 'p') {
            @poss = (
                (map {$_ . 'p'} @poss),
                (map {$_ . '.'} @poss),
            );
        }
        elsif ($char eq 'd') {
            @poss = (
                (map {$_ . 'd'} @poss),
                (map {$_ . '('} @poss),
                (map {$_ . ')'} @poss),
                (map {$_ . '['} @poss),
                (map {$_ . ']'} @poss),
            );
        }
        else {
            @poss = map {$_ . $char} @poss;
        }
    }

    # Now check whether any of the possible strings is a filter for the
    # given instrument.  We have to do it this way because Astro::WaveBand
    # has its list of filters in a "my" variable and only offers the
    # "has_filter" subroutine which performs exact matches.

    foreach my $filter (@poss) {
        return $filter if Astro::WaveBand::has_filter($instrument, $filter);
    }

    return undef;
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

        if ($file =~ /^g([kcimfu])(\d{8})_(\d+)_([-_A-Za-z0-9]+)\.(?:sdf|png|FIT)$/) {
            return ($ukirt_instruments{$1}, $2, $3, $4);
        }
        elsif ($file =~ /^g([kcimfu])(\d{8})_(\d+).sdf$/) {
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
