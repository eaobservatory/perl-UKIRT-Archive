package UKIRT::Archive::HeaderFix;

=head1 NAME

UKIRT::Archive::HeaderFix - UKIRT archive data FITS header correction routines

=cut

use strict;
use warnings;

use Astro::FITS::CFITSIO qw/:longnames :constants/;

use parent qw/Exporter/;
our @EXPORT_OK = qw/fix_fits_header/;

our $VERSION = '0.001';

=head1 SUBROUTINES

=over 4

=item fix_fits_header

Attempt to fix the FITS header of the given file.

This subroutine steps through the HDUs in the file and examines the header
of each, looking for problems which have previously been seen in UKIRT
data.  If any are found then they are corrected and the HDU's header
is re-written.

=cut

sub fix_fits_header {
    my $filename = shift;

    my $status = 0;

    my $fptr; fits_open_file($fptr, $filename, READWRITE(), $status);
    die 'Error opening FITS file' unless (defined $fptr) && (not $status);

    my $n; $fptr->get_num_hdus($n, $status);
    die 'Error reading number of HDUs' if $status;
    my $n_ext = $n - 1;

    # Fix primary HDU.
    _fix_hdu_header($fptr);

    # Fix extensions.
    for (my $i = 0; $i < $n_ext; $i ++) {
        fits_movrel_hdu($fptr, 1, undef, $status);
        die 'Error advancing to next HDU' if $status;

        _fix_hdu_header($fptr);
    }
}

sub _fix_hdu_header {
    my $fptr = shift;
    my $status = 0;

    # Get number of cards.
    my $keysexist; fits_get_hdrspace($fptr, $keysexist, undef, $status);
    die 'Error reading number of cards' if $status;

    # Read cards and alter if needed.
    my @card = ();
    my $n_fix = 0;
    for (my $i = 0; $i < $keysexist; $i ++) {
        my $card; fits_read_record($fptr, $i + 1, $card, $status);
        die 'Error reading card' if $status;

        $card .= ' ' x (80 - length($card));

        # TRUE / FALSE written in full.
        $n_fix ++ if $card =~ s/^(........=                )FALSE/$1    F/;
        $n_fix ++ if $card =~ s/^(........=                 )TRUE/$1   T/;

        # Short-ish non-terminated string with blank rest of line.
        $n_fix ++ if $card =~ s/^(........= '[^']{8}[^' ]{0,10}?) ( *)$/$1'$2/;

        # DATE headers with trailing Z.
        $n_fix ++ if $card =~ s/^DATE-(...= '....-..-..T..:..:..)Z'/DATE-$1' /;

        push @card, $card;
    }

    # Re-write the header if we changed anything.
    if ($n_fix) {
        # Delete the old header.
        for (my $i = 0; $i < $keysexist; $i ++) {
            fits_delete_record($fptr, $keysexist - $i, $status);
            die 'Error deleting card' if $status;
        }

        # Write the new header.
        foreach my $card (@card) {
            fits_write_record($fptr, $card, $status);
            die 'Error writing card' if $status;
        }
    }
}

1;

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
