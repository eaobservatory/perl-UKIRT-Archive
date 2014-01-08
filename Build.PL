#!/usr/bin/env perl

use strict;
use warnings;

use Module::Build;

my $build = new Module::Build(
    module_name => 'UKIRT::Archive',
    license       => 'gpl',
    requires => {
        'JSA::WrapDR' => '0.03',
        'Number::Interval' => '0.06',
    },
    build_requires => {
        'Test::More' => 0,
    },
);

$build->create_build_script();
