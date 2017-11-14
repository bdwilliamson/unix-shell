#!/bin/sh
Rscript permutation_test_example_guiless.R myn=100 myseed=101 myB=10000 &
Rscript permutation_test_example_guiless.R myn=300 myseed=301 myB=10000 &
Rscript permutation_test_example_guiless.R myn=500 myseed=501 myB=10000
