Replication code for JEP paper
"Machine Learning: An Applied Econometric Approach"
Sendhil Mullainathan and Jann Spiess

We provide code for the following steps:
0: Clean AHS 2011 data and prepare it for prediction exercise
1: Load prediction tools (simplified version of an R package for easier local replication)
2: Tune prediction algorithms (with sample code for local and distributed/parallel  tuning)
3: Fit the tuned models with tuning parameters determined in 2
4: Combine results from 3 and produce Table 1
5: Construct Figure 2 (barcode graph)

0 and 1 have to be run before 2 or 3.
3 can be run without 2, using the tuning parameters we found through running 2.
4 uses results from 3.
5 requires 0.

Based on code by David Welgus and Valerie Michelman

Jann Spiess, March/April 2017
jspiess@fas.harvard.edu

