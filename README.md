---------------------------------------------------------------------------------
STL_bend
---------------------------------------------------------------------------------

<b>This program bends an (ASCII) STL surface around the X-axis to produce a cylinder and writes the result in (ASCII and binary) STL format</b>

Compilation using gfortran:<br />
<pre>make</pre>

Usage:<br />
<pre>cd run
../bin/STL_bend.exe rough.stl mergeToleranceFactor
../bin/STL_bend.exe rough.stl mergeToleranceFactor [roughnessFactor]</pre>
   
Tips:<br />
&nbsp;&nbsp;&nbsp;- The value 1e-3 for mergeToleranceFactor usually works fine to close the cylinder<br /> 
&nbsp;&nbsp;&nbsp;- The optional argument roughnessFactor can be used to rescale the pipe surface roughness (default value: 1)

Produces:<br />
&nbsp;&nbsp;&nbsp;rough_new_ASCII.stl and rough_new_binary.stl

This code is free to use. If you use it, please cite the paper:<br />
H. Garg, L. Wang, G. Sahut and C. Fureby, "Large eddy simulations of fully developed turbulent flows over additively manufactured rough surfaces," Phys. Fluids, 35(4):045145, 2023. DOI: https://doi.org/10.1063/5.0143863

Authors:<br />
&nbsp;&nbsp;&nbsp;Guillaume Sahut, Ph.D. and Himani Garg, Ph.D.<br />
&nbsp;&nbsp;&nbsp;Department of Energy Sciences, Lund University<br />
&nbsp;&nbsp;&nbsp;Lund, Sweden

Contact:<br />
&nbsp;&nbsp;&nbsp;guillaume.sahut@energy.lth.se
