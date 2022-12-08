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
&nbsp;&nbsp;&nbsp;- The value 1e-3 for mergeToleranceFactor usually works fine to close the cylinder 
&nbsp;&nbsp;&nbsp;- The optional argument roughnessFactor can be used to rescale the pipe surface roughness (default value: 1)

Produces:<br />
&nbsp;&nbsp;&nbsp;rough_new_ASCII.stl and rough_new_binary.stl

Authors:<br />
&nbsp;&nbsp;&nbsp;Guillaume Sahut, Ph.D. and Himani Garg, Ph.D.<br />
&nbsp;&nbsp;&nbsp;Department of Energy Sciences, Lund University<br />
&nbsp;&nbsp;&nbsp;Lund, Sweden

Contact:<br />
&nbsp;&nbsp;&nbsp;guillaume.sahut@energy.lth.se
