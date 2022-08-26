---------------------------------------------------------------------------------
STL_bend
---------------------------------------------------------------------------------

<b>This program bends an (ASCII) STL surface around the X-axis to produce a cylinder and writes the result in (ASCII and binary) STL format</b>

Compilation using gfortran:<br />
&nbsp;&nbsp;&nbsp;make

Usage:<br />
&nbsp;&nbsp;&nbsp;cd run
&nbsp;&nbsp;&nbsp;../bin/STL_bend.exe rough.stl 1e-3
   
Tip:<br />
&nbsp;&nbsp;&nbsp;1e-3 as merge tolerance factor usually works fine to close the cylinder 

Produces:<br />
&nbsp;&nbsp;&nbsp;rough_new_ASCII.stl and rough_new_binary.stl

Authors:<br />
&nbsp;&nbsp;&nbsp;Guillaume Sahut, Ph.D. and Himani Garg, Ph.D.<br />
&nbsp;&nbsp;&nbsp;Lund University, Department of Energy Sciences<br />
&nbsp;&nbsp;&nbsp;Sweden

Contact:<br />
&nbsp;&nbsp;&nbsp;guillaume.sahut@energy.lth.se
