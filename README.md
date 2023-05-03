# wdb_to_x3d
A command line utility that converts MS V-Chat .wdb files to .x3d
There are still many aspects of the .wdb format that I haven't managed to figure out, but this utility creates a usable conversion with minimal manual editing necessary.


The main console app:
wdb_to_x3d.vb 
This includes a function converted from the java exemplar at http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToAngle/


Math helper classes used for matrix decomposition. These were converted from X3DOM js code:
SFVec3f.vb
SFMatrix4f.vb
Quaternion.vb