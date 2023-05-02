# wdb_to_x3d
A command line utility that converts MS V-Chat .wdb files to .x3d
There are still many aspects of the .wdb format that I haven't managed to figure out, but this utility creates a usable conversion with minimal manual editing necessary.


The main console app:
wdb_to_x3d.vb 

Math helper classes used for matrix decomposition. I'm pretty sure I didnt write these from scratch and that they were based heavily on a small part of a much larger library, but can't recall what (at the time I had no intention of releasing this). 
SFVec3f.vb
SFMatrix4f.vb
Quaternion.vb