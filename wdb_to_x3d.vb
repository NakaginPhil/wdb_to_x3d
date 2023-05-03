Imports System.IO

Friend Module wdb_to_x3d
    Public Const _RH = False      'Convert output to RH coordinate system: column 3 of 4x4 matrices *-1.0; reverse order of vertex indices;

    Public Sub Main()
        Dim args() As String = System.Environment.GetCommandLineArgs()
        Dim infilename As String = ""
        Dim corefilename As String = ""
        Dim outfilename As String = ""
        Dim titlename As String = ""
        Dim f1 As New Form1
        Dim NL As String = Environment.NewLine

        If args.Length > 1 Then
            infilename = args(1).ToString
            If LCase(Right(infilename, 4)) = ".wdb" Then
                corefilename = Left(infilename, infilename.Length - 4)
            Else
                corefilename = infilename
                infilename = infilename + ".wdb"
            End If
            titlename = corefilename
            If args.Length > 2 Then
                outfilename = args(2)
            Else
                outfilename = corefilename + ".x3d"
            End If
            If args.Length > 3 Then
                titlename = args(3)
            End If

            Console.WriteLine(NL + "infile: " + infilename)
            Console.WriteLine(NL + "outfile: " + outfilename)
            Console.WriteLine(NL + "title: " + titlename)

            f1.Readwdb(infilename)
            'f1.WriteDebugwrl(outfilename)
            'f1.Writewrl(outfilename)
            f1.WriteX3d(outfilename, titlename)

        Else
            Console.WriteLine(NL + "Error - no filename provided.")
        End If

        'Console.Write(NL + "Press any key to exit... ")
        'Console.ReadKey(True)
    End Sub


    Public Class Form1
        Public Structure VCBorderDef
            Public xCoord0 As Double
            Public zCoord0 As Double
            Public xCoord1 As Double
            Public zCoord1 As Double
        End Structure

        Public Structure VCFence
            Public numBorderDefs As UInt16      'number of 32-byte border definitions
            Public borderDefs() As VCBorderDef  'Array of numBorderDefs VCBorderDef structures
            Public yMax As Double
            Public yMin As Double
        End Structure

        Public Structure VCHeader
            Public id() As Byte     'initial dimension 4 bytes, should be characters WDBV
            Public version As Byte  'usually &H04
            Public b0 As Byte       'unidentified byte, usually &H00
            Public b1 As Byte       'unidentified byte, usually &H00
            Public b2 As Byte       'unidentified byte, usually &H00
            Public bgBlue As Byte   'background colour blue
            Public bgGreen As Byte  'background colour green
            Public bgRed As Byte    'background colour red
            Public b3 As Byte       'unidentified byte, usually &HFF
        End Structure

        Public Structure VCAnimation
            Public originX As Single    'speculative - always 0.0
            Public originY As Single    'speculative - always 0.0
            Public originZ As Single    'speculative - always 0.0
            Public axisX As Single      'rotation axis vector
            Public axisY As Single      'rotation axis vector
            Public axisZ As Single      'rotation axis vector
            Public speed As Single      'rotation speed
        End Structure

        Public Structure VCPoint
            Public x As Single
            Public y As Single
            Public z As Single
        End Structure

        Public Structure VCNormal
            Public x As Single
            Public y As Single
            Public z As Single
        End Structure

        Public Structure VCTexCoord
            Public u As Single
            Public v As Single
        End Structure

        Public Structure VCFFFlags
            Public f0 As Byte
            Public f1 As Byte
            Public f2 As Byte
            Public f3 As Byte
        End Structure

        Public Structure VCTexDef
            Public txBlue As Byte
            Public txGreen As Byte
            Public txRed As Byte
            Public txb3 As Byte     'unknown function, always &HFF - opacity?
            Public txName As String 'file name of texture image
            Public txb4 As Byte     'unknown function, usually &H00, rarely &H01 - emissive? specular?
        End Structure

        Public Structure VCPolyDef
            Public numVerts As UInt16   'number of vertices in this polygon
            Public vertices() As UInt16 'Array of numVerts indices into points table
            Public normals() As UInt16  'Array of numVerts indices into normal table
        End Structure

        Public Structure VCGeometry
            Public numPoints As UInt16          'number of 3D points in this geometry
            Public points() As VCPoint          'Array of numPoints 3D points
            Public numNormals As UInt16         'number of normals in this geometry
            Public normals() As VCNormal        'Array of numNormals normals
            Public w4 As UInt16                 'number of faces XXXunidentified wordXXX
            Public lenIFS As UInt16             'number of bytes in indexed face table
            Public numPolygons As UInt16        'calculated number of polygons
            Public polyDefs() As VCPolyDef      'Array of numPolygons VCPolyDef structures
            Public finalIFS As UInt16           'final word of indexed face set - usually 0000
            Public renderType As UInt16                 'unidentified word (usually =256)
            Public texCoords() As VCTexCoord    'Array of numPoints texture coordinates
            Public ffs() As VCFFFlags           'Array of numPoints 4-byte flag sets
            Public numTexDefs As Byte           'Number of texture definitions to follow
            Public texDefs() As VCTexDef        'Array of VCTexDef structures
            Public numTextures As Integer       'how many named texture files
            Public numMaterials As Integer      'How many non-textured materials are defined
            Public materials() As UInt16        'Array of numPolygons material indices
        End Structure

        Public Structure VCLight
            Public lightType As Byte       'unidentified byte
            Public b1 As Byte       'unidentified byte
            Public b2 As Byte       'unidentified byte
            Public b3 As Byte       'unidentified byte
            Public b4 As Byte       'unidentified byte
            Public b5 As Byte       'unidentified byte
            Public b6 As Byte       'unidentified byte
            Public b7 As Byte       'unidentified byte
            Public z As Single      'vector z
            Public y As Single      'vector y
            Public x As Single      'vector x
            Public s3 As Single     'unidentified IEEESP
            Public s4 As Single     'unidentified IEEESP
            Public worldPosX As Single  'calculated position
            Public worldPosY As Single  'calculated position
            Public worldPosZ As Single  'calculated position
            Public worldDirX As Single  'calculated direction
            Public worldDirY As Single  'calculated direction
            Public worldDirZ As Single  'calculated direction
            Public worldDirW As Single  'calculated direction
        End Structure

        Public Structure VCNode
            Public blockName As String          'name of the block, or none
            Public transform() As Single        'Transformation matrix of 16 IEEE SP values
            Public animation As VCAnimation     'Animation parameters
            Public numGeometries As UInt16      'number of geometry chunks
            Public geometries() As VCGeometry   'Array of numGeometries VCGeometry structures
            Public numLights As UInt16          'number of VCLight
            Public lights() As VCLight          'Array of numLights VCLight structures
            Public numChildren As UInt16        'number of child nodes
            Public cumTransform As SFMatrix4f      'cumulative transform for this node (ignoring animations)
        End Structure

        Public Structure VCWorld
            Public header As VCHeader           'one single header per world
            Public fence As VCFence             'one single fence per world
            Public numNodes As Integer
            Public nodes() As VCNode            'world definition is composed of a series of nodes
            Public numTexAnims As Integer
            Public texAnims() As VCTexAnimInfo 'texture animation definitions
            Public isLit As Boolean            'true if there are any lights at all
            Public isAmbientLit As Boolean     'true if only ambient light is present
        End Structure

        Public Structure VCTexAnimInfo
            Dim node As Integer
            Dim name As String
            Dim numFrames As Integer
            Dim frameDuration As Integer
        End Structure

        Private world As New VCWorld
        Private ReadOnly printNormals As Boolean

        Public Sub Readwdb(ByVal filename As String)
            Dim typeVersion As Byte() = {0, 0, 0, 0, 0, 0, 0, 0}
            Dim rgbBgColour As Byte() = {0, 0, 0}
            Dim nodeIndex As Integer = 0
            Dim nameLen As Byte
            Dim i As Integer
            Dim j As Integer
            Dim k As Integer
            Dim l As Integer
            Dim tempIFS() As UInt16
            Dim count As Integer
            Dim nPolys As Integer = 0
            Dim cIndex As Integer = 0
            Dim nIndex As Integer = 0
            Dim nVerts As UInt16
            Dim dummy As Byte
            Dim tmp As Single
            Dim NL As String = Environment.NewLine




            Dim binReader As New BinaryReader(File.Open(filename, FileMode.Open))
            world.isLit = False
            world.isAmbientLit = True               'overridden by isLit
            world.header.id = New Byte(4) {}        'initialise header
            world.numNodes = 0
            world.nodes = New VCNode() {}        'start with 100 nodes, will expand if necessary
            ReDim world.nodes(500)
            world.numTexAnims = 0
            world.texAnims = New VCTexAnimInfo() {}        'start with 100 nodes, will expand if necessary
            ReDim world.texAnims(500)                       'there will never be this many, can never be more than 1 per VCNode

            Try
                ' Read 4 bytes file header into a buffer to 
                ' determine if the file is empty.
                count = binReader.Read(world.header.id, 0, 4)

                If count <> 0 Then
                    'Read version byte
                    world.header.version = binReader.ReadByte
                    'Read b0
                    world.header.b0 = binReader.ReadByte
                    'Read b1
                    world.header.b1 = binReader.ReadByte
                    'Read b0
                    world.header.b2 = binReader.ReadByte
                    'Read bgBlue
                    world.header.bgBlue = binReader.ReadByte
                    'Read bgGreen
                    world.header.bgGreen = binReader.ReadByte
                    'Read bgRed
                    world.header.bgRed = binReader.ReadByte
                    'Read b3
                    world.header.b3 = binReader.ReadByte
                    'header read complete

                    'Read numBorderDefs
                    world.fence.numBorderDefs = binReader.ReadUInt16
                    'create an array to hold all the border definitions
                    world.fence.borderDefs = New VCBorderDef(world.fence.numBorderDefs) {}
                    For i = 0 To world.fence.numBorderDefs - 1
                        world.fence.borderDefs(i).xCoord0 = binReader.ReadDouble
                        world.fence.borderDefs(i).zCoord0 = binReader.ReadDouble
                        world.fence.borderDefs(i).xCoord1 = binReader.ReadDouble
                        world.fence.borderDefs(i).zCoord1 = binReader.ReadDouble
                        'RichTextBox1.AppendText(NL & "( " & (world.header.borderDefs(i).xCoord) & ", " & (world.header.borderDefs(i).zCoord) & " )")
                    Next
                    world.fence.yMax = binReader.ReadDouble
                    world.fence.yMin = binReader.ReadDouble
                    'fence read complete

                    Do
                        'check that there is enough node space
                        If UBound(world.nodes) < world.numNodes Then
                            ReDim Preserve world.nodes(UBound(world.nodes) + 20)
                        End If

                        'read the block name, if it exists
                        nameLen = binReader.ReadByte
                        If nameLen = 0 Then
                            world.nodes(nodeIndex).blockName = ""
                        Else
                            world.nodes(nodeIndex).blockName = binReader.ReadChars(nameLen)
                        End If
                        Console.Write(NL & " nodename " & world.nodes(nodeIndex).blockName)
                        'MsgBox(nameLen & "*" & world.nodes(nodeIndex).blockName & "*")

                        'read in the transformation matrix
                        world.nodes(nodeIndex).transform = New Single(16) {}
                        For i = 0 To 15
                            world.nodes(nodeIndex).transform(i) = binReader.ReadSingle
                        Next




                        'create and read in the VCAnimation parameters
                        world.nodes(nodeIndex).animation = New VCAnimation With {
                            .originX = binReader.ReadSingle,
                            .originY = binReader.ReadSingle,
                            .originZ = binReader.ReadSingle,
                            .axisX = binReader.ReadSingle,
                            .axisY = binReader.ReadSingle,
                            .axisZ = binReader.ReadSingle,
                            .speed = binReader.ReadSingle
                        }

                        'read number of geometry chunks
                        world.nodes(nodeIndex).numGeometries = binReader.ReadUInt16
                        If world.nodes(nodeIndex).numGeometries > 0 Then
                            world.nodes(nodeIndex).geometries = New VCGeometry(world.nodes(nodeIndex).numGeometries - 1) {}
                            'read geometry chunks - usually one
                            For i = 0 To world.nodes(nodeIndex).numGeometries - 1
                                'read number of 3D points in this geometry
                                world.nodes(nodeIndex).geometries(i).numPoints = binReader.ReadUInt16
                                world.nodes(nodeIndex).geometries(i).points = New VCPoint(world.nodes(nodeIndex).geometries(i).numPoints - 1) {}
                                For j = 0 To world.nodes(nodeIndex).geometries(i).numPoints - 1
                                    world.nodes(nodeIndex).geometries(i).points(j).x = binReader.ReadSingle
                                    world.nodes(nodeIndex).geometries(i).points(j).y = binReader.ReadSingle
                                    world.nodes(nodeIndex).geometries(i).points(j).z = binReader.ReadSingle
                                Next
                                'read number of normals in this geometry
                                world.nodes(nodeIndex).geometries(i).numNormals = binReader.ReadUInt16
                                world.nodes(nodeIndex).geometries(i).normals = New VCNormal(world.nodes(nodeIndex).geometries(i).numNormals - 1) {}
                                For j = 0 To world.nodes(nodeIndex).geometries(i).numNormals - 1
                                    world.nodes(nodeIndex).geometries(i).normals(j).x = binReader.ReadSingle
                                    world.nodes(nodeIndex).geometries(i).normals(j).y = binReader.ReadSingle
                                    world.nodes(nodeIndex).geometries(i).normals(j).z = binReader.ReadSingle
                                Next
                                'read number of faces XXXunidentified wordXXX
                                world.nodes(nodeIndex).geometries(i).w4 = binReader.ReadUInt16
                                'read number of bytes in IndexedFaceSet table
                                world.nodes(nodeIndex).geometries(i).lenIFS = binReader.ReadUInt16
                                'create temporary table storage
                                ReDim tempIFS(world.nodes(nodeIndex).geometries(i).lenIFS / 2 - 1)
                                For j = 0 To world.nodes(nodeIndex).geometries(i).lenIFS / 2 - 1
                                    tempIFS(j) = binReader.ReadUInt16
                                Next
                                cIndex = 0
                                If tempIFS(cIndex) <> 0 Then nPolys = 1
                                nIndex = cIndex + tempIFS(cIndex) * 2 + 1
                                Do Until tempIFS(nIndex) = 0
                                    cIndex = nIndex
                                    nPolys = nPolys + 1
                                    nIndex = cIndex + tempIFS(cIndex) * 2 + 1
                                Loop
                                world.nodes(nodeIndex).geometries(i).numPolygons = nPolys
                                world.nodes(nodeIndex).geometries(i).polyDefs = New VCPolyDef(nPolys - 1) {}
                                cIndex = 0
                                For j = 0 To nPolys - 1
                                    nVerts = tempIFS(cIndex)
                                    cIndex = cIndex + 1
                                    world.nodes(nodeIndex).geometries(i).polyDefs(j).numVerts = nVerts
                                    world.nodes(nodeIndex).geometries(i).polyDefs(j).vertices = New UInt16(nVerts - 1) {}
                                    world.nodes(nodeIndex).geometries(i).polyDefs(j).normals = New UInt16(nVerts - 1) {}
                                    For k = 0 To nVerts - 1
                                        world.nodes(nodeIndex).geometries(i).polyDefs(j).vertices(k) = tempIFS(cIndex)
                                        cIndex = cIndex + 1
                                        world.nodes(nodeIndex).geometries(i).polyDefs(j).normals(k) = tempIFS(cIndex)
                                        cIndex = cIndex + 1
                                    Next
                                Next
                                world.nodes(nodeIndex).geometries(i).finalIFS = tempIFS(cIndex)
                                'read unidentified word
                                world.nodes(nodeIndex).geometries(i).renderType = binReader.ReadUInt16
                                world.nodes(nodeIndex).geometries(i).texCoords = New VCTexCoord(world.nodes(nodeIndex).geometries(i).numPoints - 1) {}
                                For j = 0 To world.nodes(nodeIndex).geometries(i).numPoints - 1
                                    tmp = binReader.ReadSingle
                                    If Single.IsNaN(tmp) Then
                                        tmp = 0.0
                                    End If
                                    world.nodes(nodeIndex).geometries(i).texCoords(j).u = tmp
                                    tmp = binReader.ReadSingle
                                    If Single.IsNaN(tmp) Then
                                        tmp = 0.0
                                    End If
                                    world.nodes(nodeIndex).geometries(i).texCoords(j).v = tmp
                                Next
                                world.nodes(nodeIndex).geometries(i).ffs = New VCFFFlags(world.nodes(nodeIndex).geometries(i).numPoints - 1) {}
                                For j = 0 To world.nodes(nodeIndex).geometries(i).numPoints - 1
                                    world.nodes(nodeIndex).geometries(i).ffs(j).f0 = binReader.ReadByte
                                    world.nodes(nodeIndex).geometries(i).ffs(j).f1 = binReader.ReadByte
                                    world.nodes(nodeIndex).geometries(i).ffs(j).f2 = binReader.ReadByte
                                    world.nodes(nodeIndex).geometries(i).ffs(j).f3 = binReader.ReadByte
                                Next
                                'read number of texture definitions
                                world.nodes(nodeIndex).geometries(i).numMaterials = 0
                                world.nodes(nodeIndex).geometries(i).numTextures = 0
                                world.nodes(nodeIndex).geometries(i).numTexDefs = binReader.ReadByte
                                world.nodes(nodeIndex).geometries(i).texDefs = New VCTexDef(world.nodes(nodeIndex).geometries(i).numTexDefs - 1) {}
                                For j = 0 To world.nodes(nodeIndex).geometries(i).numTexDefs - 1
                                    world.nodes(nodeIndex).geometries(i).texDefs(j).txBlue = binReader.ReadByte
                                    world.nodes(nodeIndex).geometries(i).texDefs(j).txGreen = binReader.ReadByte
                                    world.nodes(nodeIndex).geometries(i).texDefs(j).txRed = binReader.ReadByte
                                    world.nodes(nodeIndex).geometries(i).texDefs(j).txb3 = binReader.ReadByte
                                    'read the texture filename, if it exists
                                    nameLen = binReader.ReadByte
                                    If nameLen = 0 Then
                                        world.nodes(nodeIndex).geometries(i).texDefs(j).txName = ""
                                        world.nodes(nodeIndex).geometries(i).numMaterials = world.nodes(nodeIndex).geometries(i).numMaterials + 1
                                    Else
                                        world.nodes(nodeIndex).geometries(i).texDefs(j).txName = LCase(binReader.ReadChars(nameLen))
                                        world.nodes(nodeIndex).geometries(i).numTextures = world.nodes(nodeIndex).geometries(i).numTextures + 1
                                        world.nodes(nodeIndex).geometries(i).numMaterials = world.nodes(nodeIndex).geometries(i).numMaterials + 1
                                    End If
                                    Console.Write(NL & " txname " & world.nodes(nodeIndex).geometries(i).texDefs(j).txName)
                                    world.nodes(nodeIndex).geometries(i).texDefs(j).txb4 = binReader.ReadByte
                                    If world.nodes(nodeIndex).geometries(i).texDefs(j).txb4 > 0 Then
                                        For k = 0 To world.nodes(nodeIndex).geometries(i).texDefs(j).txb4 - 1
                                            For l = 0 To 7
                                                dummy = binReader.ReadByte
                                            Next
                                        Next
                                    End If

                                Next
                                world.nodes(nodeIndex).geometries(i).materials = New UInt16(world.nodes(nodeIndex).geometries(i).numPolygons - 1) {}
                                For j = 0 To world.nodes(nodeIndex).geometries(i).numPolygons - 1
                                    world.nodes(nodeIndex).geometries(i).materials(j) = binReader.ReadUInt16
                                Next
                            Next
                        End If
                        'finished reading geometry
                        'read number of VCLight structures
                        world.nodes(nodeIndex).numLights = binReader.ReadUInt16
                        If world.nodes(nodeIndex).numLights > 0 Then
                            world.isLit = True
                            world.nodes(nodeIndex).lights = New VCLight(world.nodes(nodeIndex).numLights - 1) {}
                            For i = 0 To world.nodes(nodeIndex).numLights - 1
                                world.nodes(nodeIndex).lights(i).lightType = binReader.ReadByte
                                world.nodes(nodeIndex).lights(i).b1 = binReader.ReadByte
                                world.nodes(nodeIndex).lights(i).b2 = binReader.ReadByte
                                world.nodes(nodeIndex).lights(i).b3 = binReader.ReadByte
                                world.nodes(nodeIndex).lights(i).b4 = binReader.ReadByte
                                world.nodes(nodeIndex).lights(i).b5 = binReader.ReadByte
                                world.nodes(nodeIndex).lights(i).b6 = binReader.ReadByte
                                world.nodes(nodeIndex).lights(i).b7 = binReader.ReadByte
                                world.nodes(nodeIndex).lights(i).z = binReader.ReadSingle
                                world.nodes(nodeIndex).lights(i).y = binReader.ReadSingle
                                world.nodes(nodeIndex).lights(i).x = binReader.ReadSingle
                                world.nodes(nodeIndex).lights(i).s3 = binReader.ReadSingle
                                world.nodes(nodeIndex).lights(i).s4 = binReader.ReadSingle

                                Console.WriteLine(NL + "node: " + nodeIndex.ToString + " light: " + i.ToString + " type: " + world.nodes(nodeIndex).lights(i).lightType.ToString)


                                If world.nodes(nodeIndex).lights(i).lightType <> 0 Then
                                    world.isAmbientLit = False
                                End If


                                'transform position (0,0,0) and direction (x,y,z) by this node's transform matrix
                                'this will allow lights to be pulled out to the top level, so they can reach all nodes
                                'this assumes that only the local VCNode transform affects the light
                                world.nodes(nodeIndex).lights(i).worldPosX = world.nodes(nodeIndex).transform(12)
                                world.nodes(nodeIndex).lights(i).worldPosY = world.nodes(nodeIndex).transform(13)
                                world.nodes(nodeIndex).lights(i).worldPosZ = world.nodes(nodeIndex).transform(14)
                                world.nodes(nodeIndex).lights(i).worldDirX = world.nodes(nodeIndex).lights(i).x * world.nodes(nodeIndex).transform(0) + world.nodes(nodeIndex).lights(i).y * world.nodes(nodeIndex).transform(4) + world.nodes(nodeIndex).lights(i).z * world.nodes(nodeIndex).transform(8) ' + world.nodes(nodeIndex).transform(12)
                                world.nodes(nodeIndex).lights(i).worldDirY = world.nodes(nodeIndex).lights(i).x * world.nodes(nodeIndex).transform(1) + world.nodes(nodeIndex).lights(i).y * world.nodes(nodeIndex).transform(5) + world.nodes(nodeIndex).lights(i).z * world.nodes(nodeIndex).transform(9) ' + world.nodes(nodeIndex).transform(13)
                                world.nodes(nodeIndex).lights(i).worldDirZ = world.nodes(nodeIndex).lights(i).x * world.nodes(nodeIndex).transform(2) + world.nodes(nodeIndex).lights(i).y * world.nodes(nodeIndex).transform(6) + world.nodes(nodeIndex).lights(i).z * world.nodes(nodeIndex).transform(10) ' + world.nodes(nodeIndex).transform(14)
                                world.nodes(nodeIndex).lights(i).worldDirW = world.nodes(nodeIndex).lights(i).x * world.nodes(nodeIndex).transform(3) + world.nodes(nodeIndex).lights(i).y * world.nodes(nodeIndex).transform(7) + world.nodes(nodeIndex).lights(i).z * world.nodes(nodeIndex).transform(11) + world.nodes(nodeIndex).transform(15)
                                'world.nodes(nodeIndex).lights(i).worldDirX = world.nodes(nodeIndex).lights(i).worldDirW * world.nodes(nodeIndex).lights(i).worldDirX
                                'world.nodes(nodeIndex).lights(i).worldDirY = world.nodes(nodeIndex).lights(i).worldDirW * world.nodes(nodeIndex).lights(i).worldDirY
                                'world.nodes(nodeIndex).lights(i).worldDirZ = world.nodes(nodeIndex).lights(i).worldDirW * world.nodes(nodeIndex).lights(i).worldDirZ
                                'world.nodes(nodeIndex).lights(i).worldDirW = 1.0
                            Next
                        End If
                        'read number of child nodes
                        world.nodes(nodeIndex).numChildren = binReader.ReadUInt16


                        world.numNodes = nodeIndex + 1
                        nodeIndex = nodeIndex + 1
                    Loop
                    Return
                End If

                ' If the end of the stream is reached before reading
                ' the four data values, ignore the error and use the
                ' default settings for the remaining values.
            Catch ex As EndOfStreamException
                Console.WriteLine(" {0} caught and ignored. " &
                "Using default values.", ex.GetType().Name)
            Finally
                binReader.Close()
            End Try

            Dim cumulativeMatrix As SFMatrix4f
            cumulativeMatrix = New SFMatrix4f

            'run through and update the cumulative transform
            UpdateTransform(0, cumulativeMatrix)

        End Sub

        Private Sub UpdateTransform(ByRef node As Integer, ByRef curMatrix As SFMatrix4f)
            'Update this VCNode's child nodes, if there are any
            Dim numChildNodes As Integer
            Dim thisMatrix, newMatrix As SFMatrix4f

            thisMatrix = New SFMatrix4f(world.nodes(node).transform(0), world.nodes(node).transform(4), world.nodes(node).transform(8), world.nodes(node).transform(12), world.nodes(node).transform(1), world.nodes(node).transform(5), world.nodes(node).transform(9), world.nodes(node).transform(13), world.nodes(node).transform(2), world.nodes(node).transform(6), world.nodes(node).transform(10), world.nodes(node).transform(14), world.nodes(node).transform(3), world.nodes(node).transform(7), world.nodes(node).transform(11), world.nodes(node).transform(15))

            newMatrix = curMatrix.Mult(thisMatrix)

            world.nodes(node).cumTransform = newMatrix

            numChildNodes = world.nodes(node).numChildren
            If numChildNodes <> 0 And node < world.numNodes Then
                For i = 0 To numChildNodes - 1
                    node = node + 1
                    UpdateTransform(node, newMatrix)
                Next
            End If
        End Sub

        Private Sub ListBox1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)

        End Sub

        Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        End Sub

        Public Sub PrintNode(ByRef node As Integer, ByRef strOut As String, ByVal depth As Integer)
            Dim i As Integer
            Dim j As Integer
            Dim k As Integer
            Dim sz As Single
            Dim sfx, sfy, sfz As Single
            Dim rx, ry, rz, theta As Single
            Dim theta_old As Single
            'Dim thetax, thetay, thetaz As Single
            'Dim qw, qx, qy, qz As Single
            Dim xx, yy, zz, xy, xz, yz As Single
            Dim s As Single
            Dim epsilon, epsilon2 As Single
            Dim m00, m01, m02, m10, m11, m12, m20, m21, m22 As Single

            Dim fullmatrix, newcalc As Boolean
            Dim sourceMatrix As SFMatrix4f
            Dim translation, scaleFactor As SFVec3f
            Dim rotation, scaleOrientation As Quaternion
            Dim rotationAxis As SFVec3f
            Dim scaleOrientationAxis As SFVec3f

            Dim rotationAngle As Double
            Dim scaleOrientationAngle As Double
            Dim NL As String = Environment.NewLine

            fullmatrix = False
            newcalc = True

            epsilon = 0.01
            epsilon2 = 0.1

            sz = 1.0
            If _RH = True Then sz = -1.0

            Dim numChildNodes As Integer
            Dim indent As String

            If depth = 0 Then
                indent = ""
            Else
                indent = New String(" ", depth * 3)
            End If

            strOut &= indent & "# Node " & node & NL
            strOut &= "Info { string """ & world.nodes(node).blockName & """ }" & NL
            strOut &= indent & "Separator { " & NL

            If fullmatrix = True Then

                strOut &= indent & "   MatrixTransform { " & NL
                strOut &= indent & "      matrix " & world.nodes(node).transform(0) & " " & world.nodes(node).transform(1) & " " & world.nodes(node).transform(2) & " " & world.nodes(node).transform(3) & NL
                strOut &= indent & "             " & world.nodes(node).transform(4) & " " & world.nodes(node).transform(5) & " " & world.nodes(node).transform(6) & " " & world.nodes(node).transform(7) & NL
                strOut &= indent & "             " & world.nodes(node).transform(8) & " " & world.nodes(node).transform(9) & " " & world.nodes(node).transform(10) & " " & world.nodes(node).transform(11) & NL
                strOut &= indent & "             " & world.nodes(node).transform(12) & " " & world.nodes(node).transform(13) & " " & world.nodes(node).transform(14) & " " & world.nodes(node).transform(15) & NL
                strOut &= indent & "   }" & NL
                strOut &= NL

            Else
                If newcalc = False Then
                    m00 = world.nodes(node).transform(0)
                    m10 = world.nodes(node).transform(1)
                    m20 = world.nodes(node).transform(2)
                    m01 = world.nodes(node).transform(4)
                    m11 = world.nodes(node).transform(5)
                    m21 = world.nodes(node).transform(6)
                    m02 = world.nodes(node).transform(8)
                    m12 = world.nodes(node).transform(9)
                    m22 = world.nodes(node).transform(10)

                    sfx = Math.Sqrt(m00 * m00 + m10 * m10 + m20 * m20)
                    sfy = Math.Sqrt(m01 * m01 + m11 * m11 + m21 * m21)
                    sfz = Math.Sqrt(m02 * m02 + m12 * m12 + m22 * m22)

                    'remove scaling
                    m00 = m00 / sfx
                    m10 = m10 / sfx
                    m20 = m20 / sfx
                    m01 = m01 / sfy
                    m11 = m11 / sfy
                    m21 = m21 / sfy
                    m02 = m02 / sfz
                    m12 = m12 / sfz
                    m22 = m22 / sfz

					'This function was converted from the java exemplar at http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToAngle/
                    If Math.Abs(m01 - m10) < epsilon And Math.Abs(m02 - m20) < epsilon And Math.Abs(m12 - m21) < epsilon Then
                        'singularity found
                        'first check for identity matrix which must have +1 for all terms in leading diagonal and zero in other terms
                        If (Math.Abs(m01 + m10) < epsilon2) And (Math.Abs(m02 + m20) < epsilon2) And (Math.Abs(m12 + m21) < epsilon2) And (Math.Abs(m00 + m11 + m22 - 3) < epsilon2) Then
                            'this singularity Is identity matrix so angle = 0
                            rx = 1
                            ry = 0
                            rz = 0
                            theta = 0
                            strOut &= "#1 Identity theta=" & theta & NL
                        Else
                            'otherwise this singularity is angle = 180
                            theta = Math.PI
                            strOut &= "#2 singularity 180 theta=" & theta & NL
                            xx = (m00 + 1) / 2
                            yy = (m11 + 1) / 2
                            zz = (m22 + 1) / 2
                            xy = (m01 + m10) / 4
                            xz = (m02 + m20) / 4
                            yz = (m12 + m21) / 4
                            If ((xx > yy) And (xx > zz)) Then  'm00 Is the largest diagonal term
                                strOut &= "#m00 Is the largest diagonal term" & NL
                                If (xx < epsilon) Then
                                    strOut &= "#xx < epsilon" & NL
                                    rx = 0
                                    ry = Math.Sqrt(2.0) / 2.0
                                    rz = Math.Sqrt(2.0) / 2.0
                                Else
                                    rx = Math.Sqrt(xx)
                                    ry = xy / rx
                                    rz = xz / rx
                                End If

                            ElseIf (yy > zz) Then 'm11 Is the largest diagonal term
                                strOut &= "#m11 Is the largest diagonal term" & NL

                                If (yy < epsilon) Then
                                    strOut &= "#yy < epsilon" & NL

                                    rx = Math.Sqrt(2.0) / 2.0
                                    ry = 0
                                    rz = Math.Sqrt(2.0) / 2.0
                                Else
                                    ry = Math.Sqrt(yy)
                                    rx = xy / ry
                                    rz = yz / ry
                                End If
                            Else 'm22 Is the largest diagonal term so base result on this
                                strOut &= "#m22 Is the largest diagonal term" & NL

                                If (zz < epsilon) Then
                                    strOut &= "#zz < epsilon" & NL

                                    rx = Math.Sqrt(2.0) / 2.0
                                    ry = Math.Sqrt(2.0) / 2.0
                                    rz = 0
                                Else
                                    rz = Math.Sqrt(zz)
                                    rx = xz / rz
                                    ry = yz / rz
                                End If
                            End If
                        End If
                    Else
                        'as we have reached here there are no singularities so we can handle normally
                        s = Math.Sqrt((m21 - m12) * (m21 - m12) + (m02 - m20) * (m02 - m20) + (m10 - m01) * (m10 - m01)) 'used to normalise
                        If (Math.Abs(s) < 0.0001) Then s = 1
                        'prevent divide by zero, should Not happen if matrix Is orthogonal And should be
                        'caught by singularity test above, but I've left it in just in case
                        theta_old = Math.Acos((m00 + m11 + m22 - 1) / 2)
                        theta = 2 * Math.Acos(Math.Sqrt(1.0 + (m00) + (m11) + (m22)) / 2.0)
                        strOut &= "#m00=" & m00 & " m11=" & m11 & " m22=" & m22 & " (m00 + m11 + m22 - 1)=" & (m00 + m11 + m22 - 1) & NL
                        strOut &= "#3 theta=" & theta & "theta_old " & theta_old & NL
                        rx = (m21 - m12) / s
                        ry = (m02 - m20) / s
                        rz = (m10 - m01) / s
                    End If
                    'qw = Math.Sqrt(1.0 + (world.nodes(node).transform(0) / sfx) + (world.nodes(node).transform(5) / sfy) + (world.nodes(node).transform(10) / sfz)) / 2.0
                    'theta = 2 * Math.Acos(Math.Sqrt(1.0 + (m00) + (m11) + (m22)) / 2.0)

                    strOut &= indent & "   Transform { " & NL
                    strOut &= indent & "      translation " & world.nodes(node).transform(12) & " " & world.nodes(node).transform(13) & " " & world.nodes(node).transform(14) & NL
                    strOut &= indent & "      scaleFactor " & sfx & " " & sfy & " " & sfz & NL
                    strOut &= indent & "      rotation    " & rx & " " & ry & " " & rz & " " & theta & NL
                    strOut &= indent & "   }" & NL

                Else

                    sourceMatrix = New SFMatrix4f(world.nodes(node).transform(0), world.nodes(node).transform(4), world.nodes(node).transform(8), world.nodes(node).transform(12), world.nodes(node).transform(1), world.nodes(node).transform(5), world.nodes(node).transform(9), world.nodes(node).transform(13), world.nodes(node).transform(2), world.nodes(node).transform(6), world.nodes(node).transform(10), world.nodes(node).transform(14), world.nodes(node).transform(3), world.nodes(node).transform(7), world.nodes(node).transform(11), world.nodes(node).transform(15))

                    translation = New SFVec3f()
                    scaleFactor = New SFVec3f()
                    rotation = New Quaternion()
                    scaleOrientation = New Quaternion()
                    rotationAxis = New SFVec3f()
                    scaleOrientationAxis = New SFVec3f()
                    rotationAngle = 0
                    scaleOrientationAngle = 0

                    sourceMatrix.getTransform(translation, rotation, scaleFactor, scaleOrientation)

                    rotation.GetAxisAngle(rotationAxis, rotationAngle)
                    scaleOrientation.GetAxisAngle(scaleOrientationAxis, scaleOrientationAngle)

                    'strOut &= "#translation: " & translation.X & " " & translation.Y & " " & translation.Z & NL
                    'strOut &= "#rotation: " & rotation.X & " " & rotation.Y & " " & rotation.Z & " by " & rotationAngle & NL
                    'strOut &= "#scaleFactor: " & scaleFactor.X & " " & scaleFactor.Y & " " & scaleFactor.Z & NL
                    'strOut &= "#scaleOrientation: " & scaleOrientation.X & " " & scaleOrientation.Y & " " & scaleOrientation.Z & " by " & scaleOrientationAngle & NL

                    strOut &= indent & "   Transform { " & NL
                    strOut &= indent & "      translation      " & translation.X & " " & translation.Y & " " & translation.Z & NL
                    strOut &= indent & "      scaleFactor      " & scaleFactor.X & " " & scaleFactor.Y & " " & scaleFactor.Z & NL
                    strOut &= indent & "      rotation         " & rotation.X & " " & rotation.Y & " " & rotation.Z & " " & rotationAngle & NL
                    strOut &= indent & "      scaleOrientation " & scaleOrientation.X & " " & scaleOrientation.Y & " " & scaleOrientation.Z & " " & scaleOrientationAngle & NL
                    strOut &= indent & "   }" & NL


                End If
            End If


            '
            'VRML 1.0c Notes:
            '
            'The fields in the Material node determine the way light reflects off of an object to create color:
            'The ambientColor reflects ambient light evenly from all parts of an object regardless of viewing and lighting angles.
            'The diffuseColor reflects all VRML light sources depending on the angle of the surface with respect to the light source. The more directly the surface faces the light, the more diffuse light reflects.
            'The specularColor and shininess determine the specular highlights, e.g., the shiny spots on an apple. When the angle from the light to the surface is close to the angle from the surface to the viewer, the specularColor is added to the diffuse and ambient color calculations. Lower shininess values produce soft glows, while higher values result in sharper, smaller highlights.
            'Emissive color models "glowing" objects. This can be useful for displaying radiosity-based models (where the light energy of the room is computed explicitly), or for displaying scientific data.
            'Transparency is how "clear" the object is, with 1.0 being completely transparent, and 0.0 completely opaque.
            '
            'Specifying only emissiveColors and no diffuse, specular, emissive, or ambient colors is the way to specify pre-computed lighting.
            'Material {
            '    ambientColor [] diffuseColor [] specularColor []
            '    emissiveColor [ 0.1 0.1 0.2, 0.5 0.8 0.8 ]
            '}
            '
            'NormalBinding
            'BINDINGS
            '     DEFAULT            Use default binding
            '     OVERALL            Whole object has same normal
            '     PER_PART           One normal for each part of object
            '     PER_PART_INDEXED   One normal for each part, indexed
            '     PER_FACE           One normal for each face of object
            '     PER_FACE_INDEXED   One normal for each face, indexed  <====== Maybe this one without normal data for smooth shading
            '     PER_VERTEX         One normal for each vertex of object
            '     PER_VERTEX_INDEXED One normal for each vertex, indexed   <====== This one for smooth shading
            '
            'FILE FORMAT/DEFAULTS
            '     NormalBinding {
            '          value  DEFAULT        # SFEnum
            '     }
            '

            'output geometry sections
            If world.nodes(node).numGeometries <> 0 Then
                'just in case there are ever more than one
                For i = 0 To world.nodes(node).numGeometries - 1

                    'output the point list
                    If world.nodes(node).geometries(i).numPoints > 0 Then
                        strOut &= indent & "   Coordinate3 { " & NL
                        strOut &= indent & "      point [" & NL
                        For j = 0 To world.nodes(node).geometries(i).numPoints - 1
                            strOut &= indent & "             " & world.nodes(node).geometries(i).points(j).x & " " & world.nodes(node).geometries(i).points(j).y & " " & world.nodes(node).geometries(i).points(j).z
                            If j < world.nodes(node).geometries(i).numPoints - 1 Then
                                strOut &= "," & NL
                            Else
                                strOut &= " ]" & NL
                            End If
                        Next
                        strOut &= indent & "   }" & NL
                    End If

                    If world.nodes(node).geometries(i).renderType = 1024 Then
                        'output the normal list
                        If world.nodes(node).geometries(i).numNormals > 0 Then
                            strOut &= indent & "   Normal { " & NL
                            strOut &= indent & "     vector [" & NL
                            For j = 0 To world.nodes(node).geometries(i).numNormals - 1
                                strOut &= indent & "             " & sz * world.nodes(node).geometries(i).normals(j).x & " " & sz * world.nodes(node).geometries(i).normals(j).y & " " & sz * world.nodes(node).geometries(i).normals(j).z
                                If j < world.nodes(node).geometries(i).numNormals - 1 Then
                                    strOut &= "," & NL
                                Else
                                    strOut &= " ]" & NL
                                End If
                            Next
                            strOut &= indent & "   }" & NL
                        End If
                    End If

                    'output texture2
                    If world.nodes(node).geometries(i).numTextures > 0 Then
                        For j = 0 To world.nodes(node).geometries(i).numTexDefs - 1
                            If world.nodes(node).geometries(i).texDefs(j).txName <> "" Then
                                strOut &= indent & "Texture2 { " & NL
                                strOut &= indent & "      filename """ & world.nodes(node).geometries(i).texDefs(j).txName & """" & NL
                                strOut &= indent & "   }" & NL
                            End If
                        Next
                    End If

                    'output materials - this is speculative (haha) 
                    If world.nodes(node).geometries(i).numMaterials > 0 Then
                        k = 0
                        strOut &= indent & " Material { " & NL
                        strOut &= indent & "diffuseColor [ " '& NL
                        For j = 0 To world.nodes(node).geometries(i).numTexDefs - 1
                            'If world.nodes(node).geometries(i).texDefs(j).txName = "" Then
                            strOut &= indent & "             " & world.nodes(node).geometries(i).texDefs(j).txRed / 255.0 & " " & world.nodes(node).geometries(i).texDefs(j).txGreen / 255.0 & " " & world.nodes(node).geometries(i).texDefs(j).txBlue / 255.0
                            If j < world.nodes(node).geometries(i).numMaterials - 1 Then
                                strOut &= "," & NL
                            Else
                                strOut &= " ]" & NL
                            End If
                            'End If
                        Next
                        strOut &= indent & "ambientColor [ " '& NL
                        For j = 0 To world.nodes(node).geometries(i).numTexDefs - 1
                            'If world.nodes(node).geometries(i).texDefs(j).txName = "" Then
                            strOut &= indent & "             " & world.nodes(node).geometries(i).texDefs(j).txRed / 255.0 & " " & world.nodes(node).geometries(i).texDefs(j).txGreen / 255.0 & " " & world.nodes(node).geometries(i).texDefs(j).txBlue / 255.0
                            If j < world.nodes(node).geometries(i).numMaterials - 1 Then
                                strOut &= "," & NL
                            Else
                                strOut &= " ]" & NL
                            End If
                            'End If
                        Next

                        If world.nodes(node).geometries(i).renderType = 768 Then
                            strOut &= indent & "emissiveColor [ " '& NL
                            For j = 0 To world.nodes(node).geometries(i).numTexDefs - 1
                                'If world.nodes(node).geometries(i).texDefs(j).txName = "" Then
                                strOut &= indent & "             " & world.nodes(node).geometries(i).texDefs(j).txRed / 255.0 & " " & world.nodes(node).geometries(i).texDefs(j).txGreen / 255.0 & " " & world.nodes(node).geometries(i).texDefs(j).txBlue / 255.0
                                If j < world.nodes(node).geometries(i).numMaterials - 1 Then
                                    strOut &= "," & NL
                                Else
                                    strOut &= " ]" & NL
                                End If
                                'End If
                            Next
                        End If
                        If world.nodes(node).geometries(i).renderType = 1024 Then
                            strOut &= indent & "specularColor [ 1 1 1 ]" & NL
                            strOut &= indent & "shininess 0.2" & NL
                        End If

                        strOut &= indent & "   }" & NL
                    End If


                    'output texture coordinates
                    If world.nodes(node).geometries(i).numPoints > 0 Then
                        strOut &= indent & "TextureCoordinate2 { " & NL
                        strOut &= indent & "      point [" & NL
                        For j = 0 To world.nodes(node).geometries(i).numPoints - 1
                            strOut &= indent & "             " & world.nodes(node).geometries(i).texCoords(j).u & " " & world.nodes(node).geometries(i).texCoords(j).v
                            If j < world.nodes(node).geometries(i).numPoints - 1 Then
                                strOut &= "," & NL
                            Else
                                strOut &= " ]" & NL
                            End If
                        Next
                        strOut &= indent & "   }" & NL
                    End If



                    'output indexed face set
                    If world.nodes(node).geometries(i).numPolygons > 0 Then

                        'strOut &= indent & "ShapeHints { " & NL
                        'strOut &= indent & "  shapeType     SOLID" & NL
                        'strOut &= indent & "  creaseAngle   0.5" & NL
                        'strOut &= indent & "}" & NL

                        strOut &= indent & "IndexedFaceSet { " & NL

                        'output coordIndex
                        strOut &= indent & " coordIndex [" & NL
                        For j = 0 To world.nodes(node).geometries(i).numPolygons - 1
                            strOut &= indent & "             "
                            If _RH = True Then
                                For k = world.nodes(node).geometries(i).polyDefs(j).numVerts - 1 To 0 Step -1
                                    strOut &= world.nodes(node).geometries(i).polyDefs(j).vertices(k) & ", "
                                Next
                            Else
                                For k = 0 To world.nodes(node).geometries(i).polyDefs(j).numVerts - 1
                                    strOut &= world.nodes(node).geometries(i).polyDefs(j).vertices(k) & ", "
                                Next
                            End If

                            strOut &= "-1"
                            If j < world.nodes(node).geometries(i).numPolygons - 1 Then
                                strOut &= "," & NL
                            Else
                                strOut &= " ]" & NL
                            End If
                        Next

                        'output normalIndex
                        If world.nodes(node).geometries(i).renderType = 1024 Then
                            If world.nodes(node).geometries(i).numNormals > 0 Then
                                strOut &= indent & "   normalIndex [" & NL
                                For j = 0 To world.nodes(node).geometries(i).numPolygons - 1
                                    strOut &= indent & "             "
                                    If _RH = True Then
                                        For k = world.nodes(node).geometries(i).polyDefs(j).numVerts - 1 To 0 Step -1
                                            strOut &= world.nodes(node).geometries(i).polyDefs(j).normals(k) & ", "
                                        Next
                                    Else
                                        For k = 0 To world.nodes(node).geometries(i).polyDefs(j).numVerts - 1
                                            strOut &= world.nodes(node).geometries(i).polyDefs(j).normals(k) & ", "
                                        Next
                                    End If
                                    strOut &= "-1"
                                    If j < world.nodes(node).geometries(i).numPolygons - 1 Then
                                        strOut &= "," & NL
                                    Else
                                        strOut &= " ]" & NL
                                    End If
                                Next
                            End If
                        End If

                        'output textureCoordIndex
                        strOut &= indent & " textureCoordIndex [" & NL
                        For j = 0 To world.nodes(node).geometries(i).numPolygons - 1
                            strOut &= indent & "             "
                            If _RH = True Then
                                For k = world.nodes(node).geometries(i).polyDefs(j).numVerts - 1 To 0 Step -1
                                    strOut &= world.nodes(node).geometries(i).polyDefs(j).vertices(k) & ", "
                                Next
                            Else
                                For k = 0 To world.nodes(node).geometries(i).polyDefs(j).numVerts - 1
                                    strOut &= world.nodes(node).geometries(i).polyDefs(j).vertices(k) & ", "
                                Next
                            End If
                            strOut &= "-1"
                            If j < world.nodes(node).geometries(i).numPolygons - 1 Then
                                strOut &= "," & NL
                            Else
                                strOut &= " ]" & NL
                            End If
                        Next

                        If world.nodes(node).geometries(i).numMaterials > 0 Then
                            'output materialIndex
                            strOut &= indent & " materialIndex [" & NL
                            For j = 0 To world.nodes(node).geometries(i).numPolygons - 1
                                strOut &= indent & "             " & world.nodes(node).geometries(i).materials(j)
                                If j < world.nodes(node).geometries(i).numPolygons - 1 Then
                                    strOut &= "," & NL
                                Else
                                    strOut &= " ]" & NL
                                End If
                            Next

                        End If

                        strOut &= indent & "   }" & NL
                    End If
                Next
            End If

            'output VCLight sections
            If world.nodes(node).numLights <> 0 Then
                For i = 0 To world.nodes(node).numLights - 1
                    If world.nodes(node).lights(i).lightType = 0 Then
                        'strOut &= indent & "AmbientLight {" & NL
                        'strOut &= indent & "  on           TRUE" & NL 'VRML 1.0 default
                        'strOut &= indent & "  intensity    1" & NL 'VRML 1.0 default
                        'strOut &= indent & "  color        " & world.nodes(node).lights(i).b4 / 255.0 & " " & world.nodes(node).lights(i).b5 / 255.0 & " " & world.nodes(node).lights(i).b6 / 255.0 & NL
                        'strOut &= indent & "}" & NL
                    ElseIf world.nodes(node).lights(i).lightType = 1 Then
                        strOut &= indent & "PointLight {" & NL
                        strOut &= indent & "  on           FALSE" & NL 'VRML 1.0 default
                        strOut &= indent & "  intensity    1" & NL 'VRML 1.0 default
                        strOut &= indent & "  color        " & world.nodes(node).lights(i).b4 / 255.0 & " " & world.nodes(node).lights(i).b5 / 255.0 & " " & world.nodes(node).lights(i).b6 / 255.0 & NL
                        strOut &= indent & "  location     0 0 0" & NL 'alternatively could set default 0 0 1 and rely on VCNode.transform()
                        strOut &= indent & "}" & NL
                    ElseIf world.nodes(node).lights(i).lightType = 2 Then
                        strOut &= indent & "SpotLight {" & NL
                        strOut &= indent & "  on           FALSE" & NL 'VRML 1.0 default
                        strOut &= indent & "  intensity    1" & NL 'VRML 1.0 default
                        strOut &= indent & "  color        " & world.nodes(node).lights(i).b4 / 255.0 & " " & world.nodes(node).lights(i).b5 / 255.0 & " " & world.nodes(node).lights(i).b6 / 255.0 & NL
                        strOut &= indent & "  location     0 0 0" & NL 'alternatively could set 0 0 1 and rely on VCNode.transform()
                        strOut &= indent & "  direction    " & world.nodes(node).lights(i).x & " " & world.nodes(node).lights(i).y & " " & world.nodes(node).lights(i).z & NL
                        strOut &= indent & "  dropOffRate  0" & NL 'VRML 1.0 default
                        strOut &= indent & "  cutOffAngle  0.785398" & NL 'VRML 1.0 default
                        strOut &= indent & "}" & NL
                    ElseIf world.nodes(node).lights(i).lightType = 3 Then
                        strOut &= indent & "DirectionalLight {" & NL
                        strOut &= indent & "  on           FALSE" & NL 'VRML 1.0 default
                        strOut &= indent & "  intensity    1" & NL 'VRML 1.0 default
                        strOut &= indent & "  color        " & world.nodes(node).lights(i).b4 / 255.0 & " " & world.nodes(node).lights(i).b5 / 255.0 & " " & world.nodes(node).lights(i).b6 / 255.0 & NL
                        strOut &= indent & "  direction    " & world.nodes(node).lights(i).x & " " & world.nodes(node).lights(i).y & " " & world.nodes(node).lights(i).z & NL
                        strOut &= indent & "}" & NL
                    End If
                Next
            End If

            numChildNodes = world.nodes(node).numChildren
            If numChildNodes <> 0 And node < world.numNodes Then
                For i = 0 To numChildNodes - 1
                    node = node + 1
                    PrintNode(node, strOut, depth + 1)
                Next
            End If
            strOut &= NL
            strOut &= indent & "}" & NL
        End Sub

        Public Sub PrintFullDebugNode(ByRef node As Integer, ByRef strOut As String, ByVal depth As Integer)
            Dim i As Integer
            Dim j As Integer
            Dim blockid As String = ""
            Dim NL As String = Environment.NewLine


            Dim numChildNodes As Integer
            Dim indent As String

            If depth = 0 Then
                indent = ""
            Else
                indent = New String(" ", depth * 3)
            End If

            blockid = "# Node " & node & """" & world.nodes(node).blockName & """" & Chr(9)
            strOut &= blockid & "matrix " & Chr(9) &
                world.nodes(node).transform(0) & Chr(9) &
                world.nodes(node).transform(1) & Chr(9) &
                world.nodes(node).transform(2) & Chr(9) &
                world.nodes(node).transform(3) & Chr(9) &
                world.nodes(node).transform(4) & Chr(9) &
                world.nodes(node).transform(5) & Chr(9) &
                world.nodes(node).transform(6) & Chr(9) &
                world.nodes(node).transform(7) & Chr(9) &
                world.nodes(node).transform(8) & Chr(9) &
                world.nodes(node).transform(9) & Chr(9) &
                world.nodes(node).transform(10) & Chr(9) &
                world.nodes(node).transform(11) & Chr(9) &
                world.nodes(node).transform(12) & Chr(9) &
                world.nodes(node).transform(13) & Chr(9) &
                world.nodes(node).transform(14) & Chr(9) &
                world.nodes(node).transform(15) & NL
            strOut &= blockid & "anim matrix " & Chr(9) & world.nodes(node).animation.originX & Chr(9) & world.nodes(node).animation.originY & Chr(9) & world.nodes(node).animation.originZ & Chr(9) & world.nodes(node).animation.axisX & Chr(9) & world.nodes(node).animation.axisY & Chr(9) & world.nodes(node).animation.axisZ & Chr(9) & world.nodes(node).animation.speed & NL
            strOut &= blockid & "VCNode.w0 numGeometry:" & Chr(9) & world.nodes(node).numGeometries & NL

            'output geometry sections
            If world.nodes(node).numGeometries <> 0 Then
                'just in case there are ever more than one
                For i = 0 To world.nodes(node).numGeometries - 1
                    strOut &= blockid & "VCGeometry.numPoints numPoints:" & Chr(9) & world.nodes(node).geometries(i).numPoints & NL
                    strOut &= blockid & "--VCGeometry.points()--" & NL
                    strOut &= blockid & "VCGeometry.w3 numNormals:" & Chr(9) & world.nodes(node).geometries(i).numNormals & NL
                    strOut &= blockid & "--VCGeometry.normals()--" & NL

                    strOut &= blockid & "VCGeometry.w4 numFaces:" & Chr(9) & world.nodes(node).geometries(i).w4 & NL
                    strOut &= blockid & "VCGeometry.w5 IFSBytes:" & Chr(9) & world.nodes(node).geometries(i).lenIFS & NL
                    'strOut &= blockid & "VCGeometry.numPolygons:" & Chr(9) & world.nodes(node).geometries(i).numPolygons & NL
                    strOut &= blockid & "--VCGeometry.polyDefs()--" & NL
                    If world.nodes(node).geometries(i).numPolygons > 0 Then
                        For j = 0 To world.nodes(node).geometries(i).numPolygons - 1
                            strOut &= blockid & "Polygon " & j & ":" & Chr(9) & world.nodes(node).geometries(i).polyDefs(j).numVerts & Chr(9) & "(vertices) (normals)" & NL
                        Next
                    End If


                    strOut &= blockid & "VCGeometry.finalIFS:" & Chr(9) & world.nodes(node).geometries(i).finalIFS & NL
                    strOut &= blockid & "VCGeometry.renderType:" & Chr(9) & world.nodes(node).geometries(i).renderType & NL
                    strOut &= blockid & "--VCGeometry.texCoords()--" & NL

                    If world.nodes(node).geometries(i).numPoints > 0 Then
                        strOut &= blockid & "txCoord " & j & "U:" & Chr(9)
                        For j = 0 To Math.Min(world.nodes(node).geometries(i).numPoints, 12) - 1
                            strOut &= world.nodes(node).geometries(i).texCoords(j).u & Chr(9)
                        Next
                        strOut &= NL
                        strOut &= blockid & "txCoord " & j & "V:" & Chr(9)
                        For j = 0 To Math.Min(world.nodes(node).geometries(i).numPoints, 12) - 1
                            strOut &= world.nodes(node).geometries(i).texCoords(j).v & Chr(9)
                        Next
                        strOut &= NL
                    End If

                    For j = 0 To world.nodes(node).geometries(i).numPoints - 1
                        'If world.nodes(node).geometries(i).ffs(j).f0 <> 255 Or world.nodes(node).geometries(i).ffs(j).f1 <> 255 Or world.nodes(node).geometries(i).ffs(j).f2 <> 255 Or world.nodes(node).geometries(i).ffs(j).f3 <> 255 Then
                        strOut &= blockid & "mystery FFS:" & Chr(9) & "f0:" & world.nodes(node).geometries(i).ffs(j).f0 & Chr(9) & "f1:" & world.nodes(node).geometries(i).ffs(j).f1 & Chr(9) & "f2:" & world.nodes(node).geometries(i).ffs(j).f2 & Chr(9) & "f3:" & world.nodes(node).geometries(i).ffs(j).f3 & NL
                        'End If
                    Next

                    strOut &= blockid & "VCGeometry.numTexDefs:" & Chr(9) & world.nodes(node).geometries(i).numTexDefs & NL
                    strOut &= blockid & "--VCGeometry.texDefs()--" & NL

                    'output texDefs
                    If world.nodes(node).geometries(i).numTexDefs > 0 Then
                        For j = 0 To world.nodes(node).geometries(i).numTexDefs - 1
                            strOut &= blockid & "TexDefs " & Chr(9) & "(" & j & "):" & Chr(9) & "B" & world.nodes(node).geometries(i).texDefs(j).txBlue & Chr(9) & "G" & world.nodes(node).geometries(i).texDefs(j).txGreen & Chr(9) & "R" & world.nodes(node).geometries(i).texDefs(j).txRed & NL
                            strOut &= blockid & "TexDefs " & Chr(9) & "(" & j & "):" & Chr(9) & "txb3" & Chr(9) & world.nodes(node).geometries(i).texDefs(j).txb3.ToString("X2") & NL
                            If world.nodes(node).geometries(i).texDefs(j).txName <> "" Then
                                strOut &= blockid & "TexDefs " & Chr(9) & "(" & j & "):" & Chr(9) & "txName" & Chr(9) & """" & world.nodes(node).geometries(i).texDefs(j).txName & """" & Chr(9) & NL
                            End If
                            strOut &= blockid & "TexDefs " & Chr(9) & "(" & j & "):" & Chr(9) & "txb4" & Chr(9) & world.nodes(node).geometries(i).texDefs(j).txb4.ToString("X2") & NL
                        Next
                    End If


                    '                    'output texture2
                    '                    If world.nodes(node).geometries(i).numTextures > 0 Then
                    '                        For j = 0 To world.nodes(node).geometries(i).numTexDefs - 1
                    '                            If world.nodes(node).geometries(i).texDefs(j).txName <> "" Then
                    '                                strOut &= blockid & "Texture:" & Chr(9) & "B" & world.nodes(node).geometries(i).texDefs(j).txBlue & Chr(9) & "G" & world.nodes(node).geometries(i).texDefs(j).txGreen & Chr(9) & "R" & world.nodes(node).geometries(i).texDefs(j).txRed & Chr(9) & "?" & world.nodes(node).geometries(i).texDefs(j).txb3.ToString("X2") & Chr(9)
                    '                                strOut &= """" & world.nodes(node).geometries(i).texDefs(j).txName & """" & Chr(9) & "?" & world.nodes(node).geometries(i).texDefs(j).txb4.ToString("X2") & NL
                    '                            End If
                    '                        Next
                    '                    End If


                    strOut &= blockid & "VCGeometry.numTextures:" & Chr(9) & world.nodes(node).geometries(i).numTextures & NL
                    strOut &= blockid & "VCGeometry.numMaterials:" & Chr(9) & world.nodes(node).geometries(i).numMaterials & NL
                    strOut &= blockid & "--VCGeometry.materials()--" & NL
                    'output materials
                    If world.nodes(node).geometries(i).numPolygons > 0 Then
                        strOut &= blockid & "materials" & Chr(9)
                        For j = 0 To Math.Min(world.nodes(node).geometries(i).numPolygons, 24) - 1
                            strOut &= world.nodes(node).geometries(i).materials(j) & Chr(9)
                        Next
                        strOut &= NL
                    End If


                Next
            End If

            strOut &= blockid & "VCNode.w2 numVCLight:" & Chr(9) & world.nodes(node).numLights & NL
            If world.nodes(node).numLights > 0 Then
                For j = 0 To world.nodes(node).numLights - 1
                    strOut &= blockid & "VCLight Structure:" & Chr(9) & "lightType:" & world.nodes(node).lights(j).lightType.ToString("X2") & Chr(9)
                    strOut &= "b1:" & world.nodes(node).lights(j).b1.ToString("X2") & Chr(9)
                    strOut &= "b2:" & world.nodes(node).lights(j).b2.ToString("X2") & Chr(9)
                    strOut &= "b3:" & world.nodes(node).lights(j).b3.ToString("X2") & Chr(9)
                    strOut &= "b4:" & world.nodes(node).lights(j).b4.ToString("X2") & Chr(9)
                    strOut &= "b5:" & world.nodes(node).lights(j).b5.ToString("X2") & Chr(9)
                    strOut &= "b6:" & world.nodes(node).lights(j).b6.ToString("X2") & Chr(9)
                    strOut &= "b7:" & world.nodes(node).lights(j).b7.ToString("X2") & Chr(9)
                    strOut &= "z:" & world.nodes(node).lights(j).z & Chr(9)
                    strOut &= "y:" & world.nodes(node).lights(j).y & Chr(9)
                    strOut &= "x:" & world.nodes(node).lights(j).x & Chr(9)
                    strOut &= "s3:" & world.nodes(node).lights(j).s3 & Chr(9)
                    strOut &= "s4:" & world.nodes(node).lights(j).s4 & NL
                Next
            End If

            strOut &= blockid & "VCNode.w7 numChildNodes:" & Chr(9) & world.nodes(node).numChildren & NL

            numChildNodes = world.nodes(node).numChildren
            If numChildNodes <> 0 And node < world.numNodes Then
                For i = 0 To numChildNodes - 1
                    node = node + 1
                    PrintFullDebugNode(node, strOut, depth + 1)
                Next
            End If
            strOut &= NL
            '            strOut &= indent & "}" & NL
        End Sub

        Public Sub PrintDebugNode(ByRef node As Integer, ByRef strOut As String, ByVal depth As Integer)
            Dim i As Integer
            Dim j As Integer
            Dim blockid As String = ""
            Dim NL As String = Environment.NewLine


            Dim numChildNodes As Integer
            Dim indent As String

            If depth = 0 Then
                indent = ""
            Else
                indent = New String(" ", depth * 3)
            End If

            blockid = "# Node " & node & """" & world.nodes(node).blockName & """" & Chr(9)
            strOut &= blockid & "matrix " & Chr(9) &
                world.nodes(node).transform(0) & Chr(9) &
                world.nodes(node).transform(1) & Chr(9) &
                world.nodes(node).transform(2) & Chr(9) &
                world.nodes(node).transform(3) & Chr(9) &
                world.nodes(node).transform(4) & Chr(9) &
                world.nodes(node).transform(5) & Chr(9) &
                world.nodes(node).transform(6) & Chr(9) &
                world.nodes(node).transform(7) & Chr(9) &
                world.nodes(node).transform(8) & Chr(9) &
                world.nodes(node).transform(9) & Chr(9) &
                world.nodes(node).transform(10) & Chr(9) &
                world.nodes(node).transform(11) & Chr(9) &
                world.nodes(node).transform(12) & Chr(9) &
                world.nodes(node).transform(13) & Chr(9) &
                world.nodes(node).transform(14) & Chr(9) &
                world.nodes(node).transform(15) & NL
            strOut &= blockid & "anim matrix " & Chr(9) & world.nodes(node).transform(16) & Chr(9) & world.nodes(node).transform(17) & Chr(9) & world.nodes(node).transform(18) & Chr(9) & world.nodes(node).transform(19) & Chr(9) & world.nodes(node).transform(20) & Chr(9) & world.nodes(node).transform(21) & Chr(9) & world.nodes(node).transform(22) & NL

            'output geometry sections
            If world.nodes(node).numGeometries <> 0 Then
                'just in case there are ever more than one
                For i = 0 To world.nodes(node).numGeometries - 1

                    strOut &= blockid & "mystery w4:" & Chr(9) & world.nodes(node).geometries(i).w4 & NL
                    strOut &= blockid & "renderType:" & Chr(9) & world.nodes(node).geometries(i).renderType & NL

                    If world.nodes(node).numLights > 0 Then
                        For j = 0 To world.nodes(node).numLights - 1
                            strOut &= blockid & "lights Chunk:" & Chr(9) & "lightType:" & world.nodes(node).lights(j).lightType.ToString("X2") & Chr(9)
                            strOut &= "b1:" & world.nodes(node).lights(j).b1.ToString("X2") & Chr(9)
                            strOut &= "b2:" & world.nodes(node).lights(j).b2.ToString("X2") & Chr(9)
                            strOut &= "b3:" & world.nodes(node).lights(j).b3.ToString("X2") & Chr(9)
                            strOut &= "b4:" & world.nodes(node).lights(j).b4.ToString("X2") & Chr(9)
                            strOut &= "b5:" & world.nodes(node).lights(j).b5.ToString("X2") & Chr(9)
                            strOut &= "b6:" & world.nodes(node).lights(j).b6.ToString("X2") & Chr(9)
                            strOut &= "b7:" & world.nodes(node).lights(j).b7.ToString("X2") & Chr(9)
                            strOut &= "z:" & world.nodes(node).lights(j).z & Chr(9)
                            strOut &= "y:" & world.nodes(node).lights(j).y & Chr(9)
                            strOut &= "x:" & world.nodes(node).lights(j).x & Chr(9)
                            strOut &= "s3:" & world.nodes(node).lights(j).s3 & Chr(9)
                            strOut &= "s4:" & world.nodes(node).lights(j).s4 & NL
                        Next
                    End If

                    For j = 0 To world.nodes(node).geometries(i).numPoints - 1
                        If world.nodes(node).geometries(i).ffs(j).f0 <> 255 Or world.nodes(node).geometries(i).ffs(j).f1 <> 255 Or world.nodes(node).geometries(i).ffs(j).f2 <> 255 Or world.nodes(node).geometries(i).ffs(j).f3 <> 255 Then
                            strOut &= blockid & "mystery FFS:" & Chr(9) & "f0:" & world.nodes(node).geometries(i).ffs(j).f0 & Chr(9) & "f1:" & world.nodes(node).geometries(i).ffs(j).f1 & Chr(9) & "f2:" & world.nodes(node).geometries(i).ffs(j).f2 & Chr(9) & "f3:" & world.nodes(node).geometries(i).ffs(j).f3 & NL
                        End If
                    Next

                    'output texture2
                    If world.nodes(node).geometries(i).numTextures > 0 Then
                        For j = 0 To world.nodes(node).geometries(i).numTexDefs - 1
                            If world.nodes(node).geometries(i).texDefs(j).txName <> "" Then
                                strOut &= blockid & "Texture:" & Chr(9) & "B" & world.nodes(node).geometries(i).texDefs(j).txBlue & Chr(9) & "G" & world.nodes(node).geometries(i).texDefs(j).txGreen & Chr(9) & "R" & world.nodes(node).geometries(i).texDefs(j).txRed & Chr(9) & "?" & world.nodes(node).geometries(i).texDefs(j).txb3.ToString("X2") & Chr(9)
                                strOut &= """" & world.nodes(node).geometries(i).texDefs(j).txName & """" & Chr(9) & "?" & world.nodes(node).geometries(i).texDefs(j).txb4.ToString("X2") & NL
                            End If
                        Next
                    End If

                Next
            End If


            numChildNodes = world.nodes(node).numChildren
            If numChildNodes <> 0 And node < world.numNodes Then
                For i = 0 To numChildNodes - 1
                    node = node + 1
                    PrintDebugNode(node, strOut, depth + 1)
                Next
            End If
            strOut &= NL
            '            strOut &= indent & "}" & NL
        End Sub

        Public Sub Writewrl(ByVal filename As String)
            Dim out As String
            Dim colProxyPoints As String = ""
            Dim floorCeilGeom As String = ""
            Dim floorCeilIntro As String = ""
            Dim floorCeilCoords As String = ""
            Dim floorCeilIFS As String = ""
            Dim borderGeom As String = ""
            Dim borderIntro As String = ""
            Dim borderCoords As String = ""
            Dim borderIFS As String = ""
            Const XZOV As Double = 1.0
            Dim minxCoord, minyCoord, minzCoord, maxxCoord, maxyCoord, maxzCoord, tempSwap As Double
            Dim x0, z0, x1, z1 As Double
            minxCoord = 0.0
            minyCoord = 0.0
            minzCoord = 0.0
            maxxCoord = 0.0
            maxyCoord = 0.0
            maxzCoord = 0.0

            Dim NL As String = Environment.NewLine

            Dim i As Integer
            Dim n As Integer
            Dim sz As Single

            Dim OUTFILE As String
            '= "D:\Documents\vchat\VRML\basketballworld\basketballworld.wrl"
            OUTFILE = filename
            Dim objWriter As New System.IO.StreamWriter(OUTFILE)
            Dim node As Integer




            sz = 1.0
            If _RH = True Then sz = -1.0

            out = "#VRML V1.0 ascii"
            out &= NL
            out &= "# Background { skyColor [ " & (world.header.bgRed / 255.0) & " " & (world.header.bgGreen / 255.0) & " " & (world.header.bgBlue / 255.0) & " ] }" & NL
            out &= NL

            'output collision proxy geometry based on VCFence data
            'TODO: determine whether the VCFence data is expressed as final world coordinates
            'The collision proxy is actually a VRML97 innovation, so all this does is create the geometry for use after the file is converted to VRML97 or X3D
            'The navigation script will look for both FloorAndCeiling and Border to parse in order to create procedural equivalents for collision detection

            'out &= "# Border coordinates (without flags)" & NL
            'For i = 0 To world.fence.numBorderDefs
            '   out &= "# " & world.fence.borderDefs(i).xCoord0 & " " & world.fence.borderDefs(i).zCoord0 & NL
            '   out &= "# " & world.fence.borderDefs(i).xCoord1 & " " & world.fence.borderDefs(i).zCoord1 & NL
            'Next

            out &= "# Collision proxy geometry" & NL

            maxyCoord = world.fence.yMax
            minyCoord = world.fence.yMin
            'make sure miny and maxy are the right way round
            If minyCoord > maxyCoord Then
                tempSwap = minyCoord
                minyCoord = maxyCoord
                maxyCoord = tempSwap
            End If
            'find the X,Z rectangle enclosing the fence
            If world.fence.numBorderDefs > 0 Then
                For i = 0 To world.fence.numBorderDefs - 1
                    x0 = world.fence.borderDefs(i).xCoord0
                    z0 = world.fence.borderDefs(i).zCoord0
                    x1 = world.fence.borderDefs(i).xCoord1
                    z1 = world.fence.borderDefs(i).zCoord1
                    If x0 < minxCoord Then minxCoord = x0
                    If x1 < minxCoord Then minxCoord = x1
                    If x0 > maxxCoord Then maxxCoord = x0
                    If x1 > maxxCoord Then maxxCoord = x1
                    If z0 < minzCoord Then minzCoord = z0
                    If z1 < minzCoord Then minzCoord = z1
                    If z0 > maxzCoord Then maxzCoord = z0
                    If z1 > maxzCoord Then maxzCoord = z1
                Next
            End If
            'if there's no fence, pick something big for the floor and ceiling
            If world.fence.numBorderDefs = 0 Then
                minxCoord = -1000
                minzCoord = -1000
                maxxCoord = 1000
                maxzCoord = 1000
            End If

            'VRML 1.0 floor and ceiling geometry
            floorCeilIntro = "Info { string ""FloorAndCeiling"" }" & NL &
                "Separator {" & NL &
                "   MatrixTransform { " & NL &
                "      matrix 1 0 0 0" & NL &
                "             0 1 0 0" & NL &
                "             0 0 1 0" & NL &
                "             0 0 0 1" & NL &
                "   }" & NL
            'create the corner points for floor and ceiling rectangles, extended to ensure no gaps
            floorCeilCoords = "Coordinate3 { point [ " & NL &
             maxxCoord + XZOV & " " & minyCoord & " " & maxzCoord + XZOV & ", " & NL &
             maxxCoord + XZOV & " " & minyCoord & " " & minzCoord - XZOV & ", " & NL &
             minxCoord - XZOV & " " & minyCoord & " " & minzCoord - XZOV & ", " & NL &
             minxCoord - XZOV & " " & minyCoord & " " & maxzCoord + XZOV & ", " & NL &
             maxxCoord + XZOV & " " & maxyCoord & " " & maxzCoord + XZOV & ", " & NL &
             minxCoord - XZOV & " " & maxyCoord & " " & maxzCoord + XZOV & ", " & NL &
             minxCoord - XZOV & " " & maxyCoord & " " & minzCoord - XZOV & ", " & NL &
             maxxCoord + XZOV & " " & maxyCoord & " " & minzCoord - XZOV & NL &
            "] }" & NL
            'indexed face set for floor and ceiling collision proxy rectangles
            floorCeilIFS = "IndexedFaceSet { coordIndex [ 0, 1, 2, 3, -1, 4, 5, 6, 7, -1 ] }" & NL
            'assemble the floor and ceiling parts and send to output
            floorCeilGeom = floorCeilIntro & floorCeilCoords & floorCeilIFS & "}" & NL
            out &= floorCeilGeom

            'VRML 1.0 border geometry
            borderIntro = "Info { string ""Border"" }" & NL &
                "Separator {" & NL &
                "   MatrixTransform { " & NL &
                "      matrix 1 0 0 0" & NL &
                "             0 1 0 0" & NL &
                "             0 0 1 0" & NL &
                "             0 0 0 1" & NL &
                "   }" & NL

            'create the corner points for border wall sections (if there are any), extended up and down to ensure no gaps with the floor and ceiling
            If world.fence.numBorderDefs > 0 Then
                borderCoords = "Coordinate3 { point [ " & NL
                borderIFS = "IndexedFaceSet { coordIndex [ "
                For i = 0 To world.fence.numBorderDefs - 1
                    x0 = world.fence.borderDefs(i).xCoord0
                    z0 = world.fence.borderDefs(i).zCoord0
                    x1 = world.fence.borderDefs(i).xCoord1
                    z1 = world.fence.borderDefs(i).zCoord1
                    borderCoords = borderCoords &
                    x0 & " " & minyCoord - XZOV & " " & z0 & ", " & NL &
                    x1 & " " & minyCoord - XZOV & " " & z1 & ", " & NL &
                    x1 & " " & maxyCoord + XZOV & " " & z1 & ", " & NL &
                    x0 & " " & maxyCoord + XZOV & " " & z0 & ", " & NL
                    borderIFS = borderIFS & i * 4 & ", " & i * 4 + 1 & ", " & i * 4 + 2 & ", " & i * 4 + 3 & ", -1," & NL
                Next
                borderCoords = borderCoords & " ] }" & NL
                borderIFS = borderIFS & " ] }" & NL
            End If
            'assemble the border parts and send to output
            borderGeom = borderIntro & borderCoords & borderIFS & "}" & NL
            out &= borderGeom
            out &= NL

            'output lights
            out &= NL & "Separator { " 'start of VCLight Separator
            For n = 0 To world.nodes.Length - 1
                If world.nodes(n).numLights <> 0 Then
                    For i = 0 To world.nodes(n).numLights - 1
                        If world.nodes(n).lights(i).lightType = 1 Then
                            out &= "PointLight {" & NL
                            out &= "  on           TRUE" & NL 'VRML 1.0 default
                            out &= "  intensity    1" & NL 'VRML 1.0 default
                            out &= "  color        " & world.nodes(n).lights(i).b4 / 255.0 & " " & world.nodes(n).lights(i).b5 / 255.0 & " " & world.nodes(n).lights(i).b6 / 255.0 & NL
                            out &= "  location     " & world.nodes(n).lights(i).worldPosX & " " & world.nodes(n).lights(i).worldPosY & " " & sz * world.nodes(n).lights(i).worldPosZ & NL
                            out &= "}" & NL
                        ElseIf world.nodes(n).lights(i).lightType = 2 Then
                            out &= "SpotLight {" & NL
                            out &= "  on           TRUE" & NL 'VRML 1.0 default
                            out &= "  intensity    1" & NL 'VRML 1.0 default
                            out &= "  color        " & world.nodes(n).lights(i).b4 / 255.0 & " " & world.nodes(n).lights(i).b5 / 255.0 & " " & world.nodes(n).lights(i).b6 / 255.0 & NL
                            out &= "  location     " & world.nodes(n).lights(i).worldPosX & " " & world.nodes(n).lights(i).worldPosY & " " & sz * world.nodes(n).lights(i).worldPosZ & NL
                            out &= "  direction    " & world.nodes(n).lights(i).worldDirX & " " & world.nodes(n).lights(i).worldDirY & " " & sz * world.nodes(n).lights(i).worldDirZ & NL
                            out &= "  dropOffRate  0" & NL 'VRML 1.0 default
                            out &= "  cutOffAngle  0.785398" & NL 'VRML 1.0 default
                            out &= "}" & NL
                        ElseIf world.nodes(n).lights(i).lightType = 3 Then
                            out &= "DirectionalLight {" & NL
                            out &= "  on           TRUE" & NL 'VRML 1.0 default
                            out &= "  intensity    1" & NL 'VRML 1.0 default
                            out &= "  color        " & world.nodes(n).lights(i).b4 / 255.0 & " " & world.nodes(n).lights(i).b5 / 255.0 & " " & world.nodes(n).lights(i).b6 / 255.0 & NL
                            out &= "  direction    " & world.nodes(n).lights(i).worldDirX & " " & world.nodes(n).lights(i).worldDirY & " " & sz * world.nodes(n).lights(i).worldDirZ & NL
                            out &= "}" & NL
                        End If
                    Next
                End If
            Next


            out &= NL

            out &= NL & "Separator { "

            'Top level matrix for fast manual conversion between RH and LH coordinate systems, if necessary
            out &= NL & "MatrixTransform {"
            out &= NL & "          matrix  1 0 0 0"
            out &= NL & "                  0 1 0 0"
            out &= NL & "                  0 0 -1 0"
            out &= NL & "                  0 0 0 1"
            out &= NL & "}"

            'Global default, until VCLight implemented
            out &= NL & "DirectionalLight {"
            out &= NL & "          on         FALSE"
            out &= NL & "          intensity  1"
            out &= NL & "          color      1 1 1"
            out &= NL & "          direction  0 -1 0"
            out &= NL & "}"

            'Global default, until renderType implemented
            out &= NL & "MaterialBinding {"
            out &= NL & "     value PER_FACE_INDEXED"
            out &= NL & "}"

            'Global default, until renderType implemented
            out &= NL & "NormalBinding {"
            out &= NL & "     value PER_VERTEX_INDEXED"
            out &= NL & "}"
            out &= NL

            node = 0
            PrintNode(node, out, 0)

            out &= NL & "}"

            out &= NL & "}" 'end of VCLight Separator

            objWriter.Write(out)

            objWriter.Close()



        End Sub

        Public Sub WriteDebugwrl(ByVal filename As String)
            Dim out As String
            Dim i As Integer
            Dim OUTFILE As String
            '= "D:\Documents\vchat\VRML\basketballworld\basketballworld.wrl"
            OUTFILE = filename
            Dim objWriter As New System.IO.StreamWriter(OUTFILE)
            Dim node As Integer
            Dim NL As String = Environment.NewLine


            'printNormals = False

            out = ""
            out &= filename & NL

            'out = "#VRML V1.0 ascii"
            out &= "# version" & Chr(9) & world.header.version & NL
            out &= "# b0 b1 b2" & Chr(9) & world.header.b0 & Chr(9) & world.header.b1 & Chr(9) & world.header.b2 & NL
            out &= "# skyColor" & Chr(9) & (world.header.bgRed) & Chr(9) & (world.header.bgGreen) & Chr(9) & (world.header.bgBlue) & NL
            out &= "# b3" & Chr(9) & world.header.b3 & NL
            'out &= NL
            For i = 0 To world.fence.numBorderDefs
                'out &= "# Border coord" & Chr(9) & world.header.borderDefs(i).xFlags0.ToString("X2") & Chr(9) & world.header.borderDefs(i).xFlags1.ToString("X2") & Chr(9) & world.header.borderDefs(i).xFlags2.ToString("X2") & Chr(9) & world.header.borderDefs(i).xFlags3.ToString("X2") & Chr(9) & world.header.borderDefs(i).xCoord & Chr(9) & world.header.borderDefs(i).yFlags0.ToString("X2") & Chr(9) & world.header.borderDefs(i).yFlags1.ToString("X2") & Chr(9) & world.header.borderDefs(i).yFlags2.ToString("X2") & Chr(9) & world.header.borderDefs(i).yFlags3.ToString("X2") & Chr(9) & world.header.borderDefs(i).zCoord & NL
                out &= "# Border coord" & Chr(9) & world.fence.borderDefs(i).xCoord0 & Chr(9) & world.fence.borderDefs(i).zCoord0 & NL
                out &= "# Border coord" & Chr(9) & world.fence.borderDefs(i).xCoord1 & Chr(9) & world.fence.borderDefs(i).zCoord1 & NL
            Next

            out &= filename & NL

            'out &= NL & "Separator { "
            'out &= NL & "MatrixTransform {"
            'out &= NL & "          matrix -1 0 0 0"
            'out &= NL & "                  0 1 0 0"
            'out &= NL & "                  0 0 1 0"
            'out &= NL & "                  0 0 0 1"
            'out &= NL & "}"
            'out &= NL & "DirectionalLight {"
            'out &= NL & "          on         TRUE"
            'out &= NL & "          intensity  1"
            'out &= NL & "          color      1 1 1"
            'out &= NL & "          direction  0 -1 0"
            'out &= NL & "}"
            'out &= NL & "MaterialBinding {"
            'out &= NL & "     value PER_FACE_INDEXED"
            'out &= NL & "}"
            'If printNormals = False Then
            ' out &= NL & "NormalBinding {"
            ' out &= NL & "     value PER_VERTEX_INDEXED"
            ' out &= NL & "}"
            ' out &= NL
            ' Else
            ' out &= NL & "NormalBinding {"
            ' out &= NL & "     value PER_FACE"
            ' out &= NL & "}"
            ' out &= NL
            ' End If

            node = 0
            PrintFullDebugNode(node, out, 0)

            out &= NL

            objWriter.Write(out)

            objWriter.Close()



        End Sub

        Public Sub WriteX3d(ByVal filename As String, ByVal title As String)
            Dim out As String
            Dim OUTFILE As String
            '= "D:\Documents\vchat\VRML\basketballworld\basketballworld.wrl"
            OUTFILE = filename
            Dim objWriter As New System.IO.StreamWriter(OUTFILE)
            Dim node As Integer
            Dim NL As String = Environment.NewLine
            Dim thisDate As Date
            Dim cycleTime As Double

            Dim colProxyPoints As String = ""
            Dim floorCeilGeom As String = ""
            Dim floorCeilIntro As String = ""
            Dim floorCeilCoords As String = ""
            Dim floorCeilCoordIndex As String = ""
            Dim borderGeom As String = ""
            Dim borderIntro As String = ""
            Dim borderCoords As String = ""
            Dim borderCoordIndex As String = ""
            Const XZOV As Double = 1.0
            Dim minxCoord, minyCoord, minzCoord, maxxCoord, maxyCoord, maxzCoord, tempSwap As Double
            Dim x0, z0, x1, z1 As Double
            minxCoord = 0.0
            minyCoord = 0.0
            minzCoord = 0.0
            maxxCoord = 0.0
            maxyCoord = 0.0
            maxzCoord = 0.0

            Dim lightPosition0, lightPosition1 As SFVec3f
            Dim lightDirection0, lightDirection1 As SFVec3f

            Dim lightsourceMatrix As SFMatrix4f

            thisDate = Today
            node = 0

            out = "<?xml version=""1.0"" encoding=""UTF-8""?>" & NL
            out &= "<!DOCTYPE X3D PUBLIC ""ISO//Web3D//DTD X3D 3.3//EN"" ""http://www.web3d.org/specifications/x3d-3.3.dtd"">" & NL
            out &= "<X3D profile='Full' version='3.3'  xmlns:xsd='http://www.w3.org/2001/XMLSchema-instance' xsd:noNamespaceSchemaLocation =' https://www.web3d.org/specifications/x3d-3.3.xsd '>" & NL
            out &= "<head>" & NL
            out &= "   <meta name='title' content='" & filename & "'/>" & NL
            out &= "   <meta name='creator' content='Phil Richards'/>" & NL
            out &= "   <meta name='description' content='A conversion of the Microsoft V-Chat " & title & " 3D chat space.'/>" & NL
            out &= "   <meta name='generator' content='WDBtoX3D, https://www.flatplaces.net'/>" & NL
            out &= "   <meta name='created' content='" & thisDate.ToString("d MMM yyyy") & "'/>" & NL
            out &= "   <meta name='modified' content='" & thisDate.ToString("d MMM yyyy") & "'/>" & NL
            out &= "   <meta name='reference' content='www.flatplaces.net'/>" & NL
            out &= "   <meta name='license' content='http://www.gnu.org/licenses/gpl-3.0.html'/>" & NL
            out &= "</head>" & NL
            out &= "<Scene>" & NL
            out &= "   <!--NavigationInfo id= ""head"" headlight='true' speed='8' type='""NONE""'> </NavigationInfo-->" & NL
            out &= "   <Background skyColor ='" & (world.header.bgRed / 255.0) & " " & (world.header.bgGreen / 255.0) & " " & (world.header.bgBlue / 255.0) & " '/>" & NL
            out &= "   <!--Fog color='0.866667 0.866667 0.866667' fogType='LINEAR' visibilityRange='1000'/-->" & NL
            out &= NL
            out &= "<!--" & NL
            out &= "   <Sound direction ='0 0 -1' maxBack='1e+006' maxFront='1e+006' minBack='1e+006' minFront='1e+006' spatialize='false' >" & NL
            out &= "       <AudioClip url='""song.wav"", ""song.mp3""' loop='true' enabled='true'/>" & NL
            out &= "   </Sound>" & NL
            out &= "   <Sound direction ='0 0 -1' maxBack='1e+006' maxFront='1e+006' minBack='1e+006' minFront='1e+006' spatialize='false' >" & NL
            out &= "       <AudioClip url='""song2.wav"", song2.mp3""' loop='true' enabled='true'/>" & NL
            out &= "   </Sound>" & NL
            out &= " -->" & NL

            out &= "<WorldInfo containerField='children' info='""Flip from LH coordinate system To RH""'/>" & NL
            out &= "<Group containerField='children'>" & NL        'open Group for handedness change
            out &= "<Transform scale='1 1 -1'>" & NL               'open Transform for handedness change

            'out &= "<!--world.isLit " & world.isLit & "-->" & NL
            'out &= "<!--world.isAmbientLit " & world.isAmbientLit & "-->" & NL


            'output lights
            'out &= NL & "Separator { " 'start of VCLight Separator
            For n = 0 To world.nodes.Length - 1
                If world.nodes(n).numLights <> 0 Then
                    For i = 0 To world.nodes(n).numLights - 1

                        lightsourceMatrix = world.nodes(n).cumTransform
                        lightPosition0 = New SFVec3f(0, 0, 0)
                        lightDirection0 = New SFVec3f(0, 0, 1)
                        lightPosition1 = lightsourceMatrix.MultMatrixPnt(lightPosition0)
                        lightDirection1 = lightsourceMatrix.MultMatrixVec(lightDirection0)

                        'out &= "<!--Node " & n & " Pos:" & lightPosition1.X & " " & lightPosition1.Y & " " & lightPosition1.Z & "-->" & NL
                        'out &= "<!--Node " & n & " Dir:" & lightDirection1.X & " " & lightDirection1.Y & " " & lightDirection1.Z & "-->" & NL

                        If world.nodes(n).lights(i).lightType = 0 Then                                   'intensity zero i.e. Ambient only
                            out &= "<DirectionalLight "
                            out &= " on= 'true' "
                            'out &= " ambientIntensity= '" & world.nodes(n).lights(i).b3 / 255.0 & "' "
                            out &= " ambientIntensity= '1' "
                            out &= " intensity= '0' "
                            'out &= " color= '" & world.nodes(n).lights(i).b4 / 255.0 & " " & world.nodes(n).lights(i).b5 / 255.0 & " " & world.nodes(n).lights(i).b6 / 255.0 & "' "
                            out &= " color= '1 1 1' "

                            out &= " location= '0 0 0' "
                            out &= " />" & NL
                        ElseIf world.nodes(n).lights(i).lightType = 1 Then
                            out &= "<PointLight "
                            out &= " on= 'true' "
                            out &= " ambientIntensity= '" & world.nodes(n).lights(i).b3 / 255.0 & "' "
                            out &= " intensity= '" & world.nodes(n).lights(i).b2 / 255.0 & "' "
                            out &= " color= '" & world.nodes(n).lights(i).b4 / 255.0 & " " & world.nodes(n).lights(i).b5 / 255.0 & " " & world.nodes(n).lights(i).b6 / 255.0 & "' "
                            out &= " location= '" & lightPosition1.X & " " & lightPosition1.Y & " " & lightPosition1.Z & "' "
                            out &= " />" & NL
                        ElseIf world.nodes(n).lights(i).lightType = 2 Then
                            out &= "<SpotLight "
                            out &= " on= 'true' "
                            out &= " intensity= '" & world.nodes(n).lights(i).b2 / 255.0 & "' "
                            out &= " color= '" & world.nodes(n).lights(i).b4 / 255.0 & " " & world.nodes(n).lights(i).b5 / 255.0 & " " & world.nodes(n).lights(i).b6 / 255.0 & "' "
                            out &= " location= '" & lightPosition1.X & " " & lightPosition1.Y & " " & lightPosition1.Z & "' "
                            out &= " direction= '" & lightDirection1.X & " " & lightDirection1.Y & " " & lightDirection1.Z & "' "
                            out &= " dropOffRate= '0' "
                            out &= " cutOffAngle= '0.785398' "
                            out &= "/>" & NL
                        ElseIf world.nodes(n).lights(i).lightType = 3 Then
                            out &= "<DirectionalLight "
                            out &= " on= 'true' "
                            out &= " ambientIntensity= '0' "
                            out &= " intensity= '" & world.nodes(n).lights(i).b2 / 255.0 & "' "
                            out &= " color= '" & world.nodes(n).lights(i).b4 / 255.0 & " " & world.nodes(n).lights(i).b5 / 255.0 & " " & world.nodes(n).lights(i).b6 / 255.0 & "' "
                            out &= " direction= '" & lightDirection1.X & " " & lightDirection1.Y & " " & lightDirection1.Z & "' "
                            out &= "/>" & NL
                        ElseIf world.nodes(n).lights(i).lightType = 4 Then
                            out &= "<DirectionalLight "
                            out &= " on= 'true' "
                            out &= " ambientIntensity= '0' "
                            out &= " intensity= '" & world.nodes(n).lights(i).b2 / 255.0 & "' "
                            out &= " color= '" & world.nodes(n).lights(i).b4 / 255.0 & " " & world.nodes(n).lights(i).b5 / 255.0 & " " & world.nodes(n).lights(i).b6 / 255.0 & "' "
                            out &= " direction= '" & lightDirection1.X & " " & lightDirection1.Y & " " & lightDirection1.Z & "' "
                            out &= "/>" & NL
                        End If
                    Next
                End If
            Next

            'collision and collision proxy
            maxyCoord = world.fence.yMax
            minyCoord = world.fence.yMin
            'make sure miny and maxy are the right way round
            If minyCoord > maxyCoord Then
                tempSwap = minyCoord
                minyCoord = maxyCoord
                maxyCoord = tempSwap
            End If
            'find the X,Z rectangle enclosing the fence
            If world.fence.numBorderDefs > 0 Then
                For i = 0 To world.fence.numBorderDefs - 1
                    x0 = world.fence.borderDefs(i).xCoord0
                    z0 = world.fence.borderDefs(i).zCoord0
                    x1 = world.fence.borderDefs(i).xCoord1
                    z1 = world.fence.borderDefs(i).zCoord1
                    If x0 < minxCoord Then minxCoord = x0
                    If x1 < minxCoord Then minxCoord = x1
                    If x0 > maxxCoord Then maxxCoord = x0
                    If x1 > maxxCoord Then maxxCoord = x1
                    If z0 < minzCoord Then minzCoord = z0
                    If z1 < minzCoord Then minzCoord = z1
                    If z0 > maxzCoord Then maxzCoord = z0
                    If z1 > maxzCoord Then maxzCoord = z1
                Next
            End If
            'if there's no fence, pick something big for the floor and ceiling
            If world.fence.numBorderDefs = 0 Then
                minxCoord = -10000
                minzCoord = -10000
                maxxCoord = 10000
                maxzCoord = 10000
            End If

            'X3D floor and ceiling geometry
            'create the corner points for floor and ceiling rectangles, extended to ensure no gaps
            floorCeilCoords =
             maxxCoord + XZOV & " " & minyCoord & " " & (maxzCoord + XZOV) & " " &
             maxxCoord + XZOV & " " & minyCoord & " " & (minzCoord - XZOV) & " " &
             minxCoord - XZOV & " " & minyCoord & " " & (minzCoord - XZOV) & " " &
             minxCoord - XZOV & " " & minyCoord & " " & (maxzCoord + XZOV) & " " &
             maxxCoord + XZOV & " " & maxyCoord & " " & (maxzCoord + XZOV) & " " &
             minxCoord - XZOV & " " & maxyCoord & " " & (maxzCoord + XZOV) & " " &
             minxCoord - XZOV & " " & maxyCoord & " " & (minzCoord - XZOV) & " " &
             maxxCoord + XZOV & " " & maxyCoord & " " & (minzCoord - XZOV)

            'create the corner points for border wall sections (if there are any), extended up and down to ensure no gaps with the floor and ceiling
            If world.fence.numBorderDefs > 0 Then
                borderCoords = ""
                borderCoordIndex = ""
                For i = 0 To world.fence.numBorderDefs - 1
                    x0 = world.fence.borderDefs(i).xCoord0
                    z0 = world.fence.borderDefs(i).zCoord0
                    x1 = world.fence.borderDefs(i).xCoord1
                    z1 = world.fence.borderDefs(i).zCoord1

                    borderCoords &=
                    x0 & " " & minyCoord - XZOV & " " & z0 & " " &
                    x1 & " " & minyCoord - XZOV & " " & z1 & " " &
                    x1 & " " & maxyCoord + XZOV & " " & z1 & " " &
                    x0 & " " & maxyCoord + XZOV & " " & z0 & " "

                    borderCoordIndex &= i * 4 & " " & i * 4 + 1 & " " & i * 4 + 2 & " " & i * 4 + 3 & " -1 "
                Next
            End If

            out &= "<Group>" & NL
            out &= "  <Transform>" & NL
            out &= "    <Collision>" & NL
            out &= "	  <WorldInfo containerField ='proxy' info='""FloorAndCeiling""'/>" & NL
            out &= "      <Group containerField='proxy'>" & NL
            out &= "        <Transform>" & NL
            out &= "         <Shape DEF='proxyfloorceil'>" & NL
            out &= "           <Appearance>" & NL
            out &= "             <Material ambientIntensity ='1' diffuseColor='1 1 1' emissiveColor='1 1 1' transparency='1'/>" & NL
            out &= "           </Appearance>" & NL
            out &= "           <IndexedFaceSet DEF='proxyfloorceilifs' creaseAngle ='0.5' ccw='false' solid='true' normalIndex='-1' coordIndex='0 1 2 3 -1 4 5 6 7 -1'>" & NL
            out &= "             <Coordinate DEF='proxyfloorceilcoords' point='" & floorCeilCoords & "'/>" & NL
            out &= "           </IndexedFaceSet>" & NL
            out &= "         </Shape>" & NL
            out &= "       </Transform>" & NL
            out &= "     </Group>" & NL
            out &= "	  <WorldInfo containerField ='proxy' info='""Border""'/>" & NL
            out &= "      <Group containerField='proxy'>" & NL
            out &= "        <Transform>" & NL
            If world.fence.numBorderDefs > 0 Then
                out &= "          <Shape DEF='proxyborder'>" & NL
                out &= "           <Appearance>" & NL
                out &= "             <Material ambientIntensity ='1' diffuseColor='1 1 1' emissiveColor='1 1 1' transparency='1'/>" & NL
                out &= "           </Appearance>" & NL
                out &= "           <IndexedFaceSet DEF='proxyborderifs' creaseAngle ='0.5' ccw='false' solid='true' normalIndex='-1' coordIndex='" & borderCoordIndex & "'>" & NL
                out &= "             <Coordinate DEF='proxybordercoords' point='" & borderCoords & "'/>" & NL
                out &= "           </IndexedFaceSet>" & NL
                out &= "          </Shape>" & NL
            End If
            out &= "        </Transform>" & NL
            out &= "      </Group>" & NL
            'Main Part of Model

            PrintX3DNode(node, out, 0)

            out &= "    </Collision>" & NL
            out &= "  </Transform>" & NL
            out &= "</Group>" & NL

            out &= "</Transform>" & NL        'close Transform for handedness change
            out &= "</Group>" & NL            'close Group for handedness change

            If world.numTexAnims > 0 Then
                For i = 0 To world.numTexAnims - 1
                    out &= "<TimeSensor DEF='ts" & world.texAnims(i).name & "' cycleInterval='" & (world.texAnims(i).numFrames * world.texAnims(i).frameDuration / 100) & "' loop='true'></TimeSensor>" & NL
                    out &= "<PositionInterpolator DEF='ci" & world.texAnims(i).name & "' key='0"
                    For j = 1 To world.texAnims(i).numFrames - 1
                        out &= " " & j / world.texAnims(i).numFrames & " " & j / world.texAnims(i).numFrames
                    Next
                    out &= " 1' keyValue='0 0 0 0 0 0"
                    For j = 1 To world.texAnims(i).numFrames - 1
                        out &= " 0 " & j & " 0 0 " & j & " 0"
                    Next
                    out &= "'></PositionInterpolator>" & NL
                    out &= "<ROUTE fromNode='ts" & world.texAnims(i).name & "' fromField='fraction_changed' toNode='ci" & world.texAnims(i).name & "' toField='set_fraction'></ROUTE>" & NL
                    out &= "<ROUTE fromNode='ci" & world.texAnims(i).name & "' fromField='value_changed' toNode='tt" & world.texAnims(i).name & "' toField='set_translation'></ROUTE>" & NL

                Next
            End If

            For i = 0 To world.numNodes - 1
                If world.nodes(i).animation.speed <> 0 Then
                    If world.nodes(i).animation.speed > Math.PI Then
                        cycleTime = (1 / (20 - (10 * world.nodes(i).animation.speed / Math.PI)))
                    Else
                        cycleTime = (1 / (20 - (10 * (2 * Math.PI - world.nodes(i).animation.speed) / Math.PI)))
                    End If
                    out &= "<TimeSensor DEF='tsSpin" & i.ToString("D4") & "' cycleInterval='" & cycleTime & "' loop='true'></TimeSensor>" & NL
                    If world.nodes(i).animation.speed > Math.PI Then
                        out &= "<OrientationInterpolator DEF='oiSpin" & i.ToString("D4") & "' key='0, 0.25, 0.5, 0.75, 1' keyValue='0 -1 0 0, 0 -1 0 1.5707963267949, 0 -1 0 3.14159265358979, 0 -1 0 4.71238898038469, 0 -1 0 6.28318530717959'></OrientationInterpolator>" & NL
                    Else
                        out &= "<OrientationInterpolator DEF='oiSpin" & i.ToString("D4") & "' key='0, 0.25, 0.5, 0.75, 1' keyValue='0 1 0 0, 0 1 0 1.5707963267949, 0 1 0 3.14159265358979, 0 1 0 4.71238898038469, 0 1 0 6.28318530717959'></OrientationInterpolator>" & NL
                    End If
                    out &= "<ROUTE fromNode='tsSpin" & i.ToString("D4") & "' fromField='fraction_changed' toNode='oiSpin" & i.ToString("D4") & "' toField='set_fraction'></ROUTE>" & NL
                    out &= "<ROUTE fromNode='oiSpin" & i.ToString("D4") & "' fromField='value_changed' toNode='tSpin" & i.ToString("D4") & "' toField='rotation'></ROUTE>" & NL
                End If
            Next

            out &= "</Scene>" & NL
            out &= "</X3D>" & NL

            objWriter.Write(out)

            objWriter.Close()

        End Sub



        Public Sub PrintX3DNode(ByRef node As Integer, ByRef strOut As String, ByVal depth As Integer)
            Dim i As Integer
            Dim j As Integer
            Dim k As Integer
            Dim sz As Single
            Dim skip As Boolean = True

            Dim epsilon, epsilon2 As Single

            Dim fullmatrix, newcalc As Boolean
            Dim sourceMatrix As SFMatrix4f
            Dim translation, scaleFactor As SFVec3f
            Dim rotation, scaleOrientation As Quaternion
            Dim rotationAxis As SFVec3f
            Dim scaleOrientationAxis As SFVec3f

            Dim rotationAngle As Double
            Dim scaleOrientationAngle As Double
            Dim NL As String = Environment.NewLine

            Dim animatedTexture As Boolean = False
            Dim animatedSpin As Boolean = False

            Dim animPos As Integer
            Dim thisNode As Integer
            Dim doNormal, doColor, doMaterialNode, doMaterialContent, doAmbientIntensity, doDiffuse, doSpecular, doLineSet, doEmissive As Boolean
            Dim splitTextures As Boolean = False
            Dim texture As Integer


            fullmatrix = False
            newcalc = True

            epsilon = 0.01
            epsilon2 = 0.1

            sz = 1.0
            If _RH = True Then sz = -1.0

            Dim numChildNodes As Integer
            Dim indent As String

            If depth = 0 Then
                indent = ""
            Else
                indent = New String(" ", depth * 3)
            End If

            animPos = LCase(world.nodes(node).blockName).IndexOf("anim")
            If animPos > -1 Then
                animatedTexture = True
                world.texAnims(world.numTexAnims) = New VCTexAnimInfo With {
                                   .node = node,
                                   .name = "Anim" & node.ToString("D4") & Mid(world.nodes(node).blockName, animPos + 5, 4),
                                   .numFrames = CInt(Mid(world.nodes(node).blockName, animPos + 5, 2)),
                                   .frameDuration = CInt(Mid(world.nodes(node).blockName, animPos + 7, 2))
                                   }
                world.numTexAnims += 1
            End If

            sourceMatrix = New SFMatrix4f(world.nodes(node).transform(0), world.nodes(node).transform(4), world.nodes(node).transform(8), world.nodes(node).transform(12), world.nodes(node).transform(1), world.nodes(node).transform(5), world.nodes(node).transform(9), world.nodes(node).transform(13), world.nodes(node).transform(2), world.nodes(node).transform(6), world.nodes(node).transform(10), world.nodes(node).transform(14), world.nodes(node).transform(3), world.nodes(node).transform(7), world.nodes(node).transform(11), world.nodes(node).transform(15))

            translation = New SFVec3f()
            scaleFactor = New SFVec3f()
            rotation = New Quaternion()
            scaleOrientation = New Quaternion()
            rotationAxis = New SFVec3f()
            scaleOrientationAxis = New SFVec3f()
            rotationAngle = 0
            scaleOrientationAngle = 0

            sourceMatrix.getTransform(translation, rotation, scaleFactor, scaleOrientation)
            rotation.GetAxisAngle(rotationAxis, rotationAngle)
            scaleOrientation.GetAxisAngle(scaleOrientationAxis, scaleOrientationAngle)

            'Output stuff
            strOut &= indent & "<WorldInfo containerField ='children' info='""Node:" & node.ToString("D4") & ":" & world.nodes(node).blockName & """'/>" & NL    'could add "Node " & node & " "


            'spin animation transform goes here, if there is one
            If world.nodes(node).animation.speed <> 0 Then
                'Insert translation here if there is a spin animation
                If (translation.X <> 0.0 Or translation.Y <> 0.0 Or translation.Z <> 0.0) Then
                    strOut &= indent & "<Transform translation='" & translation.X & " " & translation.Y & " " & translation.Z & "'>" & NL
                End If
                strOut &= indent & "<Transform DEF='tSpin" & node.ToString("D4") & "' rotation='0 1 0 0'>"
                thisNode = node
            End If


            strOut &= indent & "<Group containerField='children'>" & NL

            strOut &= indent & "  <Transform "
            If (rotation.X <> 0.0 Or rotation.Y <> 0.0 Or rotation.Z <> 0.0) And rotationAngle <> 0.0 Then
                strOut &= "rotation ='" & rotation.X & " " & rotation.Y & " " & rotation.Z & " " & rotationAngle & "' "
            End If
            If scaleFactor.X <> 1.0 Or scaleFactor.Y <> 1.0 Or scaleFactor.Z <> 1.0 Then
                strOut &= "scale='" & scaleFactor.X & " " & scaleFactor.Y & " " & scaleFactor.Z & "' "
                If scaleOrientationAxis.X <> 0.0 Or scaleOrientationAxis.Y <> 0.0 Or scaleOrientationAxis.Z <> 0.0 Then
                    strOut &= "scaleOrientation='" & scaleOrientationAxis.X & " " & scaleOrientationAxis.Y & " " & scaleOrientationAxis.Z & " " & scaleOrientationAngle & "' "
                End If
            End If
            If world.nodes(node).animation.speed = 0 And (translation.X <> 0.0 Or translation.Y <> 0.0 Or translation.Z <> 0.0) Then
                strOut &= "translation='" & translation.X & " " & translation.Y & " " & translation.Z & "' "
            End If
            strOut &= ">" & NL
            'output geometry sections
            If world.nodes(node).numGeometries <> 0 Then
                'just in case there are ever more than one
                For i = 0 To world.nodes(node).numGeometries - 1
                    texture = 0

                    If world.nodes(node).geometries(i).numTextures > 1 Then
                        splitTextures = True
                    End If

                    Do
                        strOut &= indent & "    <Shape>" & NL
                        strOut &= indent & "    <Appearance>" & NL

                        'txb4=1 may be the specular flag? so texture, flat unlit, flat lit (no spec), lit with spec
                        If world.nodes(node).geometries(i).renderType = 256 Then            'TextureImage, not lit but modulated by colour
                            If world.isLit And world.isAmbientLit Then
                                doMaterialNode = True
                                doMaterialContent = True
                                doAmbientIntensity = True
                            Else
                                doMaterialNode = False
                                doMaterialContent = False
                                doAmbientIntensity = False
                            End If
                            doLineSet = False
                            doDiffuse = False
                            doEmissive = False
                            doColor = False
                            doSpecular = False
                            doNormal = False
                        ElseIf world.nodes(node).geometries(i).renderType = 1024 Then        'Lit, use normals, maybe with specular
                            If world.isAmbientLit Then
                                doEmissive = True
                                doAmbientIntensity = False
                                doDiffuse = False
                                doNormal = False
                            Else
                                doEmissive = False
                                doAmbientIntensity = True
                                doDiffuse = True
                                doNormal = True
                            End If
                            doLineSet = False
                            doMaterialNode = True
                            doMaterialContent = True
                            doColor = True
                            If world.nodes(node).geometries(i).texDefs(0).txb4 = 1 Then     'NB only doing this for first texture if there are many
                                doSpecular = True
                            Else
                                doSpecular = False
                            End If
                        ElseIf world.nodes(node).geometries(i).renderType = 768 Then       'Flat lighting, no specular, emissive
                            doLineSet = False
                            doMaterialNode = True
                            doMaterialContent = True
                            doAmbientIntensity = True
                            doDiffuse = False
                            If world.isLit = False Then
                                doEmissive = False
                            Else
                                doEmissive = True
                            End If
                            doColor = False
                            doSpecular = False
                            doNormal = False
                        ElseIf world.nodes(node).geometries(i).renderType = 0 Then       'indexedlineset
                            doLineSet = True
                            doMaterialNode = True
                            doMaterialContent = True
                            doAmbientIntensity = True
                            doDiffuse = True
                            doEmissive = False
                            doColor = True
                            doSpecular = False
                            doNormal = False
                        End If

                        '<!--Material /-->
                        '<Material ambientIntensity='1' diffuseColor='1 1 1' emissiveColor='1 1 1' specularColor='1 1 1' shininess='0.2' transparency='0'/>
                        If world.nodes(node).geometries(i).numMaterials > 0 Then
                            'For j = 0 To world.nodes(node).geometries(i).numMaterials - 1
                            If doMaterialNode = True Then
                                strOut &= indent & " <Material "
                                If doMaterialContent = True Then
                                    If doAmbientIntensity = True Then
                                        strOut &= " ambientIntensity='" & (CDbl(world.nodes(node).geometries(i).texDefs(texture).txRed / 255.0) + CDbl(world.nodes(node).geometries(i).texDefs(texture).txGreen / 255.0) + CDbl(world.nodes(node).geometries(i).texDefs(texture).txBlue / 255.0)) / 3.0 & "' "
                                    Else
                                        strOut &= " ambientIntensity='0' "
                                    End If
                                    If doDiffuse = True Then
                                        strOut &= " diffuseColor='" & world.nodes(node).geometries(i).texDefs(texture).txRed / 255.0 & " " & world.nodes(node).geometries(i).texDefs(texture).txGreen / 255.0 & " " & world.nodes(node).geometries(i).texDefs(texture).txBlue / 255.0 & "'"
                                    End If
                                    If doEmissive = True Then
                                        strOut &= " emissiveColor='" & world.nodes(node).geometries(i).texDefs(texture).txRed / 255.0 & " " & world.nodes(node).geometries(i).texDefs(texture).txGreen / 255.0 & " " & world.nodes(node).geometries(i).texDefs(texture).txBlue / 255.0 & "'"
                                    End If
                                    'strOut &=" " & world.nodes(node).geometries(i).texDefs(texture).txRed / 255.0 & " " & world.nodes(node).geometries(i).texDefs(texture).txGreen / 255.0 & " " & world.nodes(node).geometries(i).texDefs(texture).txBlue / 255.0
                                    'strOut &= "' "
                                    If doSpecular = True Then
                                        strOut &= " specularColor ='1 1 1'"
                                        strOut &= " shininess='0.02'"
                                        strOut &= " transparency='0'"
                                    End If
                                End If
                                strOut &= " />" & NL
                            End If
                            'Next
                        End If

                        'output texture
                        If world.nodes(node).geometries(i).numTextures > 0 Then
                            'For j = 0 To world.nodes(node).geometries(i).numTexDefs - 1
                            If world.nodes(node).geometries(i).texDefs(texture).txName <> "" Then
                                strOut &= indent & "      <ImageTexture url='""" & world.nodes(node).geometries(i).texDefs(texture).txName & """'>" & NL
                                strOut &= indent & "        <TextureProperties magnificationFilter ='NEAREST_PIXEL' minificationFilter='NEAREST_PIXEL'/>" & NL
                                strOut &= indent & "      </ImageTexture>" & NL
                                If animatedTexture = True Then
                                    strOut &= indent & "      <TextureTransform DEF='tt" & world.texAnims(world.numTexAnims - 1).name & "' scale='1 " & (1 / world.texAnims(world.numTexAnims - 1).numFrames) & "'/>" & NL
                                End If
                            End If
                            'Next
                        End If

                        strOut &= indent & "    </Appearance>" & NL
                        If world.nodes(node).geometries(i).numPolygons > 0 Then
                            If doLineSet = False Then
                                strOut &= indent & "    <IndexedFaceSet"
                            Else
                                strOut &= indent & "    <IndexedLineSet"
                            End If
                            strOut &= " colorPerVertex='false'"
                            strOut &= " creaseAngle='0.5'"
                            strOut &= " ccw='false'"
                            strOut &= " solid='true'"
                            'strOut &= " solid='false'"

                            If doNormal = True Then
                                ' normalIndex ='-1'
                                If world.nodes(node).geometries(i).numNormals > 0 Then
                                    strOut &= " normalIndex ='"
                                    For j = 0 To world.nodes(node).geometries(i).numPolygons - 1
                                        If splitTextures = False Or (splitTextures = True And world.nodes(node).geometries(i).materials(j) = texture) Then
                                            If _RH = True Then
                                                For k = world.nodes(node).geometries(i).polyDefs(j).numVerts - 1 To 0 Step -1
                                                    strOut &= " " & world.nodes(node).geometries(i).polyDefs(j).normals(k)
                                                Next
                                            Else
                                                For k = 0 To world.nodes(node).geometries(i).polyDefs(j).numVerts - 1
                                                    strOut &= " " & world.nodes(node).geometries(i).polyDefs(j).normals(k)
                                                Next
                                            End If
                                            strOut &= " -1"
                                        End If
                                    Next
                                    strOut &= "' "
                                End If
                            End If

                            ' colorIndex ='0 0'
                            If doColor = True Then
                                If world.nodes(node).geometries(i).numMaterials > 0 Then
                                    'output materialIndex
                                    strOut &= " colorIndex ='"
                                    For j = 0 To world.nodes(node).geometries(i).numPolygons - 1
                                        If splitTextures = True And world.nodes(node).geometries(i).materials(j) = texture Then
                                            strOut &= " 0"
                                        Else
                                            strOut &= " " & world.nodes(node).geometries(i).materials(j)
                                        End If
                                    Next
                                    strOut &= "' "
                                End If
                            End If


                            ' texCoordIndex ='2 0 1 3 -1 0 2 3 1 -1'
                            strOut &= " texCoordIndex ='"
                            For j = 0 To world.nodes(node).geometries(i).numPolygons - 1
                                If splitTextures = False Or (splitTextures = True And world.nodes(node).geometries(i).materials(j) = texture) Then
                                    If _RH = True Then
                                        For k = world.nodes(node).geometries(i).polyDefs(j).numVerts - 1 To 0 Step -1
                                            strOut &= " " & world.nodes(node).geometries(i).polyDefs(j).vertices(k)
                                        Next
                                    Else
                                        For k = 0 To world.nodes(node).geometries(i).polyDefs(j).numVerts - 1
                                            strOut &= " " & world.nodes(node).geometries(i).polyDefs(j).vertices(k)
                                        Next
                                    End If
                                    strOut &= " -1"
                                End If
                            Next
                            strOut &= "' "

                            'output coordIndex
                            strOut &= " coordIndex='"
                            For j = 0 To world.nodes(node).geometries(i).numPolygons - 1
                                If splitTextures = False Or (splitTextures = True And world.nodes(node).geometries(i).materials(j) = texture) Then
                                    If _RH = True Then
                                        For k = world.nodes(node).geometries(i).polyDefs(j).numVerts - 1 To 0 Step -1
                                            strOut &= " " & world.nodes(node).geometries(i).polyDefs(j).vertices(k)
                                        Next
                                        If doLineSet = True Then
                                            strOut &= " " & world.nodes(node).geometries(i).polyDefs(j).vertices(world.nodes(node).geometries(i).polyDefs(j).numVerts - 1)
                                        End If
                                    Else
                                        For k = 0 To world.nodes(node).geometries(i).polyDefs(j).numVerts - 1
                                            strOut &= " " & world.nodes(node).geometries(i).polyDefs(j).vertices(k)
                                        Next
                                        If doLineSet = True Then
                                            strOut &= " " & world.nodes(node).geometries(i).polyDefs(j).vertices(0)
                                        End If
                                    End If
                                    strOut &= " -1"
                                End If
                            Next
                            strOut &= "' "
                            strOut &= ">" & NL

                            If world.nodes(node).geometries(i).numPoints > 0 Then
                                'output the point list
                                strOut &= indent & "      <Coordinate point='"
                                For j = 0 To world.nodes(node).geometries(i).numPoints - 1
                                    strOut &= " " & world.nodes(node).geometries(i).points(j).x & " " & world.nodes(node).geometries(i).points(j).y & " " & world.nodes(node).geometries(i).points(j).z
                                Next
                                strOut &= "'/>" & NL
                            End If

                            If doColor = True Then
                                '<Color color='1 1 1'/>
                                If world.nodes(node).geometries(i).numMaterials > 0 Then
                                    strOut &= indent & "      <Color color='"
                                    For j = 0 To world.nodes(node).geometries(i).numTexDefs - 1
                                        If splitTextures = False Or (splitTextures = True And world.nodes(node).geometries(i).materials(j) = texture) Then
                                            strOut &= " " & world.nodes(node).geometries(i).texDefs(j).txRed / 255.0 & " " & world.nodes(node).geometries(i).texDefs(j).txGreen / 255.0 & " " & world.nodes(node).geometries(i).texDefs(j).txBlue / 255.0
                                        End If

                                        'If j < world.nodes(node).geometries(i).numTexDefs - 1 Then
                                        'strOut &= ","
                                        'End If
                                    Next
                                    strOut &= "' />" & NL
                                End If
                            End If

                            If doNormal = True Then
                                'output the normal list
                                If world.nodes(node).geometries(i).numNormals > 0 Then
                                    strOut &= indent & "      <Normal vector='"
                                    For j = 0 To world.nodes(node).geometries(i).numNormals - 1
                                        strOut &= " " & world.nodes(node).geometries(i).normals(j).x & " " & world.nodes(node).geometries(i).normals(j).y & " " & world.nodes(node).geometries(i).normals(j).z
                                    Next
                                    strOut &= "'/>" & NL
                                End If
                            End If

                            'output texture coordinates
                            If world.nodes(node).geometries(i).numPoints > 0 Then
                                strOut &= indent & "      <TextureCoordinate point='"
                                For j = 0 To world.nodes(node).geometries(i).numPoints - 1
                                    strOut &= " " & world.nodes(node).geometries(i).texCoords(j).u & " " & world.nodes(node).geometries(i).texCoords(j).v
                                Next
                                strOut &= "'/>" & NL
                            End If
                            If doLineSet = False Then
                                strOut &= indent & "    </IndexedFaceSet>" & NL
                            Else
                                strOut &= indent & "    </IndexedLineSet>" & NL
                            End If

                        End If
                        strOut &= indent & "    </Shape>" & NL
                        texture += 1
                    Loop Until texture >= world.nodes(node).geometries(i).numTexDefs
                Next
            End If

            'Output this VCNode's child nodes, if there are any
            numChildNodes = world.nodes(node).numChildren
            If numChildNodes <> 0 And node < world.numNodes Then
                For i = 0 To numChildNodes - 1
                    node = node + 1
                    PrintX3DNode(node, strOut, depth + 1)
                Next
            End If

            'Close off this VCNode
            strOut &= indent & "  </Transform>" & NL

            strOut &= indent & "</Group>" & NL

            'spin animation transform closes here, if there is one
            If world.nodes(thisNode).animation.speed <> 0 Then
                strOut &= indent & "  </Transform>" & NL
            End If
            If world.nodes(thisNode).animation.speed <> 0 And (translation.X <> 0.0 Or translation.Y <> 0.0 Or translation.Z <> 0.0) Then
                strOut &= indent & "  </Transform>" & NL
            End If

        End Sub
    End Class
End Module