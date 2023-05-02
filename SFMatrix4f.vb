Public Class SFMatrix4f
    Public Property _00 As Double
    Public Property _01 As Double
    Public Property _02 As Double
    Public Property _03 As Double
    Public Property _10 As Double
    Public Property _11 As Double
    Public Property _12 As Double
    Public Property _13 As Double
    Public Property _20 As Double
    Public Property _21 As Double
    Public Property _22 As Double
    Public Property _23 As Double
    Public Property _30 As Double
    Public Property _31 As Double
    Public Property _32 As Double
    Public Property _33 As Double

    Public Sub New()
        _00 = 1
        _01 = 0
        _02 = 0
        _03 = 0
        _10 = 0
        _11 = 1
        _12 = 0
        _13 = 0
        _20 = 0
        _21 = 0
        _22 = 1
        _23 = 0
        _30 = 0
        _31 = 0
        _32 = 0
        _33 = 1
    End Sub

    Public Sub New(ByVal _00 As Double, ByVal _01 As Double, ByVal _02 As Double, ByVal _03 As Double, ByVal _10 As Double, ByVal _11 As Double, ByVal _12 As Double, ByVal _13 As Double, ByVal _20 As Double, ByVal _21 As Double, ByVal _22 As Double, ByVal _23 As Double, ByVal _30 As Double, ByVal _31 As Double, ByVal _32 As Double, ByVal _33 As Double)
        Me._00 = _00
        Me._01 = _01
        Me._02 = _02
        Me._03 = _03
        Me._10 = _10
        Me._11 = _11
        Me._12 = _12
        Me._13 = _13
        Me._20 = _20
        Me._21 = _21
        Me._22 = _22
        Me._23 = _23
        Me._30 = _30
        Me._31 = _31
        Me._32 = _32
        Me._33 = _33
    End Sub

    Public Function Translation(ByVal vec As SFVec3f) As SFMatrix4f
        Translation = New SFMatrix4f(1, 0, 0, vec.X, 0, 1, 0, vec.Y, 0, 0, 1, vec.Z, 0, 0, 0, 1)
    End Function

    Public Function Transpose() As SFMatrix4f
        Transpose = New SFMatrix4f(Me._00, Me._10, Me._20, Me._30, Me._01, Me._11, Me._21, Me._31, Me._02, Me._12, Me._22, Me._32, Me._03, Me._13, Me._23, Me._33)
    End Function

    Public Function Mult(ByRef that As SFMatrix4f) As SFMatrix4f
        Mult = New SFMatrix4f(Me._00 * that._00 + Me._01 * that._10 + Me._02 * that._20 + Me._03 * that._30, Me._00 * that._01 + Me._01 * that._11 + Me._02 * that._21 + Me._03 * that._31, Me._00 * that._02 + Me._01 * that._12 + Me._02 * that._22 + Me._03 * that._32, Me._00 * that._03 + Me._01 * that._13 + Me._02 * that._23 + Me._03 * that._33, Me._10 * that._00 + Me._11 * that._10 + Me._12 * that._20 + Me._13 * that._30, Me._10 * that._01 + Me._11 * that._11 + Me._12 * that._21 + Me._13 * that._31, Me._10 * that._02 + Me._11 * that._12 + Me._12 * that._22 + Me._13 * that._32, Me._10 * that._03 + Me._11 * that._13 + Me._12 * that._23 + Me._13 * that._33, Me._20 * that._00 + Me._21 * that._10 + Me._22 * that._20 + Me._23 * that._30, Me._20 * that._01 + Me._21 * that._11 + Me._22 * that._21 + Me._23 * that._31, Me._20 * that._02 + Me._21 * that._12 + Me._22 * that._22 + Me._23 * that._32, Me._20 * that._03 + Me._21 * that._13 + Me._22 * that._23 + Me._23 * that._33, Me._30 * that._00 + Me._31 * that._10 + Me._32 * that._20 + Me._33 * that._30, Me._30 * that._01 + Me._31 * that._11 + Me._32 * that._21 + Me._33 * that._31, Me._30 * that._02 + Me._31 * that._12 + Me._32 * that._22 + Me._33 * that._32, Me._30 * that._03 + Me._31 * that._13 + Me._32 * that._23 + Me._33 * that._33)
    End Function

    Public Function Copy(ByRef that As SFMatrix4f) As SFMatrix4f
        Return New SFMatrix4f(that._00, that._01, that._02, that._03, that._10, that._11, that._12, that._13, that._20, that._21, that._22, that._23, that._30, that._31, that._32, that._33)
    End Function

    Public Function Copy() As SFMatrix4f
        Return Copy(Me)
    End Function

    Public Function Identity() As SFMatrix4f
        Return New SFMatrix4f(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)
    End Function

    Public Function Negate() As SFMatrix4f
        Negate = New SFMatrix4f(-_00, -_01, -_02, -_03, -_10, -_11, -_12, -_13, -_20, -_21, -_22, -_23, -_30, -_31, -_32, -_33)
    End Function

    Public Function MultMatrixPnt(ByRef vec As SFVec3f)
        Return New SFVec3f(Me._00 * vec.X + Me._01 * vec.Y + Me._02 * vec.Z + Me._03, Me._10 * vec.X + Me._11 * vec.Y + Me._12 * vec.Z + Me._13, Me._20 * vec.X + Me._21 * vec.Y + Me._22 * vec.Z + Me._23)
    End Function

    Public Function MultMatrixVec(ByRef vec As SFVec3f)
        Return New SFVec3f(Me._00 * vec.X + Me._01 * vec.Y + Me._02 * vec.Z, Me._10 * vec.X + Me._11 * vec.Y + Me._12 * vec.Z, Me._20 * vec.X + Me._21 * vec.Y + Me._22 * vec.Z)
    End Function


    Public Sub SetValue(v1 As SFVec3f, v2 As SFVec3f, v3 As SFVec3f, v4 As SFVec3f)
        Me._00 = v1.X
        Me._01 = v2.X
        Me._02 = v3.X
        Me._10 = v1.Y
        Me._11 = v2.Y
        Me._12 = v3.Y
        Me._20 = v1.Z
        Me._21 = v2.Z
        Me._22 = v3.Z
        Me._30 = 0
        Me._31 = 0
        Me._32 = 0
        Me._03 = v4.X
        Me._13 = v4.Y
        Me._23 = v4.Z
        Me._33 = 1
    End Sub

    Public Sub SetValue(v1 As SFVec3f, v2 As SFVec3f, v3 As SFVec3f)
        Me._00 = v1.X
        Me._01 = v2.X
        Me._02 = v3.X
        Me._10 = v1.Y
        Me._11 = v2.Y
        Me._12 = v3.Y
        Me._20 = v1.Z
        Me._21 = v2.Z
        Me._22 = v3.Z
        Me._30 = 0
        Me._31 = 0
        Me._32 = 0
    End Sub

    Public Function At(ByVal i As Integer, ByVal j As Integer) As Double
        Dim field As String
        field = "_" & i.ToString & j.ToString
        Return CallByName(Me, field, CallType.Get)
    End Function

    Public Sub SetAt(ByVal i As Integer, ByVal j As Integer, d As Double)
        Dim field As String
        field = "_" & i.ToString & j.ToString
        CallByName(Me, field, CallType.Set, d)
    End Sub

    Public Function Norm1_3x3() As Double
        Dim max, t As Double

        max = Math.Abs(Me._00) + Math.Abs(Me._10) + Math.Abs(Me._20)
        t = Math.Abs(Me._01) + Math.Abs(Me._11) + Math.Abs(Me._21)
        If t > max Then
            max = t
        End If
        t = Math.Abs(Me._02) + Math.Abs(Me._12) + Math.Abs(Me._22)
        If (t > max) Then
            max = t
        End If
        Return max
    End Function

    Public Function NormInf_3x3() As Double
        Dim max, t As Double

        max = Math.Abs(Me._00) + Math.Abs(Me._01) + Math.Abs(Me._02)
        t = Math.Abs(Me._10) + Math.Abs(Me._11) + Math.Abs(Me._12)
        If (t > max) Then
            max = t
        End If
        t = Math.Abs(Me._20) + Math.Abs(Me._21) + Math.Abs(Me._22)
        If (t > max) Then
            max = t
        End If
        Return max
    End Function

    Public Function AdjointT_3x3() As SFMatrix4f
        Dim result As SFMatrix4f
        result = Identity()
        result._00 = Me._11 * Me._22 - Me._12 * Me._21
        result._01 = Me._12 * Me._20 - Me._10 * Me._22
        result._02 = Me._10 * Me._21 - Me._11 * Me._20
        result._10 = Me._21 * Me._02 - Me._22 * Me._01
        result._11 = Me._22 * Me._00 - Me._20 * Me._02
        result._12 = Me._20 * Me._01 - Me._21 * Me._00
        result._20 = Me._01 * Me._12 - Me._02 * Me._11
        result._21 = Me._02 * Me._10 - Me._00 * Me._12
        result._22 = Me._00 * Me._11 - Me._01 * Me._10
        Return result
    End Function

    Public Sub SetValues(ByRef that As SFMatrix4f)
        Me._00 = that._00
        Me._01 = that._01
        Me._02 = that._02
        Me._03 = that._03
        Me._10 = that._10
        Me._11 = that._11
        Me._12 = that._12
        Me._13 = that._13
        Me._20 = that._20
        Me._21 = that._21
        Me._22 = that._22
        Me._23 = that._23
        Me._30 = that._30
        Me._31 = that._31
        Me._32 = that._32
        Me._33 = that._33
    End Sub

    Public Function Multiply(ByVal s As Double) As SFMatrix4f
        Multiply = New SFMatrix4f(s * Me._00, s * Me._01, s * Me._02, s * Me._03, s * Me._10, s * Me._11, s * Me._12, s * Me._13, s * Me._20, s * Me._21, s * Me._22, s * Me._23, s * Me._30, s * Me._31, s * Me._32, s * Me._33)
    End Function

    Public Function AddScaled(ByRef that As SFMatrix4f, ByVal s As Double) As SFMatrix4f
        AddScaled = New SFMatrix4f(Me._00 + s * that._00, Me._01 + s * that._01, Me._02 + s * that._02, Me._03 + s * that._03, Me._10 + s * that._10, Me._11 + s * that._11, Me._12 + s * that._12, Me._13 + s * that._13, Me._20 + s * that._20, Me._21 + s * that._21, Me._22 + s * that._22, Me._23 + s * that._23, Me._30 + s * that._30, Me._31 + s * that._31, Me._32 + s * that._32, Me._33 + s * that._33)
    End Function

    Public Sub getTransform(ByRef _translation As SFVec3f, ByRef _rotation As Quaternion, ByRef _scaleFactor As SFVec3f, ByRef _scaleOrientation As Quaternion, ByRef _center As SFVec3f)
        Dim m As SFMatrix4f
        Dim c As SFMatrix4f
        Dim flip As Double

        m = Translation(_center.Negate())
        m = m.Mult(Me)
        c = Translation(_center)
        m = m.Mult(c)

        flip = m.Decompose(_translation, _rotation, _scaleFactor, _scaleOrientation)
        _scaleFactor.SetValues(_scaleFactor.Multiply(flip))
    End Sub

    Public Sub getTransform(ByRef _translation As SFVec3f, ByRef _rotation As Quaternion, ByRef _scaleFactor As SFVec3f, ByRef _scaleOrientation As Quaternion)
        Dim m As SFMatrix4f
        Dim flip As Double

        m = Copy(Me)

        flip = m.Decompose(_translation, _rotation, _scaleFactor, _scaleOrientation)
        _scaleFactor.SetValues(_scaleFactor.Multiply(flip))
    End Sub

    Public Function Decompose(ByRef _t As SFVec3f, ByRef _r As Quaternion, ByRef _s As SFVec3f, ByRef _so As Quaternion) As Double
        Dim A, Q, S, SO As SFMatrix4f
        Dim det, f As Double

        A = Copy(Me)
        Q = Identity()
        S = Identity()
        SO = Identity()
        _t.X = A._03
        _t.Y = A._13
        _t.Z = A._23
        A._03 = 0.0
        A._13 = 0.0
        A._23 = 0.0
        A._30 = 0.0
        A._31 = 0.0
        A._32 = 0.0
        det = A.PolarDecompose(Q, S)
        f = 1.0
        If (det < 0.0) Then
            Q = Q.Negate()
            f = -1.0
        End If
        _r.SetValue(Q)
        S.SpectralDecompose(SO, _s)
        _so.SetValue(SO)
        Return f
    End Function

    Public Function PolarDecompose(ByRef Q As SFMatrix4f, ByRef S As SFMatrix4f) As Double
        Dim TOL As Double
        Dim Mk, Ek As SFMatrix4f
        Dim MkAdjT As SFMatrix4f
        Dim MkAdjT_one, MkAdjT_inf As Double
        Dim Ek_one As Double
        Dim Mk_one, Mk_inf, Mk_det As Double
        Dim gamma, g1, g2 As Double
        'Dim i, j As Integer

        TOL = 0.000000000001
        Mk = Me.Transpose()
        Ek = Identity()
        Mk_one = Mk.Norm1_3x3()
        Mk_inf = Mk.NormInf_3x3()

        Do
            MkAdjT = Mk.AdjointT_3x3()
            Mk_det = Mk._00 * MkAdjT._00 + Mk._01 * MkAdjT._01 + Mk._02 * MkAdjT._02
            If (Mk_det = 0.0) Then
                'x3dom.debug.logWarning("polarDecompose: Mk_det == 0.0")
                Exit Do
            End If
            MkAdjT_one = MkAdjT.Norm1_3x3()
            MkAdjT_inf = MkAdjT.NormInf_3x3()
            gamma = Math.Sqrt(Math.Sqrt((MkAdjT_one * MkAdjT_inf) / (Mk_one * Mk_inf)) / Math.Abs(Mk_det))
            g1 = 0.5 * gamma
            g2 = 0.5 / (gamma * Mk_det)
            Ek.SetValues(Mk)
            Mk = Mk.Multiply(g1)
            Mk = Mk.AddScaled(MkAdjT, g2)
            Ek = Ek.AddScaled(Mk, -1.0)
            Ek_one = Ek.Norm1_3x3()
            Mk_one = Mk.Norm1_3x3()
            Mk_inf = Mk.NormInf_3x3()
        Loop While (Ek_one > (Mk_one * TOL))

        Q.SetValues(Mk.Transpose())
        S.SetValues(Mk.Mult(Me))
        'S._00 = 0.5 * (S._00 + S._00)
        'S._00 = 0.5 * (S._00 + S._00)
        S._10 = 0.5 * (S._10 + S._01)
        S._01 = 0.5 * (S._10 + S._01)
        S._20 = 0.5 * (S._20 + S._02)
        S._02 = 0.5 * (S._20 + S._02)
        'S._11 = 0.5 * (S._11 + S._11)
        'S._11 = 0.5 * (S._11 + S._11)
        S._21 = 0.5 * (S._21 + S._12)
        S._12 = 0.5 * (S._21 + S._12)
        'S._22 = 0.5 * (S._22 + S._22)
        'S._22 = 0.5 * (S._22 + S._22)

        'For (var i= 0; i<3; ++i){
        '   For (var j= i; j<3; ++j){
        '	    S['_'+j+i]=0.5*(S['_'+j+i]+S['_'+i+j]);
        '		S['_'+i+j]=0.5*(S['_'+j+i]+S['_'+i+j]);
        '	}
        '}

        Return Mk_det
    End Function


    Public Sub SpectralDecompose(ByRef SO As SFMatrix4f, ByRef k As SFVec3f)
        Dim nxt = New Integer() {1, 2, 0}
        Dim maxIterations As Integer = 20
        Dim diag = New Double() {Me._00, Me._11, Me._22}
        Dim offDiag = New Double() {Me._12, Me._20, Me._01}
        Dim sm As Double
        Dim p, q As Integer
        Dim g, absOffDiag As Double
        Dim t, h, absh As Double
        Dim theta As Double
        Dim c, s, tau, ta As Double
        Dim offDiagq As Double
        Dim a, b As Double

        For iter As Integer = 0 To iter < maxIterations - 1
            sm = Math.Abs(offDiag(0)) + Math.Abs(offDiag(1)) + Math.Abs(offDiag(2))
            If (sm = 0) Then
                Exit For
            End If
            For i As Integer = 2 To 0 Step -1
                p = nxt(i)
                q = nxt(p)
                absOffDiag = Math.Abs(offDiag(i))
                g = 100.0 * absOffDiag
                If (absOffDiag > 0.0) Then
                    t = 0
                    h = diag(q) - diag(p)
                    absh = Math.Abs(h)
                    If absh + g = absh Then
                        t = offDiag(i) / h
                    Else
                        theta = 0.5 * h / offDiag(i)
                        t = 1.0 / (Math.Abs(theta) + Math.Sqrt(theta * theta + 1.0))
                        If theta < 0.0 Then
                            t = -t
                        End If
                    End If
                    c = 1.0 / Math.Sqrt(t * t + 1.0)
                    s = t * c
                    tau = s / (c + 1.0)
                    ta = t * offDiag(i)
                    offDiag(i) = 0.0
                    diag(p) -= ta
                    diag(q) += ta
                    offDiagq = offDiag(q)
                    offDiag(q) -= s * (offDiag(p) + tau * offDiagq)
                    offDiag(p) += s * (offDiagq - tau * offDiag(p))
                    For j As Integer = 2 To 0 Step -1
                        a = SO.At(j, p)
                        b = SO.At(j, q)
                        SO.SetAt(j, p, a - s * (b + tau * a))
                        SO.SetAt(j, q, b + s * (a - tau * b))
                    Next
                    'For (var j= 2; j>=0; --j){
                    '   var a = SO['_'+j+p];
                    '   var b = SO['_'+j+q];
                    '   SO['_'+j+p]-=s*(b+tau*a);
                    '	SO['_'+j+q]+=s*(a-tau*b);
                    '}
                End If
            Next
        Next
        k.X = diag(0)
        k.Y = diag(1)
        k.Z = diag(2)
    End Sub

End Class

