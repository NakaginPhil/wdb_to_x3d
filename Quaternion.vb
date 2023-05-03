'Converted from X3DOM js code.

Public Class Quaternion

    Public Property X As Double
    Public Property Y As Double
    Public Property Z As Double
    Public Property W As Double

    Public Sub New()
        X = 0
        Y = 0
        Z = 0
        W = 1
    End Sub

    Public Sub New(ByVal x As Double, ByVal y As Double, ByVal z As Double, ByVal w As Double)
        Me.X = x
        Me.Y = y
        Me.Z = z
        Me.W = w
    End Sub

    Public Sub SetValue(ByRef matrix As SFMatrix4f)
        Dim tr, s As Double
        Dim epsilon As Double
        Dim qt = New Double() {0, 0, 0}
        Dim i, j, k As Integer
        epsilon = 0.000000001
        s = 1
        i = 0
        j = 0
        k = 0
        Dim nxt = New Integer() {1, 2, 0}
        Dim errThreshold As Double

        tr = matrix._00 + matrix._11 + matrix._22
        If (tr > 0.0) Then
            s = Math.Sqrt(tr + 1.0)
            Me.W = s * 0.5
            s = 0.5 / s
            Me.X = (matrix._21 - matrix._12) * s
            Me.Y = (matrix._02 - matrix._20) * s
            Me.Z = (matrix._10 - matrix._01) * s
        Else
            If (matrix._11 > matrix._00) Then
                i = 1
            Else
                i = 0
            End If
            If (matrix._22 > matrix.At(i, i)) Then
                i = 2
            End If
            j = nxt(i)
            k = nxt(j)
            s = Math.Sqrt(matrix.At(i, i) - (matrix.At(j, j) + matrix.At(k, k)) + 1.0)
            qt(i) = s * 0.5
            s = 0.5 / s
            Me.W = (matrix.At(k, j) - matrix.At(j, k)) * s
            qt(j) = (matrix.At(j, i) + matrix.At(i, j)) * s
            qt(k) = (matrix.At(k, i) + matrix.At(i, k)) * s
            Me.X = qt(0)
            Me.Y = qt(1)
            Me.Z = qt(2)
        End If
        If (Me.W > 1.0 Or Me.W < -1.0) Then
            errThreshold = 1 + (epsilon * 100)
            If (Me.W > errThreshold Or Me.W < -errThreshold) Then
                'x3dom.debug.logInfo("MatToQuat: BUG: |quat[4]| (" + this.w + ") >> 1.0 !")
            End If
            If (Me.W > 1.0) Then
                Me.W = 1.0
            Else
                Me.W = -1.0
            End If
        End If
    End Sub


    Public Function AxisAngle(ByRef axis As SFVec3f, ByVal a As Double)
        Dim t, s, c As Double
        Dim Eps As Double = 0.00000001
        t = axis.Length()
        If (t > Eps) Then
            s = Math.Sin(a / 2) / t
            c = Math.Cos(a / 2)
            Return New Quaternion(axis.X * s, axis.Y * s, axis.Z * s, c)
        Else
            Return New Quaternion(0, 0, 0, 1)
        End If
    End Function

    Public Function Dot(ByRef that As Quaternion) As Double
        Return Me.X * that.X + Me.Y * that.Y + Me.Z * that.Z + Me.W * that.W
    End Function


    Public Function Normalize() As Quaternion
        Dim d2, id As Double
        d2 = Me.Dot(Me)
        id = 1.0
        If d2 > 0 Then
            id = 1.0 / Math.Sqrt(d2)
        End If
        Me.X *= id
        Me.Y *= id
        Me.Z *= id
        Me.W *= id
        Return Me
    End Function


    Public Sub GetAxisAngle(ByRef axis As SFVec3f, ByRef angle As Double)
        Dim X, Y, Z As Double
        Dim s, a As Double
        X = 0
        Y = 0
        Z = 0
        s = 0
        a = 0
        If (Me.W > 1) Then
            Me.Normalize()
        End If
        a = 2 * Math.Acos(Me.W)
        s = Math.Sqrt(1 - Me.W * Me.W)
        If (s = 0) Then
            X = Me.X
            Y = Me.Y
            Z = Me.Z
        Else
            X = Me.X / s
            Y = Me.Y / s
            Z = Me.Z / s
        End If
        axis.X = X
        axis.Y = Y
        axis.Z = Z
        angle = a
    End Sub

End Class
