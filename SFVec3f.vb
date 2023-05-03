'Converted from X3DOM js code.

Public Class SFVec3f
    Public Property X As Double
    Public Property Y As Double
    Public Property Z As Double

    Public Sub New()
        X = 0
        Y = 0
        Z = 0
    End Sub

    Public Sub New(ByVal x As Double, ByVal y As Double, ByVal z As Double)
        Me.X = x
        Me.Y = y
        Me.Z = z
    End Sub

    Public Sub SetValues(ByRef that As SFVec3f)
        Me.X = that.X
        Me.Y = that.Y
        Me.Z = that.Z
    End Sub

    Public Function NullVector() As SFVec3f
        NullVector = New SFVec3f(0, 0, 0)
    End Function

    Public Function OneVector() As SFVec3f
        OneVector = New SFVec3f(1, 1, 1)
    End Function

    Public Function Copy(v As SFVec3f) As SFVec3f
        Copy = New SFVec3f(v.X, v.Y, v.Z)
    End Function

    Public Function Negate(v As SFVec3f) As SFVec3f
        Negate = New SFVec3f(-v.X, -v.Y, -v.Z)
    End Function

    Public Function Negate() As SFVec3f
        Negate = New SFVec3f(-X, -Y, -Z)
    End Function

    Public Function Multiply(ByVal n As Double)
        Multiply = New SFVec3f(Me.X * n, Me.Y * n, Me.Z * n)
    End Function

    Public Function Length() As Double
        Length = Math.Sqrt((Me.X * Me.X) + (Me.Y * Me.Y) + (Me.Z * Me.Z))
    End Function

End Class



