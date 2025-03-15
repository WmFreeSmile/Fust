Namespace 表

Type 线性表
    表(Any) As Any Ptr
    Declare Sub 加入(项 As Any Ptr)
End Type

Sub 线性表.加入(项 As Any Ptr)
    Dim 索引 As Integer
    索引 = UBound(This.表) + 1
    ReDim Preserve This.表(索引)
    This.表(索引) = 项
End Sub

Type 映射项
   起始 As Any Ptr
   目标 As Any Ptr
End Type

Type 映射表
   表(Any) As 映射项
   
   Declare Sub 加入(起始 As Any Ptr, 目标 As Any Ptr)
   Declare Function 寻找(起始 As Any Ptr) As Any Ptr
End Type

Sub 映射表.加入(起始 As Any Ptr, 目标 As Any Ptr)
   Dim 索引 As Integer
   索引 = UBound(This.表) + 1
   ReDim Preserve This.表(索引)
   This.表(索引).起始 = 起始
   This.表(索引).目标 = 目标
End Sub

Function 映射表.寻找(起始 As Any Ptr) As Any Ptr
   For i As Integer = LBound(This.表) To UBound(This.表)
      If This.表(i).起始 = 起始 Then
         Return This.表(i).目标
      End If
   Next
   Function = 0
End Function


End Namespace