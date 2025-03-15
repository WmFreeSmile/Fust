Namespace 对象管理

Dim 对象表(Any) As Any Ptr

Function 加入(临时对象 As Any Ptr) As Any Ptr
   Dim 索引 As Integer
   索引 = UBound(对象表) + 1
   ReDim Preserve 对象表(索引)
   对象表(索引) = 临时对象
   Function = 临时对象
End Function

Sub 释放()
   For i As Integer = LBound(对象表) To UBound(对象表) 
      Delete 对象表(i)
   Next
   Erase 对象表
End Sub


End Namespace
