
'x    下一级运算
#macro Left_Value(x)
   Dim 左项 As 项 Ptr, 右项 As 项 Ptr,实参(1) As 类型格式 Ptr
   
   Dim 类型 As 运算符类型
   x(模式, 编译信息指针, 左项)
   项_返回 = 左项
   
   If *表达式结果 Is 表达式格式_值=False Then Return
   实参(0) = Cast(表达式格式_值 Ptr, 表达式结果)->值类型
#endmacro

'x    下一级运算
#macro Right_Value(x)
   x(模式,编译信息指针,右项)
   If *表达式结果 Is 表达式格式_值=False Then 错误信息("非值运算"):表达式结果 = 对象管理.加入(New 表达式格式_空白):Return
   实参(1) = Cast(表达式格式_值 Ptr, 表达式结果)->值类型
   
   Dim 方法引用 As 节点_方法 Ptr = 重载决议(编译信息指针->运算符方法表(), 对象管理.加入(New 决议线索_运算符(类型)), 实参())
   
   If 方法引用 = -1 Then 错误信息("找不到合适的方法") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
   If 方法引用 = 0 Then 错误信息("找到了多个能够接纳实参的方法") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return

   If 方法引用->是否函数 Then
      表达式结果=对象管理.加入(New 表达式格式_返回值(方法引用->返回值))
   Else
      表达式结果=对象管理.加入(New 表达式格式_空白)
   End If

   Dim temp As 项_调用 Ptr = 对象管理.加入(New 项_调用)
   temp->方法 = 方法引用
   ReDim temp->参数(1)
   temp->参数(0) = 左项
   temp->参数(1) = 右项
   项_返回 = temp
   左项 = 项_返回
#endmacro
