

Sub free_Default(参数 As Any Ptr)
   
End Sub



Sub free_is_push_byte(参数 As Any Ptr)
   Delete 参数
End Sub


Sub free_is_push_short(参数 As Any Ptr)
   Delete 参数
End Sub


Sub free_is_push_integer(参数 As Any Ptr)
   Delete 参数
End Sub


Sub free_is_push_long(参数 As Any Ptr)
   Delete 参数
End Sub



Sub free_is_push_float(参数 As Any Ptr)
   Delete 参数
End Sub


Sub free_is_push_bool(参数 As Any Ptr)
   Deallocate(参数)
End Sub


Sub free_is_push_string(参数 As Any Ptr)
   Deallocate(参数)
End Sub

Sub free_is_push_double(参数 As Any Ptr)
   Delete 参数
End Sub

Sub FreeCallBack(TheThis As 实例信息 Ptr)
   Delete( * (TheThis->对象表 + 1))->库数据
End Sub

Sub FreeArray(TheThis As 实例信息 Ptr)
   For i As Integer = 0 To Cast(表信息 Ptr,( * (TheThis->对象表 + 1))->库数据)->元素数 -1
      'If Cast(表信息 Ptr,( * (TheThis->对象表 + 1))->库数据)->支表 = False Then
      '   FreeArray(( * (Cast(容器信息 Ptr Ptr, Cast(表信息 Ptr,( * (TheThis->对象表 + 1))->库数据)->容器表) + i))->值.实例引用)
      'End If
      Delete * (Cast(容器信息 Ptr Ptr, Cast(表信息 Ptr,( * (TheThis->对象表 + 1))->库数据)->容器表) + i)
   Next
   Deallocate(Cast(表信息 Ptr,( * (TheThis->对象表 + 1))->库数据)->容器表)
   Delete( * (TheThis->对象表 + 1))->库数据
End Sub


Function 实例化回调( _
         ByRef 栈指针 As 栈信息 Ptr, _
         互斥锁 As Any Ptr _
         ) As 实例信息 Ptr
   Dim 临时实例信息 As 实例信息 Ptr
   
   临时实例信息 = 实例化(@类_CallBack, -1, 栈指针,互斥锁)
   
   栈指针->值.对象值 = 临时实例信息
   栈指针 = 栈指针 + 1
   
   If 类_构造_object->本地方法 Then
      类_构造_object->本地方法实体(栈指针,互斥锁)
   Else
      环境(类_构造_object, 栈指针, 互斥锁)
   End If
   
   ( * (临时实例信息->对象表 + 1))->库数据 = New 回调信息
   Function=临时实例信息
End Function


Function 实例化表( _
         ByRef 栈指针 As 栈信息 Ptr, _
         互斥锁 As Any Ptr _
         ) As 实例信息 Ptr
   Dim 临时实例信息 As 实例信息 Ptr
   临时实例信息 = 实例化(@类_Array, -1, 栈指针,互斥锁)
   
   栈指针->值.对象值 = 临时实例信息
   栈指针 = 栈指针 + 1
   
   If 类_构造_object->本地方法 Then
      类_构造_object->本地方法实体(栈指针,互斥锁)
   Else
      环境(类_构造_object, 栈指针,互斥锁)
   End If
   
   ( * (临时实例信息->对象表 + 1))->库数据 = New 表信息
   Function=临时实例信息
End Function

Sub 初始化表(临时表信息 As 表信息 Ptr, 维度表() As Integer, 维度 As Integer, 最大维度 As Integer, ByRef 栈指针 As 栈信息 Ptr,互斥锁 As Any Ptr)
   临时表信息->元素数 = 维度表(维度)
   临时表信息->容器表 = Allocate(SizeOf(容器信息 Ptr) *维度表(维度))
   If 维度 = 最大维度 Then
      临时表信息->支表 = True
   End If
   
   For i As Integer = 0 To 维度表(维度) -1
      Dim temp As 容器信息 Ptr
      temp = New 容器信息
      ( * (Cast(容器信息 Ptr Ptr, 临时表信息->容器表) + i)) = temp
      If 维度 <> 最大维度 Then
         temp->类型 = box_ref
         temp->值.实例引用 = 实例化表(栈指针,互斥锁)
         对象池.join(temp->值.实例引用)
         初始化表(temp->值.实例引用, 维度表(), 维度 + 1, 最大维度, 栈指针,互斥锁)
      End If
   Next
End Sub


Function 实例化( _
         临时类指针 As 类指向, _
         临时方法 As 方法指向, _
         ByRef 栈指针 As 栈信息 Ptr, _
         互斥锁 As Any Ptr _
         ) As 实例信息 Ptr
         
   Dim 临时实例信息 As 实例信息 Ptr
   
   临时实例信息 = New 实例信息
   
   临时实例信息->类 = 临时类指针
   
   临时实例信息->对象计数 = 临时类指针->深度
   '临时实例信息->对象表 = New 对象信息 Ptr(临时类指针->深度)
   临时实例信息->对象表 = Allocate(SizeOf(对象信息 Ptr) *临时类指针->深度)

   For i As Integer = 0 To 临时类指针->深度 -1
      *(临时实例信息->对象表 + i) = New 对象信息
   Next
   
   Dim Temp As 类信息 Ptr = 临时类指针
   Dim index As Integer = 临时类指针->深度 -1
   '(*临时实例信息->对象表)->类位置 = Temp
   
   While Temp->超类 = False
      (*(临时实例信息->对象表 + index))->类位置 = Temp
      Temp = Temp->基类
      index = index - 1
   Wend
   (*(临时实例信息->对象表))->类位置 = Temp
   
   临时实例信息->成员计数 = 临时类指针->成员深度
   临时实例信息->成员表 = Allocate(SizeOf(容器信息 Ptr) *临时类指针->成员深度)
   For i As Integer = 0 To 临时类指针->成员深度 -1
      ( * (Cast(容器信息 Ptr Ptr, 临时实例信息->成员表) + i)) = New 容器信息
   Next
   
   临时实例信息->虚方法表 = New TMAPULONGINTINTEGER
   
   重写(临时类指针, 临时实例信息->虚方法表)
   
   If 临时方法 <> -1 Then
      栈指针->类型 = stack_object
      栈指针->值.对象值 = 临时实例信息
      栈指针 += 1
      
      If 临时方法->本地方法 Then
         临时方法->本地方法实体(栈指针,互斥锁)
      Else
         环境(临时方法, 栈指针,互斥锁)
      End If
   End If
   Function = 临时实例信息
End Function


Sub 重写(临时类信息 As 类信息 Ptr, 临时虚方法表 As TMAPULONGINTINTEGER Ptr)
   If 临时类信息->超类 = False Then
      重写(临时类信息->基类, 临时虚方法表)
   End If
   
   For i As Integer = 0 To UBound(临时类信息->函数表)
      If 临时类信息->函数表(i)->实例化方式 = method_Virtual OrElse 临时类信息->函数表(i)->实例化方式 = method_Override OrElse 临时类信息->函数表(i)->实例化方式 = method_Abstract Then
         If 临时虚方法表->Find( 临时类信息->函数表(i)->方法签名) <> 0 Then
            临时虚方法表->DeleteNode(临时类信息->函数表(i)->方法签名)
            临时虚方法表->Insert(临时类信息->函数表(i)->方法签名, 临时类信息->函数表(i))
         Else
            临时虚方法表->Insert(临时类信息->函数表(i)->方法签名, 临时类信息->函数表(i))
         End If
      End If
   Next
End Sub







Sub DefaultNative(value As Any Ptr)
   ErrorBox("调用了没有实体的本地方法")
End Sub


Sub free_is_call_virtual(参数 As Any Ptr)
   Delete 参数
End Sub


Sub free_is_jmp(参数 As Any Ptr)
   Delete 参数
End Sub









Sub free_is_load(参数 As Any Ptr)
   Deallocate(参数)
End Sub



Sub free_is_jz(参数 As Any Ptr)
   Delete 参数
End Sub

