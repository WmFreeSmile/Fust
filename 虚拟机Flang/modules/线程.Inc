



Type 线程结构
   Dim 方法 As 方法指向
   ReDim 对象表(Any) As 实例信息 Ptr
End Type

Function 启动线程(临时方法指向 As 方法指向) As Any Ptr
   Function = ThreadCreate(@工作线程, 临时方法指向)
End Function

Function 启动线程_初始化(临时方法指向 As 方法指向, 对象表() As 实例信息 Ptr) As Any Ptr
   Dim 临时线程结构 As 线程结构 Ptr
   临时线程结构 = New 线程结构
   
   临时线程结构->方法 = 临时方法指向
   ReDim 临时线程结构->对象表(UBound(对象表))
   For i As Integer = 0 To UBound(对象表)
      临时线程结构->对象表(i) = 对象表(i)
   Next
   Function = ThreadCreate(@工作线程_初始化_引导, 临时线程结构)
End Function

Sub 工作线程_初始化_引导(临时线程结构 As 线程结构 Ptr)
   Dim 方法 As 方法指向
   ReDim 对象表() As 实例信息 Ptr
   
   方法 = 临时线程结构->方法
   ReDim 对象表(UBound(临时线程结构->对象表))
   For i As Integer = 0 To UBound(临时线程结构->对象表)
     对象表(i)= 临时线程结构->对象表(i) 
   Next
   
   
   MutexLock tlock
   Delete 临时线程结构
   MutexUnlock tlock
   
   工作线程_初始化(方法, 对象表())
End Sub


Function 申请栈() As 栈标记类型 Ptr
   'ReDim 栈(Any) As 栈信息
   'ReDim 栈(max_stack_size-1)
   
   Dim 栈指针指针 As 栈信息 Ptr Ptr
   
   '栈指针 = @栈(0)
   栈指针指针 = New 栈信息 Ptr
   
   ( *栈指针指针) = Allocate(SizeOf(栈信息) *max_stack_size)
   
   Dim 临时栈标记 As 栈标记类型 Ptr
   临时栈标记 = New 栈标记类型
   临时栈标记->栈底 = ( *栈指针指针)
   临时栈标记->栈指针指针 = 栈指针指针
   临时栈标记->互斥锁 = New StackMutex
   
   栈池.join(临时栈标记)
   
   Function = 临时栈标记
End Function

Sub 回收栈(临时栈标记 As 栈标记类型 Ptr)
   Deallocate(临时栈标记->栈底)
   Delete 临时栈标记->栈指针指针
   Delete(临时栈标记->互斥锁)
   
   栈池.del(临时栈标记)
   Delete 临时栈标记
End Sub


Sub 工作线程(方法 As 方法指向)
   ReDim 栈(Any) As 栈信息
   ReDim 栈(max_stack_size-1)
   
   Dim 栈指针 As 栈信息 Ptr
   
   栈指针 = @栈(0)
   
   Dim 临时栈标记 As 栈标记类型 Ptr
   临时栈标记 = New 栈标记类型
   临时栈标记->栈底 = 栈指针
   临时栈标记->栈指针指针 = @栈指针
   临时栈标记->互斥锁 = New StackMutex
   栈池.join(临时栈标记)
   
   
   If 方法->本地方法 Then
      方法->本地方法实体(栈指针,临时栈标记->互斥锁)
   Else
      环境(方法, 栈指针,临时栈标记->互斥锁)
   End If
   
   Delete(临时栈标记->互斥锁)
   栈池.del(临时栈标记)
   Delete 临时栈标记
End Sub



Sub 工作线程_初始化(方法 As 方法指向, 对象表() As 实例信息 Ptr)
   ReDim 栈(Any) As 栈信息
   ReDim 栈(max_stack_size-1)
   
   Dim 栈指针 As 栈信息 Ptr
   
   栈指针 = @栈(0)
   
   Dim 临时栈标记 As 栈标记类型 Ptr
   临时栈标记 = New 栈标记类型
   临时栈标记->栈底 = 栈指针
   临时栈标记->栈指针指针 = @栈指针
   临时栈标记->互斥锁 =New StackMutex
   栈池.join(临时栈标记)
   
   For i As Integer = 0 To UBound(对象表)
      栈指针->类型 = stack_object
      栈指针->值.对象值 = 对象表(i)
      栈指针 = 栈指针 + 1
   Next
   
   If 方法->本地方法 Then
      方法->本地方法实体(栈指针, 临时栈标记->互斥锁)
   Else
      环境(方法, 栈指针, 临时栈标记->互斥锁)
   End If
   
   Delete(临时栈标记->互斥锁)
   栈池.del(临时栈标记)
   Delete 临时栈标记
End Sub

Sub 环境(方法 As 方法指向, ByRef 栈指针 As 栈信息 Ptr, 互斥锁 As Any Ptr)
   ReDim 局部容器(方法->最大容器数量 -1) As 容器信息 Ptr
   
   Dim 待跳转回调 As 回调信息
   
   Dim 本类指针 As 类信息 Ptr
   Dim 指令指针 As 指令信息 Ptr
   
   本类指针 = 方法->类
   指令指针 = @本类指针->指令表(方法->地址)
   
   '临时解释传递信息.局部容器 = @局部容器(0)
   '临时解释传递信息.待跳转回调 = @待跳转回调
   '临时解释传递信息.指令指针指针 = @指令指针
   
   Dim 局部容器指针 As 容器信息 Ptr Ptr
   Dim 待跳转回调指针 As 回调信息 Ptr
   Dim 指令指针指针 As Any Ptr Ptr
   
   
   Dim 临时实例信息 As 实例信息 Ptr
   
   Dim temp As 容器信息 Ptr
   
   Dim 目标 As 栈信息 Ptr
   
   局部容器指针 = @局部容器(0)
   待跳转回调指针 = @待跳转回调
   指令指针指针 = @指令指针

   Goto start
   
   label_is_nop: 
   '
   'nop
   Goto skip
   
   label_is_push_box:
   '
   栈指针->类型 = stack_box
   栈指针->值.容器值 = * (局部容器指针 + Cast(Integer, Cast(指令信息 Ptr, ( *指令指针指针))->参数))
   栈指针 += 1
   Goto skip
   
   label_is_push_box_ref:
   '
   栈指针->类型 = stack_box
   栈指针->值.容器值 = ( * (局部容器指针 + Cast(Integer, Cast(指令信息 Ptr, ( *指令指针指针))->参数)))->值.容器引用
   栈指针 += 1
   Goto skip
   
   
   label_is_push_method_static:
   '
   临时实例信息 = 实例化回调(栈指针,互斥锁)
   Cast(回调信息 Ptr,( * (临时实例信息->对象表 + 1))->库数据)->类型 = callback_static
   Cast(回调信息 Ptr,( * (临时实例信息->对象表 + 1))->库数据)->方法 = Cast(指令信息 Ptr, ( *指令指针指针))->参数
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 临时实例信息
   栈指针 += 1
   对象池.join(临时实例信息)
   Goto skip
   
   label_is_push_method_instance:
   '
   
   临时实例信息 = 实例化回调(栈指针, 互斥锁)
   Cast(回调信息 Ptr,( * (临时实例信息->对象表 + 1))->库数据)->类型 = callback_instance
   Cast(回调信息 Ptr, ( * (临时实例信息->对象表 + 1))->库数据)->实例引用 = (栈指针-1)->值.对象值
   栈指针 -= 1
   Cast(回调信息 Ptr,( * (临时实例信息->对象表 + 1))->库数据)->方法 = Cast(指令信息 Ptr,( *指令指针指针))->参数
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 临时实例信息
   栈指针 += 1
   对象池.join(临时实例信息)
   Goto skip
   
   label_is_push_byte:
   '
   GC_Check()
   
   临时实例信息 = 实例化(类_byte, 类_构造_byte, 栈指针,互斥锁)
   Cast(type_byte Ptr, (*(临时实例信息->对象表 + 1))->库数据)->值 = *Cast(Byte Ptr, Cast(指令信息 Ptr,( *指令指针指针))->参数)
   
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 临时实例信息
   栈指针 += 1
   
   对象池.join(临时实例信息)
   Goto skip
   
   label_is_push_short: 
   '
   GC_Check()
   
   
   临时实例信息 = 实例化(类_short,类_构造_short, 栈指针,互斥锁)
   Cast(type_short Ptr, (*(临时实例信息->对象表 + 1))->库数据)->值 = *Cast(Short Ptr, Cast(指令信息 Ptr,( *指令指针指针))->参数)
   
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 临时实例信息
   栈指针 += 1
   
   对象池.join(临时实例信息)
   Goto skip
   
   label_is_push_int:
   '
   GC_Check()
   临时实例信息 = 实例化(类_int, 类_构造_int, 栈指针, 互斥锁)
   Cast(type_int Ptr, (*(临时实例信息->对象表 + 1))->库数据)->值 = *Cast(Long Ptr, Cast(指令信息 Ptr,( *指令指针指针))->参数)
   
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 临时实例信息
   栈指针 += 1
   
   对象池.join(临时实例信息)
   Goto skip
   
   label_is_push_long:
   '
   GC_Check()

   临时实例信息 = 实例化(类_long,类_构造_long, 栈指针,互斥锁)
   Cast(type_long Ptr, (*(临时实例信息->对象表 + 1))->库数据)->值 = *Cast(LongInt Ptr, Cast(指令信息 Ptr,( *指令指针指针))->参数)
   
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 临时实例信息
   栈指针 += 1
   
   对象池.join(临时实例信息)
   Goto skip
   
   label_is_push_float:
   '
   GC_Check()

   临时实例信息 = 实例化(类_float,类_构造_float, 栈指针,互斥锁)
   Cast(type_float Ptr, (*(临时实例信息->对象表 + 1))->库数据)->值 = *Cast(Single Ptr, Cast(指令信息 Ptr,( *指令指针指针))->参数)
   
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 临时实例信息
   栈指针 += 1
   
   对象池.join(临时实例信息)
   Goto skip
   
   label_is_push_double:
   '
   GC_Check()
   
   临时实例信息 = 实例化(类_double,类_构造_double, 栈指针,互斥锁)
   Cast(type_double Ptr, (*(临时实例信息->对象表 + 1))->库数据)->值 = *Cast(Double Ptr,Cast(指令信息 Ptr,( *指令指针指针))->参数)
   
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 临时实例信息
   栈指针 += 1
   
   对象池.join(临时实例信息)
   Goto skip
   
   label_is_push_bool:
   '
   GC_Check()

   临时实例信息 = 实例化(类_bool, 类_构造_bool, 栈指针,互斥锁)
   Cast(type_bool Ptr, (*(临时实例信息->对象表 + 1))->库数据)->值 = *Cast(BOOL Ptr,Cast(指令信息 Ptr,( *指令指针指针))->参数)
   
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 临时实例信息
   栈指针 += 1
   'Print "Make", 临时实例信息
   
   对象池.join(临时实例信息)
   Goto skip
   
   
   label_is_push_string:
   '
   GC_Check()

   临时实例信息 = 实例化(类_string, 类_构造_string, 栈指针,互斥锁)
   Cast(type_string Ptr, (*(临时实例信息->对象表 + 1))->库数据)->值 = *Cast(ZString Ptr,Cast(指令信息 Ptr,( *指令指针指针))->参数)
   
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 临时实例信息
   栈指针 += 1
   
   对象池.join(临时实例信息)
   Goto skip
   
   label_is_push_global:
   '
   栈指针->类型 = stack_box
   栈指针->值.容器值 = Cast(指令信息 Ptr,( *指令指针指针))->参数
   栈指针 += 1
   Goto skip
   
   label_is_popn:
   '
   栈指针 -= 1
   Goto skip
   
   label_is_new: 
   '
   GC_Check()
   临时实例信息 = 实例化(Cast(方法指向, Cast(指令信息 Ptr,( *指令指针指针))->参数)->类, Cast(指令信息 Ptr,( *指令指针指针))->参数, 栈指针,互斥锁)
   
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 临时实例信息
   栈指针 += 1
   
   对象池.join(临时实例信息)
   Goto skip
   
   label_is_next:
   '
   Scope
      Dim 临时容器 As Any Ptr
      临时容器 = * (Cast(容器信息 Ptr Ptr, (栈指针-1)->值.对象值->成员表) + Cast(Integer, Cast(指令信息 Ptr, ( *指令指针指针))->参数))
      'Print "NEXT",(栈指针-1)->值.对象值
      栈指针 -= 1
      
      栈指针->类型 = stack_box
      栈指针->值.容器值 = 临时容器
      栈指针 += 1
   End Scope
   
   Goto skip
   
   label_is_change:
   '
   临时实例信息 = (栈指针 -2)->值.对象值
   
   Scope
      Dim 临时容器 As Any Ptr
      临时容器 = * (Cast(容器信息 Ptr Ptr, Cast(表信息 Ptr, ( * (临时实例信息->对象表 + 1))->库数据)->容器表) + Cast(Integer, Cast(type_int Ptr, ( * ((栈指针 - 1)->值.对象值->对象表 + 1))->库数据)->值))
      栈指针 -= 2
      
      栈指针->值.容器值 = 临时容器
      栈指针->类型 = stack_box
      栈指针 += 1
   End Scope
   
   Goto skip
   
   label_is_create: 
   '
   temp = New 容器信息
   temp->关键容器 = True
   * (局部容器指针 + Cast(Integer, Cast(指令信息 Ptr, ( *指令指针指针))->参数)) = temp
   容器池.join(temp)
   Goto skip
   
   label_is_close:
   '
   temp = * (局部容器指针 + Cast(Integer, Cast(指令信息 Ptr, ( *指令指针指针))->参数))
   容器池.del(temp)
   Delete temp
   Goto skip
   
   label_is_return:
   '
   Goto quit
   Goto skip
   
   label_is_call_path: 
   '
   环境(Cast(指令信息 Ptr, ( *指令指针指针))->参数, 栈指针,互斥锁)
   Goto skip
   
   
   label_is_call_path_native: 
   '
   (*Cast(Sub(ByRef 栈指针 As 栈信息 Ptr, 互斥锁 As Any Ptr) Ptr, Cast(指令信息 Ptr, ( *指令指针指针))->参数))(栈指针,互斥锁)
   Goto skip
   
   label_is_call_virtual:
   '
   Dim 临时方法指向 As 方法指向
   栈指针 -= 1
   
   临时方法指向 = 栈指针->值.容器值->值.实例引用->虚方法表->Find(Cast(调用虚方法信息 Ptr, Cast(指令信息 Ptr, ( *指令指针指针))->参数)->方法签名)
   If 临时方法指向->本地方法 Then
      临时方法指向->本地方法实体(栈指针,互斥锁)
   Else
      环境(临时方法指向, 栈指针,互斥锁)
   End If
   Goto skip
   
   label_is_jmp:
   '
   Cast(指令信息 Ptr,*指令指针指针) += Cast(Integer, *Cast(LongInt Ptr, Cast(指令信息 Ptr,( *指令指针指针))->参数))
   Goto skip
   
   
   label_is_jz:
   '
   If Cast(type_bool Ptr, ( * ((栈指针-1)->值.对象值->对象表 + 1))->库数据)->值 = False Then
      Cast(指令信息 Ptr,*指令指针指针) += Cast(Integer, *Cast(LongInt Ptr, Cast(指令信息 Ptr,( *指令指针指针))->参数))
   End If
   栈指针 -= 1
   
   Goto skip
   
   label_is_new_array:
   '
   GC_Check()
   临时实例信息 = 实例化表(栈指针, 互斥锁)
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 临时实例信息
   栈指针 += 1
   对象池.join(临时实例信息)
   Goto skip
   
   label_is_set:
   '
   Scope
      临时实例信息 = (栈指针-1)->值.对象值
      栈指针 -= 1
      
      ReDim 维度表(Any) As Integer
      ReDim 维度表(Cast(指令信息 Ptr,( *指令指针指针))->参数 -1)
      
      For i As Integer = Cast(指令信息 Ptr,( *指令指针指针))->参数 -1 To 0 Step -1
         维度表(i) = Cast(Integer, Cast(type_int Ptr, ( * ((栈指针-1)->值.对象值->对象表 + 1))->库数据)->值)
         栈指针 -= 1
      Next
      
      初始化表(( * (临时实例信息->对象表 + 1))->库数据, 维度表(), 0, Cast(指令信息 Ptr, ( *指令指针指针))->参数 -1, 栈指针,互斥锁)
      栈指针->类型 = stack_object
      栈指针->值.对象值 = 临时实例信息
      栈指针 += 1
   End Scope
   Goto skip
   
   label_is_assign_box_box_ref:
   '
   
   Scope
      (栈指针-2)->值.容器值->类型 = box_ref
      (栈指针-2)->值.容器值->值.实例引用 = (栈指针 - 1)->值.容器值->值.实例引用
      栈指针 -= 2
      
      栈指针->类型 = stack_box
      栈指针 += 1
   End Scope
   
   '栈指针 -= 2
   '栈指针->类型 = stack_box
   '栈指针->值.容器值->类型 = box_ref
   '栈指针->值.容器值->值.实例引用 = (栈指针 + 1)->值.容器值->值.实例引用
   '栈指针 += 1
   Goto skip
   
   label_is_assign_box_obj:
   '
   栈指针 -= 2
   栈指针->类型 = stack_box
   栈指针->值.容器值->类型 = box_ref
   栈指针->值.容器值->值.实例引用 = (栈指针 + 1)->值.对象值
   栈指针 += 1
   Goto skip
   
   label_is_pop_box_box_ref:
   '
   目标 = 栈指针 -1
   栈指针 -= 2
   目标->类型 = stack_box
   目标->值.容器值->类型 = box_ref
   目标->值.容器值->值.实例引用 = 栈指针->值.容器值->值.实例引用
   Goto skip
   
   label_is_pop_box_obj:
   '
   目标 = 栈指针 -1
   栈指针 -= 2
   目标->类型 = stack_box
   目标->值.容器值->类型 = box_ref
   目标->值.容器值->值.实例引用 = 栈指针->值.对象值
   Goto skip
   
   label_is_locate:
   '
   栈指针 -= 1
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 栈指针->值.容器值->值.实例引用
   栈指针 += 1
   Goto skip
   
   label_is_ref:
   '
   栈指针 -= 2
   (栈指针 + 1)->值.容器值->类型 = box_refpass
   (栈指针 + 1)->值.容器值->值.容器引用 = 栈指针->值.容器值
   Goto skip
   
   label_is_call_callback_set:
   '
   栈指针 -= 1
   *(待跳转回调指针) = *Cast(回调信息 Ptr,(* (栈指针->值.对象值->对象表 + 1))->库数据)
   If 待跳转回调指针->类型 = callback_instance Then
      栈指针->类型 = stack_object
      栈指针->值.对象值 = 待跳转回调指针->实例引用
      栈指针 += 1
   End If
   Goto skip
   
   label_is_call_callback:
   '
   If 待跳转回调指针->方法->本地方法 Then
      待跳转回调指针->方法->本地方法实体(栈指针,互斥锁)
   Else
      环境(待跳转回调指针->方法, 栈指针,互斥锁)
   End If
   Goto skip
   
   label_is_print:
   '
   #ifdef __FB_WIN32__
   Select Case (*((栈指针 -1)->值.对象值->对象表 + 1))->类位置
      Case 类_byte
         Print "Print:";MyUtf8toStr(Str(Cast(type_byte Ptr, ( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值)) ;"B"
      Case 类_short
         Print "Print:";MyUtf8toStr(Str(Cast(type_short Ptr,( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值)) ;"S"
      Case 类_int
         Print "Print:";MyUtf8toStr(Str(Cast(type_int Ptr, ( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值)) ;"I"
      Case 类_long
         Print "Print:";MyUtf8toStr(Str(Cast(type_long Ptr, ( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值)) ;"L"
      Case 类_float
         Print "Print:";MyUtf8toStr(Str(Cast(type_float Ptr,( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值)) ;"F"
      Case 类_double
         Print "Print:";MyUtf8toStr(Str(Cast(type_double Ptr, ( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值)) ;"D"
      Case 类_bool
         Print "Print:";MyUtf8toStr(IIf(Cast(type_bool Ptr,( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值, "TRUE", "FALSE"))
      Case 类_string
         Print !"Print:\34";MyUtf8toStr(Cast(type_string Ptr, ( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值);!"\34"
   End Select
   #else
   Select Case (*((栈指针 -1)->值.对象值->对象表 + 1))->类位置
      Case 类_byte
         Print "Print:";(Str(Cast(type_byte Ptr, ( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值)) ;"B"
      Case 类_short
         Print "Print:";(Str(Cast(type_short Ptr,( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值)) ;"S"
      Case 类_int
         Print "Print:";(Str(Cast(type_int Ptr, ( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值)) ;"I"
      Case 类_long
         Print "Print:";(Str(Cast(type_long Ptr, ( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值)) ;"L"
      Case 类_float
         Print "Print:";(Str(Cast(type_float Ptr,( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值)) ;"F"
      Case 类_double
         Print "Print:";(Str(Cast(type_double Ptr, ( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值)) ;"D"
      Case 类_bool
         Print "Print:";(IIf(Cast(type_bool Ptr,( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值, "TRUE", "FALSE"))
      Case 类_string
         Print !"Print:\34";(Cast(type_string Ptr, ( * ((栈指针 -1)->值.对象值->对象表 + 1))->库数据)->值);!"\34"
   End Select
   #endif
   栈指针 -= 1
   Goto skip
   
   
   label_is_load:
   '
   Scope
      Dim 库名 As String
      Dim 库路径 As String
      Dim 库依赖目录 As String
      
      栈指针 -= 1
      库名 = *Cast(ZString Ptr, Cast(指令信息 Ptr, ( *指令指针指针))->参数)
      
      Dim 临时库信息 As 库信息类型 Ptr
      
      Dim LibHandle As Any Ptr
      Dim GetInfo As Sub(临时库信息 As 库信息类型 Ptr, 临时库接口信息 As 库接口信息 Ptr)
      
      #ifdef __FB_WIN32__
      
      #ifdef __FB_64BIT__
      库路径 = ".\lib\" + 库名 + ".dll"
      库依赖目录 = ".\lib\" + 库名 + "\"
      #else
      库路径 = ".\lib32\" + 库名 + ".dll"
      库依赖目录 = ".\lib32\" + 库名 + "\"
      #endif
      
      #else
      
      #ifdef __FB_64BIT__
      库路径 = "/usr/local/lib/polar/lib/" + 库名 + ".so"
      库依赖目录 = "/usr/local/lib/polar/lib/" + 库名 + "/"
      #else
      库路径 = "/usr/local/lib/polar/lib32/" + 库名 + ".so"
      库依赖目录 = "/usr/local/lib/polar/lib32/" + 库名 + "/"
      #endif
      
      #endif
      LibHandle = DyLibLoad(库路径)
      If LibHandle = 0 Then
         ErrorBox("加载库'" + 库名 + "'的时候，发生错误")
      End If
      GetInfo = DyLibSymbol(LibHandle, "GetInfo")
      
      Dim 索引 As Integer
      索引 = UBound(库表) + 1
      ReDim Preserve 库表(索引)
      
      临时库信息 = New 库信息类型
      
      库表(索引) = 临时库信息
      
      临时库信息->句柄 = LibHandle
      临时库信息->库依赖目录 = 库依赖目录
      GetInfo(临时库信息, @库接口)
      'Print 库名+":"
      For i As Integer = LBound(临时库信息->覆写表) To UBound(临时库信息->覆写表)
         Dim 临时方法指向 As 方法指向
         临时方法指向 = Find_ClassMethod(临时库信息->覆写表(i).路径)
         
         'Print "Override ";
         '
         'For j As Integer = 0 To UBound(临时库信息->覆写表(i).路径.类.命名空间路径)
         '   Print 临时库信息->覆写表(i).路径.类.命名空间路径(j);"::";
         'Next
         'Print 临时库信息->覆写表(i).路径.类.类名;".";
         'Print 临时库信息->覆写表(i).路径.方法签名;"->";
         'Print 临时库信息->覆写表(i).实体
         '
         If 临时方法指向 = 0 Then
            ErrorBox("未能找到覆写方法，方法签名:'" + Str(临时库信息->覆写表(i).路径.方法签名) + "'")
         End If
         
         临时方法指向->本地方法实体 = 临时库信息->覆写表(i).实体
      Next
   End Scope
   Goto skip
   
   
   label_is_size:
   '
   GC_Check()
   '
   Dim 元素数 As Integer
   临时实例信息 = (栈指针 -1)->值.对象值
   元素数 = Cast(表信息 Ptr,( * (临时实例信息->对象表 + 1))->库数据)->元素数
   栈指针 -= 1
   
   临时实例信息 = 实例化(类_int, 类_构造_int, 栈指针,互斥锁)
   Cast(type_int Ptr, (*(临时实例信息->对象表 + 1))->库数据)->值 = 元素数
   
   栈指针->类型 = stack_object
   栈指针->值.对象值 = 临时实例信息
   栈指针 += 1
   
   对象池.join(临时实例信息)
   Goto skip
   
   
   label_is_clear:
   '
   (栈指针 -1)->值.容器值->值.实例引用 = 0
   栈指针 -= 1
   Goto skip
   
   label_is_state:
   '
   Scope
      Dim 状态 As BOOL
      
      状态 = ((栈指针 -1)->值.容器值->值.实例引用 <> 0)
      栈指针 -= 1
      
      GC_Check()
      
      临时实例信息 = 实例化(类_bool, 类_构造_bool, 栈指针,互斥锁)
      Cast(type_bool Ptr, (*(临时实例信息->对象表 + 1))->库数据)->值 = 状态
      
      栈指针->类型 = stack_object
      栈指针->值.对象值 = 临时实例信息
      栈指针 += 1
      
      对象池.join(临时实例信息)
   End Scope
   Goto skip
   
   label_is_type:
   '
   
   
   Scope
      Dim 状态 As BOOL
      
      状态 = ((栈指针 -1)->值.对象值->类 = Cast(指令信息 Ptr, ( *指令指针指针))->参数)
      栈指针 -= 1
      
      GC_Check()
      
      临时实例信息 = 实例化(类_bool, 类_构造_bool, 栈指针,互斥锁)
      Cast(type_bool Ptr, (*(临时实例信息->对象表 + 1))->库数据)->值 = 状态
      
      栈指针->类型 = stack_object
      栈指针->值.对象值 = 临时实例信息
      栈指针 += 1
      
      对象池.join(临时实例信息)
   End Scope
   Goto skip
   
   
   label_is_lock:
   '
   Cast(StackMutex Ptr,互斥锁)->Lock()
   Goto skip
   label_is_unlock:
   '
   Cast(StackMutex Ptr, 互斥锁)->Unlock()
   Goto skip
   
   skip:
   '
   'Print "Code",指令指针->控制码,"OK",Cast(StackMutex Ptr, 互斥锁)->m_level
   Cast(StackMutex Ptr, 互斥锁)->Unlock()
   指令指针 += 1
   
   start:
   '
   'Print "Code",指令指针->控制码
   Cast(StackMutex Ptr,互斥锁)->Lock()
   On 指令指针->控制码 Goto _
   label_is_nop, _ '1
   label_is_push_box, _ '2
   label_is_push_box_ref, _ '3
   label_is_push_method_static, _ '4
   label_is_push_method_instance, _ '5
   label_is_push_byte, _ '6
   label_is_push_short, _ '7
   label_is_push_int, _ '8
   label_is_push_long, _ '9
   label_is_push_float, _ '10
   label_is_push_double, _ '11
   label_is_push_bool, _ '12
   label_is_push_string, _ '13
   label_is_push_global, _ '14
   label_is_popn, _ '15
   label_is_new, _ '16
   label_is_next, _ '17
   label_is_change, _ '18
   label_is_create, _ '19
   label_is_close, _ '20
   label_is_return, _ '21
   label_is_call_path, _ '22
   label_is_call_path_native, _ '23
   label_is_call_virtual, _ '24
   label_is_jmp, _ '25
   label_is_jz, _ '26
   label_is_new_array, _ '27
   label_is_set, _ '28
   label_is_assign_box_box_ref, _ '29
   label_is_assign_box_obj, _ '30
   label_is_pop_box_box_ref, _ '31
   label_is_pop_box_obj, _ '32
   label_is_locate, _ '33
   label_is_ref, _ '34
   label_is_call_callback_set, _ '35
   label_is_call_callback, _ '36
   label_is_print, _ '37
   label_is_load, _ '38
   label_is_size, _ '39
   label_is_clear, _ '40
   label_is_state, _ '41
   label_is_type, _ '42
   label_is_lock, _ '43
   label_is_unlock '44
   
   ErrorBox("未知控制码:'" + Str(指令指针->控制码) + "'")
   
   quit:
   Cast(StackMutex Ptr, 互斥锁)->Unlock()
End Sub
