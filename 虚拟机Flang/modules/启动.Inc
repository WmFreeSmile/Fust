
'__   ___         __          __    ____           _____ _____ _____ 
'\ \ / (_)        \ \        / /   |  _ \   /\    / ____|_   _/ ____|
' \ V / _  __ _  __\ \  /\  / /   _| |_) | /  \  | (___   | || |     
'  > < | |/ _` |/ _ \ \/  \/ / | | |  _ < / /\ \  \___ \  | || |     
' / . \| | (_| | (_) \  /\  /| |_| | |_) / ____ \ ____) |_| || |____ 
'/_/ \_\_|\__,_|\___/ \/  \/  \__,_|____/_/    \_\_____/|_____\_____|

Dim Shared tlock As Any Ptr

Sub ErrorBox(text As String)
   'MsgBox(text)
   Print "Error:";text
   End(0)
End Sub

Sub main()
   tlock = MutexCreate()
   
   exit_cond = CondCreate()
   exit_mutex = MutexCreate()

#ifdef __FB_WIN32__
   CondWait(exit_cond, exit_mutex)
#endif
'   cond = CondCreate()
'   mutex = MutexCreate()
'#ifdef __FB_WIN32__
'   CondWait(cond, mutex)
'#endif
   GC_Trigger = New Trigger
   
   GC_Flag_Mutex = MutexCreate()
   

   thread_handle = ThreadCreate(@GMT)

   初始化库接口表()
   
   Dim position As Integer, arg As String,nowArg As String

   position = 1
   Do
      arg = Command( position )
      If( Len( arg ) = 0 ) Then
         Exit Do
      End If
      nowArg = arg
      
      If UCase(arg) = UCase("-AddimportDir") Then
         position += 1
         arg = Command(position)
         
         Dim 索引 As Integer
         索引 = UBound(引用目录) + 1
         ReDim Preserve 引用目录(索引)
         引用目录(索引) = arg
      End If
      position += 1
   Loop

#ifdef DebugMode
   Dim 索引 As Integer
   索引 = UBound(引用目录) + 1
   ReDim Preserve 引用目录(索引)
   引用目录(索引) = "C:\Users\WmFreeSmile\Desktop\Flang\bin\project\class\"
   
   索引 = UBound(引用目录) + 1
   ReDim Preserve 引用目录(索引)
   引用目录(索引) = "C:\Users\WmFreeSmile\Desktop\Flang\bin\class\"
#endif
   
   命名空间.名称 = "ROOT"
   命名空间.超空间 = True
   
   Dim temp As 类路径
   ReDim temp.命名空间路径(1)
   temp.命名空间路径(0) = "ROOT"
   temp.命名空间路径(1) = "lang"
   
   temp.类名 = "object" : 类_object = Find_Class(temp)
   temp.类名 = "byte":类_byte=Find_Class(temp)
   temp.类名 = "short":类_short=Find_Class(temp)
   temp.类名 = "int":类_int=Find_Class(temp)
   temp.类名 = "long":类_long=Find_Class(temp)
   temp.类名 = "float":类_float=Find_Class(temp)
   temp.类名 = "double":类_double=Find_Class(temp)
   temp.类名 = "bool":类_bool=Find_Class(temp)
   temp.类名 = "string":类_string=Find_Class(temp)
   
   类_Array_析构方法.实例化方式 = method_Path
   类_Array_析构方法.本地方法 = True
   类_Array_析构方法.本地方法实体 = @FreeArray
   
   类_Array.名称 = "Array"
   类_Array.命名空间 = Create_Namespace(temp.命名空间路径())
   类_Array.基类 = 类_object
   类_Array.成员偏移计数器 = 类_object->成员偏移计数器
   类_Array.成员深度 = 类_object->成员深度
   类_Array.是否抽象 = False
   类_Array.有析构方法 = True
   类_Array.析构方法 = @类_Array_析构方法
   类_Array.深度 = 2
   
   类_CallBack_析构方法.实例化方式 = method_Path
   类_CallBack_析构方法.本地方法 = True
   类_CallBack_析构方法.本地方法实体 = @FreeCallBack
   
   
   类_CallBack.名称 = "CallBack"
   类_CallBack.命名空间 = Create_Namespace(temp.命名空间路径())
   类_CallBack.基类 = 类_object
   类_CallBack.成员偏移计数器 = 类_object->成员偏移计数器
   类_CallBack.成员深度 = 类_object->成员深度
   类_CallBack.是否抽象 = False
   类_CallBack.有析构方法 = True
   类_CallBack.析构方法 = @类_CallBack_析构方法
   类_CallBack.深度 = 2
   
   类_构造_object = 寻找默认构造(类_object):If 类_构造_object = -1 Then ErrorBox("无法找到lang::object的默认构造方法"):Return
   类_构造_byte = 寻找默认构造(类_byte):If 类_构造_byte = -1 Then ErrorBox("无法找到lang::byte的默认构造方法"):Return
   类_构造_short = 寻找默认构造(类_short):If 类_构造_short = -1 Then ErrorBox("无法找到lang::short的默认构造方法"):Return
   类_构造_int = 寻找默认构造(类_int):If 类_构造_int = -1 Then ErrorBox("无法找到lang::int的默认构造方法"):Return
   类_构造_long = 寻找默认构造(类_long):If 类_构造_long = -1 Then ErrorBox("无法找到lang::long的默认构造方法"):Return
   类_构造_float = 寻找默认构造(类_float):If 类_构造_float = -1 Then ErrorBox("无法找到lang::float的默认构造方法"):Return
   类_构造_double = 寻找默认构造(类_double):If 类_构造_double = -1 Then ErrorBox("无法找到lang::double的默认构造方法"):Return
   类_构造_bool = 寻找默认构造(类_double):If 类_构造_bool = -1 Then ErrorBox("无法找到lang::bool的默认构造方法"):Return
   类_构造_string = 寻找默认构造(类_string):If 类_构造_string = -1 Then ErrorBox("无法找到lang::string的默认构造方法"):Return
   
   Dim 临时方法路径 As 方法路径
   Dim 临时方法指向 As 方法指向
   
   ReDim 临时方法路径.类.命名空间路径(0)
   临时方法路径.类.命名空间路径(0) = "ROOT"
   临时方法路径.类.类名 = "program"
   临时方法路径.方法签名 = 1088842128993965246 '无参 静态 main函数
   
   临时方法指向 = Find_ClassMethod(临时方法路径)
   
   If 临时方法指向 = 0 Then
      ErrorBox("找不到启动方法")
   End If
   
   For i As Integer = 0 To UBound(静态构造方法表)
      '工作线程(静态构造方法表(i))
      ThreadWait(启动线程(静态构造方法表(i)))
   Next
   
'//////////////////////////////////////////////////////////////////////////////测试区域
   'ReDim 栈(Any) As 栈信息
   'ReDim 栈(max_stack_size-1)
   'Dim 栈指针 As 栈信息 Ptr
   '栈指针 = @栈(0)
   'Dim 临时栈标记 As 栈标记类型 Ptr
   '临时栈标记 = New 栈标记类型
   '临时栈标记->栈底 = 栈指针
   '临时栈标记->栈指针指针 = @栈指针
   '栈池.join(临时栈标记)
   '
   'Dim path_lang_bool As 类路径
   'Dim point_lang_bool As 类指向
   'Dim method_lang_bool As 方法指向
   '
   'ReDim path_lang_bool.命名空间路径(1)
   'path_lang_bool.命名空间路径(0) = "ROOT"
   'path_lang_bool.命名空间路径(1) = "lang"
   'path_lang_bool.类名 = "bool"
   '
   'point_lang_bool = Find_Class(path_lang_bool)
   '
   'For i As Integer = 0 To UBound(point_lang_bool->函数表)
   '   If point_lang_bool->函数表(i)->方法名 = "$bool" Then
   '      method_lang_bool = point_lang_bool->函数表(i)
   '   End If
   'Next
   '
   'For i As Integer = 0 To UBound(静态构造方法表)
   '   工作线程(静态构造方法表(i))
   'Next
   '
   '
   'While True
   '   Dim 临时实例信息 As 实例信息 Ptr
   '   临时实例信息 = 实例化(point_lang_bool, method_lang_bool, 栈指针)
   '   'Print 临时实例信息
   '   Free_Obj(临时实例信息)
   '   '对象池.join(临时实例信息)
   '   
   '   'GC_Feedback()
   'Wend
   '
   '栈池.del(临时栈标记)
   'Delete 临时栈标记
   '
   'Print "Exit"
   'Sleep
'///////////////////////////////////////////////////////////////////////////////
   
   ThreadWait(启动线程(临时方法指向))
   
   MutexLock(对象池.m_mutex)
   For i As Integer = 0 To UBound(对象池.m_pool)
      Free_Obj(对象池.m_pool(i))
   Next
   MutexUnlock(对象池.m_mutex)
   
   MutexLock(容器池.m_mutex)
   For i As Integer = 0 To UBound(容器池.m_pool)'程序结束还没有释放的容器，多半是静态容器
      Delete 容器池.m_pool(i)
   Next
   MutexUnlock(容器池.m_mutex)
   
   For i As Integer = 0 To UBound(库表)
      库表(i)->释放函数()
      DyLibFree(库表(i)->句柄)
      Delete 库表(i)
   Next
   
   MutexDestroy tlock
   
   
   
   skip = True
   'MutexLock mutex
   'CondBroadcast(cond)
   'MutexUnlock mutex
   GC_Trigger->Run()
   
   CondWait(exit_cond, exit_mutex)
   
   CondDestroy(exit_cond)
   MutexDestroy(exit_mutex)
   
   'CondDestroy(cond)
   'MutexDestroy(mutex)
   Delete GC_Trigger
   
   MutexDestroy(GC_Flag_Mutex)
   
#ifdef DebugMode
   Sleep
#endif
End Sub
