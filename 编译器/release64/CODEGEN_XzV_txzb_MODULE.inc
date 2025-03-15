'[FILE:C:\Users\WmFreeSmile\Desktop\fust\编译器\modules\哈希算法.Inc
'-----------------------------------------------------------------------------
' 由 VisualFreeBasic 5.8.11 生成的源代码
' 生成时间：2025年03月15日 17时48分58秒
' 更多信息请访问 www.yfvb.com 
'-----------------------------------------------------------------------------



Namespace CH_E59388E5B88CE7AE97E6B395_


Function MD5u64(sInput As String) As ULongInt '字符MD5加密转换为 64位整数
 Dim ctx As MD5Context 
 Dim md5sum(0 To 15) As UByte 
 MYMD5Init( @ctx )
 MYMD5Update( @ctx,Cast(UByte Ptr, StrPtr(sInput) ), Len(sInput) )
 MYMD5Final( @md5sum(0), @ctx)
 Function = *CPtr(ULongInt Ptr, @md5sum(8))
End Function



End Namespace
