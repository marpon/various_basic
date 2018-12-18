'#Include Once "windows.bi"


#IFNDEF INCFILE_BI
   #DEFINE INCFILE_BI

   Private Function MakeFileImport(ByVal TabU1 as ubyte ptr ,ByVal  IncFil_len As Uinteger ,ByRef  NewFile As String) As integer
      Dim lFile             AS short
      lFile = FreeFile                           'find next open file number
      Open NewFile For Binary As #lFile          'create the new file
      Put #lFile , , TabU1[0] , IncFil_len       'write the buffer to the file
      Close #lFile                               'close the file
      Function = 1                               'worked so return a 1
   End Function

   #MACRO IncFileEx(label , file , sectionName , attr)
      #if __FUNCTION__ <> "__FB_MAINPROC__"
         #IfnDef Var__##label##__Import__
            Dim label As Const UByte Ptr = Any
            Dim label##_len As UInteger = Any
         #endif
         #if __FB_DEBUG__
            asm jmp .LT_END_OF_FILE_##label##_DEBUG_JMP
         #else
            ' Switch to/Create the specified section
            #if attr = ""
               asm .section sectionName
            #else
               asm .section sectionName , attr
            #endif
         #endif

         ' Assign a label to the beginning of the file
         asm .LT_START_OF_FILE_##label#:
         asm __##label##__start = .
         ' Include the file
         asm .incbin ##file
         ' Mark the end of the the file
         asm __##label##__len = . - __##label##__start
         asm .LT_END_OF_FILE_##label:
         ' Pad it with a NULL Integer (harmless, yet useful for text files)
         asm .LONG 0
         #if __FB_DEBUG__
            asm .LT_END_OF_FILE_##label##_DEBUG_JMP:
         #else
            ' Switch back to the .text (code) section
            asm .section .text
            asm .balign 16
         #endif
         asm .LT_SKIP_FILE_##label:
         asm mov dword ptr [label] , offset .LT_START_OF_FILE_##label#
         asm mov dword ptr [label##_len] , offset __##label##__len

      #else
         #IfnDef Var__##label##__Import__
            Extern "c"
               Extern label As UByte Ptr
               Extern label##_len As UInteger
            End Extern
         #endif
         #if __FB_DEBUG__
            asm jmp .LT_END_OF_FILE_##label##_DEBUG_JMP
         #else
            ' Switch to/create the specified section
            #if attr = ""
               asm .section sectionName
            #else
               asm .section sectionName , attr
            #endif
         #endif

         ' Assign a label to the beginning of the file
         asm .LT_START_OF_FILE_##label#:
         asm __##label##__start = .
         ' Include the file
         asm .incbin ##file
         ' Mark the end of the the file
         asm __##label##__len = . - __##label##__start
         asm .LT_END_OF_FILE_##label:
         ' Pad it with a NULL Integer (harmless, yet useful for text files)
         asm .LONG 0
         asm label:
         asm .int .LT_START_OF_FILE_##label#
         asm label##_len:
         asm .int __##label##__len
         #if __FB_DEBUG__
            asm .LT_END_OF_FILE_##label##_DEBUG_JMP:
         #else
            ' Switch back to the .text (code) section
            asm .section .text
            asm .balign 16
         #endif
         asm .LT_SKIP_FILE_##label:
      #endif

   #endmacro

   #macro IncFile(label , file)
      IncFileEx(label , file , .data , "")       'Use the .data (storage) section (SHARED)
   #endmacro

   #macro ImportFile(label , file , NewFile)
      #define Var__##label##__Import__
      #if __FUNCTION__ <> "__FB_MAINPROC__"
         Dim label As Const Ubyte Ptr = Any
         Dim label##_len As Uinteger = Any
      #Else
         Extern "c"
            Extern label As Ubyte Ptr
            Extern label##_len As Uinteger
         End Extern
      #endif
      IncFileEx(label , file , .data , "")       'Use the .rsrc (storage) section (SHARED)
      MakeFileImport(label , label##_len , NewFile)
   #endmacro

#endif


'usage :
'ImportFile(EXE1, "xxx.exe", Str1Path)

'where EXE1 is the label of the File :in fact an UByte Ptr
' EXE1_len is the length of that file
' "xxx.exe" the origin file must be a constant , a string variable is not accepted
' str1Path the destination path an name for the imported file, can be a string variable







