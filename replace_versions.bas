' -gen gcc -O 2

#include "crt.bi"                                ' *** compile as 32- or 64-bit ***
#INCLUDE ONCE "crt/string.bi"                    ' marpon
#include "Windows.bi"
Dim shared as zstring ptr retStr
#define testfile "mshtmlc.bi" ' the real thing - over 2MB!
#define tempfile "TmpSaved.txt"

#define findstring "declare" ' some bi files have lowercase declare
#define replstring "* DECLARE *" ' for testing: a longer string




Function TALLY(SomeString As String , PartString As String) As Long
    Dim As integer LenP = Len(PartString) , count
    Dim As integer position = Instr(SomeString , PartString)
    If position = 0 Then Return 0
    While position > 0
        count += 1
        position = Instr(position + LenP , SomeString , PartString)
    Wend
    Return count
End Function

Function findAndReplace(original As String , find As String , replace As String) As String
    If Len(find) = 0 Then Return original
    Var t = tally(original , find)               'find occurencies of find
    Dim As integer found , n , staid , m
    Var Lf = Len(find) , Lr = Len(replace) , Lo = Len(original)
    Dim As integer x = Len(original) - t * Lf + t * Lr 'length of output string
    dim As String res = String(x , 0)            'output string
    Do
        If original[n] = find[0] Then            'got a possible
            For m = 0 To Lf - 1
                If original[n + m] <> find[m] Then Goto lbl 'nope
            Next m
            found = 1                            'Bingo
        End If
        If found Then
            For m = 0 To Lr - 1
                res[staid] = replace[m]          'load the replacerment
                staid += 1
            Next m
            n += Lf
            found = 0
        End If
lbl:
        res[staid] = original[n]
        staid += 1
        n += 1
    Loop Until n >= Lo
    Return res
End Function


Function ReplaceAllJ(byref s1 as string , byref s2 as const string , byref s3 as const string) as zstring ptr
    ' in <s1> replace all occurrences of <s2> by <s3>
    dim as integer posx = 0 , posPrevious , ct = 0 , diff , l2 = len(s2) , l3 = len(s3)
    dim as any ptr posSrc
    dim as any ptr posDest
    if s3 <> s2 then
        diff = l3 - l2
        Do
            posx = Instr(posx + 1 , s1 , s2)
            ct += diff
        Loop until posx = 0
        posSrc = StrPtr(s1)                      ' pointer to source
        retStr = CAllocate(len(s1) + ct)         ' pointer to destination
        posDest = retStr
        Do                                       ' ########## innermost loop ###########
            posPrevious = posx + 1               ' set new start
            posx = Instr(posPrevious , s1 , s2)
            if posx then
                diff = posx - posPrevious        ' bytes to copy from source
                memcpy(posDest , posSrc , diff)  ' copy string between matches
                posDest += diff                  ' correct destination
                posSrc += diff + l2              ' correct source position, including string to be replaced
                memcpy(posDest , StrPtr(s3) , l3)' copy the replace string
                posDest += l3                    ' and correct the destination
                posx += l2 - 1                   ' same for source
            end if
        Loop until posx = 0
        diff = len(s1) - posPrevious + 1         ' bytes to copy from source È
        ' Print "Rest=";len(s1);"-";posPrevious;"=";diff
        memcpy(posDest , posSrc , diff)
    end if
    ' print ct; " times ";s2
    return retStr
end function


'marpon
function replace_c(byref soriginal as string , byref spattern as string , byref sreplacement as string) as string
    dim as uinteger orilen = len(soriginal)
    dim as uinteger patlen = len(spattern)
    dim as uinteger replen = len(sreplacement)
    if (orilen = 0 or patlen = 0 or spattern = sreplacement) then return soriginal

    dim as zstring ptr original = strptr(soriginal)
    dim as zstring ptr pattern = strptr(spattern)
    dim as zstring ptr replacement = strptr(sreplacement)

    dim as zstring ptr oriptr = original
    dim as zstring ptr patloc

    dim as uinteger patcnt

    ' find how many times the pattern occurs in the original string
    patloc = strstr(oriptr , pattern)
    do while(patloc <> NULL)
        patcnt += 1
        oriptr = patloc + patlen
        patloc = strstr(oriptr , pattern)
    loop
	if patcnt = 0 THEN return soriginal 'added

    '' allocate memory for the new string
    dim as uinteger retlen = orilen + patcnt * (replen - patlen)
    if retlen = 0 THEN return ""
    dim as zstring ptr returned = allocate(retlen + 1)
    if returned = NULL THEN return ""
    dim as zstring ptr retptr = returned
    dim as uinteger skplen
    ' replacing all the instances of the pattern
    oriptr = original
    patloc = strstr(oriptr , pattern)
    while(patloc <> NULL)
        skplen = patloc - oriptr
        ' copy the section until the occurence of the pattern
        memcpy(retptr , oriptr , skplen)
        retptr += skplen
        ' copy the replacement
        memcpy(retptr , replacement , replen)
        retptr += replen
        oriptr = patloc + patlen
        patloc = strstr(oriptr , pattern)
    wend
    ' copy the rest of the string if any.
    memcpy(retptr , oriptr , orilen - (oriptr - original) + 1)
    function = *returned
    deallocate(returned)
end function


'marpon2
function replace_c2(byref soriginal as string , byref spattern as string , byref sreplacement as string) byref as string
    dim as uinteger orilen = len(soriginal)
    dim as uinteger patlen = len(spattern)
    dim as uinteger replen = len(sreplacement)
    if (orilen = 0 or patlen = 0 or spattern = sreplacement) then return soriginal

    dim as zstring ptr original = strptr(soriginal)
    dim as zstring ptr pattern = strptr(spattern)
    dim as zstring ptr replacement = strptr(sreplacement)

    dim as zstring ptr oriptr = original
    dim as zstring ptr patloc

    dim as uinteger patcnt

    ' find how many times the pattern occurs in the original string
    patloc = strstr(oriptr , pattern)
    do while(patloc <> NULL)
        patcnt += 1
        oriptr = patloc + patlen
        patloc = strstr(oriptr , pattern)
    loop
	if patcnt = 0 THEN return soriginal 'added
    '' allocate memory for the new string
    static as string sreturned : sreturned = ""
    dim as uinteger retlen = orilen + patcnt * (replen - patlen)
    if retlen = 0 THEN return sreturned
	sreturned = string(retlen , 0)
    dim as zstring ptr returned = strptr(sreturned)

    dim as zstring ptr retptr = returned
    dim as integer skplen
    ' replacing all the instances of the pattern
    oriptr = original
    patloc = strstr(oriptr , pattern)
    while(patloc <> NULL)
        skplen = patloc - oriptr
        ' copy the section until the occurence of the pattern
        memcpy(retptr , oriptr , skplen)
        retptr += skplen
        ' copy the replacement
        memcpy(retptr , replacement , replen)
        retptr += replen
        oriptr = patloc + patlen
        patloc = strstr(oriptr , pattern)
    wend
    ' copy the rest of the string if any.can skip copy the last zero because string(retlen , 0) already made it
    memcpy(retptr , oriptr , orilen - (oriptr - original))
    function = sreturned

end function



'marpon  MrSwiss adaptation
Function replaceFB( _
            ByVal original As ZString Ptr , _
            ByVal pattern As ZString Ptr , _
            ByVal repace As ZString Ptr _
            ) As String
    Dim As UInteger orilen = len(*original) , _
            patlen = len(*pattern) , _
            replen = len(*repace) , patcnt

    If (orilen = 0) orelse(patlen = 0) Then
        If (*pattern = *repace) Then Return * original
    End If

    Dim As ZString Ptr oriptr = original , patloc
    ' find how many times the pattern occurs in the original string
    patloc = strstr(oriptr , pattern)
    While patloc <> NULL
        patcnt += 1
        oriptr = patloc + patlen
        patloc = strstr(oriptr , pattern)
    Wend
    ' allocate memory for the new string
    Dim As UInteger retlen = orilen + patcnt * (replen - patlen)
    If retlen = 0 Then Return ""

    Dim As ZString Ptr returned = Allocate(retlen + 1)
    If returned = NULL Then Return ""
    Dim As ZString Ptr retptr = returned
    Dim As UInteger skplen
    ' replacing all the instances of the pattern
    oriptr = original
    patloc = strstr(oriptr , pattern)
    While patloc <> NULL
        skplen = patloc - oriptr
        ' copy the section until the occurence of the pattern
        memcpy(retptr , oriptr , skplen)
        retptr += skplen
        ' copy the repace
        memcpy(retptr , repace , replen)
        retptr += replen
        oriptr = patloc + patlen
        patloc = strstr(oriptr , pattern)
    Wend
    ' copy the rest of the string if any.
    memcpy(retptr , oriptr , orilen - (oriptr - original) + 1)
    Function = *returned
    DeAllocate(returned)
End Function

Function replaceFB2( _
    ByVal original As ZString Ptr, _
    ByVal pattern  As ZString Ptr, _
    ByVal replace  As ZString Ptr _
    ) ByRef As String

    Static As String sret : sret = ""
    Dim As UInteger orilen = len(*original), _
                    patlen = len(*pattern), _
                    replen = len(*replace), patcnt

    If (orilen = 0) OrElse (patlen = 0) OrElse _
        (*pattern = *replace) Then Return sret

    Dim As ZString Ptr oriptr = original, patloc
    ' find how many times the pattern occurs in the original string
    patloc = strstr(oriptr, pattern)
    While patloc > 0
        patcnt += 1
        oriptr = patloc + patlen
        patloc = strstr(oriptr, pattern)
    Wend
    ' calc. required memory for static string
    Dim As UInteger retlen = orilen + patcnt * (replen - patlen)
    If retlen = 0 Then Return sret
    sret = String((retlen + 1), 0)  ' allocate memory
    Dim As ZString Ptr retptr = StrPtr(sret)
    Dim As UInteger skplen
    ' replacing all the instances of the pattern
    oriptr = original
    patloc = strstr(oriptr, pattern)
    While patloc > 0
        skplen = patloc - oriptr
        ' copy the section until the occurence of the pattern
        memcpy(retptr, oriptr, skplen)
        retptr += skplen
        ' copy the replace
        memcpy(retptr, replace, replen)
        retptr += replen
        oriptr = patloc + patlen
        patloc = strstr(oriptr, pattern)
    Wend
    ' copy the rest of the string if any.
    memcpy(retptr, oriptr, orilen - (oriptr - original) + 1)

    Return sret
End Function


Function LoadFile(ByRef filename As String) As String
    Dim h As Integer = FreeFile
    Dim txt As String = ""
    If Open(filename For Binary Access Read As #h) = 0 Then
        If Lof(h) Then
            txt = String(Lof(h) , 0)
            If Get(#h , 0 , txt) Then txt = ""
        End If
        Close #h
    end if
    Return txt
End Function

Sub SaveFilePtr(ByRef filename As String , ByRef source As zstring ptr)
    Dim written As long
    Dim h As handle = CreateFile(filename , GENERIC_WRITE , 0 , 0 , CREATE_ALWAYS , FILE_ATTRIBUTE_NORMAL , 0)
    If h <> INVALID_HANDLE_VALUE Then
        WriteFile(h , source , strlen(source) , @written , 0)
        CloseHandle(h)
        print written ; " bytes written"
    end if
End Sub


function count_nb(byref soriginal as string , byref spattern as string ) as uinteger
    dim as uinteger orilen = len(soriginal)
    dim as uinteger patlen = len(spattern)

    if (orilen = 0 or patlen = 0 ) then return 0

    dim as zstring ptr original = strptr(soriginal)
    dim as zstring ptr pattern = strptr(spattern)


    dim as zstring ptr oriptr = original
    dim as zstring ptr patloc

    dim as uinteger patcnt

    ' find how many times the pattern occurs in the original string
    patloc = strstr(oriptr , pattern)
    do while(patloc <> NULL)
        patcnt += 1
        oriptr = patloc + patlen
        patloc = strstr(oriptr , pattern)
    loop
	return patcnt
end function


Function rnd_range(first As Double , last As Double) As integer
   Function = int(Rnd * (last - first) + first)
End Function


function GenerateRandomString(byval ival as integer) as string
    dim size1 as integer = rnd_range(1 , 26) 'nb letters
	dim nb as integer = rnd_range(1 , 26) ' nb words
	dim i as integer
	dim j as integer
	dim k as integer
	dim l as integer
	Dim Words(0 To 31) As String = { "make" , "freebasic" , "declare" , "declare" , _
      "declare" , "declare" , "x86" , "array" , _
      "volume" , "control" , "declare" , "number" , _
      "declare" , "executive" , "files" , "declare" , _
      "compact" , "optimize" , "clock" , "folder" , _
      "declare" , "declare" , "megabyte" , "declare" , _
      "when" , "declare" , "declare" , "choice" , _
      "routine" , "sub" , "function" , "declare"}

    dim as string res
	for l = 0 to ival
		for j = 0 to nb
			for i = 0 to size1
				res &= chr(rnd_range(35, 128 ))
			next
			res &=" "
			k = rnd_range(0 , 128)
			if k < 31 THEN
				res &= Words(k) & " for test "
			END IF
		NEXT
		res &= "."
	NEXT
    return res
end function



Randomize 5,3


Dim Content As String = GenerateRandomString(rnd_range(2000, 5000))
print "len string = " & len(content)


'Dim Content As String = LoadFile(testfile)
var p = instr(content , findstring)
print "Nb found =  "  & count_nb(content , findstring ) & "       firt position = " & p

Dim as double t = timer
Dim ContentNew as zstring ptr = ReplaceAllJ(Content , findstring , replstring)
print using "##.#### seconds for replace all" ; timer - t ; : print tab(50) ; mid(*contentnew , p , 20)

t = timer
ReplaceAllJ(Content , findstring , replstring)
print using "##.#### seconds for replace all" ; timer - t

dim s2 as string
t = timer
s2 = replace_c(Content , findstring , replstring)
print using "##.#### seconds for replace all marpon" ; timer - t ; : print tab(50) ; mid(s2 , p , 20)

t = timer
s2 = replace_c(Content , findstring , replstring)
print using "##.#### seconds for replace all marpon" ; timer - t

t = timer
s2 = replace_c(Content , findstring , replstring)
print using "##.#### seconds for replace all marpon" ; timer - t


t = timer
s2 = replace_c2(Content , findstring , replstring)
print using "##.#### seconds for replace all marpon2" ; timer - t ; : print tab(50) ; mid(s2 , p , 20)

t = timer
s2 = replace_c2(Content , findstring , replstring)
print using "##.#### seconds for replace all marpon2" ; timer - t

t = timer
s2 = replace_c2(Content , findstring , replstring)
print using "##.#### seconds for replace all marpon2" ; timer - t

t = timer
ReplaceAllJ(Content , findstring , replstring)
print using "##.#### seconds for replace all" ; timer - t

t = timer
ReplaceAllJ(Content , findstring , replstring)
print using "##.#### seconds for replace all" ; timer - t

t = timer
ReplaceAllJ(Content , findstring , replstring)
print using "##.#### seconds for replace all" ; timer - t
s2 = content
t = timer
s2 = findAndReplace(Content , findstring , replstring)
print using "##.#### seconds for dodicat replace all" ; timer - t ; : print tab(50) ; mid(s2 , p , 20)


t = timer
s2 = findAndReplace(Content , findstring , replstring)
print using "##.#### seconds for dodicat replace all" ; timer - t

t = timer
s2 = findAndReplace(Content , findstring , replstring)
print using "##.#### seconds for dodicat replace all" ; timer - t

t = timer
s2 = replacefb(Content , findstring , replstring)
print using "##.#### seconds for marpon  MrSwiss adaptation replace all" ; timer - t
t = timer
s2 = replacefb(Content , findstring , replstring)
print using "##.#### seconds for marpon  MrSwiss adaptation replace all" ; timer - t

t = timer
s2 = replacefb(Content , findstring , replstring)
print using "##.#### seconds for marpon  MrSwiss adaptation replace all" ; timer - t

t = timer
s2 = replacefb2(Content , findstring , replstring)
print using "##.#### seconds for marpon  MrSwiss adaptation2 replace all" ; timer - t

t = timer
s2 = replacefb2(Content , findstring , replstring)
print using "##.#### seconds for marpon  MrSwiss adaptation2 replace all" ; timer - t

t = timer
s2 = replacefb2(Content , findstring , replstring)
print using "##.#### seconds for marpon  MrSwiss adaptation2 replace all" ; timer - t

'SaveFilePtr(tempfile, ContentNew)


sleep
