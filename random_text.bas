


Private Function rnd_range(byval first As Double , byval last As Double) As integer
    static istat as Double
    istat += (last - first) *(rnd * 16957)

	if istat > 18446744073709551615 THEN istat = (last - first) *(rnd * 16459)

    Randomize istat , 3
	if first < 1 THEN
		return int(Rnd * last + .5)
	else
		return int(Rnd * (last - first) + .5) + first
    END IF

End Function

Private Function rnd_low(byval first As Double , byval last As Double) As integer
    dim as integer it = rnd_range(first , last)
    dim as integer it2
    if it > (last - first) / 2 THEN
        it2 = rnd_range(first , last)
        if it2 < ((last - first) *7) / 5 THEN return it2
        return rnd_range(first , last)
    END IF
    return it
End Function

Private Function rnd_hight(byval first As Double , byval last As Double) As integer
    dim as integer it = rnd_range(first , last)
    dim as integer it2
    if it < (last - first) / 2 THEN
        it2 = rnd_range(first , last)
        if it2 > ((last - first) *5) / 7 THEN return it2
        return rnd_range(first , last)
    END IF
    return it
End Function


Private Function BuildWord(byval length as integer) as string
    Dim Words2(0 To...) As String = { "a" , "e" , "i" , "o" , "eu" , "ea" , "e" , "e" , "e" , "e" , "e" , "e" , "a" , _
            "u" , "y" , "ai" , "ou" , "a" , "e" , "i" , "o" , "io" , "iu" , "e" , "e" , "e" , "e" , "e" , "e" , "a" , _
            "oi" , "ui" , "ei" , "ae" , "a" , "e" , "i" , "o" , "ia" , "ie" , "e" , "e" , "e" , "e" , "e" , "a" , "e"}
    Dim Words3(0 To...) As String = { "b" , "c" , "d" , "f" , "r" , "s" , "n" , "t" , _
            "g" , "h" , "j" , "k" , "b" , "c" , "d" , "f" , "s" , "l" , "s" , "n" , "t" , _
            "l" , "m" , "n" , "p" , "b" , "c" , "d" , "f" , "t" , "s" , "s" , "n" , "t" , _
            "q" , "r" , "s" , "t" , "b" , "c" , "d" , "r" , "b" , "c" , "s" , "n" , "t" , _
            "v" , "w" , "x" , "z" , "l" , "m" , "n" , "p" , "d" , "f" , "s" , "n" , "t"}
    Dim Words4(0 To...) As String = { "bl" , "br" , "tr" , "st" , "str" , _
            "dr" , "fr" , "sc" , "sh" , _
            "dl" , "cl" , "cr" , "vl" , _
            "gr" , "gl" , "tl" , "vr" , _
            "pl" , "pr" , "sl" , "sr" , _
            "fl" , "ch"}
    Dim Words5(0 To...) As String = { "d" , "c" , "x" , "l" , _
            "m" , "n" , "s" , "s" , "s" , "s" , _
            "r" , "s" , "t" , "z" , "n" , "n"}
	Dim Words6(0 To...) As String = { "ment" , "tion" , "gre" , "ble" , "cle" , "ssion" , _
            "tre" , "gne" , "ge" , "so" , "sa" , "se" , "mo" , "ma" , "me" , "to" , "ta" , "te" , _
            "r" , "s" , "t" , "z" , "n" , "n"}
    dim as integer k = 0
    dim as string str1
    dim itemp as integer
	dim i as integer = 1
	if length = 0 THEN length = 1
    do
        if i <> length then
            if i = 1 THEN
                itemp = rnd_range(1 , 3)
                if itemp = 1 THEN
                    str1 &= Words3(rnd_range(0 , ubound(Words3)))
					k = 0
                elseif itemp = 2 THEN
                    str1 &= Words4(rnd_range(0 , ubound(Words4)))
					k = 0
                else
                    str1 &= Words2(rnd_range(0 , ubound(Words2)))
                    k = 1
                end if
            else
                if  k = 1 then
					itemp = rnd_range(1 , 2)
					if itemp = 1 THEN
						str1 &= Words3(rnd_range(0 , ubound(Words3)))
					else
						str1 &= Words4(rnd_range(0 , ubound(Words4)))
					end if
					k = 0
                else
                    str1 &= Words2(rnd_range(0 , ubound(Words2)))
                    k = 1
                end if
            end if
        else
            if i = 1 then
                str1 &= Words2(rnd_range(0 , ubound(Words2)))
				k = 1
            else
                itemp = rnd_range(1 , 7)
                if itemp < 3  and  k = 1 THEN
                    str1 &= Words5(rnd_range(0 , ubound(Words5)))
					k = 0
				elseif k = 1 THEN
                    str1 &= Words6(rnd_range(0 , ubound(Words6)))
					k = 0
				elseif k = 0 THEN
					str1 &= Words2(rnd_range(0 , ubound(Words2)))
					k = 1
                END IF
            end if
        end if
		i+=1
    loop while i < length + 1
    return str1
End Function

Private Function GenerateRandomString(byval ival as integer , byref iw as integer = 0 , byref ip as integer = 0 , byref ichar as integer = 0) as string
    rnd_range(ival , ival * 39157)

    dim size1 as integer         				' nb letters
    dim nb as integer  					        ' nb words
    dim p as integer 							' nb sentences
    dim i as integer
    dim j as integer
    dim k as integer
    dim l as integer
    dim m as integer

    Dim Words(0 To 31) As String = { "make" , "freebasic" , "declare" , "declare" , _
            "declare" , "declare" , "x86" , "array" , _
            "volume" , "control" , "declare" , "number" , _
            "declare" , "executive" , "files" , "declare" , _
            "compact" , "optimize" , "clock" , "folder" , _
            "declare" , "declare" , "megabyte" , "declare" , _
            "when" , "declare" , "declare" , "choice" , _
            "routine" , "sub" , "Private Function" , "declare"}

    dim size0 as integer = 0
    dim as string res , trmp
    dim as zstring ptr pres
    for l = 0 to ival
		p = rnd_hight(0 , 8)                     ' nb sentences
        for m = 0 to p
			nb = rnd_range(3 , 12)               ' nb words
            for j = 0 to nb
				size1 = rnd_low(1 , 7)           ' nb letters
				if j = nb and size1 < 5 THEN size1 += 4
				if size1 = 1 and  size0 = 1 THEN size1 = 3
				size0 = size1
                trmp = BuildWord(size1)
				ichar += len(trmp)
                if j = 0 then
                    pres = strptr(trmp)
                    pres[0] = pres[0] - 32
                end if
                i += 1
                res &= trmp
                if (i mod rnd_range(6, 9)) = 0 and j < nb - 1 THEN
                    res &= ", "
                elseif ((i mod rnd_range(24, 29)) = 0 and j < nb) and j > 0 THEN
                    res &= "; "
                else
                    res &= " "
                END IF
                iw += 1
                k = rnd_range(0 , 4096)
                if k < 31 THEN
                    trmp = Words(k)
                    res &= trmp & " "
					ichar += len(trmp)
                    iw += 1
                END IF
            NEXT
            pres = strptr(res)
            k = rnd_range(0 , 25)
            if k < 20 THEN
                pres[len(res) - 1] = "."
            elseif k < 22 THEN
                pres[len(res) - 1] = "!"
            else
                pres[len(res) - 1] = "?"
            END IF
            ip += 1
            if m < p THEN res &= " "
        NEXT
        if l < ival THEN res &= chr(10)
		rnd_range(ival , ival * 7359)
    NEXT
    return res
End Function

#INCLUDE ONCE "crt/string.bi"                    ' marpon
Private Function count_nb(byref soriginal as string , byref spattern as string) as uinteger
    dim as uinteger orilen = len(soriginal)
    dim as uinteger patlen = len(spattern)

    if (orilen = 0 or patlen = 0) then return 0

    dim as zstring ptr oriptr = strptr(soriginal)
    dim as zstring ptr pattern = strptr(spattern)
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
End Function




 /' for i as integer = 1 to 100
	print rnd_range(0 , 8)
NEXT     '/

dim as integer iw , ip, ichar
dim as string sret = GenerateRandomString(2500 , iw , ip , ichar)

print "len string = " ; len(sret) ; "    words = " ; iw
print : print sret
print : print

print "len string = " ; len(sret) , : print "sentences = " ; ip ; "   words = " ; iw ; "    alpha chars = " ; ichar

print "found '. ' = " ; count_nb(sret , ". ")
print "found '.' = " ; count_nb(sret , ".")
print "found '? ' = " ; count_nb(sret , "? ")
print "found '?' = " ; count_nb(sret , "?")
print "found '! ' = " ; count_nb(sret , "! ")
print "found '!' = " ; count_nb(sret , "!")
print "found ';' = " ; count_nb(sret , ";")
print "found ',' = " ; count_nb(sret , ",")
print "found 'nl' = " ; count_nb(sret , chr(10))
print "found 'declare' = " ; count_nb(sret , "declare")
print "found 'e' = " ; count_nb(sret , "e")
print "found 'E' = " ; count_nb(sret , "E")
print "found 's' = " ; count_nb(sret , "s")
print "found 'S' = " ; count_nb(sret , "S")
print "found 'a' = " ; count_nb(sret , "a")
print "found 'A' = " ; count_nb(sret , "A")
sleep





