''
'' fbmld (FB Memory Leak Detector) version 0.6
'' Copyright (C) 2006 Daniel R. Verkamp
'' Tree storage implemented by yetifoot
''
'' This software is provided 'as-is', without any express or implied warranty.
'' In no event will the authors be held liable for any damages arising from
'' the use of this software.
''
'' Permission is granted to anyone to use this software for any purpose,
'' including commercial applications, and to alter it and redistribute it
'' freely, subject to the following restrictions:
''
'' 1. The origin of this software must not be misrepresented; you must not claim
'' that you wrote the original software. If you use this software in a product,
'' an acknowledgment in the product documentation would be appreciated but is
'' not required.
''
'' 2. Altered source versions must be plainly marked as such, and must not be
'' misrepresented as being the original software.
''
'' 3. This notice may not be removed or altered from any source
'' distribution.
''


'#define FBMLD_NO_MULTITHREADING
#define FBMLD_DEBUGG


#ifndef __FBMLD__
#define __FBMLD__

#include "crt.bi"

#ifdef FBMLD_DEBUGG
	#ifndef FBMLD_DEB
		#define	FBMLD_DEB
	#endif
#else
	#ifdef FBMLD_DEB
		#undef	FBMLD_DEB
	#endif
#ENDIF


#undef allocate
#undef callocate
#undef reallocate
#undef deallocate


#define allocate(bytes) fbmld_allocate((bytes), __FILE__, __LINE__)
#define callocate(bytes) fbmld_callocate((bytes), __FILE__, __LINE__)
#define reallocate(pt, bytes) fbmld_reallocate((pt), (bytes), __FILE__, __LINE__, #pt)
#define deallocate(pt) fbmld_deallocate((pt), __FILE__, __LINE__, #pt)

type fbmld_t
	pt      as any ptr
	bytes   as uinteger
	file    as string
	linenum as integer
	left    as fbmld_t ptr
	right   as fbmld_t ptr
end type

common shared fbmld_tree as fbmld_t ptr
common shared fbmld_mutex as any ptr
common shared fbmld_instances as integer

private sub fbmld_print(byref s as string)
	fprintf(stderr, !"\n(FBMLD) " & s & chr(10))
end sub

private sub fbmld_mutexlock( )
#ifndef FBMLD_NO_MULTITHREADING
	mutexlock(fbmld_mutex)
#endif
end sub

private sub fbmld_mutexunlock( )
#ifndef FBMLD_NO_MULTITHREADING
	mutexunlock(fbmld_mutex)
#endif
end sub

_


private function new_node _
	( _
		byval pt      as any ptr, _
		byval bytes   as uinteger, _
		byref file    as string, _
		byval linenum as integer _
	) as fbmld_t ptr

	dim as fbmld_t ptr node = calloc(1, sizeof(fbmld_t))

	node->pt = pt
	node->bytes = bytes
	node->file = file
	node->linenum = linenum
	node->left = NULL
	node->right = NULL

	function = node

end function

private sub free_node _
	( _
		byval node as fbmld_t ptr _
	)

	node->file = ""
	free( node )

end sub

private function fbmld_search _
	( _
		byval root    as fbmld_t ptr ptr, _
		byval pt      as any ptr _
	) as fbmld_t ptr ptr

	dim as fbmld_t ptr ptr node = root
	dim as any ptr a = pt, b = any

	asm
		mov eax, dword ptr [a]
		bswap eax
		mov dword ptr [a], eax
	end asm

	while *node <> NULL
		b = (*node)->pt
		asm
			mov eax, dword ptr [b]
			bswap eax
			mov dword ptr [b], eax
		end asm
		if a < b then
			node = @(*node)->left
		elseif a > b then
			node = @(*node)->right
		else
			exit while
		end if
	wend

	function = node

end function

private sub fbmld_insert _
	( _
		byval root    as fbmld_t ptr ptr, _
		byval pt      as any ptr, _
		byval bytes   as uinteger, _
		byref file    as string, _
		byval linenum as integer _
	)

	dim as fbmld_t ptr ptr node = fbmld_search(root, pt)

	if *node = NULL then
		*node = new_node( pt, bytes, file, linenum )
	end if

end sub

private sub fbmld_swap _
	( _
		byval node1 as fbmld_t ptr ptr, _
		byval node2 as fbmld_t ptr ptr _
	)

	swap (*node1)->pt,      (*node2)->pt
	swap (*node1)->bytes,   (*node2)->bytes
	swap (*node1)->file,    (*node2)->file
	swap (*node1)->linenum, (*node2)->linenum

end sub

private sub fbmld_delete _
	( _
		byval node as fbmld_t ptr ptr _
	)

	dim as fbmld_t ptr old_node = *node
	dim as fbmld_t ptr ptr pred

	if (*node)->left = NULL then
		*node = (*node)->right
		free_node( old_node )
	elseif (*node)->right = NULL then
		*node = (*node)->left
		free_node( old_node )
	else
		pred = @(*node)->left
		while (*pred)->right <> NULL
			pred = @(*pred)->right
		wend
		fbmld_swap( node, pred )
		fbmld_delete( pred )
	end if

end sub



private sub fbmld_tree_clean _
	( _
		byval node as fbmld_t ptr ptr _
	)

	if *node <> NULL then
		fbmld_tree_clean( @((*node)->left) )
		fbmld_tree_clean( @((*node)->right) )
		fbmld_print( "error: " & (*node)->bytes & " bytes allocated at " & (*node)->file & ":" & (*node)->linenum & " [&H" & hex( (*node)->pt, 8 ) & "] not deallocated" )
		(*node)->file = ""
		free( (*node)->pt )
		free( *node )
		*node = NULL
	end if
end sub

private sub fbmld_init _
	( _
	) constructor 101
#ifdef FBMLD_DEB
	print "Init leak verification"
#endif
	if fbmld_instances = 0 then
#ifndef FBMLD_NO_MULTITHREADING
		fbmld_mutex = mutexcreate()
#endif
	end if
	fbmld_instances += 1
end sub

private sub fbmld_exit _
	( _
	) destructor 101
	dim ifound as long

	fbmld_instances -= 1
	if fbmld_instances = 0 then

		if fbmld_tree <> NULL then
			fbmld_print("---- memory leaks ----")
			fbmld_tree_clean(@fbmld_tree)
			ifound = 2
		else
#ifdef FBMLD_DEB
			fbmld_print("all memory deallocated")
			ifound = 1
#endif
		end if

#ifndef FBMLD_NO_MULTITHREADING
		if fbmld_mutex <> 0 then
			mutexdestroy(fbmld_mutex)
			fbmld_mutex = 0
		end if
#endif

	end if
	if ifound >0 then  print !"\nExit leak verification\nPress any key to finish" : sleep

end sub

private function fbmld_allocate(byval bytes as uinteger, byref file as string, byval linenum as integer) as any ptr
	dim ret as any ptr = any

	fbmld_mutexlock()

	if bytes = 0 then
#ifdef FBMLD_DEB
		fbmld_print("warning: allocate(0) called at " & file & ":" & linenum & "; returning NULL")
# endif
		ret = 0
	else
		'fbmld_print("warning: allocate(" & str(bytes) & ") called at " & file & ":" & linenum & "   ok")
		ret = malloc(bytes)
		fbmld_insert(@fbmld_tree, ret, bytes, file, linenum)
	end if

	fbmld_mutexunlock()

	return ret
end function

private function fbmld_callocate(byval bytes as uinteger, byref file as string, byval linenum as integer) as any ptr
	dim ret as any ptr = any

	fbmld_mutexlock()

	if bytes = 0 then
#ifdef FBMLD_DEB
		fbmld_print("warning: callocate(0) called at " & file & ":" & linenum & "; returning NULL")
# endif
		ret = 0
	else
		ret = calloc(1, bytes)
		fbmld_insert(@fbmld_tree, ret, bytes, file, linenum)
	end if

	fbmld_mutexunlock()

	return ret
end function

private function fbmld_reallocate(byval pt as any ptr, byval bytes as uinteger, byref file as string, byval linenum as integer, byref varname as string) as any ptr
	dim ret as any ptr = any
	dim node as fbmld_t ptr ptr = any

	fbmld_mutexlock()

	node = fbmld_search(@fbmld_tree, pt)

	if pt = NULL then
		if bytes = 0 then
			fbmld_print("error: reallocate(" & varname & " [NULL] , 0) called at " & file & ":" & linenum)
			ret = NULL
		else
			ret = malloc(bytes)
			fbmld_insert(@fbmld_tree, ret, bytes, file, linenum)
		end if
	elseif *node = NULL then
		fbmld_print("error: invalid reallocate(" & varname & " [&H" & hex(pt, 8) & "] ) at " & file & ":" & linenum)
		ret = NULL
	elseif bytes = 0 then
#ifdef FBMLD_DEB
		fbmld_print("warning: reallocate(" & varname & " [&H" & hex(pt, 8) & "] , 0) called at " & file & ":" & linenum & "; deallocating")
# endif
		free(pt)
		if *node <> NULL then fbmld_delete(node)
		ret = NULL
	else
		ret = realloc(pt, bytes)

		if ret = pt then
			(*node)->bytes = bytes
			(*node)->file = file
			(*node)->linenum = linenum
		else
			fbmld_delete(node)
			fbmld_insert(@fbmld_tree, ret, bytes, file, linenum)
		end if
	end if

	fbmld_mutexunlock()

	return ret
end function

private sub fbmld_deallocate(byval pt as any ptr, byref file as string, byval linenum as integer, byref varname as string)
	dim node as fbmld_t ptr ptr

	fbmld_mutexlock()

	if pt = NULL then
#ifdef FBMLD_DEB
		fbmld_print("warning: deallocate(" & varname & " [NULL] ) at " & file & ":" & linenum)
# endif
	else
		node = fbmld_search(@fbmld_tree, pt)

		if *node = NULL then
			fbmld_print("error: invalid deallocate(" & varname & " [&H" & hex(pt, 8) & "] ) at " & file & ":" & linenum)
		else
			fbmld_delete(node)
			free(pt)
		end if
	end if

	fbmld_mutexunlock()
end sub



#endif '' __FBMLD__

'fbmld_init()

Const integerCount As Integer = 15

    '' Try allocating memory for a number of integers.
    ''
    Dim buffer As Integer Ptr
    buffer = Allocate(integerCount * SizeOf(Integer))

    If (0 = buffer) Then
        Print "Error: unable to allocate memory, quitting."
        End -1
    End If

    '' Prime and fill the memory with the fibonacci sequence.
    ''
    buffer[0] = 0
    buffer[1] = 1
    For i As Integer = 2 To integerCount - 1
        buffer[i] = buffer[i - 1] + buffer[i - 2]
    Next

    '' Display the sequence.
    ''
    For i As Integer = 0 To integerCount - 1
        Print buffer[i] ;
    Next


   'Deallocate(buffer)


print !"\nPress any key to stop" : sleep

'fbmld_exit()

