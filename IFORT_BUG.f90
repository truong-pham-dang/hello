
program ifort_bug
use lib_abstract_buggy, only : raise_bug
use type_buggy, only : buggy
implicit none
type(buggy) :: bug

bug = buggy(array=[1., 2., 3.], scalar=3)
call raise_bug(bug=bug, scalar=2)
stop
endprogram ifort_bug

