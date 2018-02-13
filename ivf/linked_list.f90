! A simple implementation of linked list
! in modern fortran 2008+
! Rev 0.1
! First implementation
! Thursday 2018-02-01
!
! Rev 0.2
! Friday, 2018-02-02
! - new methods added
!  a. isempty
!  b. add_after
!  c. add_before



! Mohammad Rahmani
! Chemical Engineering Department
! Amirkabir University of Technology
! Tehran, Ir
! m.rahmani[at]aut.ac.ir
! https:://github.com/kookma
!


!module md_linked_list
! MD_LINKED_LIST
! module liked list is a simple implementation
! of single linked list
!

implicit none

private

public :: element
public :: linked_list


!!    type element
!!        ! user-defined-type to store node elem
!!        character(len=10) :: name      ! element name
!!        real(kind=4)      :: mw        ! molecular weight
!!    end type element
!!

type node
    private
    type(element)        :: data             ! node elem
    type (node), pointer :: next => null()  ! a pointer to the next node in the linked_list
end type node

type linked_list

    private

    integer     :: num_nodes = 0 ! number of nodes
    type(node), pointer :: head => null()
    type(node), pointer :: tail => null()

contains

    private

    procedure, pass, public :: add    => list_add_item_last
    procedure, pass, public :: get    => list_get_item
    procedure, pass, public :: getp   => list_get_item_pointer
    !!!  procedure, pass, public :: prn    => list_print
    procedure, pass, public :: len    => list_length
    procedure, pass, public :: delete => list_destroy
    ! Added in Rev 0.2
    procedure, pass, public :: isempty    => list_is_empty
    procedure, pass, public :: add_first  => list_add_item_first
    procedure, pass, public :: add_after  => list_add_item_after


end type linked_list


contains

!...............................................................................
subroutine list_destroy(this)
    ! LIST_DESTROY
    ! delete the list and free up the allocated memory

    class(linked_list) :: this

    ! local variables
    type(node), pointer :: pnode

    ! transverse the linked_list and deallocate each node
    if (.not. associated(this%head)) then
        print*, 'linked_list is empty'
        return
    end if

    pnode => this%head ! make current point to head of list
    do while (associated(pnode))
        print*, this%num_nodes
        this%head => this%head%next ! make list point to next node of head
        deallocate(pnode)           ! deallocate current head node
        this%num_nodes = this%num_nodes - 1  ! one node removed, decrease num_nodes by one
        pnode => this%head ! make pnode points to the new head
    end do

    print*, 'list deleted, number of nodes: '
    print*, 'head=> ', associated(this%head)
end subroutine list_destroy



!...............................................................................
function list_length(this) result (n)
    ! LIST_LENGTH
    ! return the length (number of nodes) in the list

    class(linked_list) :: this
    integer  :: n

    n = this%num_nodes
end function list_length

!...............................................................................
function list_is_empty(this) result (res)
    ! LIST_IS_empty
    ! check if the list is empty (has any node)
    ! res: True for empty list
    class(linked_list), intent(in) :: this
    logical  :: res

    res = .not. associated(this%head)

end function list_is_empty



!...............................................................................
subroutine list_add_item_last(this, item)
    ! LIST_ADD_ITEM_LAST
    ! adds a new node to the end of list
    ! item: the data for new node
    class(linked_list), intent(inout) :: this
    type(element), intent(in)         :: item

    ! check if list is empty
    if (.not. associated(this%head)) then ! this is the first element
        allocate(this%head)
        this%head%data = item
        this%head%next => null()
        this%tail => this%head   ! tail and head points to the same node
        write(*, fmt='(a)', advance='no') 'first: node number= '

    else ! if list has some nodes

        allocate(this%tail%next)  !add a new node to the list
        this%tail => this%tail%next ! tail now point to the new item
        this%tail%data = item  ! tail store data of new point
        this%tail%next => null()     !tail%next points to nothing
        write(*, fmt='(a)', advance='no') 'other: node number= '
    end if

    this%num_nodes = this%num_nodes + 1  ! increase number of nodes in the list
    print '(I0)', this%num_nodes
end subroutine list_add_item_last



!...............................................................................
subroutine list_add_item_first(this, item)
    ! LIST_ADD_ITEM_FIRST
    ! adds a new node to the begining of list
    ! item: the data for new node
    class(linked_list) :: this
    type(element), intent(in)  :: item

    ! local variable
    type(node), pointer :: new_node

    ! check if list is empty
    if (.not. associated(this%head)) then ! this is the first element
        allocate(this%head)
        this%head%data = item
        this%head%next => null()
        this%tail => this%head   ! tail and head points to the same node
        write(*, fmt='(a)', advance='no') 'first: node number= '

    else ! if list has some nodes
        allocate(new_node)          ! create a new node
        new_node%next => this%head  ! point to the previous head
        new_node%data = item        ! update the data of new node
        this%head => new_node       ! update the head
        nullify(new_node)           ! nullify the temporary node pointer
        write(*, fmt='(a)', advance='no') 'new item at the begining: node number= '
    end if

    this%num_nodes = this%num_nodes + 1  ! increase number of nodes in the list
    print '(I0)', this%num_nodes
end subroutine list_add_item_first



!...............................................................................
subroutine list_add_item_after(this, inode, item)
    ! LIST_ADD_ITEM_AFTER
    ! adds a new node to list after inode
    ! - inode: the location of node in the list
    ! - item: the data for the new node
    class(linked_list), intent(inout)  :: this
    integer, intent(in) :: inode
    type(element), intent(in)  :: item

    ! local variable
    type(node), pointer :: new_node, pnode
    integer :: i

    if (inode <= 0 ) then
        print*, 'inode sould be equal to or greater than 1'
        return
    elseif (inode > this%num_nodes) then
        print '(a,I0)', 'inode sould be equal to or less than total number of nodes: ', this%num_nodes
        return
    elseif (inode == this%num_nodes) then ! add to the end of list
        call this%add(item)
        return
    end if

    ! get the ith node
    nullify(pnode, new_node)
    pnode => this%head  ! for inode = 1
    do i = 2, inode        ! for inode> 1, will not execute if inode=1
        pnode => pnode%next
    end do

    allocate(new_node)
    new_node%next => pnode%next
    new_node%data = item
    pnode%next => new_node

    this%num_nodes = this%num_nodes + 1  ! increase number of nodes in the list
    write(*, fmt='(a, I0)') 'new item added after node number= ', inode
end subroutine list_add_item_after



!...............................................................................
subroutine list_get_item(this, inode, item)
    ! LIST_GET_ITEM
    ! returns the data of ith node
    ! inode: the node number
    ! item: the node data to be returned

    class(linked_list)          :: this
    integer, intent(in)         :: inode
    type(element), intent(out)  :: item

    !local variables
    integer :: i
    type(node), pointer :: pnode

    ! check for the validity of inode
    if (inode <= 0 ) then
        print*, 'negative key in list_get_item'
        return
    elseif (inode > this%num_nodes) then
        print*, 'iNode exceeds list bounds'
        return
    end if

    if (this%num_nodes == 0) then
        print*, 'list is empty'
        return
    end if

    pnode => this%head
    do i = 2, inode
        pnode => pnode%next
    end do
    item = pnode%data

end subroutine list_get_item

!...............................................................................
subroutine list_get_item_pointer(this, inode, pitem)
    ! LIST_GET_ITEM_POINTER
    ! retuens a pointer to the data part of the ith node in the list
    ! inode: the node number
    ! pitem: the pointer to node data

    class(linked_list), intent(in)    :: this
    integer, intent(in)   :: inode
    type(element), pointer, intent(out)   :: pitem

    !local variables
    integer :: i
    type(node), pointer :: pnode ! temporary pointer variable

    ! check for the validity of inode
    if (inode <= 0 ) then
        print*, 'negative key in list_get_item'
        return
    elseif (inode > this%num_nodes) then
        print*, 'iNode exceeds list bounds'
        return
    end if

    if (this%num_nodes == 0) then
        print*, 'list is empty'
        return
    end if


    pnode => this%head  ! for inode = 1
    do i = 2, inode     ! for inode> 1, will not execute if inode=1
        pnode => pnode%next
    end do

    ! set a pointer to the data of ith node
    pitem => pnode%data
    nullify(pnode)
end subroutine list_get_item_pointer

!!    !...............................................................................
!!    subroutine list_print(this)
!!        ! LIST_PRINT
!!        ! print the list from head to tail
!!
!!        class(linked_list), intent(in) :: this
!!
!!        ! local variables
!!        type(node), pointer :: pnode
!!        integer :: i
!!
!!
!!        pnode => this%head   ! point to the start of list
!!
!!        ! print all data in the list
!!        i=1
!!        do while (associated(pnode))
!!            print '(I2, 2x, a10, F8.4)', i, pnode%data%name, pnode%data%mw
!!            pnode => pnode%next
!!            i=i+1
!!        end do
!!        nullify(pnode)
!!    end subroutine list_print




