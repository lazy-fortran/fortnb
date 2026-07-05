module fpm_filesystem
    implicit none
    private

    public :: join_path

contains

    function join_path(head, tail) result(path)
        character(len=*), intent(in) :: head, tail
        character(len=:), allocatable :: path

        if (len_trim(head) == 0) then
            path = trim(tail)
        else if (head(len_trim(head):len_trim(head)) == '/') then
            path = trim(head)//trim(tail)
        else
            path = trim(head)//'/'//trim(tail)
        end if
    end function join_path

end module fpm_filesystem
