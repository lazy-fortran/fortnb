module system_utils
    use fpm_environment, only: get_os_type, OS_WINDOWS
    implicit none
    private

    public :: sys_remove_file, sys_remove_dir, sys_get_current_dir
    public :: escape_shell_arg

contains

    subroutine sys_remove_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, iostat
        logical :: exists

        inquire (file=path, exist=exists)
        if (.not. exists) return

        open (newunit=unit, file=path, status='old', iostat=iostat)
        if (iostat == 0) then
            close (unit, status='delete')
        end if
    end subroutine sys_remove_file

    subroutine sys_remove_dir(path)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: command

        if (len_trim(path) == 0) return

        if (get_os_type() == OS_WINDOWS) then
            command = 'rmdir /s /q "'//trim(path)//'" >nul 2>&1'
        else
            command = 'rm -rf "'//trim(escape_shell_arg(path))//'"'
        end if
        call execute_command_line(command)
    end subroutine sys_remove_dir

    subroutine sys_get_current_dir(directory)
        character(len=*), intent(out) :: directory
        integer :: status

        call getcwd(directory, status)
        if (status /= 0) directory = '.'
    end subroutine sys_get_current_dir

    function escape_shell_arg(arg) result(escaped)
        character(len=*), intent(in) :: arg
        character(len=:), allocatable :: escaped
        integer :: i
        character(len=1) :: c

        escaped = ''
        do i = 1, len_trim(arg)
            c = arg(i:i)
            if (c == '"' .or. c == '\' .or. c == '$' .or. c == '`') then
                escaped = escaped//'\'//c
            else
                escaped = escaped//c
            end if
        end do
    end function escape_shell_arg

end module system_utils
