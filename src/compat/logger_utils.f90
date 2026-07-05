module logger_utils
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private

    public :: debug_print, print_error

contains

    subroutine debug_print(message)
        character(len=*), intent(in) :: message
        character(len=:), allocatable :: flag

        call get_env_value('FORTNB_DEBUG', flag)
        if (len_trim(flag) > 0) then
            write (error_unit, '(a)') 'DEBUG: '//trim(message)
        end if
    end subroutine debug_print

    subroutine print_error(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(a)') 'ERROR: '//trim(message)
    end subroutine print_error

    subroutine get_env_value(name, value)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: value
        integer :: length, status

        call get_environment_variable(name, length=length, status=status)
        if (status /= 0 .or. length == 0) then
            value = ''
            return
        end if
        allocate (character(len=length) :: value)
        call get_environment_variable(name, value=value)
    end subroutine get_env_value

end module logger_utils
