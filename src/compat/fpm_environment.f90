module fpm_environment
    implicit none
    private

    integer, parameter, public :: OS_UNKNOWN = 0
    integer, parameter, public :: OS_LINUX = 1
    integer, parameter, public :: OS_MACOS = 2
    integer, parameter, public :: OS_WINDOWS = 3

    public :: get_os_type, get_env

contains

    function get_os_type() result(os_type)
        integer :: os_type
        character(len=:), allocatable :: os_name

        os_name = get_env('OS', '')
        if (index(os_name, 'Windows') > 0) then
            os_type = OS_WINDOWS
            return
        end if

        if (len_trim(get_env('WINDIR', '')) > 0) then
            os_type = OS_WINDOWS
        else
            os_type = OS_LINUX
        end if
    end function get_os_type

    function get_env(name, default) result(value)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: default
        character(len=:), allocatable :: value
        integer :: length, status

        call get_environment_variable(name, length=length, status=status)
        if (status /= 0 .or. length == 0) then
            if (present(default)) then
                value = default
            else
                value = ''
            end if
            return
        end if

        allocate (character(len=length) :: value)
        call get_environment_variable(name, value=value)
    end function get_env

end module fpm_environment
