module cache_lock
    use fpm_environment, only: get_os_type, OS_WINDOWS
    implicit none
    private

    integer, parameter :: max_wait_attempts = 100

    public :: acquire_lock, release_lock

contains

    function lock_path(cache_dir, lock_name) result(path)
        character(len=*), intent(in) :: cache_dir, lock_name
        character(len=:), allocatable :: path

        path = trim(cache_dir)//'/'//trim(lock_name)//'.lock'
    end function lock_path

    function try_acquire(path) result(acquired)
        character(len=*), intent(in) :: path
        logical :: acquired
        character(len=:), allocatable :: command
        integer :: exit_code

        if (get_os_type() == OS_WINDOWS) then
            command = 'mkdir "'//trim(path)//'" 2>nul'
        else
            command = 'mkdir "'//trim(path)//'" 2>/dev/null'
        end if
        call execute_command_line(command, exitstat=exit_code)
        acquired = (exit_code == 0)
    end function try_acquire

    function acquire_lock(cache_dir, lock_name, wait) result(acquired)
        character(len=*), intent(in) :: cache_dir, lock_name
        logical, intent(in) :: wait
        logical :: acquired
        character(len=:), allocatable :: path
        integer :: attempt

        path = lock_path(cache_dir, lock_name)

        acquired = try_acquire(path)
        if (acquired .or. .not. wait) return

        do attempt = 1, max_wait_attempts
            call execute_command_line('sleep 0.1')
            acquired = try_acquire(path)
            if (acquired) return
        end do
    end function acquire_lock

    subroutine release_lock(cache_dir, lock_name)
        character(len=*), intent(in) :: cache_dir, lock_name
        character(len=:), allocatable :: path, command

        path = lock_path(cache_dir, lock_name)
        if (get_os_type() == OS_WINDOWS) then
            command = 'rmdir /q "'//trim(path)//'" 2>nul'
        else
            command = 'rmdir "'//trim(path)//'" 2>/dev/null'
        end if
        call execute_command_line(command)
    end subroutine release_lock

end module cache_lock
