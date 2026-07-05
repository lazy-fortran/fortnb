module temp_utils
    use, intrinsic :: iso_c_binding, only: c_int
    use fpm_environment, only: get_os_type, OS_WINDOWS, get_env
    use system_utils, only: sys_remove_dir
    implicit none
    private

    integer, save :: unique_counter = 0

    interface
        function c_getpid() bind(C, name="getpid")
            import :: c_int
            integer(c_int) :: c_getpid
        end function c_getpid
    end interface

    type, public :: temp_dir_manager
        character(len=:), allocatable :: path
    contains
        procedure :: create => temp_dir_manager_create
        procedure :: get_path => temp_dir_manager_get_path
        procedure :: get_file_path => temp_dir_manager_get_file_path
        final :: temp_dir_manager_final
    end type temp_dir_manager

    public :: create_temp_dir, create_temp_file, create_test_cache_dir
    public :: cleanup_temp_dir, mkdir

contains

    subroutine temp_dir_manager_create(this, prefix)
        class(temp_dir_manager), intent(inout) :: this
        character(len=*), intent(in) :: prefix

        this%path = create_temp_dir(prefix)
    end subroutine temp_dir_manager_create

    subroutine ensure_created(this)
        class(temp_dir_manager), intent(inout) :: this

        if (.not. allocated(this%path)) then
            this%path = create_temp_dir('fortnb')
        end if
    end subroutine ensure_created

    function temp_dir_manager_get_path(this) result(path)
        class(temp_dir_manager), intent(inout) :: this
        character(len=:), allocatable :: path

        call ensure_created(this)
        path = this%path
    end function temp_dir_manager_get_path

    function temp_dir_manager_get_file_path(this, name) result(path)
        class(temp_dir_manager), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: path

        call ensure_created(this)
        path = this%path//'/'//trim(name)
    end function temp_dir_manager_get_file_path

    subroutine temp_dir_manager_final(this)
        type(temp_dir_manager), intent(inout) :: this

        if (allocated(this%path)) then
            call sys_remove_dir(this%path)
            deallocate (this%path)
        end if
    end subroutine temp_dir_manager_final

    function temp_base_dir() result(base)
        character(len=:), allocatable :: base

        base = get_env('TMPDIR', '')
        if (len_trim(base) == 0) base = '/tmp'
        if (base(len_trim(base):len_trim(base)) == '/') then
            base = base(1:len_trim(base) - 1)
        end if
    end function temp_base_dir

    function unique_suffix() result(suffix)
        character(len=:), allocatable :: suffix
        character(len=32) :: pid_str, count_str

        unique_counter = unique_counter + 1
        write (pid_str, '(i0)') int(c_getpid())
        write (count_str, '(i0)') unique_counter
        suffix = trim(pid_str)//'_'//trim(count_str)
    end function unique_suffix

    function create_temp_dir(prefix) result(dir)
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable :: dir

        dir = temp_base_dir()//'/'//trim(prefix)//'_'//unique_suffix()
        call mkdir(dir)
    end function create_temp_dir

    function create_test_cache_dir(name) result(dir)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: dir

        dir = create_temp_dir('nb_cache_'//trim(name))
    end function create_test_cache_dir

    function create_temp_file(prefix, suffix) result(path)
        character(len=*), intent(in) :: prefix, suffix
        character(len=:), allocatable :: path

        path = temp_base_dir()//'/'//trim(prefix)//'_'//unique_suffix()//trim(suffix)
    end function create_temp_file

    subroutine cleanup_temp_dir(dir)
        character(len=*), intent(in) :: dir

        call sys_remove_dir(dir)
    end subroutine cleanup_temp_dir

    subroutine mkdir(path)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: command

        if (len_trim(path) == 0) return

        if (get_os_type() == OS_WINDOWS) then
            command = 'if not exist "'//trim(path)//'" mkdir "'//trim(path)//'"'
        else
            command = 'mkdir -p "'//trim(path)//'"'
        end if
        call execute_command_line(command)
    end subroutine mkdir

end module temp_utils
