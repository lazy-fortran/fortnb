module cache
    use fx_hash, only: sha256_string
    use fpm_environment, only: get_env
    use temp_utils, only: mkdir
    implicit none
    private

    public :: get_cache_dir, get_content_hash, ensure_cache_structure

contains

    function get_cache_dir() result(dir)
        character(len=:), allocatable :: dir
        character(len=:), allocatable :: base

        base = get_env('XDG_CACHE_HOME', '')
        if (len_trim(base) == 0) then
            base = get_env('HOME', '')
            if (len_trim(base) == 0) then
                base = '/tmp'
            else
                base = trim(base)//'/.cache'
            end if
        end if
        dir = trim(base)//'/fortnb'
    end function get_cache_dir

    function get_content_hash(content) result(hash)
        character(len=*), intent(in) :: content
        character(len=64) :: hash

        hash = sha256_string(content)
    end function get_content_hash

    subroutine ensure_cache_structure(cache_dir, success)
        character(len=*), intent(in) :: cache_dir
        logical, intent(out) :: success
        logical :: exists

        call mkdir(cache_dir)
        inquire (file=trim(cache_dir), exist=exists)
        success = exists
    end subroutine ensure_cache_structure

end module cache
