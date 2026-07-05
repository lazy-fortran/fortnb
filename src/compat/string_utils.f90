module string_utils
    implicit none
    private

    public :: int_to_char, logical_to_char

contains

    function int_to_char(value) result(text)
        integer, intent(in) :: value
        character(len=:), allocatable :: text
        character(len=32) :: buffer

        write (buffer, '(i0)') value
        text = trim(adjustl(buffer))
    end function int_to_char

    function logical_to_char(value) result(text)
        logical, intent(in) :: value
        character(len=:), allocatable :: text

        if (value) then
            text = 'true'
        else
            text = 'false'
        end if
    end function logical_to_char

end module string_utils
