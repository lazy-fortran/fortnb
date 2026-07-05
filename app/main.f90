program fnb_main
    use notebook_parser, only: notebook_t, parse_notebook_file
    use notebook_types, only: execution_result_t
    use notebook_executor, only: execute_notebook, free_execution_results
    use notebook_renderer, only: save_notebook_markdown
    implicit none

    character(len=256) :: input_file, output_file, command
    type(notebook_t) :: notebook
    type(execution_result_t) :: results
    integer :: num_args
    logical :: execute_mode, render_mode

    num_args = command_argument_count()
    if (num_args < 1) then
        call print_usage()
        stop 1
    end if

    call get_command_argument(1, command)

    execute_mode = .false.
    render_mode = .false.
    output_file = ""

    select case (trim(command))
    case ("run")
        execute_mode = .true.
    case ("render")
        render_mode = .true.
    case ("convert")
        continue
    case default
        print *, "Error: unknown command '", trim(command), "'"
        stop 1
    end select

    if (num_args < 2) then
        print *, "Error: ", trim(command), " command requires a notebook file"
        stop 1
    end if
    call get_command_argument(2, input_file)

    call parse_options(output_file)

    call parse_notebook_file(input_file, notebook)

    if (execute_mode) then
        call run_and_report(notebook, results, input_file, output_file)
    else if (render_mode) then
        if (len_trim(output_file) == 0) then
            output_file = replace_extension(input_file, '.md')
        end if
        call render_only(notebook, results, output_file)
    else
        if (len_trim(output_file) == 0) output_file = input_file
        call render_only(notebook, results, output_file)
        print *, "Notebook converted to: ", trim(output_file)
    end if

    call free_execution_results(results)

contains

    subroutine print_usage()
        print *, "Usage: fnb <command> [options]"
        print *, ""
        print *, "Commands:"
        print *, "  run <analysis.f>          Execute notebook"
        print *, "  render <analysis.f>       Render to markdown"
        print *, "  convert <analysis.f>      Convert notebook format"
        print *, ""
        print *, "Options:"
        print *, "  -o <file>    Output file"
    end subroutine print_usage

    subroutine run_and_report(nb, res, in_file, out_file)
        type(notebook_t), intent(inout) :: nb
        type(execution_result_t), intent(out) :: res
        character(len=*), intent(in) :: in_file, out_file
        character(len=:), allocatable :: target

        call execute_notebook(nb, res)
        if (.not. res%success) then
            if (allocated(res%error_message)) then
                print *, "Error executing notebook: ", trim(res%error_message)
            else
                print *, "Error executing notebook"
            end if
            stop 1
        end if
        print *, "Notebook executed successfully"

        if (len_trim(out_file) > 0) then
            target = trim(out_file)
        else
            target = replace_extension(in_file, '.md')
        end if
        call save_notebook_markdown(nb, res, target)
        print *, "Notebook saved to: ", trim(target)
    end subroutine run_and_report

    subroutine render_only(nb, res, out_file)
        type(notebook_t), intent(inout) :: nb
        type(execution_result_t), intent(out) :: res
        character(len=*), intent(in) :: out_file
        integer :: i

        allocate (res%cells(nb%num_cells))
        res%success = .true.
        do i = 1, nb%num_cells
            res%cells(i)%success = .true.
            res%cells(i)%output = ""
        end do
        call save_notebook_markdown(nb, res, out_file)
        print *, "Notebook rendered to: ", trim(out_file)
    end subroutine render_only

    subroutine parse_options(out_file)
        character(len=*), intent(out) :: out_file
        integer :: i
        character(len=256) :: arg

        out_file = ""
        i = 3
        do while (i <= command_argument_count())
            call get_command_argument(i, arg)
            if (trim(arg) == "-o") then
                i = i + 1
                if (i <= command_argument_count()) then
                    call get_command_argument(i, out_file)
                end if
            end if
            i = i + 1
        end do
    end subroutine parse_options

    function replace_extension(filename, new_ext) result(new_filename)
        character(len=*), intent(in) :: filename, new_ext
        character(len=:), allocatable :: new_filename
        integer :: dot_pos

        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0) then
            new_filename = filename(1:dot_pos - 1)//new_ext
        else
            new_filename = trim(filename)//new_ext
        end if
    end function replace_extension

end program fnb_main
