!> 命令行参数
module to_f90_command_line

    use argparse, only: argparser
    implicit none

    !> 命令行参数
    type command_line_type
        character(len=256) :: input_file      !! 输入文件名
    contains
        procedure :: parse
    end type command_line_type

contains

    !> 解析命令行参数
    subroutine parse(self)
        class(command_line_type), intent(inout) :: self
        type(argparser) :: args
        character(*), parameter :: version = "0.1.0"

        args = argparser("Fixed format to free format converter")

        call args%set_program_name("to_f90")
        call args%add_help_option()
        call args%add_sc_option("-v", "--version", "show version information", show_version)
        call args%add_option_string("-f", "--file", "input fixed-format file name", "main.f")
        call args%parse()

        self%input_file = trim(args%get_option_string("-f"))

    contains

        subroutine show_version()

            print "(2a)", "to_f90 version ", version

        end subroutine show_version

    end subroutine parse

end module to_f90_command_line
