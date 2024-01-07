module to_f90_filesystem

    implicit none

contains

    !> 去掉目录路径，单独返回文件名
    function basename(filename)
        character(*), intent(in) :: filename    !! 文件名
        character(:), allocatable :: basename
        integer :: i

        if (len_trim(filename) == 0) then
            basename = ""
        else
            i = index(unix_path(filename), "/", .true.)
            if (i > 0) then
                basename = filename(i + 1:)
            else
                basename = filename
            end if
        end if

    end function basename

    !> 转化为 unix 路径, unix 路径全平台可用
    pure function unix_path(path)
        character(len=*), intent(in) :: path
        character(len=len_trim(path)) :: unix_path
        integer :: i

        do i = 1, len_trim(path)
            if (path(i:i) == "\") then
                unix_path(i:i) = "/"
            else
                unix_path(i:i) = path(i:i)
            end if
        end do

    end function unix_path

    !> 获取文件所在的目录
    function dirname(filename)
        character(*), intent(in) :: filename   !! 文件名
        character(:), allocatable :: dirname
        integer :: i

        if (len_trim(filename) == 0) then
            dirname = ""
        else
            i = index(unix_path(filename), "/", .true.)
            if (i > 0) then
                dirname = filename(1:i - 1)
            else
                dirname = "."
            end if
        end if

    end function dirname

    !> 更改文件名后缀
    function change_suffix(filename, new_suffix)
        character(*), intent(in) :: filename    !! 文件名
        character(*), intent(in) :: new_suffix  !! 新后缀
        character(:), allocatable :: change_suffix
        integer :: i

        if (len_trim(filename) == 0) then
            change_suffix = ""
        else
            change_suffix = basename(filename)
            i = index(change_suffix, ".", .true.)
            if (i > 0) then
                change_suffix = join(dirname(filename), change_suffix(1:i - 1)//"."//new_suffix)
            else
                change_suffix = join(dirname(filename), change_suffix//"."//new_suffix)
            end if
        end if

    end function change_suffix

    !> 连接路径
    pure function join(path, filename)
        character(len=*), intent(in) :: path, filename
        character(len_trim(path) + len_trim(filename) + 1) :: join

        join = trim(path)//"/"//trim(filename)

    end function join

end module to_f90_filesystem
