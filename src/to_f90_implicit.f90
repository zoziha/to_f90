module to_f90_implicit
    ! module to set and reset implicit variable types for use by to_f90.

    implicit none
    integer, save :: var_type(26) = [1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, &
                                     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ! a b c d e f g h i j k l m n o p q r s t u v w x y z
    character(len=24), save :: vt(0:7) = ['no type                 ', &
                                          'real                    ', 'integer                 ', &
                                          'double precision        ', 'logical                 ', &
                                          'complex                 ', 'character               ', &
                                          'other type              ']

contains

    subroutine reset_defaults()

        var_type(1:8) = 1              ! real (a-h)
        var_type(9:14) = 2             ! integer (i-n)
        var_type(15:26) = 1            ! real (o-z)

        return
    end subroutine reset_defaults

    subroutine set_implicit_types(text)
        ! read in implicit statement and interpret.

        character(len=*), intent(inout) :: text

        ! local variables
        integer :: ivt, length, start, i, j, pos, left, right
        logical :: first

        i = index(text, 'implicit')
        if (i > 0) text = text(i + 8:)
        text = adjustl(text)

        do
            if (text(1:4) == 'none') then
                var_type = 0
                return
            else if (text(1:4) == 'real') then
                ivt = 1
            else if (text(1:7) == 'integer') then
                ivt = 2
            else if (text(1:24) == 'double precision complex') then
                ivt = 7
                vt(7) = 'double precision complex'
            else if (text(1:16) == 'double precision') then
                ivt = 3
            else if (text(1:7) == 'logical') then
                ivt = 4
            else if (text(1:7) == 'complex') then
                ivt = 5
            else if (text(1:9) == 'character') then
                ivt = 6
            else
                ivt = 7
                i = index(text, ' ')
                vt(7) = text(1:i - 1)
            end if

            ! interpret the part in brackets, e.g. (a - h, o - z)

            length = len_trim(text)
            start = 5
            left = index(text(start:length), '(') + start - 1
            if (left < start) return
            right = index(text(start:length), ')') + start - 1
            if (right < left) return
            ! interpret text(left+1:right-1)
            first = .true.
            do pos = left + 1, right
                select case (text(pos:pos))
                case (' ')
                    cycle
                case ('-')
                    first = .false.
                case (',', ')')
                    if (first) then
                        var_type(i) = ivt
                    else
                        var_type(i:j) = ivt
                        first = .true.
                    end if
                case default
                    if (first) then
                        i = ichar(text(pos:pos)) - ichar('a') + 1
                        if (i < 1) then
                            i = ichar(text(pos:pos)) - ichar('a') + 1
                        end if
                    else
                        j = ichar(text(pos:pos)) - ichar('a') + 1
                        if (j < 1) then
                            j = ichar(text(pos:pos)) - ichar('a') + 1
                        end if
                    end if
                end select
            end do

            start = right + 1
            if (start >= length) return
            text = text(start:length)
            do
                if (text(1:1) == ',' .or. text(1:1) == ' ') then
                    text = text(2:)
                else
                    exit
                end if
            end do
        end do

        return
    end subroutine set_implicit_types

    function implicit_type(ch) result(vtype)
        ! return the variable type given the first character of its name.
        ! the first character is expected to be lower case, but just in case ..

        character(len=1), intent(in) :: ch
        character(len=24) :: vtype

        ! local variable
        integer :: i, j

        i = ichar(ch) - ichar('a') + 1
        if (i >= 1 .and. i <= 26) then
            j = var_type(i)
            vtype = vt(j)
        else
            i = ichar(ch) - ichar('a') + 1
            if (i >= 1 .and. i <= 26) then
                j = var_type(i)
                vtype = vt(j)
            else
                vtype = ' '
            end if
        end if

        return
    end function implicit_type

end module to_f90_implicit
