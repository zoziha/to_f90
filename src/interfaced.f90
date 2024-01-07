module interfaced
    ! module containing formely external text processing procedures with explicit
    ! interfaces declared in main program to_f90.

    implicit none

contains

    subroutine mark_text(text, n_marks, pos1, pos2, continuation)

        ! look for exclamation marks or quotes to find any text which must be
        ! protected from case changes.
        ! it is assumed that strings are not continued from one line to the next.
        implicit none

        character(len=*), intent(in) :: text
        logical, intent(in) :: continuation
        integer, intent(out) :: n_marks, pos1(:), pos2(:)

        ! local variables
        integer :: mark, start, pos_exclaim, pos_sngl_quote, pos_dbl_quote, pos, &
                   endpos
        character(len=1), save :: quote
        logical, save :: protect = .false.

        mark = 1
        start = 1
        if (continuation .and. protect) then
            pos1(mark) = 1
            pos = 0
            go to 110
        end if

        ! find next opening quote or exclamation mark

100     protect = .false.
        pos_exclaim = index(text(start:80), '!')
        pos_sngl_quote = index(text(start:80), '''')
        pos_dbl_quote = index(text(start:80), '"')
        if (pos_exclaim == 0) pos_exclaim = 81
        if (pos_sngl_quote == 0) pos_sngl_quote = 81
        if (pos_dbl_quote == 0) pos_dbl_quote = 81
        pos1(mark) = min(pos_exclaim, pos_sngl_quote, pos_dbl_quote)

        if (pos1(mark) == 81) then       ! no more protected regions
            n_marks = mark - 1
            return
        else if (pos_exclaim == pos1(mark)) then ! rest of line is a comment
            pos1(mark) = pos1(mark) + start - 1
            pos2(mark) = 80
            n_marks = mark
            return
        end if

        pos = start - 1 + pos1(mark)
        pos1(mark) = pos
        quote = text(pos:pos)

        ! search for matching quote

110     endpos = index(text(pos + 1:), quote)
        if (endpos > 0) then
            pos2(mark) = pos + endpos
            start = pos2(mark) + 1
            mark = mark + 1
            go to 100
        end if

        ! no matching end quote - it should be on the next line

        pos2(mark) = 80
        n_marks = mark
        protect = .true.

        return
    end subroutine mark_text

    subroutine convert_text(text, n_marks, pos1, pos2)

        ! convert unprotected text to upper case if it is a fortran word,
        ! otherwise convert to lower case.
        implicit none

        character(len=*), intent(inout) :: text
        integer, intent(in) :: n_marks
        integer, intent(inout) :: pos1(:), pos2(:)

        ! local variables

        integer :: length, inc = ichar('a') - ichar('a'), pos, mark, i, i1, j, j1, &
                   j2, ptr
        logical :: matched
        character(len=11) :: fortran_word(186) = ['abs        ', 'access     ', &
                                                  'acos       ', 'aimag      ', 'aint       ', 'alog       ', &
                                                  'alog10     ', 'amax0      ', 'amax1      ', 'amin0      ', &
                                                  'amin1      ', 'amod       ', 'and        ', 'anint      ', &
                                                  'append     ', 'asin       ', 'assign     ', 'atan       ', &
                                                  'atan2      ', 'backspace  ', 'blank      ', 'block      ', &
                                                  'blockdata  ', 'blocksize  ', 'call       ', 'ccos       ', &
                                                  'cdabs      ', 'cdcos      ', 'cdexp      ', 'cdlog      ', &
                                                  'cdsin      ', 'cdsqrt     ', 'cexp       ', 'char       ', &
                                                  'character  ', 'clog       ', 'close      ', 'cmplx      ', &
                                                  'common     ', 'complex    ', 'conjg      ', 'continue   ', &
                                                  'cos        ', 'cosh       ', 'csin       ', 'csqrt      ', &
                                                  'dabs       ', 'dacos      ', 'dasin      ', 'data       ', &
                                                  'datan      ', 'datan2     ', 'dble       ', 'dcmplx     ', &
                                                  'dconjg     ', 'dcos       ', 'dcosh      ', 'delete     ', &
                                                  'dexp       ', 'dimag      ', 'dint       ', 'direct     ', &
                                                  'dlog       ', 'dlog10     ', 'dmax1      ', 'dimension  ', &
                                                  'dmin1      ', 'dmod       ', 'dnint      ', 'do         ', &
                                                  'double     ', 'dsign      ', 'dsin       ', 'dsinh      ', &
                                                  'dsqrt      ', 'dtan       ', 'dtanh      ', 'else       ', &
                                                  'elseif     ', 'end        ', 'endfile    ', 'endif      ', &
                                                  'entry      ', 'eq         ', 'equivalence', 'eqv        ', &
                                                  'err        ', 'exist      ', 'exit       ', 'exp        ', &
                                                  'external   ', 'file       ', 'float      ', 'fmt        ', &
                                                  'form       ', 'format     ', 'formatted  ', 'function   ', &
                                                  'ge         ', 'goto       ', 'go         ', 'gt         ', &
                                                  'iabs       ', 'iand       ', 'ichar      ', 'idint      ', &
                                                  'idnint     ', 'ieor       ', 'if         ', 'ifix       ', &
                                                  'implicit   ', 'include    ', 'index      ', 'input      ', &
                                                  'inquire    ', 'int        ', 'integer    ', 'intrinsic  ', &
                                                  'iostat     ', 'isign      ', 'keep       ', 'le         ', &
                                                  'len        ', 'lge        ', 'lgt        ', 'lle        ', &
                                                  'llt        ', 'log        ', 'log10      ', 'logical    ', &
                                                  'lt         ', 'max        ', 'max0       ', 'max1       ', &
                                                  'min        ', 'min0       ', 'min1       ', 'mod        ', &
                                                  'name       ', 'namelist   ', 'named      ', 'ne         ', &
                                                  'neqv       ', 'new        ', 'nextrec    ', 'none       ', &
                                                  'not        ', 'number     ', 'old        ', 'open       ', &
                                                  'opened     ', 'or         ', 'parameter  ', 'pause      ', &
                                                  'position   ', 'precision  ', 'print      ', 'program    ', &
                                                  'read       ', 'real       ', 'rec        ', 'recl       ', &
                                                  'return     ', 'rewind     ', 'save       ', 'scratch    ', &
                                                  'sequential ', 'sign       ', 'sin        ', 'sinh       ', &
                                                  'sngl       ', 'space      ', 'sqrt       ', 'status     ', &
                                                  'stop       ', 'subroutine ', 'tan        ', 'tanh       ', &
                                                  'then       ', 'to         ', 'type       ', 'unformatted', &
                                                  'unit       ', 'unknown    ', 'while      ', 'write      ']
        character(len=4) :: compare(6) = ['.lt.', '.le.', '.eq.', '.ge.', &
                                          '.gt.', '.ne.']
        character(len=2) :: replacement(6) = ['< ', '<=', '==', '>=', '> ', &
                                              '/=']

        ! a   b   c   d   e   f   g    h    i    j    k    l    m    n    o
        ! p    q    r    s    t    u    v    w    x    y    z
        integer, parameter :: indx(27) = [1, 20, 25, 47, 78, 92, 99, 103, 103, &
                                          121, 121, 122, 132, 139, 149, 153, 159, 159, 165, 177, 182, 185, 185, &
                                          187, 187, 187, 187]

        if (pos1(1) == 1 .and. pos2(1) == 80) return ! entire line protected

        pos = 1
        mark = 1
        length = len_trim(text)
        do                             ! convert to upper case
            if (n_marks >= mark .and. pos == pos1(mark)) then
                pos = pos2(mark) + 1
                mark = mark + 1
                if (pos >= length) exit
            end if
            if (text(pos:pos) >= 'a' .and. text(pos:pos) <= 'z') text(pos:pos) &
                = char(ichar(text(pos:pos)) + inc)
            pos = pos + 1
            if (pos > length) exit
        end do

        ! search for `words' in text.
        ! convert to lower case if they are not fortran words.
        i1 = 1
        pos = 1
        mark = 1
        do
            if (pos > length) exit
            if (n_marks >= mark .and. pos >= pos1(mark)) then
                pos = pos2(mark) + 1
                i1 = pos
                mark = mark + 1
                if (pos >= length) exit
            end if

            do
                if ((text(pos:pos) >= 'a' .and. text(pos:pos) <= 'z') .or. (text( &
                                                                            pos:pos) >= '0' .and. text(pos:pos) <= '9') .or. text(pos:pos) == '_') &
                    then
                    pos = pos + 1
                    cycle
                else
                    exit
                end if
            end do

            pos = pos - 1
            ! now i1 & pos = positions of 1st & last characters of current string

            if (pos < i1) then             ! single non-alphanumeric character
                pos = i1 + 1
                i1 = pos
                cycle
            end if

            ptr = ichar(text(i1:i1)) - ichar('a') + 1
            if (ptr < 1 .or. ptr > 26) then
                pos = pos + 1
                if (pos > length) exit
                i1 = pos
                cycle
            end if

            matched = .false.
            if (pos > i1) then
                j1 = indx(ptr)
                j2 = indx(ptr + 1) - 1
                do j = j1, j2
                    if (text(i1:pos) == fortran_word(j)) then
                        matched = .true.
                        exit
                    end if
                end do
            end if

            ! replace .lt. with <, etc.
            if (matched .and. i1 > 1) then
                if (text(i1 - 1:i1 - 1) == '.') then
                    do j = 1, 6
                        if (text(i1 - 1:pos + 1) == compare(j)) then
                            text(i1 - 1:pos + 1) = ' '//replacement(j)//' '
                            exit
                        end if
                    end do
                    do                       ! remove excess blanks
                        i1 = max(i1, 3)
                        j1 = index(text(i1 - 2:pos + 2), '  ')
                        if (j1 == 0) exit
                        j1 = j1 + i1 - 3
                        text(j1:) = text(j1 + 1:)
                        pos2(mark) = pos2(mark) - 1 ! adjust mark positions
                        do i = mark + 1, n_marks
                            pos1(i) = pos1(i) - 1
                            pos2(i) = pos2(i) - 1
                        end do
                        pos = pos - 1
                    end do
                end if
            end if

            ! output line of text to screen if it contains subroutine or function.
            ! convert endif to end if, elseif to else if, and goto to go to.
            if (matched) then
                if (text(i1:pos) == 'subroutine' .or. text(i1:pos) == 'function') then
                    write (*, '(1x, a)') text(1:length)
                else if (text(i1:pos) == 'endif') then
                    text(i1:) = 'end if'//text(pos + 1:)
                    pos = pos + 1
                else if (text(i1:pos) == 'elseif') then
                    text(i1:) = 'else if'//text(pos + 1:)
                    pos = pos + 1
                else if (text(i1:pos) == 'goto') then
                    text(i1:) = 'go to'//text(pos + 1:)
                    pos = pos + 1
                end if
            end if

            ! if text is not matched, convert to lower case, if necessary.
            if (.not. matched) then
                do j = i1, pos
                    if (text(j:j) >= 'a' .and. text(j:j) <= 'z') text(j:j) &
                        = char(ichar(text(j:j)) - inc)
                end do
            end if

            pos = pos + 1
            if (pos > length) exit
            i1 = pos
        end do

        return
    end subroutine convert_text

    subroutine remove_data_blanks(text)
        ! remove any blanks embedded between numerical digits in data statements

        implicit none
        character(len=*), intent(inout) :: text

        ! local variables
        integer :: length, pos, i1
        character(len=10) :: numbers = '1234567890'

        length = len_trim(text)
        i1 = 2
        do
            pos = index(text(i1:length), ' ')
            if (pos == 0) exit
            i1 = i1 + pos - 1
            if (scan(text(i1 - 1:i1 - 1), numbers) > 0 .and. scan(text(i1 + 1:i1 + &
                                                                       1), numbers) > 0) then
                text = text(:i1 - 1)//text(i1 + 1:length)
                length = length - 1
            end if
            i1 = i1 + 2
            if (i1 > length) exit
        end do

        return
    end subroutine remove_data_blanks

    function last_char(text) result(ch)
        ! return the last character on a line
        implicit none

        character(len=*), intent(in) :: text
        character(len=1) :: ch

        ! local variable
        integer :: last

        last = len_trim(text)
        if (last == 0) then
            ch = ' '
        else
            ch = text(last:last)
        end if

        return
    end function last_char

    function find_delimited_name(text, name) result(pos)
        ! find a name in a character string with delimiters either side of it,
        ! or after it if it starts at position 1.
        ! an extended version of the intrinsic index.
        ! pos = the position of the first character of name in text (= 0 if not
        ! found).
        ! n.b. when the name is short (e.g. i or n) it could occur as part of some
        ! other name.

        implicit none
        character(len=*), intent(in) :: text, name
        integer :: pos

        ! local variables
        integer :: i1, ltext, lname

        i1 = 1
        ltext = len_trim(text)
        lname = len_trim(name)
        do
            pos = index(text(i1:ltext), trim(name))
            if (pos == 0) return
            pos = pos + i1 - 1
            if (pos > 1) then
                if (scan(text(pos - 1:pos - 1), ' <=+-/*,') > 0) then
                    if (scan(text(pos + lname:pos + lname), ' >=(+-/*,') > 0) return
                end if
            else
                if (scan(text(pos + lname:pos + lname), ' >=(+-/*,') > 0) return
            end if
            i1 = pos + lname
            if (i1 + lname > ltext) exit
        end do

        pos = 0

        return
    end function find_delimited_name

end module interfaced
