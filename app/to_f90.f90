program to_f90

    use to_f90_implicit
    ! jbdv: replaced the explicit interface block within, and external
    ! procedures at the end of this program, with a module called `interfaced`.
    ! `use`'ing the module here. may be a glaring oversight, but i don't see any
    ! reason for not doing so?
    use to_f90_interfaced
    use to_f90_command_line, only: command_line_type
    use to_f90_string, only: split
    implicit none

    type :: code
        character(len=140) :: text
        character(len=5) :: label
        type(code), pointer :: next
    end type code

    type :: argument
        character(len=10) :: name
        integer :: intention           ! in = 1, out = 2, in out = 3
        character(len=24) :: var_type ! room for double precision complex
        integer :: dim                 ! dim = 0 for scalars
        character(len=24) :: dimensions ! not used if dim = 0
        type(argument), pointer :: next
    end type argument

    character(len=60) :: f77_name, f90_name
    character(len=1) :: tab = char(9), ch
    character(len=50) :: prog_unit_name = ' ', blank = ' ', case_expr
    character(len=9) :: delimiters = ' =+-*/,()'
    character(len=10) :: numbers = '1234567890'
    character(len=5) :: lab
    character(len=30) :: text, vtype
    character(len=140) :: statement
    character(len=8) :: date
    character(len=10) :: time
    integer :: iostatus, pos, count, last, n_marks, pos1(20), pos2(20), &
               lab_length, indent, i, i1, i2, length, numb_arg, i3, i4
    type(code), pointer :: head, current, tail, last_line, next_line, &
                           first_decl, last_decl, start_prog_unit, end_prog_unit
    logical :: asterisk, ok, data_stmnt, first_arg, continuation
    type(argument), pointer :: arg_start, arg, last_arg
    type(command_line_type) :: command_line
    character(:), allocatable :: split_text(:)

    call command_line%parse()
    call split(command_line%input_file, ".f", split_text)

    f77_name = command_line%input_file
    if (len_trim(f77_name) == 0) stop
    open (8, file=f77_name, status='old', iostat=iostatus)
    if (iostatus /= 0) then
        write (*, *) '** unable to open file: ', f77_name
        stop
    end if

    f90_name = trim(split_text(1))//'.f90'
    open (9, file=f90_name)

    ! set up a linked list containing the lines of code

    nullify (head, tail)
    allocate (head)
    tail => head
    read (8, '(a)') head%text
    if (head%text(1:1) == 'c' .or. head%text(1:1) == 'c' .or. head%text(1:1) == '*') &
        then
        head%text(1:1) = '!'
    else if (head%text(1:1) == tab) then
        head%text = '      '//head%text(2:)
    end if
    head%label = ' '
    count = 1

    do
        nullify (current)
        allocate (current)
        read (8, '(a)', iostat=iostatus) current%text
        if (iostatus /= 0) exit

        ! change c, c or * in column 1 to !
        if (current%text(1:1) == 'c' .or. current%text(1:1) == 'c' .or. &
            current%text(1:1) == '*') then
            if (len_trim(current%text) > 1) then
                current%text(1:1) = '!'
            else
                current%text = ' '       ! leave blank if nothing else on line
            end if
            current%label = ' '
        else
            current%label = adjustl(current%text(1:5))
        end if

        count = count + 1
        if (current%label(1:1) == tab) then ! expand tabs
            current%label = ' '
            current%text = '      '//current%text(2:)
        else if (current%label(1:1) == '!') then
            current%label = ' '
        else
            current%label = adjustl(current%label)
        end if

        nullify (current%next)
        tail%next => current
        tail => current
    end do

    write (*, *) 'no. of lines read =', count

    ! ---------------------------------------------------------------------------

    current => head
    nullify (last_line)
    data_stmnt = .false.

    do
        ! look for blanks in columns 1-5 followed by non-blank in column 6.
        ! if found, add an ampersand at the end of the previous line.

        if (current%label == '     ' .and. current%text(6:6) /= ' ' .and. &
            current%text(1:1) /= '!') then
            last = len_trim(last_line%text)
            last_line%text(last + 3:last + 3) = '&'
            current%text(6:6) = ' '
            continuation = .true.
        else
            data_stmnt = .false.
            continuation = .false.
        end if

        ! replace tabs with single spaces
        do
            pos = index(current%text, tab)
            if (pos == 0) exit
            current%text(pos:pos) = ' '
        end do

        ! remove leading blanks
        current%text = adjustl(current%text)

        ! mark regions of text which must not have their case changed.
        call mark_text(current%text, n_marks, pos1, pos2, continuation)

        ! convert cases of regions which are not protected.
        call convert_text(current%text, n_marks, pos1, pos2)

        ! if line is start of a program unit, record its name
        if (current%text(1:7) == 'program') then
            prog_unit_name = current%text(1:50)
        else if (current%text(1:10) == 'subroutine') then
            pos = index(current%text, '(') - 1
            if (pos < 0) pos = len_trim(current%text)
            prog_unit_name = current%text(1:pos)
        else if (current%text(1:9) == 'blockdata') then
            prog_unit_name = current%text(1:50)
        else
            ! n.b. 'function' could be part of a comment
            pos = index(current%text, 'function')
            if (pos > 0 .and. index(current%text, '!') == 0 .and. &
                index(current%text, '''') == 0) then
                last = index(current%text, '(') - 1
                if (last < 0) last = len_trim(current%text)
                prog_unit_name = current%text(pos:last)
            end if
        end if

        ! if first word is one of integer, real, double precision, character ,
        ! logical or complex, add :: unless function appears on the same line
        ! or next non-blank character is '*' as in real*8.
        if (index(current%text, 'function') == 0) then
            pos = 0
            if (index(current%text, 'integer') == 1) then
                pos = 9
            else if (index(current%text, 'real') == 1) then
                pos = 6
            else if (index(current%text, 'double precision') == 1) then
                pos = 18
            else if (index(current%text, 'character') == 1) then
                pos = 11
            else if (index(current%text, 'complex') == 1) then
                pos = 9
            else if (index(current%text, 'logical') == 1) then
                pos = 9
            end if

            if (pos > 0) then
                asterisk = index(current%text(pos - 1:pos), '*') > 0
                if (.not. asterisk) then
                    if (pos /= 11) then
                        current%text = current%text(1:pos - 1)//':: '// &
                                       adjustl(current%text(pos:))
                    else                   ! character type, default length = 1
                        current%text = 'character (len=1) :: '// &
                                       adjustl(current%text(pos:))
                    end if
                else
                    if (pos == 11) then      ! character * found
                        i1 = index(current%text, '*') + 1
                        length = len_trim(current%text)
                        ! get length, could be (*)
                        do
                            if (current%text(i1:i1) /= ' ') exit
                            if (i1 >= length) exit
                            i1 = i1 + 1
                        end do
                        if (current%text(i1:i1) == '(') then
                            i1 = i1 + 1
                            i2 = index(current%text, ')') - 1
                        else
                            i2 = index(current%text(i1:), ' ') + i1 - 2
                        end if
                        current%text = 'character (len='//current%text(i1:i2)// &
                                       ') :: '//adjustl(current%text(i2 + 2:))
                    end if
                end if
                ! check for 2 or more lengths in character declaration.
                ! e.g. character a, b, c*10, d
                ! put 2nd (& later) declarations on separate lines:
                ! character*10 c
                ! but check for character*10 a(*) where last * is not a
                ! length but a dimension
                if (pos == 11) then
                    pos = index(current%text, '::') + 2
                    do
                        i = index(current%text(pos:), '*')
                        if (i == 0) exit
                        i = i + pos - 1
                        length = len_trim(current%text)
                        i1 = index(current%text(:i - 1), ',', back=.true.)
                        i1 = max(pos, i1)
                        i2 = index(current%text(i + 1:), ',')
                        if (i2 == 0) then
                            i2 = length + 1
                        else
                            i2 = i2 + i
                        end if
                        ! i1, i2 mark commas at beginning & end of `, name*xx,'
                        ! but we could have `name(xx, *), '
                        ! test for * after ( , or ) before ,
                        i3 = index(current%text(i1:i2), '(')
                        i4 = index(current%text(i1:), ')')
                        if (i3 > 0) then
                            i4 = i4 + i1 - 1
                            i2 = index(current%text(i4 + 1:), ',')
                            if (i2 == 0) then
                                i2 = length + 1
                            else
                                i2 = i2 + i4
                            end if
                            pos = i2 + 1
                            cycle
                        else if (i4 > 0) then
                            i4 = i4 + i1 - 1
                            i2 = index(current%text(i4 + 1:), ',')
                            if (i2 == 0) then
                                i2 = length + 1
                            else
                                i2 = i2 + i4
                            end if
                            pos = i2 + 1
                            cycle
                        end if

                        if (i1 == pos .and. i2 == length + 1) then
                            ! only one declaration left on line, e.g.
                            ! character :: name*50
                            current%text = 'character (len='//current%text(i + 1:length) &
                                           //') :: '//adjustl(current%text(i1:i - 1))
                            exit
                        end if

                        allocate (next_line)
                        next_line%next => current%next
                        current%next => next_line
                        next_line%text = 'character'//current%text(i:i2 - 1)//' '// &
                                         current%text(i1 + 1:i - 1)
                        if (i2 < length) then
                            current%text = current%text(:i1)//current%text(i2 + 1:length)
                        else
                            current%text = current%text(:i1 - 1)
                        end if
                    end do
                end if
            end if
        end if

        ! if this is in a data statement, eliminate any blanks within numbers
        if (data_stmnt .or. current%text(1:4) == 'data') then
            call remove_data_blanks(current%text)
            last = len_trim(current%text)
            data_stmnt = .true.
        end if

        ! if line only contains 'end', add the program unit name
        if (len_trim(current%text) == 3 .and. current%text(1:3) == 'end') then
            current%text = current%text(1:3)//' '//prog_unit_name
            prog_unit_name = ' '

            ! convert `enddo' to 'end do'
        else if (current%text(1:5) == 'enddo') then
            current%text = 'end do'//current%text(6:)
        end if

        last_line => current
        if (associated(current, tail)) exit
        if (.not. associated(current)) exit
        current => current%next
    end do

    ! -------------------------------------------------------------------------

    ! now convert do-loops

    current => head
    write (*, *) '      converting do-loops, 3-way ifs, & computed go tos'
    do
        if (current%text(1:1) /= '!' .and. current%text(1:1) /= ' ') then
            pos = index(current%text, 'do')
            if (pos > 0 .and. (current%text(pos + 2:pos + 2) == ' ' .or. current%text(pos + &
                                                                                      2:pos + 2) == ',')) then
                if (current%text(pos + 2:pos + 2) == ',') current%text(pos + 2:pos + 2) = ' '
                if (pos == 1) then
                    ok = .true.
                else if (scan(current%text(pos - 1:pos - 1), delimiters) > 0) then
                    ok = index(current%text(:pos - 1), 'end ') == 0
                else
                    ok = .false.
                end if
                if (ok) then
                    text = adjustl(current%text(pos + 3:))
                    last = index(text, ' ')
                    lab = text(:last - 1)
                    if (scan(lab(1:1), numbers) == 0) lab = ' '
                    lab_length = len_trim(lab)
                    if (lab_length > 0) then
                        pos = index(lab, ',') ! check for a comma after label
                        if (pos > 0) then
                            lab(pos:) = ' '
                            i = index(current%text, ',')
                            current%text(i:i) = ' '
                            lab_length = pos - 1
                        end if
                        call do_loop_fixup(current, lab)
                    end if
                end if

                ! test for computed go to
            else if (index(current%text, 'go to') > 0) then
                i1 = index(current%text, 'go to')
                statement = adjustl(current%text(i1 + 5:))
                ! test for a `('
                if (statement(1:1) == '(') then
                    ok = .true.
                    ! if current line is continued, try appending
                    ! the next line
                    if (last_char(statement) == '&') then
                        next_line => current%next
                        length = len_trim(statement) + len_trim(next_line%text)
                        ok = (length <= 141) .and. (last_char(next_line%text) /= '&')
                        if (ok) then
                            pos = len_trim(statement)
                            statement = trim(statement(:pos - 1))//trim(next_line%text)
                            current%next => next_line%next
                            deallocate (next_line)
                        end if
                    end if

                    if (ok) then
                        ! check for comma between ( and )
                        pos = index(statement, ')')
                        if (index(statement(2:pos - 1), ',') > 0) then
                            ! we could have something like:
                            ! if (condition) go to (100, 200, 300) ivar
                            ! before doing any more, split into 3 lines:
                            ! if (condition) then
                            ! go to (100, 200, 300) ivar
                            ! end if
                            if (current%text(1:2) == 'if') then
                                if (current%text(3:3) == ' ' .or. current%text(3:3) == '(') then
                                    current%text = current%text(:i1 - 1)//'then'
                                    i1 = 2
                                    call insert_and_moveto_newline(current)
                                    current%text = ' '
                                    next_line => current
                                    call insert_and_moveto_newline(next_line)
                                    next_line%text = 'end if'
                                end if
                            end if
                            ! get the case variable or expression
                            case_expr = adjustl(statement(pos + 1:))
                            if (case_expr(1:1) == ',') case_expr = adjustl(case_expr(2:))
                            current%text = current%text(:i1 - 1)//'select case ( '// &
                                           trim(case_expr)//' )'
                            ! put in pairs of lines:  case ( i )
                            ! go to i-th label
                            call goto_cases(statement(2:pos - 1))
                        end if
                    end if
                end if

                ! look for if, then a number as last non-blank character
            else
                pos = index(current%text, 'if')
                if (pos > 0) then
                    last = len_trim(current%text)
                    if (scan(current%text(last:last), numbers) > 0) then
                        call fix_3way_if(current)
                    end if
                end if
            end if
        end if

        if (associated(current, tail)) exit
        if (.not. associated(current)) exit
        current => current%next
    end do

    ! -------------------------------------------------------------------------

    ! determine intents for dummy arguments

    write (*, *) '      determining intents of dummy arguments'

    ! search for either function or subroutine.
    ! extract name of program unit.

    current => head
    nullify (last_line)
    outer_loop: do
        do
            if (current%text(1:1) /= '!' .and. current%text(1:1) /= ' ') then
                if (current%text(1:10) == 'subroutine') then
                    pos = index(current%text, '(') - 1
                    if (pos < 0) pos = len_trim(current%text)
                    prog_unit_name = current%text(1:pos)
                    exit
                else
                    pos = index(current%text, 'function')
                    if (pos > 0) then
                        last = index(current%text, '(') - 1
                        if (last < 0) last = len_trim(current%text)
                        prog_unit_name = current%text(pos:last)
                        exit
                    end if
                end if
            end if

            last_line => current
            current => current%next
            if (associated(current, tail)) exit outer_loop
        end do

        ! if there is no blank line between this program unit and the previous
        ! one, then insert one.

        if (associated(last_line)) then
            if (len_trim(last_line%text) > 0) then
                call insert_and_moveto_newline(last_line)
                last_line%text = ' '
            end if
        end if

        allocate (start_prog_unit)
        start_prog_unit => current

        ! find end of program unit

        do
            current => current%next
            if (current%text(1:1) /= '!' .and. current%text(1:1) /= ' ') then
                if (current%text(1:3) == 'end') then
                    if (index(current%text(5:), prog_unit_name) > 0) then
                        allocate (end_prog_unit)
                        end_prog_unit => current
                        exit
                    end if
                end if
            end if
            if (associated(current, tail)) exit outer_loop
        end do

        ! find first & last declarations

        allocate (first_decl, last_decl)
        call find_declarations(start_prog_unit, end_prog_unit, first_decl, &
                               last_decl)
        if (.not. associated(last_decl)) go to 100

        ! extract list of dummy arguments

        call get_arg_list()
        if (numb_arg == 0) go to 100

        ! see if the declarations contain any implicit statements

        call reset_defaults()
        current => first_decl
        do
            if (current%text(1:8) == 'implicit') then
                statement = current%text(10:)
                call set_implicit_types(statement)
            end if
            if (associated(current, last_decl)) exit
            current => current%next
        end do

        ! search through the declarations for variable types & dimensions

        call get_var_types()

        ! search through rest of code to try to determine the intents

        call get_intents()

        ! insert intent statements

        statement = first_decl%text
        first_decl%text = ' '
        current => first_decl
        arg => arg_start
        do
            call insert_and_moveto_newline(current)
            current%text = arg%var_type
            select case (arg%intention)
            case (0, 3)
                current%text = trim(current%text)//', intent(in out)'
            case (1)
                current%text = trim(current%text)//', intent(in)'
            case (2)
                current%text = trim(current%text)//', intent(out)'
            end select
            current%text = current%text(:41)//':: '//arg%name
            if (arg%dim > 0) current%text = trim(current%text)//arg%dimensions

            if (associated(arg, last_arg)) exit
            arg => arg%next
        end do
        call insert_and_moveto_newline(current)
        current%text = statement

        ! search for, and convert, any parameter statements

        current => first_decl
        do
            if (current%text(1:9) == 'parameter') then
                call convert_parameter(current)
            end if
            if (associated(current, last_decl)) exit
            current => current%next
        end do

        ! insert a blank line after the last declaration if there is not one
        ! there already, or a comment.

        next_line => last_decl%next
        if (next_line%text(1:1) /= ' ' .and. next_line%text(1:1) /= '!') then
            call insert_and_moveto_newline(last_decl)
            last_decl%text = ' '
        end if

        ! move onto the next subroutine or function

100     current => end_prog_unit
        if (associated(current, tail)) exit
        last_line => current
        current => current%next
        if (associated(current, tail)) exit
    end do outer_loop

    ! -------------------------------------------------------------------------

    ! indenting and writing output file

    ! output header line & any continuation lines

    current => head
    continuation = .false.
    do
        if (continuation) then
            write (9, '(t9, a)') trim(current%text)
        else
            write (9, '(a)') trim(current%text)
        end if
        ch = last_char(current%text)
        current => current%next
        if (ch /= '&') exit
        continuation = .true.
    end do
    ! date & time stamp
    call date_and_time(date, time)
    if (ch /= ' ') write (9, *)
    write (9, '("! code converted using to_f90 by alan miller")')
    write (9, '("! date: ", a4, "-", a2, "-", a2, "  time: ", a2, &
      &":", a2,      ":", a2)') date(1:4), date(5:6), date(7:8), time(1:2), &
      time(3:4), time(5:6)
    if (len_trim(current%text) > 0) write (9, *)

    indent = 0
    continuation = .false.
    write (*, *) '      writing file: ', f90_name

    do
        if (current%text(1:1) /= '!') then
            if (index(current%text, 'end ') > 0) then
                if (index(current%text, 'end select') == 0) indent = max(indent - 2, 0)
                write (9, '(a)') blank(:indent)//trim(current%text)
                continuation = (last_char(current%text) == '&')
            else if (index(current%text, 'do ') > 0) then
                write (9, '(a)') blank(:indent)//trim(current%text)
                continuation = (last_char(current%text) == '&')
                indent = indent + 2
                ! temporary reduction in
                ! indentation for `else'
            else if (index(current%text, 'else') > 0) then
                last = max(0, indent - 2)
                write (9, '(a)') blank(:last)//trim(current%text)
                continuation = (last_char(current%text) == '&')
                ! indent increased if `if'
                ! is followed by `then'
            else if (index(current%text, 'if ') > 0 .or. index(current%text, 'if(') > 0) &
                then
                current%text = blank(:indent)//trim(current%text)
                ! if if statement runs onto
                ! next line, try joining
                last = len_trim(current%text)
                if (current%text(last:last) == '&') then
                    next_line => current%next
                    if (last + len_trim(next_line%text) < 80) then
                        current%text(last:last) = ' '
                        current%text = trim(current%text)//' '//trim(next_line%text)
                        current%next => next_line%next
                    end if
                end if

                write (9, '(a)') trim(current%text)
                continuation = (last_char(current%text) == '&')
                next_line => current
                do
                    if (index(next_line%text, ' then') > 0 .or. &
                        index(next_line%text, ')then') > 0) then
                        indent = indent + 2
                        exit
                    else
                        if (last_char(next_line%text) /= '&') exit
                    end if
                    next_line => next_line%next
                end do
            else

                ! if line ends with '&', attempt to join on the next line if it is
                ! short.

                last = len_trim(current%text)
                if (last > 0) then
                    if (current%text(last:last) == '&') then
                        last = len_trim(current%text(:last - 1))
                        next_line => current%next
                        if (last + indent + len_trim(next_line%text) < 78) then
                            current%text = current%text(:last)//' '// &
                                           trim(next_line%text)
                            current%next => next_line%next
                            deallocate (next_line)
                        end if
                    end if
                end if

                if (continuation) then
                    write (9, '(a)') blank(:indent + 4)//trim(current%text)
                else
                    write (9, '(a)') blank(:indent)//trim(current%text)
                end if
                continuation = (last_char(current%text) == '&')
            end if
            ! comment line (unchanged)
        else
            write (9, '(a)') trim(current%text)
            continuation = .false.
        end if
        if (associated(current, tail)) exit
        if (.not. associated(current)) exit
        current => current%next
    end do

    close (8)
    close (9)

contains

    subroutine do_loop_fixup(start, lab)

        ! convert do-loops from:    do xxx i=1,n    to:   do i=1,n
        ! xxx continue              end do

        ! `start' points to the first line of the do loop
        ! `lab' is the label

        type(code), pointer :: start
        character(len=*), intent(in) :: lab

        ! local variables

        type(code), pointer :: current, end_loop
        integer :: i, j, level, nmult, nl_length
        logical :: continued, jump_from_inner, referenced
        character(len=5) :: label(20), next_label, text
        character(len=10) :: loop_name

        ! -------------------------------------------------------------------
        ! pass 1. analysis
        ! find end of loop (end_loop)
        ! test for multiple loops using same label
        ! test for jumps to end of this loop from this do loop (referenced)
        ! or from inner loops (jump_from_inner)
        ! find if label is on a statement other than continue
        ! find if next executable line beyond loop is labelled (for exit)

        current => start%next
        nmult = 1
        level = 0
        jump_from_inner = .false.
        referenced = .false.
        do
            if (current%label == lab) then
                continued = (index(current%text, 'continue') > 0)
                exit
            end if

            ! check for nested do loop or multiple use of current loop

            if (current%text(1:1) == '!' .or. current%text(1:1) == ' ') go to 100
            i = index(current%text, 'do ')
            if (i > 0 .and. index(current%text, 'end do') == 0) then
                text = adjustl(current%text(i + 3:))
                if (scan(text(1:1), numbers) > 0) then
                    if (text(:lab_length) == lab) then
                        nmult = nmult + 1
                    else
                        level = level + 1
                        i = scan(text, ' ,')
                        if (i > 0) text = text(:i - 1)
                        label(level) = text
                    end if
                end if
            end if

            ! check for end of nested loop

            if (current%label /= '     ' .and. level > 0) then
                do
                    if (current%label == label(level)) then
                        level = level - 1
                        if (level <= 0) exit
                    else
                        exit
                    end if
                end do
            end if

            ! test for go to current loop label

            i = index(current%text, 'go to')
            if (i > 0) then
                text = adjustl(current%text(i + 5:))
                if (text(:lab_length) == lab) then
                    if (level > 0) then
                        jump_from_inner = .true.
                    else
                        referenced = .true.
                    end if
                end if
            end if

            ! get next line

100         if (.not. associated(current)) return
            current => current%next
        end do

        end_loop => current

        ! find label of next executable line.
        ! first advance past any continuation lines after the end of the do loop.

        next_label = ' '
        do
            if (last_char(current%text) /= '&') exit
            if (.not. associated(current)) go to 110
            current => current%next
        end do

        do
            current => current%next
            if (current%text(1:1) /= '!') exit
            if (.not. associated(current)) go to 110
        end do
        next_label = current%label
        nl_length = len_trim(next_label)

        ! -------------------------------------------------------------------
        ! pass 2. transform beginning & end of loop

110     current => start

        ! remove label from do line
        ! there may be a comma after the label, if so, remove it.

        i = index(current%text, lab(:lab_length))
        current%text = current%text(:i - 1)//current%text(i + lab_length:)
        length = len_trim(current%text)
        do j = i, length
            if (current%text(j:j) == ' ') cycle
            if (current%text(j:j) == ',') current%text(j:j) = ' '
            exit
        end do

        ! jump out of inner loop detected, set up do construct.

        if (jump_from_inner) then
            loop_name = 'loop'//lab
            current%text = trim(loop_name)//':  '//current%text
            current%label = ' '
        end if

        ! insert `end do' at end of loop

        current => end_loop
        if (continued) then
            current%text = 'end do'
            current%label = ' '
        else
            if (.not. referenced) then
                current%label = ' '
                i = index(current%text, lab(:lab_length))
                if (i > 0) current%text = adjustl(current%text(i + lab_length:))
            end if
            ! if there are continuation lines, advance to last one
            do
                if (last_char(current%text) == '&') then
                    current => current%next
                else
                    exit
                end if
            end do
            call insert_and_moveto_newline(current)
            end_loop => current
            current%text = 'end do'
        end if
        if (jump_from_inner) current%text = trim(current%text)//' '//loop_name

        ! insert multiple continue's if necessary

        if (nmult > 1) then
            call insert_and_moveto_newline(current)
            end_loop => current
            current%text = lab//' continue'
            current%label = lab
        end if

        ! -------------------------------------------------------------------
        ! pass 3. change go tos to cycle or exit where appropriate

        current => start%next
        if (continued) then
            do
                if (current%text(1:1) == '!' .or. current%text(1:1) == ' ') go to 120
                i = index(current%text, 'go to')
                if (i > 0) then
                    text = adjustl(current%text(i + 5:))
                    if (text(:5) == lab) then
                        current%text(i:) = 'cycle'
                        if (jump_from_inner) current%text = trim(current%text)//' '// &
                                                            loop_name
                    else if (nl_length > 0 .and. text(:nl_length) == next_label) then
                        current%text(i:) = 'exit'
                        if (jump_from_inner) current%text = trim(current%text)//' '// &
                                                            loop_name
                    end if
                end if

                ! get next line

120             current => current%next
                if (associated(current, end_loop)) exit
                if (.not. associated(current)) exit
            end do
        end if

        return
    end subroutine do_loop_fixup

    subroutine fix_3way_if(start)
        ! convert 3-way ifs to if () then .. else if () then .. else

        type(code), pointer :: start

        ! local variables

        type(code), pointer :: current
        integer :: pos1, count, length, pos2, i, lab1, lab2, lab3, lenq, &
                   next_label, lenz
        character(len=1) :: ch
        character(len=128) :: quantity
        character(len=3) :: zero_txt

        current => start
        length = len_trim(current%text)

        ! find closing bracket to match the opening bracket.
        ! only cases with the closing bracket on the same line are converted.

        pos1 = index(current%text, 'if')

        ! check that next non-blank character after 'if' is '('.
        i = pos1 + 2
        do
            ch = current%text(i:i)
            if (ch /= ' ') exit
            i = i + 1
            if (i > length) return
        end do
        if (ch /= '(') return

        pos1 = i
        count = 1
        pos2 = pos1 + 1
        do
            i = scan(current%text(pos2:length), '()')
            if (i == 0) return
            pos2 = i + pos2 - 1
            if (current%text(pos2:pos2) == '(') then
                count = count + 1
            else
                count = count - 1
            end if
            if (count == 0) exit
            pos2 = pos2 + 1
        end do

        ! see if there are 3 labels after the closing bracket.

        read (current%text(pos2 + 1:), *, err=100) lab1, lab2, lab3

        ! as it is probably very old code, the first alphabetic character in the
        ! expression should tell us whether the quantity is real or integer.

        do i = pos1 + 1, pos2 - 1
            ch = current%text(i:i)
            if (ch >= 'i' .and. ch <= 'n') then
                zero_txt = '0'
                lenz = 1
                exit
            else if (ch >= 'a' .and. ch <= 'z') then
                zero_txt = '0.0'
                lenz = 3
                exit
            else if (i == pos2 - 1) then
                return
            end if
        end do

        quantity = current%text(pos1:pos2)
        lenq = len_trim(quantity)

        ! find the next executable line to see if it is labelled.
        next_label = 0
        do
            if (.not. associated(current)) exit
            current => current%next
            if (current%text(1:1) == '!' .or. len_trim(current%text) == 0) cycle
            if (len_trim(current%label) > 0) read (current%label, *) next_label
            exit
        end do
        current => start

        if (lab1 == lab2) then
            current%text = current%text(:pos2 - 1)//' > '//zero_txt(:lenz)// &
                           ') then'
            call insert_and_moveto_newline(current)
            current%text = ' '
            write (current%text, '(a, i5)') 'go to ', lab3
            if (lab1 /= next_label) then
                call insert_and_moveto_newline(current)
                current%text = 'else'
                call insert_and_moveto_newline(current)
                current%text = ' '
                write (current%text, '(a, i5)') 'go to ', lab1
            end if
            call insert_and_moveto_newline(current)
            current%text = 'end if'

        else if (lab2 == lab3) then
            current%text = current%text(:pos2 - 1)//' < '//zero_txt(:lenz)// &
                           ') then'
            call insert_and_moveto_newline(current)
            current%text = ' '
            write (current%text, '(a, i5)') 'go to ', lab1
            if (lab2 /= next_label) then
                call insert_and_moveto_newline(current)
                current%text = 'else'
                call insert_and_moveto_newline(current)
                current%text = ' '
                write (current%text, '(a, i5)') 'go to ', lab2
            end if
            call insert_and_moveto_newline(current)
            current%text = 'end if'

        else if (lab1 == lab3) then
            current%text = current%text(:pos2 - 1)//' == '//zero_txt(:lenz)// &
                           ') then'
            call insert_and_moveto_newline(current)
            current%text = ' '
            write (current%text, '(a, i5)') 'go to ', lab2
            if (lab1 /= next_label) then
                call insert_and_moveto_newline(current)
                current%text = 'else'
                call insert_and_moveto_newline(current)
                current%text = ' '
                write (current%text, '(a, i5)') 'go to ', lab1
            end if
            call insert_and_moveto_newline(current)
            current%text = 'end if'

        else
            current%text = current%text(:pos2 - 1)//' < '//zero_txt(:lenz)// &
                           ') then'
            call insert_and_moveto_newline(current)
            current%text = ' '
            write (current%text, '(a, i5)') 'go to ', lab1
            call insert_and_moveto_newline(current)
            current%text = 'else if '//quantity(1:lenq - 1)//' == '// &
                           zero_txt(:lenz)//') then'
            call insert_and_moveto_newline(current)
            current%text = ' '
            write (current%text, '(a, i5)') 'go to ', lab2
            if (lab3 /= next_label) then
                call insert_and_moveto_newline(current)
                current%text = 'else'
                call insert_and_moveto_newline(current)
                current%text = ' '
                write (current%text, '(a, i5)') 'go to ', lab3
            end if
            call insert_and_moveto_newline(current)
            current%text = 'end if'

        end if

100     return
    end subroutine fix_3way_if

    subroutine insert_and_moveto_newline(current)
        ! insert a new line after the current line, and move `current' to point to
        ! it.

        type(code), pointer :: current

        ! local variable
        type(code), pointer :: new_line

        allocate (new_line)
        new_line%next => current%next
        current%next => new_line
        current => new_line

        return
    end subroutine insert_and_moveto_newline

    subroutine find_declarations(start, tail, first_decl, last_decl)
        ! find the first & last declaration lines in a program unit.

        type(code), pointer :: start, tail
        type(code), pointer :: first_decl, last_decl

        ! local variables
        character(len=9), parameter :: declaration(13) = ['implicit ', &
                                                          'integer  ', 'real     ', 'double   ', 'logical  ', 'complex  ', &
                                                          'dimension', 'external ', 'data     ', 'common   ', 'parameter', &
                                                          'save     ', 'character']
        type(code), pointer :: current
        integer :: pos, length, i

        nullify (first_decl, last_decl)

        ! search for first declaration
        current => start%next
        search1: do
            if (current%text(1:1) /= '!' .and. current%text(1:1) /= ' ') then
                pos = scan(current%text(1:13), delimiters)
                if (pos > 0) then
                    length = min(9, pos - 1)
                    if (length >= 4) then
                        do i = 1, 13
                            if (current%text(:length) == declaration(i) (:length)) then
                                first_decl => current
                                exit search1
                            end if
                        end do
                    end if
                end if
            end if

            current => current%next
            if (associated(current, tail)) return
        end do search1

        ! search for last declaration

        last_decl => first_decl
        do
            if (current%text(1:1) /= '!' .and. current%text(1:1) /= ' ') then
                pos = index(current%text, '=')
                if (pos > 0) then
                    if (pos < 12) return
                    if (current%text(1:9) /= 'parameter' .and. current%text(1:9) /= &
                        'character') return
                end if

                if (current%text(1:4) == 'call') return

                if (current%text(1:2) == 'if') then
                    if (current%text(3:3) == ' ') return
                    if (current%text(3:3) == '(') return
                end if

                if (current%text(1:3) == 'do ') return

                ! skip continuation lines

                do
                    if (last_char(current%text) /= '&') exit
                    current => current%next
                end do

                last_decl => current
            end if

            current => current%next
            if (associated(current, tail)) return
        end do

        return
    end subroutine find_declarations

    subroutine get_arg_list()
        ! extract list of dummy arguments

        ! local variables
        integer :: pos, last

        current => start_prog_unit
        numb_arg = 0
        do                             ! find '(' if there are any arguments
            pos = index(current%text, '(')
            if (pos == 0) then
                if (last_char(current%text) /= '&') return
                current => current%next
            else
                exit
            end if
        end do
        pos = pos + 1

        nullify (arg_start)
        allocate (arg_start)
        first_arg = .true.
        do                             ! loop through lines of arguments
            last = scan(current%text(pos:), ',)')
            if (last == 0) then
                if (last_char(current%text) /= '&') exit
                current => current%next
                pos = 1
            else
                last = last + pos - 1
                nullify (arg)
                allocate (arg)
                if (first_arg) then
                    if (len_trim(current%text(pos:last - 1)) == 0) exit
                    arg_start => arg
                    first_arg = .false.
                    nullify (last_arg)
                    allocate (last_arg)
                else
                    last_arg%next => arg
                end if
                numb_arg = numb_arg + 1
                last_arg => arg

                arg%name = adjustl(current%text(pos:last - 1))
                arg%intention = 0
                arg%var_type = ' '
                arg%dim = 0
                pos = last + 1
            end if
        end do

        return
    end subroutine get_arg_list

    subroutine get_var_types()
        ! search thru the declarations for the types of dummy arguments

        current => first_decl
        do
            text = current%text(:30)
            if (text(:4) == 'real' .or. text(:7) == 'integer' .or. &
                text(:6) == 'double' .or. text(:9) == 'character' .or. &
                text(:7) == 'logical' .or. text(:7) == 'complex') then
                ! copy the variable type to vtype
                last = index(text, ' ::') - 1
                if (last < 0) then
                    last = index(text, '*')
                    if (last == 0) then
                        last = 24
                    else
                        last = index(text(last + 2:), ' ') + last
                    end if
                    i1 = last + 2
                else
                    i1 = last + 4
                end if
                vtype = text(:last)
                call extract_declarations(i1)

            else if (text(:9) == 'dimension') then
                i1 = 11
                vtype = ' '
                call extract_declarations(i1)
            end if

            if (associated(current, last_decl)) exit
            current => current%next
        end do

        ! if there are any arguments for which the type has not been determined,
        ! use the implicit types

        arg => arg_start
        do
            if (arg%var_type == ' ') arg%var_type = implicit_type(arg%name(1:1))
            if (associated(arg, last_arg)) exit
            arg => arg%next
        end do

        return
    end subroutine get_var_types

    subroutine get_intents()
        ! search thru the body of the current program unit to try to determine
        ! the intents of dummy arguments.

        character(len=80) :: last_part
        integer :: j, nbrac

        do
            if (current%text(1:1) /= '!' .and. current%text(1:1) /= ' ') then
                statement = current%text
                if (statement(1:3) == 'if ' .or. statement(1:3) == 'if(') then
                    ! split line into two parts
                    ! if (condition) | last_part
                    i = index(statement, '(')
                    length = len_trim(statement)
                    nbrac = 1
                    do j = i + 1, length - 1
                        if (statement(j:j) == ')') then
                            nbrac = nbrac - 1
                            if (nbrac == 0) exit
                        else if (statement(j:j) == '(') then
                            nbrac = nbrac + 1
                        end if
                    end do
                    if (j < length) then
                        last_part = statement(j + 1:)
                    else
                        last_part = ' '
                    end if
                    statement = statement(:j)
                    ! it is assumed that a variable inside
                    ! an if-expression cannot be altered
                    arg => arg_start
                    do
                        i = find_delimited_name(statement, arg%name)
                        if (i > 0) then
                            if (arg%intention == 0) arg%intention = 1
                        end if
                        if (associated(arg, last_arg)) exit
                        arg => arg%next
                    end do
                    statement = last_part
                end if

                pos = index(statement, '=', back=.true.)
                if (pos > 0) then
                    if (statement(pos - 1:pos - 1) /= '=' .and. statement(pos - 1:pos - 1) /= '/' &
                        .and. statement(pos - 1:pos - 1) /= '<' .and. statement(pos - 1:pos - 1) /= &
                        '>') then

                        ! look for each argument name;
                        ! is it before or after '='?
                        arg => arg_start
                        do
                            i = find_delimited_name(statement, arg%name)
                            if (i > 0) then
                                if (i < pos) then
                                    arg%intention = ior(arg%intention, 2)
                                else
                                    if (arg%intention == 0) arg%intention = 1
                                end if
                            end if
                            if (associated(arg, last_arg)) exit
                            arg => arg%next
                        end do
                    end if
                end if
            end if

            if (associated(current, end_prog_unit)) exit
            current => current%next
        end do

        return
    end subroutine get_intents

    subroutine goto_cases(text)
        ! inserts pairs:
        ! case (i)
        ! go to i-th label
        ! terminated with:
        ! end select

        character(len=*), intent(inout) :: text

        integer :: case_number, pos, i2

        case_number = 1

        do
            pos = index(text, ',')
            if (pos > 0) then
                i2 = pos - 1
            else
                i2 = len_trim(text)
            end if
            call insert_and_moveto_newline(current)
            write (current%text, '("  case (", i5, ")")') case_number
            call insert_and_moveto_newline(current)
            current%text = '    go to '//trim(text(:i2))
            if (pos == 0) exit
            text = text(pos + 1:)
            case_number = case_number + 1
        end do

        call insert_and_moveto_newline(current)
        current%text = 'end select'

        return
    end subroutine goto_cases

    subroutine extract_declarations(start_pos)
        ! take the current line, and any continuations, look for dummy variables,
        ! and remove them, after storing any relevant type & dimension info.

        integer, intent(in) :: start_pos

        ! local variables

        integer :: i, i1, j, ndim
        character(len=70) :: text

        i1 = start_pos
        do
            i = scan(current%text(i1:), '(,') ! find next ( or ,
            ndim = 0
            if (i == 0) then               ! no comma or ( on this line
                if (last_char(current%text) == '&') then
                    current => current%next
                    i1 = 1
                    cycle
                else
                    text = adjustl(current%text(i1:))
                    ! just in case there is an in-line
                    pos = index(text, '!')   ! comment (though illegal in f77)
                    if (pos > 0) text = text(:pos - 1)

                    if (len_trim(text) == 0) return
                    pos = len_trim(current%text)
                end if
            else
                pos = i + i1 - 1
                if (current%text(pos:pos) == ',') then ! comma found
                    text = current%text(i1:pos - 1)
                else                       ! ( found; find matching )
                    count = 1
                    ndim = 1
                    pos = pos + 1
                    do
                        j = scan(current%text(pos:), '(,)')
                        if (j == 0) then         ! no bracket or comma
                            if (last_char(current%text) == '&') then
                                length = len_trim(current%text)
                                next_line => current%next
                                current%text = trim(current%text(:length - 1))//' '// &
                                               adjustl(next_line%text)
                                if (associated(next_line, last_decl)) last_decl => current
                                current%next => next_line%next
                                cycle
                            else
                                return
                            end if
                        end if

                        pos = pos + j - 1
                        select case (current%text(pos:pos))
                        case ('(')
                            count = count + 1
                        case (')')
                            count = count - 1
                            if (count <= 0) then
                                text = current%text(i1:pos)
                                exit
                            end if
                        case (',')
                            ndim = ndim + 1
                        end select
                        pos = pos + 1
                    end do                   ! end matching ) search
                end if
            end if

            ! variable name isolated, with ndim dimensions
            ! now see if it matches a dummy argument

            arg => arg_start
            text = adjustl(text)
            if (ndim <= 0) then
                length = len_trim(text)
            else
                length = index(text, '(') - 1
            end if
            do
                if (text(:length) == arg%name) then ! argument matched
                    ! insert variable type
                    if (arg%var_type == ' ') arg%var_type = vtype
                    if (ndim > arg%dim) then
                        arg%dim = ndim
                        i = index(text, '(')
                        arg%dimensions = text(i:)
                    end if
                    ! remove variable ( & comma)
                    text = adjustl(current%text(pos + 1:))
                    if (len_trim(text) == 0) then
                        if (i1 > 1) then
                            current%text(i1 - 1:) = ' '
                        else
                            current%text = ' '
                        end if
                        if (i1 == start_pos) current%text = ' '
                        return
                    else
                        if (text(1:1) == ',') text = adjustl(text(2:))
                        if (text(1:1) == '&') then
                            next_line => current%next
                            if (i1 == start_pos) then
                                current%text = current%text(:i1 - 1)//' '// &
                                               adjustl(next_line%text)
                                if (associated(next_line, last_decl)) last_decl => current
                                current%next => next_line%next
                            else
                                current%text = current%text(:i1 - 1)//'  &'
                                current => next_line
                                i1 = 1
                            end if
                        else
                            current%text = current%text(:i1 - 1)//' '//text
                        end if
                    end if
                    exit
                end if

                if (associated(arg, last_arg)) then
                    i1 = pos + 1             ! skip over comma, if present
                    exit
                end if
                arg => arg%next
            end do
        end do

        return
    end subroutine extract_declarations

    subroutine convert_parameter(start)

        ! convert parameter statements from:
        ! parameter (name1 = value1, name2 = value2, ... )
        ! to:
        ! type1, parameter :: name1 = value1
        ! type2, parameter :: name2 = value2

        type(code), pointer :: start

        ! local variables

        type(code), pointer :: current, next_line
        integer :: count, i, j, length, pos
        character(len=10) :: text
        character(len=30) :: vtype

        current => start

        ! replace opening ( with ::

        i = index(current%text, '(')
        if (i == 0) return
        current%text = trim(current%text(:i - 1))//' :: '// &
                       adjustl(current%text(i + 1:))
        i = index(current%text, '::') + 3
        do
            j = index(current%text(i:), '=')
            if (j == 0) then
                if (last_char(current%text) /= '&') return
                next_line => current%next
                j = len_trim(current%text)
                current%text = trim(current%text(:j - 1))//next_line%text
                current%next => next_line%next
                j = index(current%text(i:), '=')
                if (j == 0) return
            end if
            j = i + j - 1
            text = adjustl(current%text(i:j - 1))
            call find_type(text, vtype, first_decl, start)

            current%text = trim(vtype)//', '//current%text
            j = j + 2 + len_trim(vtype)

            ! is there another value set in this statement?
            ! find end of the expression for the value, which may involve brackets
            ! and commas.

100         length = len_trim(current%text)
            count = 0
            do i = j + 1, length
                select case (current%text(i:i))
                case ('(')
                    count = count + 1
                case (')')
                    count = count - 1
                    if (count < 0) then
                        ! remove final ) and return
                        current%text = current%text(:i - 1)
                        return
                    end if
                case (',')
                    ! if count = 0, there is another declaration
                    if (count == 0) then
                        ! break line, check for '&' as first character
                        text = adjustl(current%text(i + 1:))
                        if (text(1:1) == '&') then
                            current%text = current%text(:i - 1)
                            current => current%next
                            current%text = 'parameter :: '//adjustl(current%text)
                        else
                            allocate (next_line)
                            next_line%next => current%next
                            current%next => next_line
                            next_line%text = 'parameter :: '//adjustl(current%text(i + 1:))
                            current%text = current%text(:i - 1)
                            if (associated(current, last_decl)) last_decl => next_line
                            current => next_line
                            start => start%next
                        end if
                        exit
                    end if
                case ('&')
                    ! expression continued on next line, merge lines
                    next_line => current%next
                    pos = len_trim(current%text(:i - 1))
                    current%text = current%text(:pos)//next_line%text
                    current%next => next_line%next
                    go to 100
                end select
            end do

            if (i > length) exit
            i = 14
        end do

        return
    end subroutine convert_parameter

    subroutine find_type(vname, vtype, first_decl, last_decl)

        ! find the type of variable 'vname'

        character(len=*), intent(in) :: vname
        character(len=*), intent(out) :: vtype
        type(code), pointer :: first_decl, last_decl

        ! local variables

        type(code), pointer :: current
        character(len=30) :: text
        integer :: i1, last, length, pos

        current => first_decl
        length = len_trim(vname)
        if (length == 0) return
        do
            text = current%text(:30)
            if (text(:4) == 'real' .or. text(:7) == 'integer' .or. &
                text(:6) == 'double' .or. text(:9) == 'character' .or. &
                text(:7) == 'logical' .or. text(:7) == 'complex') then
                ! copy the variable type to vtype
                last = index(text, ' ::') - 1
                if (last < 0) then
                    last = index(text, '*')
                    if (last == 0) then
                        last = 24
                    else
                        last = index(text(last + 2:), ' ') + last
                    end if
                    i1 = last + 2
                else
                    i1 = last + 4
                end if
                vtype = text(:last)

                ! see if variable is declared on this line (& any continuation)

                do
                    pos = find_delimited_name(current%text(i1:), vname(:length))
                    if (pos == 0) then
                        if (last_char(current%text) == '&') then
                            current => current%next
                            i1 = 1
                            cycle
                        end if
                    end if
                    exit
                end do

                ! variable name found if pos > 0.

                if (pos > 0) then            ! remove variable name
                    pos = pos + i1 - 1
                    current%text = current%text(:pos - 1)//current%text(pos + length:)
                    ! delete line if only type :: remains
                    if (last_char(current%text) == ':') then
                        current%text = ' '
                        return
                    end if
                    ! remove any following comma
                    i = pos
                    length = len_trim(current%text)
                    do
                        if (i > length) then
                            return
                        else if (current%text(i:i) == ',') then
                            current%text = current%text(:i - 1)//current%text(i + 1:)
                            return
                        else if (current%text(i:i) /= ' ') then
                            return
                        end if
                        i = i + 1
                    end do
                end if

            end if

            ! if last declaration has been reached, return default type.
            ! otherwise proceed to next line.

            if (associated(current, last_decl)) then
                vtype = implicit_type(vname(1:1))
                exit
            else
                current => current%next
            end if
        end do

        return
    end subroutine find_type

end program to_f90
