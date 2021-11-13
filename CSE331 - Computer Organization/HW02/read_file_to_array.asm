.data  
fin: .asciiz "file.txt" # filename
buffer: .space 1024     # buffer - it will keep file content    

arr: .word 0:16         # every array assumed 16 size
longestArr: .word 0:16  # longestArr to hold longest sequence

val: .asciiz " <-\n"
str_carriage: .asciiz "carriage\n"
str_newline: .asciiz "newline\n"
str_space: .asciiz "space\n"
str_zero: .asciiz "zero\n"

new_line: .asciiz "\n"
comma: .asciiz ","

.text

main:
    # call file reader func
    # keep all content in s0
    jal openFileAndRead
    
    addi $t3, $zero, 0 # t3 = 0, keeps array size
    addi $t4, $zero, 0 # t4 = 0, keeps array address location

    while:
        lb $t2, 0($s0) # load char

        beq $t2, $zero, end_arr
        beq $t2, 13, end_arr # carriage return / new line
        beq $t2, '\n', end_arr
        beq $t2, ' ', new_int


        jal atoi
        j while

        end_arr:
            li $v0, 4
            la $a0, str_carriage
            syscall

            sw $t1, arr($t4) # store in array
            addi $t3, $t3, 1 # increment array size
            addi $t4, $t4, 4 # increment array address location

            addi $t5, $zero, 0
            
            whilePrint:
                beq $t5, $t4, endPrint

                lb $t9, arr($t5)

                li $v0, 1
                move $a0, $t9
                syscall

                li $v0, 4
                la $a0, comma
                syscall

                addi $t5, $t5, 4

                j whilePrint

            endPrint:

            li $v0, 4
            la $a0, new_line
            syscall

            addi $t3, $zero, 0 # t3 = 0, keeps array size
            addi $t4, $zero, 0 # t4 = 0, keeps array address location

            beq $t2, 13, increment_one 
            beq $t2, '\n', increment_one

            j end


        new_int:
            sw $t1, arr($t4) # store in array

            addi $t3, $t3, 1 # increment array size
            addi $t4, $t4, 4 # increment array address location

            li $v0, 4
            la $a0, str_newline
            syscall

            li $v0, 1
            move $a0, $t4
            syscall

            j increment_one

        increment_one:
            addi $s0, $s0, 1

            j while

        # _space:
        #     la $a0, str_space
        #     j end
        # _carriage:
        #     la $a0, str_carriage
        #     j end

        
    end:

    li $v0, 4
    syscall


    # end of main
    li $v0, 10
    syscall





openFileAndRead:
    li   $v0, 13       # sys call for opening a file
    la   $a0, fin      # file name
    li   $a1, 0        # open in read mode
    li   $a2, 0        # ignore second arr
    syscall
    move $s6, $v0      # save the file descriptor 

    li   $v0, 14       # sys call for reading file
    move $a0, $s6      # file descriptor 
    la   $a1, buffer   # string buffer = file_content
    li   $a2, 1024     # hardcoded buffer length
    syscall      

    # Close the file 
    li   $v0, 16       # system call for close file
    move $a0, $s6      # file descriptor to close
    syscall

    la $s0, buffer   # keep address at $s0

    jr $ra  
    

atoi:
    # la $t0, str # load string address
    addi $t1, $zero, 0 # t1 = res = 0

    whileAtoi:

        lb $t2, 0($s0) # load char

        beq $t2, $zero, endAtoi
        beq $t2, '\n', endAtoi
        beq $t2, 13, endAtoi # carriage return / new line
        beq $t2, ' ', endAtoi

        addi $t2, $t2, -48 # convert to char to int

        li $v0, 1
        move $a0, $t2
        syscall

        li $v0, 4
        la $a0, val
        syscall

        mul $t1, $t1, 10 # res *= 10
        add $t1, $t1, $t2 # res += temp

        addi $s0, $s0, 1 # go to next char
    
        j whileAtoi

    endAtoi:

    li $v0, 1
    move $a0, $t1
    syscall

    jr $ra


