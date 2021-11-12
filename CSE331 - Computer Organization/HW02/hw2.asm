.data
    arr: .word 1, 11, 10, 22, 9, 33, 21, 50, 41, 60
    longestArr: .word 0:10
    tempArr: .word 0:10
    size: .word 10
    whitespace: .asciiz " "
    here: .asciiz "here\n"

.text
    # keep this for letter use, 
    # this ads new value to arr
    #addi $s0, $zero, 99
    #sw $s0, arr($t0) # load value 99 to arr[0]

    # i
    addi $t0, $zero, 0 # make sure t0 is zero
    # j
    addi $t1, $zero, 0 # make sure t1 is zero


    # it will track array with incrementing 4 for int
    addi $t8, $zero, 0 # make sure t1 is zero
    # it will track longestArr incrementing 4 for int
    addi $t9, $zero, 0 # make sure t2 is zero


    lw $s6, size # load size
    # it will track longestSeq size
    # lw $s7, 0 
    

    while:
        beq $t0, $s6, end # for (int i = 0; i < size (t2); i++)

        lw $t5, arr($t8) # load arr[i]

        # load new variable to one,
        # it will keep track of the temp arr index
        # start from 1, because, I will assign first value manually
        addi $t2, $zero, 1 # tempArr index

        addi $t9, $zero, 0 # tempArr increemnter

        # assign tempArr[0] = arr[0]
        sw $t5, tempArr($t9) # store arr[0] to tempArr[0]
        addi $t9, $t9, 4 # increment t9 for next int

        addi $t7, $t5, 0 # int temp = arr[i]
        
            # inner for loop
            addi $t1, $t0, 0 # j = i

            while2:
                beq $t1, $s6, end2 # for (int j = i; j < size (t2); j++)

                lw $t6, arr($t9) # load arr[j]

                

                # if arr[j] is bigger than temp
                # and increment tempArrIndex
                # else, do nothing
                    bgt $t6, $t7, assign # arr[j] > temp
                    
 
 
                addi $t1, $t1, 1 # j++
                j while2
            end2:


        # jal printNum # call printNum

        li $v0, 1 # print int
        move $a0, $t2 # print int
        syscall

        li $v0, 4 # for printing string
        la $a0, whitespace #print space
        syscall


        addi $t0, $t0, 1 # i++
        addi $t8, $t8, 4 # increment int value to print arr


        j while
    end:

    # end of main
    li $v0, 10
    syscall


assign:
    # assign arr[j] to tempArr[tempArrIndex]
    sw $t6, tempArr($t9) #  tempArr[tempArrIndex] = arr[j]
    addi $t7, $t6, 0 # temp = arr[j]
    addi $t2, $t2, 1 # increment tempArrIndex
    addi $t9, $t9, 4 # increment t9 for next int



printNum:
    li $v0, 1 # for printing int
    move $a0, $t5 # load arr
    syscall

    li $v0, 4 # for printing string
    la $a0, whitespace #print space
    syscall
    
    jr $ra
        
    
