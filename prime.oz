    call proc_main
    halt
proc_main:
    # prologue
    push_stack_frame 3
    int_const r0, 0
    # initialise "i" val integer
    store 0, r0
    # initialise "attempts" val integer
    store 1, r0
    # initialise "flag" val boolean
    store 2, r0
    # write "This program finds the first N prime numbers.";
    string_const r0, "This program finds the first N prime numbers."
    call_builtin print_string
    string_const r0, "\n"
    call_builtin print_string
    # write "Please type a positive integer value for N:";
    string_const r0, "Please type a positive integer value for N:"
    call_builtin print_string
    string_const r0, "\n"
    call_builtin print_string
    # read i;
    call_builtin read_int
    store 0, r0
    # if i <= 0
label_0:
    load r0, 0
    int_const r1, 0
    cmp_le_int r0, r0, r1
    branch_on_false r0, label_1
    # do
    # attempts <- attempts + 1;
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
    # write "I said positive! Try again.";
    string_const r0, "I said positive! Try again."
    call_builtin print_string
    string_const r0, "\n"
    call_builtin print_string
    # write "Please type a positive integer value for N:";
    string_const r0, "Please type a positive integer value for N:"
    call_builtin print_string
    string_const r0, "\n"
    call_builtin print_string
    # read i;
    call_builtin read_int
    store 0, r0
    # if attempts = 2
    load r0, 1
    int_const r1, 2
    cmp_eq_int r0, r0, r1
    branch_on_false r0, label_2
    # then
    # i <- 1;
    int_const r0, 1
    store 0, r0
    branch_uncond label_2
    # fi
label_2:
    branch_uncond label_0
    # od
label_1:
    # if attempts = 2
    load r0, 1
    int_const r1, 2
    cmp_eq_int r0, r0, r1
    branch_on_false r0, label_3
    # then
    # write "You had your chance.";
    string_const r0, "You had your chance."
    call_builtin print_string
    string_const r0, "\n"
    call_builtin print_string
    branch_uncond label_4
label_3:
    # else
    # call firstNPrimes(i);
    load_address r0, 0
    call proc_firstNPrimes
    # fi
    branch_uncond label_4
label_4:
    # epilogue
    pop_stack_frame 3
    return
proc_firstNPrimes:
    # prologue
    push_stack_frame 4
    # Writing parameters
    store 0, r0
    int_const r0, 0
    # initialise "count" val integer
    store 1, r0
    # initialise "curr" val integer
    store 2, r0
    # initialise "flag" val boolean
    store 3, r0
    # curr <- 2;
    int_const r0, 2
    store 2, r0
    # count <- 0;
    int_const r0, 0
    store 1, r0
    # if count < N
label_5:
    load r0, 1
    load r1, 0
    load_indirect r1, r1
    cmp_lt_int r0, r0, r1
    branch_on_false r0, label_6
    # do
    # call isPrime(curr, flag);
    load r0, 2
    load_address r1, 3
    call proc_isPrime
    # if flag
    load r0, 3
    branch_on_false r0, label_7
    # then
    # write curr;
    load r0, 2
    call_builtin print_int
    # write " ";
    string_const r0, " "
    call_builtin print_string
    # count <- count + 1;
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
    branch_uncond label_7
    # fi
label_7:
    # curr <- curr + 1;
    load r0, 2
    int_const r1, 1
    add_int r0, r0, r1
    store 2, r0
    branch_uncond label_5
    # od
label_6:
    # write "";
    string_const r0, ""
    call_builtin print_string
    string_const r0, "\n"
    call_builtin print_string
    # epilogue
    pop_stack_frame 4
    return
proc_isPrime:
    # prologue
    push_stack_frame 3
    # Writing parameters
    store 0, r0
    store 1, r1
    int_const r0, 0
    # initialise "i" val integer
    store 2, r0
    # flag <- true;
    int_const r0, 1
    load r1, 1
    store_indirect r1, r0
    # i <- 2;
    int_const r0, 2
    store 2, r0
    # if i < num
label_8:
    load r0, 2
    load r1, 0
    cmp_lt_int r0, r0, r1
    branch_on_false r0, label_9
    # do
    # if num - num / i * i = 0
    load r0, 0
    load r2, 0
    load r4, 2
    div_int r2, r2, r4
    load r3, 2
    mul_int r2, r2, r3
    sub_int r0, r0, r2
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_false r0, label_10
    # then
    # flag <- false;
    int_const r0, 0
    load r1, 1
    store_indirect r1, r0
    # i <- num;
    load r0, 0
    store 2, r0
    branch_uncond label_10
    # fi
label_10:
    # i <- i + 1;
    load r0, 2
    int_const r1, 1
    add_int r0, r0, r1
    store 2, r0
    branch_uncond label_8
    # od
label_9:
    # if num = 1 or num = 2
    load r0, 0
    int_const r2, 1
    cmp_eq_int r0, r0, r2
    branch_on_true r0, label_12
    load r1, 0
    int_const r3, 2
    cmp_eq_int r1, r1, r3
    or r0, r0, r1
label_12:
    branch_on_false r0, label_11
    # then
    # flag <- true;
    int_const r0, 1
    load r1, 1
    store_indirect r1, r0
    branch_uncond label_11
    # fi
label_11:
    # epilogue
    pop_stack_frame 3
    return
