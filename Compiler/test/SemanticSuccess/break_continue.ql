def func_test(int a) : int ret_name { 
        
        int i;

        for(i from 0 to 2 by 1)
        a=a+5;

        for(i from 2 to 0 by -1)
        {
            a=a*10;
            print(a);
            break;
        }

        for(i from 1 to 5)
        {
            print(a);
            continue;
            a=a*10;

        }

    ret_name = a;
}

def compute(): int trial {

   trial = func_test(20);
}