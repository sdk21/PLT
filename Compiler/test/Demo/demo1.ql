def compute() : mat output{
        
        mat a;
        mat b;
        mat c;
        mat k;

        a = |11>;
        b = |0>;
        k = <0|;
        
        c = a @ b;
        printq(c);
        
        c = H*b;
        printq(c);

        output = b*k;
}
