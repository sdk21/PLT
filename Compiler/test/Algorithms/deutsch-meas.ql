
def measure(mat top) : mat result{
       # returns the measurement matrix
        
       mat ad;
       ad = adj(top);
       result = top * ad;
}


def compute () : mat outcome{
        mat top; 
        mat Ub;
        mat Uc;
        int n;
        
        top = |00>;

        outcome = measure(top);
}
