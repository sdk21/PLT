def measure(mat top): mat outcome{
        mat ad;
        
        ad = adj(top);
        outcome = top*ad;
}

def outcomezero(mat bottom) : float probability{
        
        mat top;
        mat input;
        mat had;
        mat cnot;
        mat ynot;
        mat output;
        mat meas;
        
        top = |0>;
        input = top @ bottom;
        
        had = H @ IDT;
        cnot = [(1,0,0,0)
                (0,1,0,0)
                (0,0,0,1)
                (0,0,1,0)];
      
        
        ynot = [(1,0,0,0)
                (0,0,0,-1)
                (0,0,1,0)
                (0,-1,0,0)];
    
        output = (ynot*(cnot*(had*input)));
        
        printq(output);
        
        probability = norm(output);
   
}

def compute() : float outcome{
        
        mat bottom;
        
        bottom = |1>;
        outcome = outcomezero(bottom);
        print(outcome);
}
