
public class Matrix {
	public static void show(int[][] a){
		for (int i = 0; i < a.length; i++){
			for (int j = 0; j<a[i].length; j++){
				System.out.print(a[i][j]);
				System.out.print("\t");
			}
			System.out.println();
		}
	}
	
	public static void show(double[][] a){
		for (int i = 0; i < a.length; i++){
			for (int j = 0; j<a[i].length; j++){
				System.out.print(a[i][j]);
				System.out.print("\t");
			}			
			System.out.println();
		}
	}
	
	public static int[][] matrixPlus(int[][] a, int[][] b) throws Exception{
		int m = a.length;
		int n = a[0].length;
		int m1 = b.length;
		int n1 = b[0].length;
		if(m!=m1 || n!=n1){
			throw new Exception("Illegal Matrix Plus operation;\nMatrix dimensions must agree\n");
		}
		int c[][] = new int[m][n];
		for(int i=0; i <a.length; i++){
			for(int j=0; j<a[i].length; j++){
				c[i][j] = a[i][j] + b[i][j];
			}
		}
		return c;
	}
	
	public static double[][] matrixPlus(double[][] a, double[][] b) throws Exception{
		int m = a.length;
		int n = a[0].length;
		int m1 = b.length;
		int n1 = b[0].length;
		if(m!=m1 || n!=n1){
			throw new Exception("Illegal Matrix Plus operation;\nMatrix dimensions must agree\n");
		}
		double c[][] = new double[m][n];
		for(int i=0; i <a.length; i++){
			for(int j=0; j<a[i].length; j++){
				c[i][j] = a[i][j] + b[i][j];
			}
		}
		return c;
	}

    public static double[][] matrixPlus(int[][] a, double[][] b) throws Exception{
        int m = a.length;
        int n = a[0].length;
        int m1 = b.length;
		int n1 = b[0].length;
		if(m!=m1 || n!=n1){
			throw new Exception("Illegal Matrix Plus operation;\nMatrix dimensions must agree\n");
		}
        double c[][] = new double[m][n];
        for(int i=0; i <a.length; i++){
            for(int j=0; j<a[i].length; j++){
                c[i][j] = a[i][j] + b[i][j];
            }
        }
        return c;
    }

    public static double[][] matrixPlus(double[][] a, int[][] b) throws Exception{
        int m = a.length;
        int n = a[0].length;
        int m1 = b.length;
		int n1 = b[0].length;
		if(m!=m1 || n!=n1){
			throw new Exception("Illegal Matrix Plus operation;\nMatrix dimensions must agree\n");
		}
        double c[][] = new double[m][n];
        for(int i=0; i <a.length; i++){
            for(int j=0; j<a[i].length; j++){
                c[i][j] = a[i][j] + b[i][j];
            }
        }
        return c;
    }
	
	public static int[][] matrixMinus(int[][] a, int[][] b) throws Exception{
		int m = a.length;
		int n = a[0].length;
		int m1 = b.length;
		int n1 = b[0].length;
		if(m!=m1 || n!=n1){
			throw new Exception("Illegal Matrix Minus operation;\nMatrix dimensions must agree\n");
		}
		int c[][] = new int[m][n];
		for(int i=0; i <a.length; i++){
			for(int j=0; j<a[i].length; j++){
				c[i][j] = a[i][j] - b[i][j];
			}
		}
		return c;
	}
	
	
	public static double[][] matrixScalarT(int[][] a, double x){
		int m = a.length;
		int n = a[0].length;
		double c[][] = new double[m][n];
		for(int i=0;i<m;i++){
			for(int j=0; j<n; j++){
				c[i][j] = a[i][j]*x;
			}
		}
		return c;
	}
	
	public static double[][] matrixScalarT(double[][] a, double x){
		int m = a.length;
		int n = a[0].length;
		double c[][] = new double[m][n];
		for(int i=0;i<m;i++){
			for(int j=0; j<n; j++){
				c[i][j] = a[i][j]*x;
			}
		}
		return c;
	}
	
	public static double[][] matrixScalarT(double[][] a, int x){
		int m = a.length;
		int n = a[0].length;
		double c[][] = new double[m][n];
		for(int i=0;i<m;i++){
			for(int j=0; j<n; j++){
				c[i][j] = a[i][j]*x;
			}
		}
		return c;
	}
	
	public static double[][] matrixScalarD(double[][] a, double x){
		int m = a.length;
		int n = a[0].length;
		double c[][] = new double[m][n];
		for(int i=0;i<m;i++){
			for(int j=0; j<n; j++){
				c[i][j] = a[i][j]/x;
			}
		}
		return c;
	}
	
	public static double[][] matrixScalarD(double[][] a, int x){
		int m = a.length;
		int n = a[0].length;
		double c[][] = new double[m][n];
		for(int i=0;i<m;i++){
			for(int j=0; j<n; j++){
				c[i][j] = a[i][j]/x;
			}
		}
		return c;
	}
	
	public static double[][] matrixPlusDigit(double[][] a, double x){
		int m = a.length;
		int n = a[0].length;
		double c[][] = new double[m][n];
		for(int i=0;i<m;i++){
			for(int j=0;j<n;j++){
				c[i][j] = a[i][j]+x;
			}
		}
		return c;
	}
	
	public static double[][] matrixPlusDigit(double[][] a, int x){
		int m = a.length;
		int n = a[0].length;
		double c[][] = new double[m][n];
		for(int i=0;i<m;i++){
			for(int j=0;j<n;j++){
				c[i][j] = a[i][j]+x;
			}
		}
		return c;
	}
	
	public static double[][] matrixMinusDigit(double[][] a, double x){
		int m = a.length;
		int n = a[0].length;
		double c[][] = new double[m][n];
		for(int i=0;i<m;i++){
			for(int j=0;j<n;j++){
				c[i][j] = a[i][j]-x;
			}
		}
		return c;
	}
	
	public static double[][] matrixMinusDigit(double[][] a, int x){
		int m = a.length;
		int n = a[0].length;
		double c[][] = new double[m][n];
		for(int i=0;i<m;i++){
			for(int j=0;j<n;j++){
				c[i][j] = a[i][j]-x;
			}
		}
		return c;
	}
	
	public static double[][] matrixMinus(double[][] a, double[][] b) throws Exception{
		int m = a.length;
		int n = a[0].length;
		int m1 = b.length;
		int n1 = b[0].length;
		if(m!=m1 || n!=n1){
			throw new Exception("Illegal Matrix Minus operation;\nMatrix dimensions must agree\n");
		}
		double c[][] = new double[m][n];
		for(int i=0; i <a.length; i++){
			for(int j=0; j<a[i].length; j++){
				c[i][j] = a[i][j] - b[i][j];
			}
		}
		return c;
	}
	
    public static int[][] matrixTimes(int[][] a, int[][] b) throws Exception {
        int m = a.length;
        int n = b[0].length;
        int w = a[0].length;
        int wb = b.length;
        if(w!=wb){
        	throw new Exception("Illegal Matrix Times operator;\n Inner matrix dimensions must agree\n");
        }
        int[][] c = new int[m][n];
        for (int i = 0; i < m; i++)
            for (int j = 0; j < n; j++)
                for (int k = 0; k < w; k++)
                    c[i][j] += a[i][k] * b[k][j];
        return c;
    }
    
    public static double[][] matrixTimes(double[][] a, double[][] b) throws Exception {
        int m = a.length;
        int n = b[0].length;
        int w = a[0].length;
        int wb = b.length;
        if(w!=wb){
        	throw new Exception("Illegal Matrix Times operator;\n Inner matrix dimensions must agree\n");
        }
        double[][] c = new double[m][n];
        for (int i = 0; i < m; i++)
            for (int j = 0; j < n; j++)
                for (int k = 0; k < w; k++)
                    c[i][j] += a[i][k] * b[k][j];
        return c;
    }
    
    public static int[][] matrixTrans(int[][] a){
    	int m = a.length;
    	int n = a[0].length;
    	int b[][] = new int[n][m];
    	for(int i=0; i<m; i++){
    		for(int j=0; j<n ; j++){
    			b[j][i] = a[i][j]; 
    		}
    	}
    	return b;
    }
    
    public static double[][] matrixTrans(double[][] a){
    	int m = a.length;
    	int n = a[0].length;
    	double b[][] = new double[n][m];
    	for(int i=0; i<m; i++){
    		for(int j=0; j<n ; j++){
    			b[j][i] = a[i][j]; 
    		}
    	}
    	return b;
    }
    
    public static double[][] getDY(double[][] a, int h, int v) {  
        int m = a.length;  
        int n = a[0].length;  
        double[][] c = new double[m - 1][n - 1];  
  
        for (int i = 0; i < c.length; i++) {  
  
            if (i < h - 1) {  
                for (int j = 0; j < c[i].length; j++) {  
                    if (j < v - 1) {  
                        c[i][j] = a[i][j];  
                    } else {  
                        c[i][j] = a[i][j + 1];  
                    }  
                }  
            } else {  
                for (int j = 0; j < c[i].length; j++) {  
                    if (j < v - 1) {  
                        c[i][j] = a[i + 1][j];  
                    } else {  
                        c[i][j] = a[i + 1][j + 1];  
                    }  
                }  
  
            }  
            
        } 
        return c;
    }
    
    public static int[][] getDY(int[][] a, int h, int v) {  
        int m = a.length;  
        int n = a[0].length;  
        int[][] c = new int[m - 1][n - 1];  
  
        for (int i = 0; i < c.length; i++) {  
  
            if (i < h - 1) {  
                for (int j = 0; j < c[i].length; j++) {  
                    if (j < v - 1) {  
                        c[i][j] = a[i][j];  
                    } else {  
                        c[i][j] = a[i][j + 1];  
                    }  
                }  
            } else {  
                for (int j = 0; j < c[i].length; j++) {  
                    if (j < v - 1) {  
                        c[i][j] = a[i + 1][j];  
                    } else {  
                        c[i][j] = a[i + 1][j + 1];  
                    }  
                }  
  
            }  
            
        } 
        return c;
    }
    
    public static double matrixDet(double[][] a) throws Exception{
    	 if (a.length == 1){
    		 return a[0][0];
    	 }
    	
    	 if (a.length == 2) {  
             return a[0][0] * a[1][1] - a[0][1] * a[1][0];  
         }  
    	
    	double total = 0;  
        int m = a.length; 
        int n = a[0].length;
        if(m!=n){
        	throw new Exception("Illegal Matrix Determinant operation;\nMatrix must be square;\n");
        }
        double[] nums = new double[m];  
  
        for (int i = 0; i < m; i++) {  
        	double value = matrixDet(getDY(a, 1, i + 1));
 
        	if (i % 2 == 0) {  
                nums[i] = a[0][i] * value;  
            } else {  
                nums[i] = -a[0][i] * value;  
            }  
        }  
        for (int i = 0; i < m; i++) {  
            total += nums[i];  
        }  
        return total;  
    }
    
    public static int matrixDet(int[][] a) throws Exception{
   	 if (a.length == 1){
		 return a[0][0];
	 }
   	 if (a.length == 2) {  
            return a[0][0] * a[1][1] - a[0][1] * a[1][0];  
        }  
   	
   	int total = 0;  
       int m = a.length;  
       int n = a[0].length;
       if(m!=n){
       	throw new Exception("Illegal Matrix Determinant operation;\nMatrix must be square;\n");
       }
       int[] nums = new int[m];  
 
       for (int i = 0; i < m; i++) {  
       	int value = matrixDet(getDY(a, 1, i + 1));

       	if (i % 2 == 0) {  
               nums[i] = a[0][i] * value;  
           } else {  
               nums[i] = -a[0][i] * value;  
           }  
       }  
       for (int i = 0; i < m; i++) {  
           total += nums[i];  
       }  
       return total;  
   }
    
    public static double[][] matrixInverse(int[][] data)throws Exception{ 
    	int m = data.length;
    	int n = data[0].length;
        if(m!=n){
        	throw new Exception("Illegal Matrix Inverse operation;\nMatrix must be square;\n");
        }
    	
    	double[][] a = new double[m][n];
    	for (int i=0; i<m ; i++){
    		for(int j=0; j<n; j++){
    			a[i][j] = (double) data[i][j];
    		}
    	}
    	return matrixInverse(a);
    }
    
    public static double[][] matrixInverse(double[][] data) throws Exception{ 
        int m = data.length;  
        int n = data[0].length;
        if(m!=n){
        	throw new Exception("Illegal Matrix Inverse operation;\nMatrix must be square;\n");
        }
    	
    	
        double A = matrixDet(data);  
        double[][] newData = new double[data.length][data.length];  
  
        for (int i = 0; i < data.length; i++) {  
            for (int j = 0; j < data.length; j++) {  
                double num;  
                if ((i + j) % 2 == 0) {  
                	double[][] c = getDY(data,i+1,j+1);
                    num = matrixDet(c);  
                } else {  
                    num = -matrixDet(getDY(data, i + 1, j + 1));  
                }  
  
                newData[i][j] = num / A;  
            }  
        }  
        newData = matrixTrans(newData);  
        return newData;  
    } 
    
    public static double[][] matrixConv(double[][] a, double[][] b){
    	int ma = a.length;
    	int na = a[0].length;
    	int mb = b.length;
    	int nb = b[0].length;
    	double c[][] = new double[ma+mb-1][na+nb-1];
    	for (int i=0; i < ma+mb-1; i++){
    		for (int j =0; j< na+nb-1; j++){
    			double sum = 0;
    			for(int m=0; m< ma; m++){
    				for(int n=0; n<na;n++){
    					double t = 0;
    					if((i-m)>=0&&(i-m)<mb&&(j-n)>=0&&(j-n)<nb)
                            sum+=a[m][n]*b[i-m][j-n];
    				}
    			}
    			c[i][j] = sum;
    		}
    	}
    	return c;
    }
    
    public static int[][] matrixConv(int[][] a, int[][] b){
    	int ma = a.length;
    	int na = a[0].length;
    	int mb = b.length;
    	int nb = b[0].length;
    	int c[][] = new int[ma+mb-1][na+nb-1];
    	for (int i=0; i < ma+mb-1; i++){
    		for (int j =0; j< na+nb-1; j++){
    			int sum = 0;
    			for(int m=0; m< ma; m++){
    				for(int n=0; n<na;n++){
    					if((i-m)>=0&&(i-m)<mb&&(j-n)>=0&&(j-n)<nb)
                            sum+=a[m][n]*b[i-m][j-n];
    				}
    			}
    			c[i][j] = sum;
    		}
    	}
    	return c;
    }

    public static int matrixTrace(int[][] a) throws Exception{
    	int n = a.length;
    	int sum = 0;
        int m = a[0].length;
        if(m!=n){
        	throw new Exception("Illegal Matrix Trace operation;\nMatrix must be square;\n");
        }
    	for (int i=0; i < n; i++){
    		sum = sum+a[i][i];
    	}
    	return sum;
    }
    
    public static double matrixTrace(double[][] a) throws Exception{
    	int n = a.length;
    	double sum = 0;
        int m = a[0].length;
        if(m!=n){
        	throw new Exception("Illegal Matrix Trace operation;\nMatrix must be square;\n");
        }
    	for (int i=0; i < n; i++){
    		sum = sum+a[i][i];
    	}
    	return sum;
    }
    
    public static int[][] subMat(int[][] a, int x1, int y1, int x2, int y2) throws Exception{
    	if(x1>x2 || y1>y2){
    		throw new Exception("Illegal subMat operation;\nx1,y1 shoule be less than x2,y2");
    	}
    	int l = a.length;
    	int k = a[0].length;
    	if(x1<0||x1>l-1||y1<0||y1>k-1||x2<0||x2>l-1||y2<0||y2>k-1){
    		throw new Exception("Illegal subMat operaiton;\n x1,y1,x2,y2 out of Matrix bound");
    	}
    	int m = Math.abs(x1-x2)+1;
    	int n = Math.abs(y1-y2)+1;
    	int c[][] = new int[m][n];
    	for (int i=0; i < m; i++){
    		for(int j=0; j<n ; j++){
    			c[i][j] = a[x1+i][y1+j];
    		}
    	}
    	return c;
    }
    
    public static double[][] subMat(double[][] a, int x1, int y1, int x2, int y2) throws Exception{
    	if(x1>x2 || y1>y2){
    		throw new Exception("Illegal subMat operation;\nx1,y1 shoule be less than x2,y2");
    	}
    	int l = a.length;
    	int k = a[0].length;
    	if(x1<0||x1>l-1||y1<0||y1>k-1||x2<0||x2>l-1||y2<0||y2>k-1){
    		throw new Exception("Illegal subMat operaiton;\n x1,y1,x2,y2 out of Matrix bound");
    	}
    	int m = Math.abs(x1-x2)+1;
    	int n = Math.abs(y1-y2)+1;
    	double c[][] = new double[m][n];
    	for (int i=0; i < m; i++){
    		for(int j=0; j<n ; j++){
    			c[i][j] = a[x1+i][y1+j];
    		}
    	}
    	return c;
    }
    
    public static int matrixSum(int[][] a){
    	int m = a.length;
    	int n = a[0].length;
    	int sum = 0;
    	for(int i=0;i<m; i++){
    		for(int j=0;j<n;j++){
    			sum += a[i][j];
    		}
    	}
    	return sum;
    }
    
    public static double matrixSum(double[][] a){
    	int m = a.length;
    	int n = a[0].length;
    	double sum = 0;
    	for(int i=0;i<m; i++){
    		for(int j=0;j<n;j++){
    			sum += a[i][j];
    		}
    	}
    	return sum;
    }
    
    public static int areaSum(int[][] a, int x1, int y1, int x2, int y2) throws Exception{
    	if(x1>x2 || y1>y2){
    		throw new Exception("Illegal areaSum operation;\nx1,y1 shoule be less than x2,y2");
    	}
    	int l = a.length;
    	int k = a[0].length;
    	if(x1<0||x1>l-1||y1<0||y1>k-1||x2<0||x2>l-1||y2<0||y2>k-1){
    		throw new Exception("Illegal areaSum operaiton;\n x1,y1,x2,y2 out of Matrix bound");
    	}
    	int[][] c = subMat(a, x1, y1, x2, y2);
    	int sum = matrixSum(c);
    	return sum;
    }
    
    public static double areaSum(double[][] a, int x1, int y1, int x2, int y2) throws Exception{
    	if(x1>x2 || y1>y2){
    		throw new Exception("Illegal areaSum operation;\nx1,y1 shoule be less than x2,y2");
    	}
    	int l = a.length;
    	int k = a[0].length;
    	if(x1<0||x1>l-1||y1<0||y1>k-1||x2<0||x2>l-1||y2<0||y2>k-1){
    		throw new Exception("Illegal areaSum operaiton;\n x1,y1,x2,y2 out of Matrix bound");
    	}

    	double[][] c = subMat(a, x1, y1, x2, y2);
    	double sum = matrixSum(c);
    	return sum;
    }

    public static int[][] intMatrixInit(String e) throws Exception
    {
        int x;
        int y;
        String[] rows=e.split("[;]");
        x = rows.length;
        //System.out.println("x= " + x);
        y = rows[0].split("[,]").length;
        //System.out.println("y= " + y);
        int[][] newM= new int[x][y];
        for ( int i=0; i< x; i++)
        {
            
            String[] integers = rows[i].split("[,]");
            if(integers.length != y)
                throw new Exception("Inconsistent Number of Elements Per Row");
            for (int j=0; j< y ; j++)
                newM[i][j]=Integer.parseInt(integers[j]);
        }
        return newM;

    }

    public static double[][] floatMatrixInit(String e) throws Exception
    {
        int x;
        int y;
        String[] rows=e.split("[;]");
        x = rows.length;
    //    System.out.println("x= " + x);
        y = rows[0].split("[,]").length;
      //  System.out.println("y= " + y);
        double[][] newM= new double[x][y];
        for ( int i=0; i< x; i++)
        {
            
            String[] doubles = rows[i].split("[,]");
            if(doubles.length != y)
                throw new Exception("Inconsistent Number of Elements Per Row");
            for (int j=0; j< y ; j++)
                newM[i][j]=Double.parseDouble(doubles[j]);
        }
        return newM;

    }

  /*  	public static void main(String[] args) throws Exception {
	//	int a[][] = {{1,2,3},{1,2,3},{1,2,3}};
	//	int b[][] = { { 1, 2, 3 }, { 4, 5, 6 }, {3,4,5}};
		Matrix m = new Matrix();
	//	int c[][] = m.matrixPlus(a, b);
//		double a[][] = {{1,0,1},{2,1,0},{-3,2,-5}};
		double a[][] = {{1,2},{1,2},{1,2}};
		
/*		double[][] a = { { 1, 2, 3, 4, 5 },   
                { 2, 3, 4, 5, 1 },  
                { 3, 4, 5, 1, 2 },   
                { 4, 5, 1, 2, 3 },   
                { 5, 1, 2, 3, 4 },  

};  
		double b[][] = {{7,8}};
//		int d[][] = {{1,0,1},{2,1,0},{-3,2,-5}};
		double e[][] = {{7},{8}};
	//	int b[][] = {{1,1,1},{2,2,2}};
	//	int c[][] = m.matrixTimes(b, a);
		m.show(a);
//		m.show(b);
//		double c[][] = m.subMat(a, 0, 0, 1, 0);
//		double c[][] = m.subMat(a,2, 2, 4, 4);
//		double c[][] = m.matrixInverse(a)
//		System.out.println(m.matrixDet(d));
//		double c[][] = m.getDY(a, 1, 1);
		double c[][] = m.matrixTimes(a, b);
//		double c[][] = m.matrixInverse(a);
		//		int k[][] = m.matrixMinus(d,e);
//		int b = m.matrixDet(a);
		System.out.println(c);
		m.show(c);
//		m.show(k);
		

	} */
}





