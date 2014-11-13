public class MatCab {

public static void main(String[] args) throws Exception
{
int i;
int j;
double tmp;
double b;
double[][] c;
double[][] d;
double[][] e;

{
c = new double[3][3];
c = Matrix.floatMatrixInit("1,2,3;4,5,6;9,8,7");
for (i = 0; i<3; i = (i+1))
{
for (j = 0; j<3; j = (j+1))
{
tmp = c[i][j];
c[i][j] = tmp/10;

}

}
e = new double[3][2];
e = Matrix.floatMatrixInit("6,4;2,9;5,1");
e = Matrix.matrixTimes(c,e);
System.out.println(e[1][1]);
System.out.println(e[2][1]);

}
}

}