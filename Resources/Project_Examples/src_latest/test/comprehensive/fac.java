public class MatCab {

public static int fac(int n ) throws Exception
{

{
if (n==0||n==1)
{return 1;
}
else
{return (fac((n-1))*n);
}
}
}

public static void main(String[] args) throws Exception
{

{
System.out.println(fac(6));

}
}

}