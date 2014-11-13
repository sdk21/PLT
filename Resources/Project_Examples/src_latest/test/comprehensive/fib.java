public class MatCab {

public static int fib(int n ) throws Exception
{
int tmp;

{
if (n==1)
{return 1;
}if (n==2)
{return 1;
}if (n>=3)
{{
tmp = (fib((n-1))+fib((n-2)));
return tmp;

}
}return 0;

}
}

public static void main(String[] args) throws Exception
{

{
System.out.println(fib(5));

}
}

}