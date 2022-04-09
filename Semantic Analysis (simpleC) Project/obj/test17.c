void main()
{
  int x;     // define x:
  cin >> x;  // input a value:
  
  int  y;
  real z;
  y = 3^2;   // 3 squared:
  z = x-y;
  z = x+y;
  z = 4*y;
  z = x/2;

  int var;

  z = 3.14;
  z = 1.;

  z = 123;
  z = z+z;
  z = z-1.2;
  z = 3.5*10.9;
  z = 100.0/z;
  z = 3.2^4.5;

  cout << "x: ";
  cout << x;
  cout << endl;

  cout << "y: ";
  cout << y;
  cout << endl;

  var = 123;

  if (z < 0.0)
    cout << "z is negative";
  else
    cout << "whatever makes sense";
 
  cout << endl;
}
