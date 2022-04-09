void main()
{
  int x;     
  cin >> x; 
  
  int  y;
  real z;
  y = 3^2;  // 3 squared:
  z = x-y;
  x = x*y;

  cout << "x: ";
  cout << x;
  cout << endl;

  y = x^x;
  y = 123/456;
  z = 123.0/456.0;

  cout << "y: ";
  cout << y;
  cout << endl;

  if (z <= 1.0)
    cout << "z is 1.0";
  else if ("apple" > "pear")
    cout << "something is wrong";
  else if (true != false)
    cout << "that's better";
  else if (123 < 456)
    cout << "also good";
  else if (0.5 >= 0.4)
    cout << "+1";
  else
    cout << z;
 
  cout << endl;
}
