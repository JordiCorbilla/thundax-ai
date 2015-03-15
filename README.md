A series of A.I. Algorithms coded in Delphi
==============
**thundax-ai** brings a series of **A.I** (Artificial Intelligence) algorithms (**machine learning**) including examples and other interesting algorithms for **pattern recognition**, **graph theory** and **image processing**.
--------------
**Available machine learning algorithms:**
  - **Decision tree, with pruning**. 
  - **fuzzy c-means**  

It also includes a basic **Matrix** class which allows you to perform the following operations using simple mechanisms to instantiate them using chaining methods, interfaces, generics, bindings, etc getting rid of memory leaks:
  - **Addition**
```delphi
        [1 1 1]   [1 1 1]   [2 2 2]
        [1 1 1] + [1 1 1] = [2 2 2]
        [1 1 1]   [1 1 1]   [2 2 2]
```
```delphi
var
  matrix1, matrix2: IMatrix;
begin
  matrix1 := TBind.New([TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1])]).matrix;
  matrix2 := TBind.New([TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1])]).matrix;
  Display(matrix1.Add(matrix2).ToString);
```
  - **Subtraction**
```delphi
        [1 1 1]   [1 1 1]   [0 0 0]
        [1 1 1] - [1 1 1] = [0 0 0]
        [1 1 1]   [1 1 1]   [0 0 0]
```
```delphi
var
  matrix1, matrix2: IMatrix;
begin
  matrix1 := TBind.New([TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1])]).matrix;
  matrix2 := TBind.New([TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1])]).matrix;
  Display(matrix1.Subtract(matrix2).ToString);
```
  - **Multiplication**
```delphi
        [1 1 1]   [1 1 1]   [3 3 3]
        [1 1 1] * [1 1 1] = [3 3 3]
        [1 1 1]   [1 1 1]   [3 3 3]
```
```delphi
var
  matrix1, matrix2: IMatrix;
begin
  matrix1 := TBind.New([TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1])]).matrix;
  matrix2 := TBind.New([TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1])]).matrix;
  Display(matrix1.Multiply(matrix2).ToString);
```
  - **Transposed**
```delphi
        [1 1 1]    [1 2 3]
        [2 2 2] -> [1 2 3]
        [3 3 3]    [1 2 3]
```
```delphi
var
  matrix: IMatrix;
begin
  matrix := TBind.New([TColumn.create([1, 2, 3]), TColumn.create([1, 2, 3]), TColumn.create([1, 2, 3])]).matrix;
  Display(matrix.Transpose.ToString);
```
  - **Mean**
```delphi
        [1 1 1]    [-1 -1 -1]
        [2 2 2] -> [ 0  0  0]
        [3 3 3]    [ 1  1  1]

   ^x = [2 2 2]
```
```delphi
var
  matrix: IMatrix;
begin
  matrix := TBind.New([TColumn.create([1, 2, 3]), TColumn.create([1, 2, 3]), TColumn.create([1, 2, 3])]).matrix;
  Display(matrix.Mean.ToString);
```
  - **Covariance**
```delphi
        [1 8 12]    [ 9 -3   -6]
        [7 6  8] -> [-3  6.3  7.3]
        [4 3  6]    [-6  7.3  9.3]
```
```delphi
var
  matrix: IMatrix;
begin
  matrix := TBind.New([TColumn.create([1, 7, 4]), TColumn.create([8, 6, 3]), TColumn.create([12, 8, 6])]).matrix;
  Display(matrix.Covariance.ToString);
```
  ![](https://thundax-ai.googlecode.com/files/Covariance.png)
  - **Distance**
```delphi
        [1 8 12]    [dist(0,1)          ]        [ 7.48     ]
        [7 6  8] -> [dist(0,2) dist(1,2)]        [ 8.37  4.7]
        [4 3  6]    
```
```delphi
var
  matrix: IMatrix;
begin
  matrix := TBind.New([TColumn.create([1, 7, 4]), TColumn.create([8, 6, 3]), TColumn.create([12, 8, 6])]).matrix;
  Display(matrix.Distance.ToString);
```

**More complex examples:**
```delphi
Matrix:
 1  1 -1.645497224  2.597414305  1.89738712  
 0  1 -0.645497224  1.792842914 -0.817753946 
 1  1  1.290994449 -0.697614305 -0.808876973 
 1  0 -0.845497224 -0.797614305  0.220626409 
 0  1  1.290994449 -0.597614305 -0.208876973 
 0  1 -0.645557224 -0.797614305 -0.882505637 

Mean:
 0.5  0.166666666666667 -1.44548722433333   2.3474476385  1.99738712  
-0.5  0.166666666666667 -0.445487224333333  1.5428762475 -0.717753946 
 0.5  0.166666666666667  1.49100444866667  -0.9475809715 -0.708876973 
 0.5 -0.833333333333333 -0.645487224333333 -1.0475809715  0.320626409 
-0.5  0.166666666666667  1.49100444866667  -0.8475809715 -0.108876973 
-0.5  0.166666666666667 -0.445547224333333 -1.0475809715 -0.782505637 

Covariance:
 0.3          -0.1               -0.119994           0.0704571391       0.3218273112      
-0.1           0.166666666666667  0.129097444866667  0.2095161943      -0.0641252818      
-0.119994      0.129097444866667  1.46984936010627  -1.12283694872954  -0.729007831690734 
 0.0704571391  0.2095161943      -1.12283694872954   2.34042650301889   0.965842486972813 
 0.3218273112 -0.0641252818      -0.729007831690734  0.965842486972813  1.14684063166495  

Distance:
 3.1653319149017   0                 0                 0                 0                
 5.17726397118961  3.30944949499417  0                 0                 0                
 3.99721725012186  3.13507613386328  2.57574724737265  0                 0                
 4.92620945492353  3.13608304796241  1.17046999107196  2.60558435364273  0                
 4.61019554972884  2.59126636773539  2.18303769146607  1.80468177652243  2.06009901683427 

```
```delphi
var
  matrix: IMatrix;
  c1, c2, c3, c4, c5: IColumn;
  bind: IBind;
begin
  c1 := TColumn.create([1, 0, 1, 1, 0, 0]);
  c2 := TColumn.create([1, 1, 1, 0, 1, 1]);
  c3 := TColumn.create([-1.645497224, -0.645497224, 1.290994449, -0.845497224, 1.290994449, -0.645557224]);
  c4 := TColumn.create([2.597414305, 1.792842914, -0.697614305, -0.797614305, -0.597614305, -0.797614305]);
  c5 := TColumn.create([1.89738712, -0.817753946, -0.808876973, 0.220626409, -0.208876973, -0.882505637]);

  bind := TBind.New([c1, c2, c3, c4, c5]);
  matrix := bind.matrix;

  Display('Matrix:');
  Display(matrix.ToString);
  Display('Mean:');
  Display(matrix.Mean.ToString);
  Display('Covariance:');
  Display(matrix.Covariance.ToString);
  Display('Distance:');
  Display(matrix.Distance.ToString);
```

**Goodness of fit for Attributes.**
  ![](https://thundax-ai.googlecode.com/files/GoodnessOfFit.png)

**[Spanning Tree](http://en.wikipedia.org/wiki/Spanning_tree):**
  ![](https://thundax-ai.googlecode.com/files/SpanningTree.png)

**[Convex Hull](http://en.wikipedia.org/wiki/Convex_hull):**
  ![](https://thundax-ai.googlecode.com/files/ConvexHull.png)

**Some examples can be found here:**

 - [thundax.ai.testGoodnessOfFitAttributes.exe](https://app.box.com/s/yk452fla6jocrlnisw728r9nn2dufm3w)
 - [thundax.ai.testMatrix.exe](https://app.box.com/s/grhhdrgp3g9vywvx2pemjur4chxc85j1)
 - [thundax.ai.gui.exe](https://app.box.com/s/unq4xlol712wt74v0xczgmfparvihx8f)
 - [thundax.ai.testConvexHull.exe](https://app.box.com/s/02kr44l59zb9avl1l18fd1lakebqb8wd)
 - [thundax.ai.testSpanningTree.exe](https://app.box.com/s/x7565m5iep8uergbrk4909ntftpp643e)

## Sponsors
No sponsors yet! Will you be the first?

[![PayPayl donate button](https://img.shields.io/badge/paypal-donate-yellow.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=L5FCF6LX5C9AW "Donate once-off to this project using Paypal")
