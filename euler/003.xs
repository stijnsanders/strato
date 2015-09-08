euler.problem3

/*
https://projecteuler.net/problem=3
Largest prime factor
The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?
*/


pcount=1000;
{
  //literal value 'cast' to u64 here to avoid the value being clipped to the system-word-size (e.g. 32 bit)
	:a:=600851475143:u64;

  //it's a bit of 'blunt force' to first calculate _all_ primes, and then use it to check factors,
	//but I'm also using this as a test to see how a large array is handled (in stack memory!)
	//also mathematically, pcount should be at least the square root of the start value of `a`

	//calculate the first x primes
	:x:=1;
	:y:=0;
	:z:=1;

	:primes,next:number[pcount];
	primes[0]:=2;
	next[0]:=2;

	&(z<pcount){
		x+=2;
		y:=0;
		&(y<z&&next[y]<>x){
			next[y]<x next[y]+=primes[y];;
			next[y]<>x y++;;
		}
		y==z{
			primes[y]:=x;
			next[y]:=x;
			z++;
		}
	}

  //find largest factor
	&(x:=pcount-1;x<>0&&a%primes[x]:u64!=0:u64;x--);

	x==0
		__writeln("no factor found");
		__writeln(primes[x]:string);
}
