years=0.25 
n1=10
n=15
volatility=0.3 
R=1.02
divident=1.01
u = exp( volatility * sqrt(years/n))
d = 1/u
q = ( exp( (R-divident)*years/n) - d ) / ( u - d );
R = exp( (R-1) * years/n)

spot=100
strike=100


prices = vector( "list", n + 1 )
call_payofs = vector( "list", n  + 1)
put_payofs = vector( "list", n + 1 )
put_early_excercise = numeric(0)
call_early_excercise = numeric(0)

r = c(1);
prices[[1]] = c( spot * r ) 
call_payofs[[1]] = put_payofs[[1]] = 0
for( i in 2:(n+1) ) {
  r = c( r * u, tail( r, 1 ) * d )	
  price = spot * r;
  prices[[i]] = price;
  call_payof = price - strike;
  call_payof[ call_payof < 0 ] = 0;
  call_payofs[[i]] = call_payof
  put_payof = strike - price;
  put_payof[ put_payof < 0 ] = 0;
  put_payofs[[i]]  = put_payof
}


#manually propagate level by level down to selection point
put_price = put_payofs[[n+1]]
print( put_price )
for( i in n:(n1+1))
{
  put_price = rep( put_price, c(1,rep(2,length(put_price)-2),1))
  put_price = matrix( put_price, ncol = 2, byrow=T)
  put_price = put_price %*% c( q, 1-q ) / R
  print(put_price)
}


call_price = call_payofs[[n+1]]
print( call_price )
for( i in n:(n1+1))
{
  call_price = rep( call_price, c(1,rep(2,length(call_price)-2),1))
  call_price = matrix( call_price, ncol = 2, byrow=T)
  call_price = call_price %*% c( q, 1-q ) / R
  print(call_price)
}

chooser_price = pmax( put_price, call_price )
print(chooser_price)
print(q)
print(R)
print(c( q, 1-q ) / R)
for( i in n1:1)
{
  chooser_price = rep( chooser_price, c(1,rep(2,length(chooser_price)-2),1))
  print(chooser_price)
  cat("\n")
  chooser_price = matrix( chooser_price, ncol = 2, byrow=T)
  print(chooser_price)
  cat("\n")
  chooser_price = chooser_price %*% c( q, 1-q ) / R
  print(chooser_price)
  cat("\n")
}
